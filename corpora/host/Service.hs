{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Service (Service, APIError(..), openService, startService, closeService, serviceRoot, attachEndpoint, dispatch, snapshot, corporaIdentity) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Network.URI (unEscapeString)
import System.Directory
import System.Environment
import System.FileLock
import System.FilePath
import System.Info
#ifndef mingw32_HOST_OS
import System.Posix.Files (setFileMode)
#endif
import Types
import qualified Store as DB
import Population
import Workspace
import Memo
import qualified Management as Mgt
import qualified Execution as E
import qualified Delegation as D
import qualified Routines as R
import qualified Groups as G
import qualified GroupChat as GC
import qualified GroupWork as GW

openService :: FilePath -> IO Service
openService path = do
 existed <- doesDirectoryExist path
 createDirectoryIfMissing True path
#ifndef mingw32_HOST_OS
 unless existed (setFileMode path 0o700)
#endif
 root <- canonicalizePath path
 repo <- lookupEnv "SAPIENS_REPOSITORY" >>= maybe getCurrentDirectory pure >>= canonicalizePath
 lock <- tryLockFile (root </> "host.lock") Exclusive >>= maybe (failure 409 "Another Sapiens4 server is using this data directory") pure
 database <- (DB.openDatabase root >>= newMVar . Just) `onException` unlockFile lock
 let initialize = do
      guard <- newMVar ()
      stopping <- newIORef False
      worker <- newMVar Nothing
      runs <- newMVar M.empty
      lease <- newIORef Nothing
      endpoint <- newIORef ""
      pulse <- newIORef empty
      actors <- newMVar M.empty
      let s=Service root repo guard lock stopping worker runs lease endpoint pulse actors database
      DB.initialize s
      let identityPath=root </> "corpora-identity.json"
      exists <- doesFileExist identityPath
      unless exists $ do i<-UUID.toText <$> UUID.nextRandom; writeJSON identityPath (obj [("version",num 1),("corpora_id",str i)])
      identity <- readJSON identityPath empty
      require (get identity "version"==num 1 && maybe False ((==at identity "corpora_id") . UUID.toText) (UUID.fromText (at identity "corpora_id"))) 409 "Invalid CORPORA identity. Restore corpora-identity.json from backup."
      rows <- DB.agents s
      when (null rows) $ void (createAgent s (obj [("name",str "ChiefSapi"),("role",str "Head of Corpora")]))
      repairAvatars s
      agents <- DB.agents s
      mapM_ (ensureAgent s) agents
      chief <- DB.mainAgent s
      d <- directory s
      saveDirectory s $ foldl (\dir row -> let a=at row "id"; entry=get dir a in put a (put "parent" (if a==chief then Null else get entry "parent" `fallback` str chief) entry) dir) d agents
      mapM_ (E.recover s . (`at` "id")) agents
      prefs <- DB.singleton s DB.Preferences
      let panel=at prefs "panel"; updated=if panel=="mindmap" then put "panel" (str "notes") prefs else if panel `elem` ["cron","log"] then put "panel" (str "chat") prefs else prefs
      DB.saveSingleton s DB.Preferences (remove ["work_views"] updated)
      migrateWorkspaces s
      savedPulse <- DB.singleton s DB.SystemPulse
      identityPulse <- if has savedPulse "pulse" then pure (at savedPulse "pulse") else ("pulse_" <>) <$> newId
      cfg <- corporaSettings s
      writeIORef pulse (obj [("id",str identityPulse),("running",Bool (corporaStatus cfg==Active)),("numbers",merge (obj [(k,num 0) | k<-["bpm120","bpm60","bph60"]]) (get savedPulse "numbers"))])
      R.initializeRoutines s
      D.reconcileWorks s
      D.pendingWorks s
      GC.reconcileGroups s
      GC.dispatchGroups s
      DB.record s [chief] "corpora" "system.started" empty "system" Nothing
      pure s
 initialize `onException` (withMVar database (mapM_ DB.closeConnection) `finally` unlockFile lock)
attachEndpoint :: Service -> Text -> IO ()
attachEndpoint s url = locked s $ do
 writeIORef (serviceEndpoint s) url
 DB.agents s >>= mapM_ (Mgt.prepare s . (`at` "id"))
startService :: Service -> IO ()
startService = E.startExecution
closeService :: Service -> IO ()
closeService s = do
 already <- atomicModifyIORef' (serviceStopping s) (\old -> (True,old))
 unless already $ do
  E.stopRuns s
  worker <- swapMVar (serviceWorker s) Nothing
  mapM_ (void . waitCatch) worker
  let awaitRuns = do runs<-readMVar (serviceRuns s); unless (M.null runs) (threadDelay 50000 >> awaitRuns)
  awaitRuns
  (do chief <- DB.mainAgent s
      DB.record s [chief] "corpora" "system.stopped" empty "system" Nothing)
   `finally` (DB.closeDatabase s `finally` unlockFile (serviceFileLock s))
snapshot :: Service -> IO Value
snapshot s = locked s (snapshotUnlocked s)
snapshotUnlocked :: Service -> IO Value
snapshotUnlocked s = do
 rows <- DB.agents s
 mapM_ (DB.project s . (`at` "id")) rows
 GC.reconcileGroups s
 turns <- DB.turns s
 prefs <- DB.singleton s DB.Preferences
 corpora <- corporaIdentity s
 groups <- G.snapshotGroups s
 routines <- DB.values s DB.ScheduledRoutines
 creation <- prompt s "creation-request"
 agentRows <- forM rows $ \row -> put "retired" . Bool <$> retired s (at row "id") <*> pure row
 owner <- maybe Null (toJSON . fst) <$> readIORef (serviceLease s)
 built <- blindly s >>= doesFileExist
 pulses <- DB.recentPulses s
 events <- DB.events s
 pulse <- readIORef (servicePulse s)
 chief <- DB.mainAgent s
 works <- D.readWorks s
 drafts <- forM (pairs (get prefs "attachment_drafts")) $ \(a,ids) -> (a,) . arr . filter (\x->get x "id" `elem` list ids) <$> DB.attachments s a
 dir <- directory s
 orchestration <- forM rows $ \row -> do
  let a=at row "id"
  policy <- executionSettings s a
  notes <- memoMetadata (workspaceRoot s a)
  pure (a,obj [("manager",get (get dir a) "parent"),("execution",toJSON policy),("notes",notes)])
 runs <- M.elems <$> readMVar (serviceRuns s)
 activity <- forM runs $ \run -> do
  value <- readIORef (runActivity run)
  stopping <- readIORef (runCancel run)
  pure (run,merge value (obj [("turn",str (runIdText (runId run))),("stopping",Bool stopping),("agencyKind",str "chatInput"),("slot",num (runSlot run))]))
 let groupTurns=M.fromList [(at request "id",at g "id") | g<-groups,request<-items g "requests"]
     projected=[maybe t (\gid->put "group" (str gid) t) (M.lookup (at t "id") groupTurns) | t<-turns]
 pure $ obj [("agents",arr agentRows),("turns",arr projected),("preferences",prefs),("corpora",corpora),("groups",arr groups),("routines",arr routines),("creation_template",str (T.strip creation)),("computer",obj [("owner",owner),("built",Bool built),("platform",str (if os=="mingw32" then "win32" else T.pack os))]),("pulses",arr pulses),("events",events),("pulse",pulse),("provider",str "codex"),("activity",obj [(agentIdText (runOwner r),v) | (r,v)<-activity]),("run_activity",obj [(runIdText (runId r),v) | (r,v)<-activity]),("main_agent_id",str chief),("workloads",arr (D.workSummaries works)),("attachment_drafts",obj drafts),("orchestration",obj orchestration)]
savePreferences :: Service -> Value -> IO Value
savePreferences s d = do
 require (all (`elem` ["selected","panel","scope","groupView","panes","drafts","attachment_drafts"]) (keys d)) 400 "Unknown preference field"
 forM_ ["panes","drafts","attachment_drafts"] $ \k -> when (has d k) $ require (case get d k of Object _->True; _->False) 400 (k<>" must be an object")
 rows <- DB.agents s
 groups <- DB.values s DB.Groups
 when (has d "selected") $ require (get d "selected" `elem` map (`get` "id") (rows++groups)) 400 "Unknown selected Sapi"
 when (get d "groupView"/=Null) $ require (get d "groupView" `elem` map (`get` "id") groups) 400 "Unknown Group view"
 require (get d "panel" `fallback` str "chat" `elem` map str ["chat","tasks","notes","work","updates"]) 400 "Unknown panel"
 require (get d "scope" `fallback` str "all" `elem` map str ["all","sapis","groups"]) 400 "Unknown view"
 forM_ (pairs (get d "panes")) $ \(k,v) -> require (k `elem` ["sidebar","chat","workspace"] && (case v of Bool _->True; _->False)) 400 "Invalid panel visibility"
 forM_ (pairs (get d "drafts")) $ \(_,v) -> require (case v of String t->T.length t<=16000; _->False) 400 "Invalid draft"
 forM_ (pairs (get d "attachment_drafts")) $ \(a,ids) -> void (resolveAttachments s a ids False)
 prefs <- DB.singleton s DB.Preferences
 let current=merge prefs d
 DB.saveSingleton s DB.Preferences current
 pure (obj [("saved",Bool True),("preferences",current)])
dispatch :: Service -> Text -> Text -> Maybe Value -> IO (Int,BL.ByteString,Text)
dispatch s method target body = locked s $ do
 let (path,query)=T.breakOn "?" target; parts=filter (not . T.null) (T.splitOn "/" path); write=method `elem` ["POST","PUT"]; d=fromMaybe Null body
 when write $ require (case d of Object _->True; _->False) 400 "Expected a JSON object"
 cfg <- corporaSettings s
 when (write && corporaStatus cfg/=Active && ("/messages" `T.isSuffixOf` path || "/run" `T.isSuffixOf` path || "/turns/" `T.isInfixOf` path)) $ failure 409 "Activate this corpora before starting work"
 let json code action = do value<-action; pure (code,encode value,"application/json; charset=utf-8")
     owner kind a=if kind=="groups" then void (G.getGroup s a False) else void (DB.agent s a)
 case (method,parts) of
  ("GET",["api","health"]) -> json 200 (pure (obj [("status",str "ok"),("provider",str "codex")]))
  ("GET",["api","state"]) -> json 200 (snapshotUnlocked s)
  ("GET",["api","corpora"]) -> json 200 (corporaIdentity s)
  ("GET",["api","corpora","operations",operation]) -> json 200 $ do identity<-corporaIdentity s; pure (obj [("applied",Bool (has (toJSON (corporaOperations cfg)) operation)),("corpora",identity)])
  ("PUT",["api","corpora","lifecycle"]) -> json 200 (Mgt.lifecycle s d)
  ("GET",["api","decision-prompts"]) -> json 200 (obj . (:[]) . ("prompts",) . arr <$> D.templates s)
  ("PUT",["api","decision-prompts",node]) -> json 200 (D.editTemplate s node d)
  ("GET",["api","agents",a,"tasks"]) -> do
   void (DB.agent s a)
   if "format=json" `T.isInfixOf` query then json 200 (D.taskList s a) else do value<-D.workTasks s a True; pure (200,encode value,"application/yaml; charset=utf-8")
  ("GET",["api",kind,a,"notes"]) | kind `elem` ["agents","groups"] -> do
   owner kind a
   let image=lookup "image" [(k,T.drop 1 v) | entry<-T.splitOn "&" (T.drop 1 query),let (k,v)=T.breakOn "=" entry]
   case image of
    Nothing -> json 200 (readMemo (workspaceRoot s a))
    Just name -> do (bytes,mime)<-memoImage (workspaceRoot s a) (T.pack (unEscapeString (T.unpack name))); pure (200,bytes,mime)
  ("GET",["api",kind,a,"browser",tab,"content"]) | kind `elem` ["agents","groups"] -> owner kind a >> json 200 (workspaceText s a tab)
  ("GET",["api","groups"]) -> json 200 (obj . (:[]) . ("groups",) . arr <$> G.snapshotGroups s)
  ("GET",["api","groups",gid]) -> json 200 (G.getGroup s gid False)
  ("POST",["api","groups"]) -> json 201 (G.createGroup s d "admin")
  ("PUT",["api","groups",gid]) -> json 200 (G.updateGroup s gid d "admin")
  ("POST",["api","groups",gid,"messages"]) -> json 202 (GC.submitGroup s gid d "admin")
  ("POST",["api","groups",gid,"tasks"]) -> json 201 (GW.createTask s gid d "admin")
  ("PUT",["api","groups",gid,"tasks",tid]) -> json 200 (GW.updateTask s gid tid d "admin")
  ("POST",["api","groups",gid,"tasks",tid,"run"]) -> json 202 (GW.runTask s gid tid (get d "revision") "admin")
  ("POST",["api","groups",gid,"calls",rid,action]) -> json 200 (GC.actionGroup s gid rid action)
  ("POST",["api","agents"]) -> json 201 $ do row<-createAgent s d; Mgt.prepare s (at row "id"); pure row
  ("PUT",["api","agents",a]) -> json 200 (updateAgent s a d)
  ("POST",["api","agents",a,"messages"]) -> json 202 (E.submitTurn s a d)
  ("POST",["api","agents",a,"control"]) -> json 200 (withActor s a Nothing (Mgt.control s a d))
  ("POST",["api","agents",a,"turns",tid,action]) -> json 200 (E.turnAction s a tid action)
  ("PUT",["api","preferences"]) -> json 200 (savePreferences s d)
  ("POST",["api",kind,a,"attachments"]) | kind `elem` ["agents","groups"] -> owner kind a >> json 201 (createAttachment s a d)
  ("POST",["api",kind,a,"browser"]) | kind `elem` ["agents","groups"] -> owner kind a >> json 200 (workspaceObserved s a d)
  ("POST",["api","groups",gid,"control"]) -> do
   void (G.getGroup s gid True)
   require ("workspace_" `T.isPrefixOf` at d "op") 400 "Only workspace operations are supported here"
   json 200 (workspaceControl s gid (T.drop 10 (at d "op")) d)
  _ -> failure 404 "Not found"
