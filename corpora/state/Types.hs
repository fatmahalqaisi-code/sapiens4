{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Types (module Types, module Domain) where

import Domain

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Typeable
import Data.Word (Word64)
import Database.SQLite.Simple (Connection)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.Directory
import System.FileLock
import System.FilePath
import System.IO
import System.Process
#ifdef mingw32_HOST_OS
import Foreign.C
import System.IO.Error (isPermissionError)
foreign import ccall unsafe "_wopen" syncOpen :: CWString -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "_commit" syncCommit :: CInt -> IO CInt
foreign import ccall unsafe "_close" syncClose :: CInt -> IO CInt
#else
import qualified System.Posix.IO as Posix
import qualified System.Posix.Unistd as Posix
#endif


data APIError = APIError Int Text deriving (Show, Typeable)
instance Exception APIError

data RunFailure = RunFailure Text [Value] deriving (Show,Typeable)
instance Exception RunFailure

data Run = Run { runOwner :: AgentId, runId :: RunId, runSlot :: Int, runCancel :: IORef Bool, runProcess :: MVar (Maybe ProcessHandle), runActivity :: IORef Value, runDeadline :: Word64 }
data Service = Service { serviceRoot :: FilePath, repositoryRoot :: FilePath, serviceGuard :: MVar (), serviceFileLock :: FileLock, serviceStopping :: IORef Bool, serviceWorker :: MVar (Maybe (Async ())), serviceRuns :: MVar (Map RunId Run), serviceLease :: IORef (Maybe (AgentId, RunId)), serviceEndpoint :: IORef Text, servicePulse :: IORef Value, serviceActors :: MVar (Map ThreadId (Text,Maybe Text)), serviceDatabase :: MVar (Maybe Connection) }

decodeValue :: FromJSON a => Int -> Value -> IO a
decodeValue code = parseValue code parseJSON
parseValue :: Int -> (Value -> Parser a) -> Value -> IO a
parseValue code parser = either (failure code . T.pack) pure . parseEither parser
field :: FromJSON a => Int -> Value -> Text -> IO a
field code value key = decodeValue code (get value key)

failure :: Int -> Text -> IO a
failure n = throwIO . APIError n
require :: Bool -> Int -> Text -> IO ()
require p n m = unless p (failure n m)
textField :: Value -> Text -> Int -> IO Text
textField v k n = parseValue 400 (textFieldP k n) v
now :: IO Text
now = timestamp <$> getCurrentTime
timestamp :: UTCTime -> Text
timestamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q+00:00"
parseTime :: Text -> Maybe UTCTime
parseTime s = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Ez" (T.unpack s)
newId :: IO Text
newId = T.filter (/= '-') . UUID.toText <$> UUID.nextRandom
fresh :: Text -> IO Text
fresh prefix = (prefix <>) . T.take 12 <$> newId
readJSON :: FromJSON a => FilePath -> a -> IO a
readJSON p def = do
 exists <- doesFileExist p
 if exists then either (throwIO . userError) pure . eitherDecode =<< BL.readFile p else pure def
atomicBytes :: FilePath -> BL.ByteString -> IO ()
atomicBytes p bytes = do
 createDirectoryIfMissing True (takeDirectory p)
 bracketOnError (openBinaryTempFile (takeDirectory p) ".writing-") (\(f,h) -> do
  hClose h `catch` (\(_::IOException) -> pure ())
  removeFile f `catch` (\(_::IOException) -> pure ())) $ \(f,h) -> do
  BL.hPut h bytes
  hFlush h
#ifdef mingw32_HOST_OS
  hClose h
  withCWString f $ \name -> bracket (syncOpen name 32770 0) (void . syncClose) $ \fd -> do
   require (fd >= 0) 500 "Cannot open durable temporary file"
   code <- syncCommit fd
   require (code == 0) 500 "Cannot synchronize temporary file"
  let replace attempt = renameFile f p `catch` \(e::IOException) ->
       if isPermissionError e && attempt < (100::Int) then threadDelay 10000 >> replace (attempt+1) else throwIO e
  replace 0
#else
  fd <- Posix.handleToFd h
  Posix.fileSynchronise fd `finally` Posix.closeFd fd
  renameFile f p
  bracket (Posix.openFd (takeDirectory p) Posix.ReadOnly Posix.defaultFileFlags) Posix.closeFd Posix.fileSynchronise
#endif
writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON p = atomicBytes p . encode
locked :: Service -> IO a -> IO a
locked s = withMVar (serviceGuard s) . const
agentRoot :: Service -> Text -> FilePath
agentRoot s a = serviceRoot s </> "agentpy/agents" </> T.unpack a
workspaceRoot :: Service -> Text -> FilePath
workspaceRoot s a = serviceRoot s </> "workspaces" </> T.unpack a
agentState :: Service -> Text -> IO AgentState
agentState s a = readJSON (agentRoot s a </> "state.json") (AgentState [] KM.empty)
saveState :: Service -> Text -> AgentState -> IO ()
saveState s a v = writeJSON (agentRoot s a </> "state.json") (put "revision" (num (int (get (stateJSON v) "revision") + 1)) (stateJSON v))
modifyState :: Service -> Text -> (Value -> Value) -> IO ()
modifyState s a f = agentState s a >>= decodeValue 500 . f . stateJSON >>= saveState s a
modifyTurns :: Service -> Text -> (Turn -> Turn) -> IO ()
modifyTurns s a f = agentState s a >>= saveState s a . mapTurns f
archivePath :: Service -> Text -> Text -> FilePath
archivePath s a k = serviceRoot s </> "agentpy/corpora/archive" </> T.unpack a </> T.unpack k <.> "json"
findId :: Text -> [Value] -> Maybe Value
findId i = find ((== i) . (`at` "id"))
readText :: FilePath -> IO Text
readText = fmap (TE.decodeUtf8 . BL.toStrict) . BL.readFile
writeText :: FilePath -> Text -> IO ()
writeText p = atomicBytes p . BL.fromStrict . TE.encodeUtf8
prompt :: Service -> Text -> IO Text
prompt s k = readText (repositoryRoot s </> "corpora/prompts" </> T.unpack k <.> "md")
render :: Text -> [(Text,Text)] -> Text
render = foldl (\t (k,v) -> T.replace ("{" <> k <> "}") v t)

withActor :: Service -> Text -> Maybe Text -> IO a -> IO a
withActor s actor agency action = do
 thread <- myThreadId
 bracket (modifyMVar (serviceActors s) (\actors -> pure (M.insert thread (actor,agency) actors,M.lookup thread actors)))
  (\previous -> modifyMVar_ (serviceActors s) (pure . maybe (M.delete thread) (M.insert thread) previous)) (const action)
currentActor :: Service -> IO (Text,Maybe Text)
currentActor s = do
 thread <- myThreadId
 fromMaybe ("admin",Nothing) . M.lookup thread <$> readMVar (serviceActors s)
