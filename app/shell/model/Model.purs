module Sapiens.Model where

import Prelude
import Data.Argonaut.Core (Json, toObject, toArray, toString)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set as Set
import Data.String as S
import Data.Tuple (Tuple(..))
import Foreign.Object as O
import Libs4.Browser as B
import Sapiens.Data as D
import Sapiens.Navigation as N

data RemoteData a = NotAsked | Loading Request (Maybe a) | Ready Request a | Failed Request String

type Request = { epoch :: Int, sequence :: Int, key :: String, revision :: String, path :: String }

data DialogKind = AppMenu | WorkspaceSettings | Computer | AgentSettings | GroupSettings | Attach | Reference String | DecisionPrompts | GroupTask | CreateCorpora | EditCorpora | ConnectRemote | HostPairing | RelayList | RelayAdd | RelaySwitch | RelayTerms | Installation | Notice

derive instance eqDialogKind :: Eq DialogKind

corporaDialog :: DialogKind -> Boolean
corporaDialog kind = A.elem kind [CreateCorpora,EditCorpora,ConnectRemote,HostPairing,RelayList,RelayAdd,RelaySwitch,RelayTerms,Notice]

type Dialog = { kind :: DialogKind, title :: String, record :: Json, fields :: Json, content :: Json, error :: String, busy :: Boolean, identity :: Int }

type Model =
  { snapshot :: Json, agents :: Array Json, messages :: O.Object (Array Json)
  , selected :: String, activity :: N.Activity, scope :: N.Scope, groupView :: Maybe String
  , mention :: Maybe { start :: Int, end :: Int, choices :: Array Json, index :: Int }
  , drafts :: O.Object String, attachments :: O.Object (Array Json), sending :: Set.Set String, uploading :: Int, clarification :: Maybe Json
  , workspaces :: Json, workspaceRevision :: Number, documents :: O.Object (RemoteData Json), expanded :: Set.Set String
  , polling :: Boolean, pollGeneration :: Int, saving :: Boolean, lastPreview :: Number, nativeReporting :: Boolean, nativeReports :: Array Json
  , epoch :: Int, sequence :: Int, now :: Number, env :: Json
  , panes :: Array N.Pane, search :: String, searchOpen :: Boolean, menu :: Maybe String, dialog :: Maybe Dialog
  , records :: Array Json, active :: String, online :: Boolean, busy :: Boolean, localAvailable :: Boolean, showArchived :: Boolean
  , toast :: String, feedback :: String, theme :: String, installRequired :: Boolean
  , dialogStack :: Array Dialog
  , imageUrls :: Json
  , preview :: RemoteData Json, previewPath :: String, address :: String, addressOpen :: Boolean
  , scrollPosition :: Json, restoringScroll :: Boolean, preferencesSaved :: String
  }

initial :: Json -> Model
initial env =
  { snapshot:D.empty, agents:[], messages:O.empty, selected:"", activity:N.Chat N.Conversation, scope:N.All, groupView:Nothing
  , mention:Nothing, drafts:O.empty, attachments:O.empty, sending:Set.empty, uploading:0, clarification:Nothing
  , workspaces:D.empty, workspaceRevision:0.0, documents:O.empty, expanded:Set.empty
  , polling:false,pollGeneration:0,saving:false,lastPreview:0.0,nativeReporting:false,nativeReports:[]
  , epoch:0, sequence:0, now:0.0, env, panes:[N.ChatPane,N.Sidebar,N.Workspace], search:"", searchOpen:false, menu:Nothing, dialog:Nothing
  , records:[], active:"", online:false, busy:false, localAvailable:false, showArchived:false
  , toast:"", feedback:"", theme:if D.b "dark" env then "dark" else "light", installRequired:false
  , dialogStack:[], imageUrls:D.empty, preview:NotAsked, previewPath:"", address:"", addressOpen:false, scrollPosition:D.null, restoringScroll:false, preferencesSaved:""
  }

agent :: Model -> String -> Json
agent model id = fromMaybe (D.first model.agents) $ A.find ((_ == id) <<< D.s "id") model.agents

selected :: Model -> Json
selected model = agent model model.selected

isGroup :: Json -> Boolean
isGroup = (_ == "group") <<< D.s "kind"

ownerPath :: Model -> String
ownerPath m = "/api/" <> (if isGroup (selected m) then "groups/" else "agents/") <> B.encodeURI m.selected

activeRecord :: Model -> Json
activeRecord m = fromMaybe D.null $ A.find ((_ == m.active) <<< D.s "key") m.records

remote :: Model -> Boolean
remote m = D.valid $ D.at ["body","remote"] m.env

draft :: Model -> String
draft m = fromMaybe "" $ O.lookup m.selected m.drafts

attachments :: Model -> Array Json
attachments m = fromMaybe [] $ O.lookup m.selected m.attachments

tabs :: Model -> Array Json
tabs m = A.filter (\t -> D.s "url" t /= "about:blank" || D.s "path" t /= "" || not (D.matches "^(blank-|id-)" $ D.s "id" t)) $ D.rows "tabs" $ D.get m.selected m.workspaces

activeTab :: Model -> Json
activeTab m = let rows = tabs m in fromMaybe (D.first rows) $ A.find ((_ == D.str (D.at [m.selected,"activeTab"] m.workspaces)) <<< D.s "id") rows

visiblePanes :: Model -> Array N.Pane
visiblePanes m = N.visiblePanes (N.limit m.env) (D.valid $ activeTab m) m.panes

withWorkspaces :: Json -> Model -> Model
withWorkspaces prefs m = if D.n "workspace_revision" prefs > m.workspaceRevision then m { workspaces=D.get "workspaces" prefs, workspaceRevision=D.n "workspace_revision" prefs } else m

blocksChat :: Json -> Boolean
blocksChat t = A.elem (D.s "status" t) ["queued","running","output_pending"]

past :: String -> Boolean
past state = A.elem state ["Completed","Unresolved","Failed","Interrupted","Cancelled"]

result :: Json -> Json
result turn =
  let output = D.s "output" turn
      parts = S.split (S.Pattern "\n\n") output
      warning = D.s "status" turn == "warning"
      wrapped = A.head parts == Just "Warning — partial result." && A.length parts >= 3 && D.matches "^The (tool|time) limit was reached after " (fromMaybe "" $ A.last parts)
  in D.obj ([Tuple "text" (D.text $ if warning && wrapped then S.joinWith "\n\n" $ A.dropEnd 1 $ A.drop 1 parts else output)] <> if warning then [Tuple "warning" (D.text $ if wrapped then fromMaybe "" (A.last parts) else D.fallback "Partial result; review before relying on it." $ D.s "error" turn)] else [])

project :: Json -> Model -> Model
project snapshot m = withWorkspaces (D.get "preferences" snapshot) m { snapshot=snapshot, agents=agents, messages=messages, selected=id, groupView=groupView, scope=scope }
  where
  personal = map (\a -> D.merge a $ D.json {kind: "sapi", status: if A.any (\t -> D.s "agent" t == D.s "id" a && D.s "status" t == "running") turns then "busy" else "online", lastActivity: B.parseTime $ D.fallback (D.s "created" a) $ D.s "created" $ D.last $ A.filter (\t -> D.s "agent" t == D.s "id" a) turns, preview: D.cut 150 $ D.replace "\\s+" " " $ D.fallback "Ready for your message." $ D.s "text" $ D.last $ fromMaybe [] $ O.lookup (D.s "id" a) personalMessages}) (D.rows "agents" snapshot)
  turns = D.rows "turns" snapshot
  append owner rows = O.alter (Just <<< (_ <> rows) <<< fromMaybe []) owner
  personalMessages = foldl (\all t -> if D.s "group" t /= "" || not (A.elem (D.s "flow" t) ["chat","computer"]) then all else
    let input = D.json {role: "user", author: D.at ["origin","caller"] t, text: D.get "input" t, created: D.get "created" t, attachments: D.get "attachments" t, requestTurn: t, chatKey: "user:" <> D.s "id" t}
        output = D.merge (result t) $ D.json {role: "assistant", created: D.fallback (D.s "created" t) $ D.s "output_at" t, outputOrder: D.get "output_order" t, turnId: D.get "id" t, chatKey: "assistant:" <> D.s "id" t}
    in append (D.s "agent" t) ([input] <> if A.elem (D.s "status" t) ["done","warning"] && D.valid (D.get "output" t) then [output] else []) all) O.empty turns
  groups = map (\g -> D.merge g $ D.json {kind: "group", status: if A.any ((_ == "running") <<< D.s "status") (D.rows "requests" g) then "busy" else "online", lastActivity: B.parseTime $ D.s "updated" g, preview: D.fallback (D.fallback "Shared workspace" $ D.s "description" g) $ D.s "text" $ D.last $ D.rows "messages" g}) (D.rows "groups" snapshot)
  agents = personal <> groups
  groupMessages = foldl (\all g -> O.insert (D.s "id" g) (map (\msg -> D.merge msg $ D.json {role: if D.s "author" msg == "admin" then "user" else "assistant", author: if D.s "author" msg == "admin" then D.null else D.get "author" msg, chatKey: D.get "id" msg}) (D.rows "messages" g)) all) personalMessages groups
  delegated = foldl (\all work -> if D.s "callId" work == D.str (D.at ["origin","call"] work) then all else
    let turn = D.find (D.s "callId" work) turns
        message = D.json {role: "assistant", author: D.get "owner" work, text: D.fallback (D.fallback ("Delegated to " <> D.fallback (D.s "owner" work) (D.s "name" $ D.find (D.s "owner" work) agents) <> " · " <> D.s "state" work) $ D.s "error" work) $ D.s "output" work, created: D.get "created" turn, delegation: work, requestTurn: if A.elem (D.s "status" turn) ["done","warning"] then D.null else turn, chatKey: "delegation:" <> D.s "id" work}
    in append (D.str $ D.at ["origin","agent"] work) [message] all) groupMessages (D.rows "workloads" snapshot)
  messages = map (A.sortBy (\a b -> compare (B.parseTime $ D.s "created" a) (B.parseTime $ D.s "created" b) <> compare (D.n "outputOrder" a) (D.n "outputOrder" b))) delegated
  target = D.find m.selected agents
  id = if not (D.valid target) || D.b "retired" target && not (D.b "retired" $ D.find m.selected m.agents) then D.s "main_agent_id" snapshot else m.selected
  groupView = m.groupView >>= \g -> if A.any (\a -> D.s "id" a == g && not (D.b "archived" a)) agents then Just g else Nothing
  scope = if m.scope == N.Groups && not (A.any (\a -> isGroup a && not (D.b "archived" a)) agents) then N.All else m.scope

reset :: Json -> Model -> Model
reset snapshot old = project snapshot old
  { selected=D.s "selected" p, activity=N.readActivity group p, scope=if group then N.Groups else case D.s "scope" p of
      "groups" -> N.Groups
      "sapis" -> N.Sapis
      _ -> N.All
  , groupView=if group then Just (D.s "selected" p) else if D.s "groupView" p == "" then Nothing else Just (D.s "groupView" p)
  , drafts=map D.str $ fromMaybe O.empty $ toObject $ D.get "drafts" p
  , attachments=map D.arr $ fromMaybe O.empty $ toObject $ D.get "attachment_drafts" snapshot
  , sending=Set.empty, uploading=0, clarification=Nothing, documents=O.empty, expanded=Set.empty, epoch=old.epoch+1
  , panes=if not (D.valid $ D.get "panes" p) then [N.ChatPane,N.Sidebar,N.Workspace] else A.filter (\pane -> D.b (N.paneId pane) $ D.get "panes" p) [N.ChatPane,N.Sidebar,N.Workspace]
  , workspaces=D.get "workspaces" p, workspaceRevision=D.n "workspace_revision" p, search="", searchOpen=false
  , preview=NotAsked, previewPath="", address="", scrollPosition=D.get "chatPosition" p, restoringScroll=D.valid $ D.get "chatPosition" p
  }
  where
  p = D.get "preferences" snapshot
  group = A.any ((_ == D.s "selected" p) <<< D.s "id") $ D.rows "groups" snapshot

select :: String -> Model -> Model
select id m = if not (D.valid target) then m else m
  { selected=id, activity=N.Chat N.Conversation, search="", groupView=if group then Just id else Nothing
  , scope=if group then N.Groups else if m.groupView /= Nothing || m.scope == N.Groups then N.All else m.scope
  , menu=Nothing, dialog=Nothing, addressOpen=false, address="", panes=N.togglePane false (N.limit m.env) (D.valid $ activeTab m) N.ChatPane m.panes
  , scrollPosition=D.null, restoringScroll=false, preview=NotAsked, previewPath=""
  }
  where
  target = D.find id m.agents
  group = isGroup target

roster :: Model -> Array Json
roster m = A.sortBy (\a b -> compare (pinned b) (pinned a) <> compare (D.n "lastActivity" b) (D.n "lastActivity" a)) $ A.filter allowed m.agents
  where
  group = maybeGroup m
  pinned a = D.s "id" a == (if D.valid group then D.s "lead" group else D.s "main_agent_id" m.snapshot)
  allowed a = not (D.b "retired" a || D.b "archived" a) && (if D.valid group then D.has (D.s "id" a) (D.get "members" group) else m.scope == N.All || (m.scope == N.Groups) == isGroup a) && S.contains (S.Pattern $ S.toLower m.search) (S.toLower $ D.s "name" a <> " " <> D.s "role" a)

maybeGroup :: Model -> Json
maybeGroup m = fromMaybe D.null $ (flip D.find m.agents) <$> m.groupView

preferences :: Model -> Json
preferences m = D.merge (N.encodeActivity m.activity) $ D.json
  { selected: m.selected, scope: N.scopeId m.scope, groupView: m.groupView
  , panes: O.fromFoldable $ map (\p -> Tuple (N.paneId p) (A.elem p m.panes)) [N.Sidebar,N.ChatPane,N.Workspace]
  , drafts: m.drafts, attachment_drafts: map (map $ D.get "id") m.attachments
  , workspaces: m.workspaces, chatPosition: m.scrollPosition
  }

currentDocument :: Model -> Maybe Request
currentDocument m = case m.activity of
  N.Work N.Memo _ | D.s "revision" info /= "missing" -> Just $ make "memo" (D.s "revision" info) "/notes"
  N.Work view _ | A.elem view [N.Tasks,N.Automated] && not (isGroup a) -> Just $ make "tasks" (D.encode $ D.array $ A.filter (\w -> D.has m.selected $ D.get "participants" w) $ D.rows "workloads" m.snapshot) "/tasks?format=json"
  _ -> Nothing
  where
  a = selected m
  info = if isGroup a then D.get "notes" a else D.at ["orchestration",m.selected,"notes"] m.snapshot
  make kind revision suffix = {key:kind <> ":" <> m.selected,revision,path:ownerPath m <> suffix,epoch:m.epoch,sequence:m.sequence+1}

requestOf :: forall a. RemoteData a -> Maybe Request
requestOf = case _ of
  NotAsked -> Nothing
  Loading r _ -> Just r
  Ready r _ -> Just r
  Failed r _ -> Just r

valueOf :: forall a. RemoteData a -> Maybe a
valueOf = case _ of
  Loading _ value -> value
  Ready _ value -> Just value
  _ -> Nothing

decodeDocument :: Request -> Json -> Either String Json
decodeDocument r value
  | S.take 5 r.key == "tasks" = if isJust (toArray $ D.get "tasks" value) && A.all (\t -> A.all (\key -> isJust (toString $ D.get key t)) ["id","title","body","state","owner","sender"]) (D.rows "tasks" value) then Right (D.get "tasks" value) else Left "Invalid tasks response"
  | isJust (toString $ D.get "content" value) && isJust (toString $ D.get "path" value) = Right value
  | otherwise = Left "Invalid Memo response"
