module Sapiens.App where

import Prelude
import Control.Alt ((<|>))
import Control.Parallel (parallel, sequential)
import Control.Promise (toAffE)
import Data.Argonaut.Core (Json)
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String as S
import Data.String.CodeUnits as CU
import Data.Foldable (traverse_)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), delay, try)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Foreign.Object as O
import Halogen as H
import Halogen.Subscription as HS
import Libs4.Browser as B
import Sapiens.Action (Action(..), Success(..))
import Sapiens.Connections as C
import Sapiens.CorporaEffects as Corpora
import Sapiens.Data as D
import Sapiens.DialogEffects as Dialog
import Sapiens.Effects as E
import Sapiens.Model as M
import Sapiens.Navigation as N
import Sapiens.Text as Text
import Sapiens.View as View
import Sapiens.Work as Work
import Sapiens.Workspace as Workspace
import Web.Event.Event as Event

component :: forall q. C.Runtime -> H.Component q Json Void Aff
component runtime = H.mkComponent
  { initialState:M.initial
  , render:View.render
  , eval:H.mkEval $ H.defaultEval {handleAction=handle runtime,initialize=Just Initialize,finalize=Just Finalize}
  }

handle :: C.Runtime -> E.Dispatch
handle runtime input = session input
  where
  dispatch = handle runtime
  session action = case action of
    Initialize -> do
      H.modify_ _ {feedback="Connecting…"}
      traverse_ (\(Tuple target event) -> void $ H.subscribe $ Global event <$> HS.makeEmitter (B.subscribe target event))
        [Tuple "window" "error",Tuple "window" "unhandledrejection",Tuple "window" "resize",Tuple "viewport" "resize",Tuple "window" "online",Tuple "window" "storage",Tuple "window" "pagehide",Tuple "window" "pageshow",Tuple "window" "popstate",Tuple "window" "beforeinstallprompt",Tuple "document" "visibilitychange",Tuple "document" "click"]
      void $ H.subscribe $ GlobalEvent "keydown" <$> HS.makeEmitter (B.subscribeEvent "document" "keydown")
      void $ H.subscribe $ HS.makeEmitter \emit -> B.subscribeMedia "(prefers-color-scheme: dark)" $ emit RefreshTheme
      void $ H.subscribe $ NativeReport <$> HS.makeEmitter (B.callback "sapiensBrowserEvent")
      now <- liftEffect B.now
      savedTheme <- liftEffect $ B.storageRead "sapi-color-theme"
      H.modify_ \m -> m {now=now,theme=if A.elem savedTheme ["dark","light"] then savedTheme else m.theme}
      m <- H.get
      let ios = N.ios m.env
          mobile = ios || D.matches "Android" (D.s "userAgent" m.env)
          required = M.remote m && mobile && not (D.b "installed" m.env)
      H.modify_ _ { installRequired=required }
      when (M.remote m) do
        liftEffect $ B.historyReplace $ D.s "path" m.env
        void $ H.fork $ void $ H.liftAff $ try $ toAffE $ B.registerWorker "/sw.js"
      unless required $ dispatch LoadRegistry
      E.sync
    Finalize -> liftEffect $ C.dispose runtime
    Poll -> do
      m <- H.get
      unless (m.polling || m.busy || m.active == "" || m.installRequired || D.b "hidden" m.env) do
        let requested = m {polling=true,pollGeneration=m.pollGeneration+1}
        H.put requested
        E.refresh runtime dispatch requested $ E.api runtime requested "/api/state" "GET" D.null
    Copy text caption -> do
      result <- H.liftAff $ try $ toAffE $ B.clipboard text
      E.notify $ case result of
        Right _ -> caption
        Left _ -> "Could not copy. You can select the task body to copy it."
    Resize -> do
      updateEnvironment
      E.sync
    GlobalEvent name event -> do
      data_ <- liftEffect $ B.eventData event
      m <- H.get
      let key = D.s "key" data_
          editing = A.elem (D.s "tag" data_) ["INPUT","TEXTAREA","SELECT"]
          shortcut = key == "/" && not editing || S.toLower key == "l" && (D.b "ctrl" data_ || D.b "meta" data_) && A.elem N.Workspace m.panes
      when (m.dialog == Nothing && shortcut) $ liftEffect $ Event.preventDefault event
      global name data_
    Global name event -> global name event
    RefreshTheme -> do
      saved <- liftEffect $ B.storageRead "sapi-color-theme"
      env <- liftEffect B.environment
      H.modify_ _ {theme=if A.elem saved ["dark","light"] then saved else if D.b "dark" env then "dark" else "light"}
      E.sync
    Theme -> do
      m <- H.get
      let theme = if m.theme == "dark" then "light" else "dark"
      H.modify_ _ {theme=theme}
      void $ liftEffect $ B.storageWrite "sapi-color-theme" theme
      E.sync
    Persist -> do
      E.persistLocal
      m <- H.get
      let value = D.encode $ M.preferences m
      when (m.online && not m.busy && not m.saving && value /= m.preferencesSaved) do
        H.modify_ _ {saving=true}
        E.attempt (E.api runtime m "/api/preferences" "PUT" $ D.json {attachment_drafts: D.get "attachment_drafts" $ M.preferences m}) \result -> E.inEpoch m.epoch \_ -> do
          H.modify_ _ {saving=false}
          case result of
            Right data_ -> H.modify_ $ \s -> (M.withWorkspaces (D.get "preferences" data_) s) {preferencesSaved=value}
            Left failure -> E.notify $ "Settings are not saved yet: " <> message failure
          latest <- H.get
          when (D.encode (M.preferences latest) /= latest.preferencesSaved) $ E.later 2000.0 $ dispatch Persist
    Mutate path method body success -> do
      m <- H.get
      H.modify_ \s -> s {dialog=map (_ {busy=true}) s.dialog}
      E.attempt (E.api runtime m path method body) \result -> E.inEpoch m.epoch \current ->
        when ((_.identity <$> current.dialog) == (_.identity <$> m.dialog)) do
          H.modify_ \s -> s {dialog=map (_ {busy=false}) s.dialog}
          case result of
            Left failure -> E.notify $ message failure
            Right value -> case success of
              CloseAndRefresh caption -> dispatch CloseDialog *> E.notify caption *> dispatch Poll
              SavedPrompt content -> do
                H.modify_ \s -> s {dialog=map (\d -> d {fields=D.put "path" (D.get "path" value) d.fields,content=D.put "prompts" (D.array $ map (\p -> if D.s "node" p == D.s "node" d.fields then D.put "content" (D.text content) $ D.put "path" (D.get "path" value) p else p) $ D.rows "prompts" d.content) d.content}) s.dialog}
                E.notify "Prompt saved for subsequent decisions."
              _ -> dispatch Poll
    Submit event next -> liftEffect (Event.preventDefault event) *> dispatch next
    Link event next -> do
      m <- H.get
      let prevent = case next of
            OpenLink _ -> Workspace.native m
            _ -> true
      when prevent $ liftEffect $ Event.preventDefault event
      liftEffect $ Event.stopPropagation event
      dispatch next
    NoteAnchor id -> liftEffect $ B.scrollIntoView $ "#" <> id
    _ -> conversation action
  conversation action = case action of
    Draft value -> do
      H.modify_ \m -> m {drafts=O.insert m.selected value m.drafts}
      E.resizeComposer
      dispatch MentionCursor
      E.save dispatch
    MentionCursor -> do
      m <- H.get
      box <- liftEffect $ B.measure "#message-input"
      let start = D.int $ D.get "start" box
          end = D.int $ D.get "end" box
          before = CU.take start $ M.draft m
          match = Text.match "(?:^|\\s)@([A-Za-z0-9_.:#+|()&$^\\-]*)$" before
      H.modify_ _ {mention=case match of
        Just hit | start == end ->
          let query = fromMaybe "" $ A.index hit.groups 1
              choices = A.take 12 $ A.filter (\a -> not (M.isGroup a || D.b "retired" a) && (not (M.isGroup $ M.selected m) || D.has (D.s "id" a) (D.get "members" $ M.selected m)) && S.take (S.length query) (S.toLower $ D.s "name" a) == S.toLower query) m.agents
          in Just {start:start-CU.length query-1,end,choices,index:0}
        _ -> Nothing}
    MentionChoice index -> do
      m <- H.get
      case m.mention of
        Just menu -> case A.index menu.choices index of
          Just entity -> do
            let value = CU.take menu.start (M.draft m) <> "@" <> D.s "name" entity <> " " <> CU.drop menu.end (M.draft m)
                cursor = menu.start + CU.length (D.s "name" entity) + 2
            H.modify_ _ {mention=Nothing,drafts=O.insert m.selected value m.drafts}
            focusComposer
            liftEffect $ B.selectRange "#message-input" cursor cursor
            E.save dispatch
          Nothing -> pure unit
        Nothing -> pure unit
    MessageKey event -> do
      data_ <- liftEffect $ B.eventData event
      m <- H.get
      unless (D.b "composing" data_) do
        let key = D.s "key" data_
        case m.mention of
          Just menu | A.elem key ["ArrowDown","ArrowUp","Enter","Tab","Escape"] -> do
            liftEffect $ Event.preventDefault event
            liftEffect $ Event.stopPropagation event
            if key == "Escape" then H.modify_ _ {mention=Nothing} else unless (A.null menu.choices) do
              if key == "Enter" || key == "Tab" then dispatch $ MentionChoice menu.index else H.modify_ _ {mention=Just $ menu {index=mod (menu.index+(if key == "ArrowDown" then 1 else A.length menu.choices-1)) (A.length menu.choices)}}
          _ -> when (key == "Enter" && not (D.b "shift" data_)) $ liftEffect (Event.preventDefault event) *> dispatch Send
    Send -> do
      m <- H.get
      let text = S.trim $ M.draft m
          items = M.attachments m
          a = M.selected m
      unless ((text == "" && A.null items) || Set.member m.selected m.sending || m.uploading > 0 || not m.online || m.busy || D.b "retired" a || D.b "archived" a || A.elem (D.str $ D.at ["corpora","status"] m.snapshot) ["paused","archived"]) do
        H.modify_ _ {sending=Set.insert m.selected m.sending,mention=Nothing}
        let payload = D.obj $ [Tuple "text" (D.text text),Tuple "attachments" $ D.array $ map (D.get "id") items] <> case m.clarification of
              Just work | D.s "owner" work == m.selected -> [Tuple "workload" $ D.get "id" work]
              _ -> []
        E.attempt (E.api runtime m (M.ownerPath m <> "/messages") "POST" payload) \result -> E.inEpoch m.epoch \current -> do
          H.modify_ _ {sending=Set.delete m.selected current.sending}
          case result of
            Left failure -> E.notify $ message failure
            Right _ -> do
              H.modify_ \s -> s {drafts=if S.trim (fromMaybe "" $ O.lookup m.selected s.drafts) == text then O.insert m.selected "" s.drafts else s.drafts,attachments=O.insert m.selected (A.filter (\attachment -> not $ A.any (( _ == D.s "id" attachment) <<< D.s "id") items) $ fromMaybe [] $ O.lookup m.selected s.attachments) s.attachments,clarification=Nothing}
              E.save dispatch
              E.scrollBottom
              dispatch Poll
    ToggleExpanded key -> H.modify_ \m -> m {expanded=(if Set.member key m.expanded then Set.delete else Set.insert) key m.expanded}
    Clarify work -> do
      dispatch $ Select $ D.s "owner" work
      H.modify_ _ {clarification=Just work}
      focusComposer
    RemoveAttachment id -> H.modify_ (\m -> m {attachments=O.alter (Just <<< A.filter (( _ /= id) <<< D.s "id") <<< fromMaybe []) m.selected m.attachments}) *> E.save dispatch
    PrepareDraft preset placeholder -> do
      m <- H.get
      dispatch $ Navigate $ N.Chat N.Conversation
      let start = CU.length (M.draft m) + if M.draft m == "" then 0 else 2
          value = M.draft m <> (if M.draft m == "" then "" else "\n\n") <> preset
          cursor = if placeholder == "" then CU.length value else start + fromMaybe (CU.length preset) (CU.indexOf (S.Pattern placeholder) preset)
      dispatch $ Draft value
      H.modify_ _ {mention=Nothing}
      focusComposer
      liftEffect $ B.selectRange "#message-input" cursor (cursor + CU.length placeholder)
    Feedback -> do
      m <- H.get
      dispatch $ Navigate $ N.Chat N.Conversation
      dispatch $ Draft $ M.draft m <> (if M.draft m == "" then "" else "\n\n") <> "Feedback: "
      focusComposer
    _ -> workspace action
  workspace action = case action of
    Select id -> do
      E.captureScroll
      E.persistLocal
      H.modify_ $ M.select id
      E.scrollBottom
      E.preview runtime
      saveView dispatch
    SetScope scope -> do
      m <- H.get
      dispatch $ Select $ D.s "main_agent_id" m.snapshot
      H.modify_ _ {scope=scope,groupView=Nothing}
      current <- H.get
      when (N.limit current.env < 3) $ H.modify_ _ {panes=N.togglePane false (N.limit current.env) (D.valid $ M.activeTab current) N.Sidebar current.panes}
      saveView dispatch
    Navigate activity -> do
      E.captureScroll
      H.modify_ _ {activity=activity,menu=Nothing}
      liftEffect $ B.scroll "#conversation-body" 0.0
      E.requestDocument runtime
      saveView dispatch
    DefaultWork -> H.get >>= dispatch <<< Navigate <<< Work.defaultActivity
    SetPeriod period -> do
      H.modify_ \m -> m {menu=Nothing,activity=case m.activity of
        N.Work view _ -> N.Work view period
        other -> other}
      E.save dispatch
    Search search -> H.modify_ _ {search=search}
    ToggleSearch -> do
      m <- H.get
      H.modify_ _ {searchOpen=not m.searchOpen,search=if m.searchOpen then "" else m.search}
      unless m.searchOpen $ liftEffect $ B.focus "#agent-search"
    TogglePane pane -> do
      m <- H.get
      if pane == N.Workspace && not (D.valid $ M.activeTab m) then dispatch $ Browser "open" $ D.json {url: "about:blank"} else do
        H.modify_ _ {panes=N.togglePane true (N.limit m.env) (D.valid $ M.activeTab m) pane m.panes}
        saveView dispatch
    Home -> do
      m <- H.get
      if M.remote m && D.b "installed" m.env && (N.ios m.env) then do
        E.persistLocal
        void $ H.liftAff $ try $ sequential $ parallel (toAffE B.updateWorker) <|> parallel (delay $ Milliseconds 12000.0)
        E.persistLocal
        liftEffect B.reload
      else do
        H.modify_ _ {panes=[N.ChatPane,N.Sidebar,N.Workspace]}
        dispatch $ Select $ D.s "main_agent_id" m.snapshot
    RetryDocument key -> H.modify_ (\m -> m {documents=O.delete key m.documents}) *> E.requestDocument runtime
    Menu key -> do
      H.modify_ \m -> m {menu=if m.menu == Just key then Nothing else Just key,mention=Nothing}
      E.sync
    CloseMenu -> H.modify_ _ {menu=Nothing} *> E.sync
    Scroll -> E.captureScroll *> E.persistLocal
    UserScroll -> H.modify_ _ {scrollPosition=D.null,restoringScroll=false}
    NativeReport data_ -> do
      m <- H.get
      when (Workspace.native m && not m.busy && A.any (( _ == D.s "owner" data_) <<< D.s "id") m.agents) do
        H.modify_ \state -> state {nativeReports=A.snoc state.nativeReports data_}
        dispatch DrainNative
    DrainNative -> do
      m <- H.get
      case A.uncons m.nativeReports of
        Just {head:data_,tail} | not m.nativeReporting -> do
          H.modify_ _ {nativeReporting=true,nativeReports=tail}
          let owner = D.s "owner" data_
              path = "/api/" <> (if S.take 6 owner == "group_" then "groups/" else "agents/") <> B.encodeURI owner
              op = if D.s "open" data_ /= "" || D.s "file" data_ /= "" then "open" else D.s "action" data_
              body = if D.s "open" data_ /= "" then D.json {url: D.get "open" data_} else if D.s "file" data_ /= "" then D.json {path: D.get "file" data_} else D.get "fields" data_
          E.attempt (do
            void $ E.api runtime m (path <> if op == "" then "/browser" else "/control") "POST" (if op == "" then data_ else D.put "op" (D.text $ "workspace_" <> op) body)
            E.api runtime m "/api/state" "GET" D.null) \result -> E.inEpoch m.epoch \_ -> do
              H.modify_ _ {nativeReporting=false}
              browserReceived m.epoch owner op result
              dispatch DrainNative
        _ -> pure unit
    Browser op fields -> do
      m <- H.get
      E.attempt (do
        void $ E.api runtime m (M.ownerPath m <> "/control") "POST" $ D.put "op" (D.text $ "workspace_" <> op) fields
        E.api runtime m "/api/state" "GET" D.null) $ browserReceived m.epoch m.selected op
    Address text -> H.modify_ _ {address=text,addressOpen=true}
    OpenAddress -> do
      m <- H.get
      let value = S.trim m.address
      when (value /= "") $ dispatch $ Browser "open" $ D.obj $ [Tuple (if D.matches "^(https?:|file:|about:)" value then "url" else "path") (D.text value)] <> if D.valid $ M.activeTab m then [Tuple "id" $ D.get "id" $ M.activeTab m] else []
    EditAddress -> do
      H.modify_ _ {addressOpen=true,menu=Nothing}
      E.sync
      liftEffect $ B.focus "#browser-address"
      m <- H.get
      liftEffect $ B.selectRange "#browser-address" 0 $ CU.length m.address
    OpenFile -> do
      m <- H.get
      let directory = if M.isGroup $ M.selected m then D.s "workspace" $ M.selected m else D.replace "[\\\\/][^\\\\/]+$" "" $ D.str $ D.at ["orchestration",m.selected,"notes","path"] m.snapshot
      dispatch CloseMenu
      if Workspace.native m then liftEffect $ B.nativeMessage $ D.json {pickFile: true, owner: m.selected, directory} else do
        H.modify_ _ {address=directory <> "/",addressOpen=true}
        liftEffect $ B.focus "#browser-address"
    OpenLink url -> do
      m <- H.get
      when (Workspace.native m) $ dispatch $ Browser "open" $ D.json {url}
    _ -> do
      handled <- Corpora.handle runtime dispatch action
      unless handled $ Dialog.handle runtime dispatch action
  browserReceived epoch owner op result = E.inEpoch epoch \m -> case result of
    Left failure -> E.notify $ message failure
    Right snapshot -> do
      H.modify_ $ M.withWorkspaces $ D.get "preferences" snapshot
      when (owner == m.selected) do
        current <- H.get
        let tab = M.activeTab current
            opened = op == "open"
            navigated = A.elem op ["open","focus","close","back","forward","reload"]
        H.modify_ _ {menu=if navigated then Nothing else current.menu,addressOpen=if navigated then opened && D.s "url" tab == "about:blank" else current.addressOpen,address=if not navigated && current.addressOpen then current.address else D.fallback (if D.s "url" tab == "about:blank" then "" else D.s "url" tab) $ D.s "path" tab,panes=if op == "close" && not (D.valid tab) then A.filter (_ /= N.Workspace) current.panes else if A.elem op ["open","focus","close","back","forward","reload"] then N.togglePane false (N.limit current.env) (D.valid tab) N.Workspace current.panes else current.panes,preview=if op == "open" || op == "focus" then M.NotAsked else current.preview}
        when (opened && D.s "url" tab == "about:blank") $ liftEffect $ B.focus "#browser-address"
      E.sync
      E.preview runtime
      E.save dispatch
  global name event = case name of
    "error" -> H.modify_ _ {feedback=D.s "message" event}
    "unhandledrejection" -> H.modify_ _ {feedback=D.s "message" event}
    "resize" -> dispatch Resize
    "online" -> dispatch LoadRegistry
    "pageshow" -> resume unit
    "visibilitychange" -> do
      env <- liftEffect B.environment
      if D.b "hidden" env then suspend unit else resume unit
    "pagehide" -> suspend unit
    "popstate" -> dispatch CloseDialog
    "storage" -> do
      when (D.s "storageKey" event == "sapi-color-theme") $ dispatch RefreshTheme
      when (D.s "storageKey" event == "s4.relay-reset") $ dispatch $ DialogCommand "relay-reset" D.empty
    "keydown" -> do
      m <- H.get
      let key = D.s "key" event
          editing = A.elem (D.s "tag" event) ["INPUT","TEXTAREA","SELECT"]
      when (key == "Escape") do
        if m.dialog /= Nothing then dispatch CloseDialog else if m.menu /= Nothing then dispatch CloseMenu else when m.searchOpen $ dispatch ToggleSearch *> liftEffect (B.focus "#search-toggle")
      when (key == "/" && not editing && m.dialog == Nothing) do
        H.modify_ _ {panes=N.togglePane false (N.limit m.env) (D.valid $ M.activeTab m) N.Sidebar m.panes}
        unless m.searchOpen $ dispatch ToggleSearch
        E.sync
      when (S.toLower key == "l" && (D.b "ctrl" event || D.b "meta" event) && m.dialog == Nothing && A.elem N.Workspace m.panes) $ dispatch EditAddress
    "click" -> do
      m <- H.get
      let ancestors = D.strings $ D.get "ancestors" event
          regions = case m.menu of
            Just "corpora" -> ["corpora-menu","corpora-picker"]
            Just "status" -> ["connection-status","corpora-status-menu"]
            Just key | A.elem key ["browser-options","browser-tabs"] -> ["browser-menu","workspace-menu","browser-overflow"]
            _ -> ["chat-panel"]
          inside = A.any (\id -> A.elem id ancestors) regions
      unless (A.any (\id -> A.elem id ancestors) ["message-input","mention-suggestions"]) $ H.modify_ _ {mention=Nothing}
      when (m.menu /= Nothing && not inside) $ dispatch CloseMenu
    _ -> pure unit
  suspend _ = do
    E.captureScroll
    E.persistLocal
    H.modify_ _ {restoringScroll=true}
    updateEnvironment
    dispatch StopScan
  resume _ = do
    dispatch Resize
    E.restoreScroll
    E.later 32.0 E.restoreScroll
    dispatch Poll
  saveView next = E.save next *> E.sync
  focusComposer = liftEffect $ B.focus "#message-input"
  updateEnvironment = liftEffect B.environment >>= \env -> H.modify_ _ {env=env}
