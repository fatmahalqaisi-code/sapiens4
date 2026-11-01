module Sapiens.Effects where

import Prelude
import Control.Promise (toAffE)
import Data.Argonaut.Core (Json)
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Aff (Aff, Milliseconds(..), delay, try, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, message)
import Foreign.Object as O
import Halogen as H
import Libs4.Browser as B
import Sapiens.Action (Action(..))
import Sapiens.Connections as C
import Sapiens.Data as D
import Sapiens.Memo as Memo
import Sapiens.Model as M
import Sapiens.Navigation as N
import Sapiens.Remote as R
import Sapiens.Workspace as W

type UI = H.HalogenM M.Model Action () Void Aff
type Dispatch = Action -> UI Unit

later :: Number -> UI Unit -> UI Unit
later ms action = void $ H.fork $ H.liftAff (delay $ Milliseconds ms) *> action

attempt :: forall a. Aff a -> (Either Error a -> UI Unit) -> UI Unit
attempt operation receive = void $ H.fork $ H.liftAff (try operation) >>= receive

inEpoch :: Int -> (M.Model -> UI Unit) -> UI Unit
inEpoch epoch receive = H.get >>= \m -> when (epoch == m.epoch) $ receive m

withDialog :: (M.Dialog -> UI Unit) -> UI Unit
withDialog receive = H.gets _.dialog >>= traverse_ receive

inDialog :: Int -> (M.Dialog -> UI Unit) -> UI Unit
inDialog id receive = withDialog \d -> when (d.identity == id) $ receive d

modifyDialog :: Int -> (M.Dialog -> M.Dialog) -> UI Unit
modifyDialog id f = H.modify_ \m -> m {dialog=map (\d -> if d.identity == id then f d else d) m.dialog}

refresh :: C.Runtime -> Dispatch -> M.Model -> Aff Json -> UI Unit
refresh runtime dispatch requested operation = attempt operation \result -> do
  m <- H.get
  let key = requested.active
  when (requested.epoch == m.epoch && requested.active == m.active) do
    H.modify_ _ { polling=false }
    case result of
      Left failure -> H.modify_ \state -> state {online=false,feedback=if D.matches "revoked|another window" (message failure) then message failure else state.feedback}
      Right raw -> do
        captureScroll
        snapshot <- liftEffect $ if D.valid m.snapshot && D.str (D.at ["corpora","id"] m.snapshot) == D.str (D.at ["corpora","id"] raw) then pure raw else C.decorate key raw
        now <- liftEffect B.now
        current <- H.get
        let same = D.str (D.at ["corpora","id"] current.snapshot) == D.str (D.at ["corpora","id"] snapshot)
            updated = (if same then M.project else M.reset) snapshot current
            tab = M.activeTab updated
        H.put updated {now=now,online=true,feedback="",records=map (\r -> if D.s "key" r == key then D.merge r $ D.get "corpora" raw else r) current.records,address=if current.addressOpen then current.address else D.fallback (if D.s "url" tab == "about:blank" then "" else D.s "url" tab) $ D.s "path" tab}
        liftEffect $ C.cache key raw
        restoreScroll
        requestDocument runtime
        when (now - m.lastPreview >= 2000.0) do
          H.modify_ _ {lastPreview=now}
          preview runtime
        sync
    latest <- H.get
    unless (D.b "hidden" latest.env) $
      later (if M.remote latest || not latest.online then 2000.0 else 750.0) $
        inEpoch latest.epoch \current -> when (current.active == latest.active && current.pollGeneration == latest.pollGeneration) $ dispatch Poll

api :: C.Runtime -> M.Model -> String -> String -> Json -> Aff Json
api runtime m path method body = do
  when (m.busy || m.active == "") $ throwError $ error "Connecting…"
  when (not m.online && method /= "GET") $ throwError $ error "Offline — read only."
  C.request runtime (M.remote m) (M.activeRecord m) path method body

notify :: String -> UI Unit
notify text = do
  H.modify_ _ { toast=text }
  later 3200.0 $ H.modify_ \m -> if m.toast == text then m {toast=""} else m

persistLocal :: UI Unit
persistLocal = do
  m <- H.get
  when (m.active /= "") do
    ok <- liftEffect $ C.write ("s4.workspace." <> m.active) $ M.preferences m
    unless ok $ H.modify_ _ { feedback="Device storage is unavailable. Keep this window open to retain your draft." }

save :: Dispatch -> UI Unit
save dispatch = persistLocal *> later 300.0 (dispatch Persist)

requestDocument :: C.Runtime -> UI Unit
requestDocument runtime = do
  m <- H.get
  case M.currentDocument m of
    Nothing -> pure unit
    Just request -> do
      let old = fromMaybe M.NotAsked $ O.lookup request.key m.documents
      unless ((_.revision <$> M.requestOf old) == Just request.revision) do
        H.modify_ _ { sequence=request.sequence, documents=O.insert request.key (M.Loading request $ M.valueOf old) m.documents }
        attempt (api runtime m request.path "GET" D.null) \result -> inEpoch m.epoch \current ->
          when ((_.sequence <$> (O.lookup request.key current.documents >>= M.requestOf)) == Just request.sequence) do
            let decoded = either (Left <<< message) (M.decodeDocument request) result
            H.modify_ \s -> s {documents=O.insert request.key (either (M.Failed request) (M.Ready request) decoded) s.documents}
            case decoded of
              Right document | D.cut 5 request.key == "memo:" -> memoImages runtime document
              _ -> pure unit

memoImages :: C.Runtime -> Json -> UI Unit
memoImages runtime document = do
  m <- H.get
  traverse_ (\source -> do
    let key = m.selected <> ":" <> source
    unless (D.valid $ D.get key m.imageUrls) $ attempt (imageRequest m source) \result -> inEpoch m.epoch \_ ->
      traverse_ (\uri -> H.modify_ \s -> s {imageUrls=D.put key (D.text uri) s.imageUrls}) result) $ Memo.imagePaths document
  where
  imageRequest m source = do
    let path = M.ownerPath m <> "/notes?image=" <> B.encodeURI source
    result <- if M.remote m then do
      client <- C.client runtime (M.activeRecord m)
      R.request client 15000 $ D.json {kind: "request", op: "api", method: "GET", path}
      else toAffE $ B.requestBytes ("/api/host/corpora/" <> B.encodeURI (D.s "id" $ M.activeRecord m) <> path) (D.json {"X-Sapiens-Local": "1"})
    unless (D.n "status" result == 200.0 && D.matches "^image/(png|jpeg|gif|webp|svg\\+xml)(;|$)" (D.s "mime" result)) $ throwError $ error "Image unavailable"
    pure $ "data:" <> D.s "mime" result <> ";base64," <> D.s "body" result

preview :: C.Runtime -> UI Unit
preview runtime = do
  m <- H.get
  let tab = M.activeTab m
      path = if D.s "path" tab == "" then "" else M.ownerPath m <> "/browser/" <> B.encodeURI (D.s "id" tab) <> "/content"
  unless (W.native m || path == "" || not (A.elem N.Workspace m.panes)) do
    let request = {epoch:m.epoch,sequence:m.sequence+1,key:D.s "id" tab,revision:D.encode $ D.get "command" tab,path}
        pending = case m.preview of
          M.Loading _ _ -> true
          _ -> false
    unless pending do
      H.modify_ _ { sequence=request.sequence,previewPath=path,preview=M.Loading request $ if path == m.previewPath then M.valueOf m.preview else Nothing }
      attempt (api runtime m path "GET" D.null) \result -> inEpoch m.epoch \current ->
        when (current.previewPath == path && ((_.sequence <$> M.requestOf current.preview) == Just request.sequence)) $
          H.modify_ _ {preview=either (M.Failed request <<< message) (M.Ready request) result}

captureScroll :: UI Unit
captureScroll = do
  m <- H.get
  when (m.activity == N.Chat N.Conversation && not (D.b "hidden" m.env) && not m.restoringScroll) do
    viewport <- liftEffect $ B.measure "#conversation-body"
    when (D.n "clientHeight" viewport > 0.0) do
      children <- liftEffect $ B.measureChildren "#conversation-body"
      let first = fromMaybe D.null $ A.find (\node -> D.str (D.at ["dataset","chatKey"] node) /= "" && D.n "bottom" node > D.n "top" viewport) children
          position = D.json {selected: m.selected, bottom: D.n "scrollHeight" viewport - D.n "scrollTop" viewport - D.n "clientHeight" viewport < 2.0, key: D.at ["dataset","chatKey"] first, offset: D.n "top" first - D.n "top" viewport, width: D.get "clientWidth" viewport, height: D.get "clientHeight" viewport}
      when (not (D.valid m.scrollPosition) || D.n "width" m.scrollPosition == D.n "clientWidth" viewport && D.n "height" m.scrollPosition == D.n "clientHeight" viewport) $ H.modify_ _ { scrollPosition=position }

restoreScroll :: UI Unit
restoreScroll = do
  m <- H.get
  when (m.activity == N.Chat N.Conversation && not (D.b "hidden" m.env)) do
    let saved = m.scrollPosition
    when (D.s "selected" saved == m.selected) do
      viewport <- liftEffect $ B.measure "#conversation-body"
      children <- liftEffect $ B.measureChildren "#conversation-body"
      let node = A.find (( _ == D.s "key" saved) <<< D.str <<< D.at ["dataset","chatKey"]) children
      when (D.n "clientHeight" viewport > 0.0 && (D.b "bottom" saved || node /= Nothing)) do
        let target = if D.b "bottom" saved then D.n "scrollHeight" viewport - D.n "clientHeight" viewport else D.n "scrollTop" viewport + D.n "top" (fromMaybe D.null node) - D.n "top" viewport - D.n "offset" saved
        liftEffect $ B.scroll "#conversation-body" target
        H.modify_ _ { restoringScroll=false, scrollPosition=D.put "width" (D.get "clientWidth" viewport) $ D.put "height" (D.get "clientHeight" viewport) saved }

scrollBottom :: UI Unit
scrollBottom = do
  m <- H.get
  H.modify_ _ { scrollPosition=D.json {selected: m.selected, bottom: true},restoringScroll=true }
  restoreScroll

sync :: UI Unit
sync = do
  m <- H.get
  liftEffect do
    B.setAttribute "html" "data-theme" m.theme
    B.setStyle "html" "color-scheme" m.theme
    B.setAttribute "link[rel=icon]" "href" $ if m.theme == "dark" then "/favicon-dark.svg" else "/favicon.svg"
    B.setAttribute "body" "data-pane-limit" $ show $ N.limit m.env
    if N.limit m.env < 3 then B.setAttribute "body" "data-mobile-ui" "" else B.removeAttribute "body" "data-mobile-ui"
    when (D.n "viewportScale" m.env == 1.0) $ B.setStyle "html" "--mobile-viewport-height" (show (D.n "viewportHeight" m.env) <> "px")
    B.showDialog (if maybe false (M.corporaDialog <<< _.kind) m.dialog then "#corpora-dialog" else "#modal") $ m.dialog /= Nothing
    W.sync m
  resizeComposer

resizeComposer :: UI Unit
resizeComposer = do
  m <- H.get
  if N.limit m.env == 3 then liftEffect $ B.setStyle "#message-input" "height" "" else do
    liftEffect $ B.setStyle "#message-input" "height" "44px"
    box <- liftEffect $ B.measure "#message-input"
    when (D.n "height" box > 0.0) $ liftEffect $ B.setStyle "#message-input" "height" (show (min 140.0 $ max 44.0 $ D.n "scrollHeight" box) <> "px")
