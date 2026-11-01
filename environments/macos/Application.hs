{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Application where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign hiding (void, new)
import Network.HTTP.Client hiding (method)
import Network.HTTP.Types.Status
import ObjC
import Browser
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process hiding (shell)


data Application = Application { native :: Obj, browser :: Browser, windowRef :: IORef Obj, pending :: IORef [IO ()], fixture :: Bool, config :: Obj, attempts :: IORef Int, child :: IORef (Maybe ProcessHandle), checking :: IORef Bool, manager :: Manager }

post application action = atomicModifyIORef' (pending application) (\actions -> (actions ++ [action],()))
check application = do
    busy <- atomicModifyIORef' (checking application) (\old -> (True,old))
    unless busy $ void $ forkIO $ do
        let finish action = post application (writeIORef (checking application) False >> action)
        response <- try $ parseRequest "http://127.0.0.1:4174/api/health" >>= \r -> httpLbs r { responseTimeout = responseTimeoutMicro 2000000 } (manager application)
        case response of
            Right result -> if statusCode (responseStatus result) == 200 && maybe False (BC.isPrefixOf "Sapiens4") (lookup "Server" (responseHeaders result)) && maybe False ((== String "ok") . field "status") (decode (responseBody result))
                then finish $ writeIORef (attempts application) 0 >> readIORef (shell (browser application)) >>= \view -> loadURL view "http://127.0.0.1:4174/workspace/"
                else finish $ alert "Port 4174 is occupied by another service. Stop that service and choose View → Reload."
            Left (_ :: HttpException) -> do
                launched <- readIORef (child application)
                started <- try $ when (isNothing launched) $ startServer application
                case started of
                    Left (e :: SomeException) -> finish $ alert (T.pack (displayException e))
                    Right () -> do
                        count <- atomicModifyIORef' (attempts application) (\n -> (n+1,n+1))
                        if count >= 60 then finish $ writeIORef (child application) Nothing >> alert "The server did not become ready. Choose View → Reload to try again."
                        else threadDelay 1000000 >> finish (check application)

configurationValue application name = str name >>= obj1 (config application) "objectForKey:" >>= string
startServer application = do
    root <- T.unpack <$> configurationValue application "Repository"
    executable <- T.unpack <$> configurationValue application "Executable"
    dataDirectory <- T.unpack <$> configurationValue application "DataDirectory"
    configuredPath <- T.unpack <$> configurationValue application "Path"
    present <- doesFileExist executable
    unless present $ ioError (userError "Repository or Haskell executable was moved. Rebuild the app with environments/macos/build.sh.")
    let destination = if null dataDirectory then root </> ".sapiens4" else dataDirectory
    createDirectoryIfMissing True destination
    logFile <- openFile (destination </> "desktop-server.log") AppendMode
    environment <- getEnvironment
    (_,_,_,process) <- createProcess (proc executable ["--port","4174","--data-dir",destination]) { cwd = Just root, env = Just (("PATH", if null configuredPath then "/usr/bin:/bin:/usr/sbin:/sbin" else configuredPath) : filter ((/= "PATH") . fst) environment), std_in = NoStream, std_out = UseHandle logFile, std_err = UseHandle logFile }
    hClose logFile
    writeIORef (child application) (Just process)

alert message = do
    panel <- new "NSAlert"
    setText panel "setMessageText:" "Sapiens4 could not open"
    setText panel "setInformativeText:" message
    void (long panel "runModal")

reload application = if fixture application then readIORef (shell (browser application)) >>= void . (`obj0` "reload") else writeIORef (attempts application) 0 >> check application
showWindow application = do win <- readIORef (windowRef application); void (obj1 win "makeKeyAndOrderFront:" nullPtr); void (obj1 (native application) "activateIgnoringOtherApps:" (int 1))

installMethods application = do
    c <- subclass "SapiensHaskellDesktop"
    let b = browser application
        on0 name action = wrap0 (\_ _ -> action) >>= method c name "v@:"
        on1 name action = wrap1 (\_ _ value -> action value) >>= method c name "v@:@"
        on2 name action = wrap2 (\_ _ a d -> action a d) >>= method c name "v@:@@"
        on3 name action = wrap3 (\_ _ a d e -> action a d e) >>= method c name "v@:@@@"
        on4 name action = wrap4 (\_ _ a d e f -> action a d e f) >>= method c name "v@:@@@@"
    on1 "tick:" $ \_ -> atomicModifyIORef' (pending application) (\items -> ([],items)) >>= sequence_
    on0 "reloadWorkspace" (reload application)
    on0 "quit" (void (obj1 (native application) "terminate:" nullPtr))
    on0 "reloadTab" (activeAction b "reload" 0)
    on0 "zoomIn" (activeAction b "zoom" 0.1)
    on0 "restoreWorkspace" $ do
        win <- readIORef (windowRef application)
        void (obj1 win "deminiaturize:" nullPtr)
        showWindow application
    on0 "zoomOut" (activeAction b "zoom" (-0.1))
    on0 "resetZoom" (activeAction b "zoom" 0)
    wrapBool2 (\_ _ _ _ -> showWindow application >> pure 1) >>= method c "applicationShouldHandleReopen:hasVisibleWindows:" "B@:@B"
    wrapBool1 (\_ _ _ -> pure 0) >>= method c "applicationShouldTerminateAfterLastWindowClosed:" "B@:@"
    on2 "userContentController:didReceiveScriptMessage:" (\_ message -> receive b message)
    on3 "webView:decidePolicyForNavigationAction:decisionHandler:" (navigation b)
    on2 "webView:didCommitNavigation:" (\view _ -> finished b view)
    on2 "webView:didFinishNavigation:" (\view _ -> finished b view)
    on3 "webView:didFailNavigation:withError:" (\view _ err -> failed b view err)
    on3 "webView:didFailProvisionalNavigation:withError:" (\view _ err -> failed b view err)
    on1 "webViewWebContentProcessDidTerminate:" $ \view -> do
        item <- lookupView b view
        case item of
            Nothing -> reload application
            Just tab -> do
                let updated = tab { tabError = "Page process stopped. Reload to continue." }
                save b updated; report b updated
    on4 "observeValueForKeyPath:ofObject:change:context:" (\_ view _ _ -> withTab b view (report b))
    on4 "webView:runOpenPanelWithParameters:initiatedByFrame:completionHandler:" (\_ parameters _ completion -> openPanel parameters completion)
    on2 "webView:startURLSchemeTask:" (serveFile b)
    on2 "webView:stopURLSchemeTask:" (\_ _ -> pure ())
    wrapObj4 (\_ _ view _ action _ -> do
        sh <- readIORef (shell b)
        when (view == sh) $ do
            request <- obj0 action "request"; u <- obj0 request "URL"
            local <- sameOrigin u (origin b)
            when local $ void (obj1 view "loadRequest:" request)
        pure nullPtr) >>= method c "webView:createWebViewWithConfiguration:forNavigationAction:windowFeatures:" "@@:@@@@"
    registerClass c
    target <- obj0 c "new"
    writeIORef (delegate b) target
    pure target

menuItem menu target title action keyEquivalent = do
    titleObject <- str title; actionSelector <- sel action; keyObject <- str keyEquivalent
    item <- allocated "NSMenuItem" >>= \o -> obj3 o "initWithTitle:action:keyEquivalent:" titleObject actionSelector keyObject
    void (obj1 item "setTarget:" target); void (obj1 menu "addItem:" item)

subMenu root title = do
    item <- new "NSMenuItem"; menu <- allocated "NSMenu" >>= \o -> str title >>= obj1 o "initWithTitle:"
    void (obj1 root "addItem:" item); void (obj1 item "setSubmenu:" menu)
    pure menu

menus application target = do
    root <- new "NSMenu"
    appMenu <- subMenu root "Sapiens4"
    if fixture application then do
        menuItem appMenu target "Reload workspace" "reloadWorkspace" "r"
        menuItem appMenu target "Restore workspace" "restoreWorkspace" "1"
        menuItem appMenu target "Quit" "quit" "q"
      else do
        menuItem appMenu (native application) "About Sapiens4" "orderFrontStandardAboutPanel:" ""
        separator <- cls "NSMenuItem" >>= \c -> obj0 c "separatorItem"; void (obj1 appMenu "addItem:" separator)
        menuItem appMenu target "Quit Sapiens4" "quit" "q"
    edit <- subMenu root "Edit"
    forM_ [("Undo","undo:","z"),("Cut","cut:","x"),("Copy","copy:","c"),("Paste","paste:","v"),("Select All","selectAll:","a")] $ \(title,action,key) -> menuItem edit nullPtr title action key
    unless (fixture application) $ do
        view <- subMenu root "View"
        forM_ [("Reload Tab","reloadTab","r"),("Reload App","reloadWorkspace",""),("Zoom In","zoomIn","="),("Zoom Out","zoomOut","-"),("Actual Size","resetZoom","0")] $ \(title,action,key) -> menuItem view target title action key
    void (obj1 (native application) "setMainMenu:" root)

initialize application target address = do
    void (obj1 (native application) "setActivationPolicy:" (int 0))
    void (obj1 (native application) "setDelegate:" target)
    menus application target
    configuration <- new "WKWebViewConfiguration"
    when (fixture application) $ cls "WKWebsiteDataStore" >>= \c -> obj0 c "nonPersistentDataStore" >>= void . obj1 configuration "setWebsiteDataStore:"
    user <- obj0 configuration "userContentController"; channel <- str "browser"
    void (obj2 user "addScriptMessageHandler:name:" target channel)
    view <- allocated "WKWebView" >>= \o -> rectObj o "initWithFrame:configuration:" (0,0,1180,820) configuration
    writeIORef (shell (browser application)) view
    unless (fixture application) $ do
        void (obj1 view "setNavigationDelegate:" target); void (obj1 view "setUIDelegate:" target)
    void (obj1 view "setAutoresizingMask:" (int 18))
    container <- allocated "NSView" >>= \o -> rect o "initWithFrame:" (0,0,1180,820)
    void (obj1 container "addSubview:" view)
    win <- allocated "NSWindow" >>= \o -> window o 1180 820 15
    writeIORef (windowRef application) win
    setText win "setTitle:" (if fixture application then "Sapiens4 behavior fixture" else "Sapiens4")
    void (obj1 win "setContentView:" container); void (obj1 win "setReleasedWhenClosed:" (int 0))
    unless (fixture application) $ do
        size win "setMinSize:" 720 540
        setText win "setFrameAutosaveName:" "Sapiens4Workspace"
    void (obj0 win "center")
    showWindow application
    timerClass <- cls "NSTimer"; timerSelector <- sel "scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:"; tick <- sel "tick:"
    void (callTimer timerClass timerSelector 0.05 target tick nullPtr 1)
    if fixture application then do
        u <- url address; path <- textProp u "path"
        loadURL view (if path == "/" || T.null path then T.dropWhileEnd (== '/') address <> "/workspace/" else address)
        void $ forkIO $ (forever $ do
            line <- BC.hGetLine stdin
            forM_ (decodeStrict line) $ \value -> post application $ do
                let width = numeric 1180 (field "width" value); h = numeric 820 (field "height" value); top = numeric 0 (field "top" value)
                size win "setContentSize:" width h
                void (rect view "setFrame:" (0,0,width,h-top))) `catch` (\(_ :: IOException) -> post application (void (obj1 (native application) "terminate:" nullPtr)))
        putStrLn "READY"; hFlush stdout
      else do
        html <- str "<body style='background:#191919;color:#eee;font:20px system-ui;padding:48px'>Starting Sapiens4…</body>"
        void (obj2 view "loadHTMLString:baseURL:" html nullPtr)
        check application

runApplication isFixture address = do
    loadFrameworks
    pool <- new "NSAutoreleasePool"
    application <- cls "NSApplication" >>= \c -> obj0 c "sharedApplication"
    http <- newManager defaultManagerSettings
    guestScript <- (do r <- parseRequest (T.unpack (T.dropWhileEnd (== '/') address <> "/workspace/guest.js")); response <- httpLbs r { responseTimeout = responseTimeoutMicro 2000000 } http; pure (TE.decodeUtf8With (\_ _ -> Just '\xfffd') (L.toStrict (responseBody response)))) `catch` (\(_ :: HttpException) -> pure "")
    b <- newBrowser address guestScript
    win <- newIORef nullPtr; tasks <- newIORef []; tries <- newIORef 0; process <- newIORef Nothing; busy <- newIORef False
    bundle <- cls "NSBundle" >>= \c -> obj0 c "mainBundle"
    resource <- str "Launcher"; extension <- str "plist"
    path <- obj2 bundle "pathForResource:ofType:" resource extension
    configuration <- cls "NSDictionary" >>= \c -> obj1 c "dictionaryWithContentsOfFile:" path
    let state = Application application b win tasks isFixture configuration tries process busy http
    target <- installMethods state
    initialize state target address
    void (obj0 application "run")
    void (obj0 pool "drain")
