module Foundation
    ( App (..)
    , Route (..)
    , AppMessage (..)
    , resourcesApp
    , Handler
    , Widget
    , Form
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model
    ) where

import qualified Settings
import Database.Persist.GenericSql
import qualified Database.Persist.Store
import Model

import FoundationAppImports

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm App App (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120

    defaultLayout widget = do
      master <- getYesod
      mmsg <- getMessage

      (breadcrumbsT, breadcrumbsH) <- breadcrumbs

      let mySettings = appExtra $ settings master

      setHeader "Server" $ extraHeaderServer mySettings

      -- We break up the default layout into two components:
      -- default-layout is the contents of the body tag, and
      -- default-layout-wrapper is the entire page. Since the final
      -- value passed to hamletToRepHtml cannot be a widget, this allows
      -- you to use normal widget features in default-layout.

      pc <- widgetToPageContent $ do
        setTitle (toHtml $ extraTitle $ mySettings)
        $(widgetFile "normalize")
        addScriptRemoteAttrs (extraJquery $ mySettings)
          [("type", "text/javascript")]
        $(widgetFile "default-layout")
      hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- custom error pages
    errorHandler NotFound = fmap chooseRep $ defaultLayout $ do
      setTitle "Not Found"
      (title,body) <- getBOFHExcusesC 300
      $(widgetFile "error-notFound")
    errorHandler other = defaultErrorHandler other

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

-- Add breadcrumb support
instance YesodBreadcrumbs App where
  breadcrumb (StaticR _) = return ("Static content", Nothing)
  breadcrumb (AuthR _) = return ("", Nothing)
  breadcrumb FaviconR = return ("favicon.ico", Nothing)
  breadcrumb RobotsR = return ("robots.txt", Nothing)
  breadcrumb HomeR = do
    title <- (extraTitle . appExtra . settings) `fmap` getYesod
    return (title, Nothing)
  breadcrumb PostsR = return ("shergill: Posts", Just HomeR)
  breadcrumb (PostR postId) = return ("shergill: Post #" `append` postId, Just PostsR)
  breadcrumb (TagR tag) = return ("shergill: #" `append` tag, Just HomeR)
  breadcrumb FeedR = return ("shergill: RSS", Just HomeR)
  breadcrumb (FeedTagR tag) = return ("shergill: RSS #" `append` tag, Just HomeR)

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authBrowserId, authGoogleEmail]

    authHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
