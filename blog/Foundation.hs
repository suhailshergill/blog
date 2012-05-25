{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Foundation
    ( module FoundationApp
    , Form
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model

    , module AppVault
    , defaultVault
    ) where

import qualified Settings
import Database.Persist.GenericSql
import qualified Database.Persist.Store

import Model

import FoundationAppImports
import FoundationApp
import AppVault

type Form x = Html -> MForm App App (FormResult x, Widget)

defaultVault :: AppVault
defaultVault = AppVault {
  vaultMFeed = Nothing
  , vaultTitlePrefix = "shergill: "
  }

instance AppVaultMethods App where
  ãDefaultLayout vault widget = do
      master <- getYesod
      mmsg <- getMessage

      (breadcrumbsT, breadcrumbsH_RTs) <- breadcrumbs

      let mySettings = appExtra $ settings master
          subscription = subscriptionWidget $ vaultMFeed vault

      setHeader "Server" $ extraHeaderServer mySettings

      -- We break up the default layout into two components:
      -- default-layout is the contents of the body tag, and
      -- default-layout-wrapper is the entire page. Since the final
      -- value passed to hamletToRepHtml cannot be a widget, this allows
      -- you to use normal widget features in default-layout.

      pc <- widgetToPageContent $ do
        setTitle . toHtml $ breadcrumbsT
        $(widgetFile "normalize")
        addScriptRemoteAttrs (extraJquery $ mySettings)
          [("type", "text/javascript")]
        $(widgetFile "default-layout")
      hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")


-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120

    defaultLayout = ãDefaultLayout defaultVault

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
  breadcrumb PostsR = return (vaultTitlePrefix defaultVault `append` "Posts",
                              Just HomeR)
  breadcrumb (PostR postId) = return (vaultTitlePrefix defaultVault `append`
                                      "Post #" `append` postId, Just PostsR)
  breadcrumb (TagR tag) = return (vaultTitlePrefix defaultVault `append` "#"
                                  `append` tag, Just HomeR)
  breadcrumb FeedR = return (vaultTitlePrefix defaultVault `append` "RSS", Just
                                                                           HomeR)
  breadcrumb (FeedTagR tag) = return (vaultTitlePrefix defaultVault `append`
                                      "RSS #" `append` tag, Just HomeR)
  breadcrumb AboutR = return (vaultTitlePrefix defaultVault `append` "About Me",
                              Just HomeR)
  breadcrumb CVR = return (vaultTitlePrefix defaultVault `append` "CV", Just
                                                                        HomeR)

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
