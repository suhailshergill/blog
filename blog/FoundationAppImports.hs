module FoundationAppImports
       ( module Prelude
       , module Yesod
       , module Yesod.Static
       , module Yesod.Auth
       , module Yesod.Auth.BrowserId
       , module Yesod.Auth.GoogleEmail
       , module Yesod.Default.Config
       , module Yesod.Default.Util
       , module Yesod.Logger
       , module Network.HTTP.Conduit
       , module Settings.StaticFiles
       , module Settings
       , module Text.Jasmine
       , module Web.ClientSession
       , module Text.Hamlet

       , module Utils
       , module Data.Text

       -- , module Data.Reflection
       -- , module Data.Proxy
       )
    where

import Prelude
import Yesod
import Yesod.Static hiding (Route)
import Yesod.Auth hiding (Route)
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logMsg, formatLogText)
import Network.HTTP.Conduit (Manager)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)

import Utils
import Data.Text (Text, append)

-- import Data.Reflection
-- import Data.Proxy
