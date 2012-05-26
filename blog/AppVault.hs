{-# LANGUAGE RankNTypes #-}
module AppVault
       ( module Widgets
       , AppVault (..)
       -- , M (..)
       , AppVaultMethods (..)
       -- , AppVaultMethods2 (..)
       -- , asProxyOf
       , AppVaultCons (..)
       , vaultCons
       )
       where

import FoundationAppImports
import FoundationApp
import Widgets

-- newtype M a s = M { runM :: a}
-- asProxyOf :: f s -> Proxy s -> f s
-- asProxyOf a _ = a

class RenderRoute a => AppVaultMethods a where
  ãDefaultLayout :: AppVault -> GWidget sub a () -> GHandler sub a RepHtml

-- class RenderRoute a => AppVaultMethods2 a where
--   ãDefaultLayout2 :: GWidget sub a () -> GHandler sub a RepHtml

data AppVault = AppVault {
  vaultMFeed :: ÃMFeed
  , vaultExtraSettings :: forall sub. GHandler sub App Extra
  }
data AppVaultCons = AppVaultCons { vaultMFeedCons :: Maybe Text -> ÃMFeed }

vaultCons :: AppVaultCons
vaultCons = AppVaultCons { vaultMFeedCons = mFeedCons }
            where
              mFeedCons Nothing = Nothing
              mFeedCons (Just tag) = Just (FeedTagR tag, "RSS")
