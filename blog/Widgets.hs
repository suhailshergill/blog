{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}
module Widgets
       where

import FoundationAppImports
import FoundationApp

type ÃFeed = (Route App, Text)
type ÃMFeed = Maybe ÃFeed

newtype SubscriptionData = SubscriptionData { getSubscriptionData :: ÃFeed }

subscriptionWidget :: forall sub. Maybe ÃFeed -> GWidget sub App ()
subscriptionWidget mFeed = $(widgetFile "subscription")

subscription :: forall sub. GWidget sub App ()
subscription = subscriptionWidget Nothing
