{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home
       ( getHomeR
       , getPostsR
       , getPostR
       , getTagR
       , getFeedR
       , getFeedTagR
       , headHomeR
       ) where

import Import

import Control.Monad (liftM)
import Text.Blaze (preEscapedText)
import Data.Text (append)
import Yesod.AtomFeed

extraSettings :: Handler Extra
extraSettings = appExtra . settings <$> getYesod

entrySort :: [SelectOpt (EntryGeneric SqlPersist)]
entrySort = [ Desc EntryUpdatedOn, Desc EntryEnteredOn, Desc EntryId]

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

headHomeR :: Handler RepHtml
headHomeR = defaultLayout $! return ()

getHomeR :: Handler RepHtml
getHomeR = getPostsR

getPostsR :: Handler RepHtml
getPostsR = do
  (entryE_s, widget) <- getPostsR_
  renderEntries entryE_s entrySort (Just widget) Nothing

getPostR :: Text -> Handler RepHtml
getPostR customId = do
  entryE <- runDB . getBy404 $ UniqueCustomId customId
  renderEntries [entryE] [] Nothing $ Just ("shergill: " `append` (entryHeading
                                                                   . entityVal
                                                                   $ entryE))

getTagR :: Text -> Handler RepHtml
getTagR tag = do
  (entryE_s, widget) <- getTagR_ tag
  renderEntries entryE_s entrySort (Just widget) $ Just ("shergill: #" `append` tag)

getFeedR :: Handler RepAtom
getFeedR = do
  (entryE_s, _) <- getPostsR_
  renderEntriesRss entryE_s

getFeedTagR :: Text -> Handler RepAtom
getFeedTagR tag = do
  (entryE_s, _) <- getTagR_ tag
  renderEntriesRss entryE_s

-- {{{ internal methods

getTag :: Entity (EntryTagGeneric SqlPersist)
          -> Handler (Maybe Text)
getTag entryTagE = do
  maybeTag <- runDB . get $ (entryTagTagId . entityVal $ entryTagE)
  return $! liftM tagName maybeTag

getEntriesTags :: [Entity (EntryGeneric SqlPersist)]
                  -> [SelectOpt (EntryGeneric SqlPersist)]
                  -> Handler [(EntryGeneric SqlPersist, [Maybe Text])]
getEntriesTags entryE_s entryOrder = do
  entryE_entryTagsE_s <- runDB . runJoin $ (selectOneMany (EntryTagEntryId <-.)
                                            entryTagEntryId)
                                                          { somFilterOne =
                                                               [EntryId <-. map entityKey entryE_s]
                                                          , somOrderOne = entryOrder}
  mapM (\(e,eT) -> do
           mTags <- mapM getTag eT
           return (entityVal e, mTags))
    entryE_entryTagsE_s


type ÃTitle = Text
renderEntries :: [Entity (EntryGeneric SqlPersist)]
                 -> [SelectOpt (EntryGeneric SqlPersist)]
                 -> Maybe Widget
                 -> Maybe ÃTitle
                 -> Handler RepHtml
renderEntries entryE_s entryOrder mWidget mTitle = do
  entry_mTags_s <- getEntriesTags entryE_s entryOrder
  shortname <- extraDisqusShortname <$> extraSettings
  developer <- extraDisqusDeveloper <$> extraSettings
  getLastModifiedStr entryE_s >>= setHeader "Last-Modified"
  now <- liftIO getCurrentTime
  let loadDisqusCommentThreads = (1==) . length $ entryE_s
    in
   defaultLayout $ do
     case mTitle of
       Just title -> setTitle . toHtml $ title
       Nothing -> return ()
     mathJaxSrc <- lift (extraMathJaxSrc <$> extraSettings)
     _ <- sequence [addScriptRemote mathJaxSrc | any (entryHasMath . fst)
                                                 entry_mTags_s]
     $(widgetFile "homepage")

renderEntriesRss :: [Entity (EntryGeneric SqlPersist)]
                    -> Handler RepAtom
renderEntriesRss entryE_s = case headMay entryE_s of
  Just headEntryE -> do
    description <- toHtml . extraFeedDescription <$> extraSettings
    entryRss_s <- mapM entryEToRss entryE_s
    atomFeed Feed
                { feedTitle = "shergill.su"
                , feedDescription = description
                , feedLanguage = "en-us"
                , feedLinkSelf = FeedR
                , feedLinkHome = HomeR
                , feedUpdated = entryUpdatedOn . entityVal $ headEntryE
                , feedEntries = entryRss_s
                }
  _ -> notFound


entryEToRss :: Entity (EntryGeneric SqlPersist)
               -> Handler (FeedEntry (Route App))
entryEToRss entryE = do
  let entryV = entityVal entryE
  return $! FeedEntry
                      {
                        feedEntryLink = PostR $ entryCustomId entryV
                      , feedEntryUpdated = entryUpdatedOn entryV
                      , feedEntryTitle = entryHeading entryV
                      , feedEntryContent = preEscapedText $ entryPost entryV
                      }

getPostsR_ :: Handler ([Entity Entry], Widget)
getPostsR_ = do
  len <- extraPaginationLength <$> extraSettings
  runDB $
    selectPaginated len
    ([] :: [Filter Entry])
    entrySort


getTagR_ :: Text -> Handler ([Entity (EntryGeneric SqlPersist)], Widget)
getTagR_ tag = runDB $ do
  tagE <- getBy404 $ UniqueTagName tag
  entryTagE_s <- selectList [EntryTagTagId ==. entityKey tagE] []
  len <- lift (extraPaginationLength <$> extraSettings)
  selectPaginated
    len
    [EntryId <-. map (entryTagEntryId . entityVal) entryTagE_s]
    entrySort


-- {{{ utility

getLastModified :: [Entity (EntryGeneric SqlPersist)]
                   -> Handler UTCTime
getLastModified entryE_s = case headMay entryE_s of
  Just headEntryE -> return $! entryUpdatedOn . entityVal $ headEntryE
  Nothing -> liftIO getCurrentTime

getLastModifiedStr :: [Entity (EntryGeneric SqlPersist)]
                      -> Handler Text
getLastModifiedStr = (pack . formatTime defaultTimeLocale rfc822DateFormat <$>)
                     . getLastModified

getLastModifiedStrFriendly :: [Entity (EntryGeneric SqlPersist)]
                              -> Handler Text
getLastModifiedStrFriendly = (liftIO . (pack <$>) . humanReadableTime =<<) . getLastModified

-- }}}

-- }}}
