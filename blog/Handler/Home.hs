{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Home
       ( getHomeR
       , getPostsR
       , getPostR
       , getTagsR
       , getTagR
       , getFeedR
       , getFeedTagR
       , headHomeR
       ) where

import Import

import Control.Monad (liftM)
import Text.Blaze (preEscapedToMarkup)
import Yesod.AtomFeed

import Data.Text (append, unpack)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types

extraSettings :: Handler Extra
extraSettings = vaultExtraSettings defaultVault

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
  renderEntries [entryE] [] Nothing Nothing

getTagsR :: Handler RepHtml
getTagsR = do
  results :: [(Single Text, Single Int)] <- runDB $ rawSql stmt []
  maxWeightResult :: [(Single Text, Single Int)] <- runDB $ rawSql
                     (subquery `append` " LIMIT 1") []
  case headMay results of
    Just _ -> do
      maxFontScale <- extraMaxFontScale <$> vaultExtraSettings defaultVault
      let maxWeight = fromIntegral.(\(Single x) -> x).snd.
                      fromMaybe (Single "", Single 1).headMay $ maxWeightResult
          scalingFactor x = maxFontScale + (2/3) - 2/(2*((x/maxWeight)+1) - 1)
          tagName_weight_s = [(t, fromIntegral w) | (Single t, Single w) <- results]
      defaultLayout $(widgetFile "tags")
    Nothing -> redirectWith temporaryRedirect307 HomeR
  where
    stmt = pack . unlines $ [
      "SELECT t.name, e.weight ",
      "FROM tag t ",
      "JOIN (",
      unpack subquery,
      ") e ",
      "ON t.id = e.t ",
      "ORDER BY t.name ASC"
      ]
    subquery = pack . unlines $ [
      "SELECT tag_id AS t, count(*) AS weight ",
      "FROM entry_tag ",
      "GROUP BY tag_id ",
      "ORDER BY weight DESC"
      ]


getTagR :: Text -> Handler RepHtml
getTagR tag = do
  (entryE_s, widget) <- getTagR_ tag
  renderEntries entryE_s entrySort (Just widget) $ Just tag

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


type ÃTag = Text
renderEntries :: [Entity (EntryGeneric SqlPersist)]
                 -> [SelectOpt (EntryGeneric SqlPersist)]
                 -> Maybe Widget
                 -> Maybe ÃTag
                 -> Handler RepHtml
renderEntries entryE_s entryOrder mPaginationWidget mTag = do
  entry_mTags_s <- getEntriesTags entryE_s entryOrder
  shortname <- extraDisqusShortname <$> extraSettings
  developer <- extraDisqusDeveloper <$> extraSettings
  getLastModifiedStr entryE_s >>= setHeader "Last-Modified"
  now <- liftIO getCurrentTime
  titlePrefix <- extraTitlePrefix `fmap` vaultExtraSettings defaultVault
  let loadDisqusCommentThreads = (1==) . length $ entryE_s
  ãDefaultLayout
    defaultVault {
      vaultMFeed = vaultMFeedCons vaultCons $ mTag
      }
    $ do
      modifyTitle titlePrefix entryE_s mTag
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
                      , feedEntryContent = preEscapedToMarkup $ entryPost entryV
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

type ÃTitlePrefix = Text
modifyTitle :: ÃTitlePrefix
               -> [Entity (EntryGeneric SqlPersist)]
               -> Maybe Text
               -> Widget
modifyTitle titlePrefix [entryE] Nothing =
  setTitle . toHtml $ titlePrefix `append` (entryHeading . entityVal $ entryE)
modifyTitle _ _ _ = return ()

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
