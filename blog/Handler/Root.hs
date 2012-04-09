module Handler.Root
       ( getRootR
       , getPostR
       , getTagR
       ) where

import Import

import Control.Monad (liftM)
import Text.Blaze (preEscapedText)

paginationLength :: Int
paginationLength = 10

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
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
                                                               [EntryId <-. (map entityKey entryE_s)]
                                                          , somOrderOne = entryOrder}
  mapM (\(e,eT) -> do
           mTags <- (mapM getTag eT)
           return (entityVal e, mTags))
    entryE_entryTagsE_s


entrySort :: [SelectOpt (EntryGeneric SqlPersist)]
entrySort = [ Desc EntryEnteredOn, Desc EntryId]

renderEntries :: [Entity (EntryGeneric SqlPersist)]
                 -> [SelectOpt (EntryGeneric SqlPersist)]
                 -> Maybe Widget
                 -> Handler RepHtml
renderEntries entryE_s entryOrder mWidget = do
  entry_mTags_s <- getEntriesTags entryE_s entryOrder
  defaultLayout $ do
    $(widgetFile "homepage")


getRootR :: Handler RepHtml
getRootR = do
  (entryE_s, widget) <- runDB $ selectPaginated paginationLength ([] :: [Filter Entry])
                        entrySort
  renderEntries entryE_s entrySort (Just widget)

getPostR :: Text -> Handler RepHtml
getPostR customId = do
  entryE <- runDB . getBy404 $ UniqueCustomId customId
  renderEntries [entryE] [] Nothing

getTagR :: Text -> Handler RepHtml
getTagR tag = do
  (entryE_s, widget) <- runDB $ do
    tagE <- getBy404 $ UniqueTagName tag
    entryTagE_s <- selectList [EntryTagTagId ==. (entityKey tagE)] []
    selectPaginated paginationLength [EntryId <-. (map (entryTagEntryId
                                                        . entityVal)
                                                   entryTagE_s)]
      entrySort
  renderEntries entryE_s entrySort (Just widget)
