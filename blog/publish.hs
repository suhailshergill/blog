{-# OPTIONS_GHC -fno-warn-unused-imports #-}
import Import -- standard imports
import Settings -- database backend settings

-- we need something which can read the config/postgresql.yml
-- file. Yesod.Default.Config provides withYamlEnvironment. see it being used
-- [[file:Application.hs::dbconf%20<-%20withYamlEnvironment%20"config/postgresql.yml"%20(appEnv%20conf)][here]].

-- import Yesod.Default.Config
-- ^is now being exported by Import

-- this also brings into scope appEnv etc. to see what else is in scope see
--   [[file:~/.haskdogs/yesod-default-0.6.1/Yesod/Default/Config.hs::module%20Yesod.Default.Config][source]].

-- next we want to do something with those connection settings (i.e, create a pool
-- etc and execute query statements). that is in Database.Persist.Store (again
--   taking cues from
--   [[file:Application.hs::getApplication%20::%20AppConfig%20DefaultEnv%20Extra%20->%20Logger%20->%20IO%20Application][getApplication]].

import qualified Database.Persist.Store as DPS

import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Database.Persist.GenericSql (runMigration)

import Data.Text (toLower)

import System.Environment (getArgs, getProgName)
import System.IO
import System.Exit (exitFailure)
import Data.Text (strip)

import Data.Time.Format (readTime)
import System.Locale (defaultTimeLocale)


type ÃCustomId = Text
type ÃEnteredOn = UTCTime
type ÃUpdatedOn = UTCTime
type ÃHeading = Text
type ÃTag = Text
type ÃPost = Text
type ÃHasMath = Bool


truncateWhitespace :: String -> Text
truncateWhitespace = strip . pack

main :: IO [KeyBackend SqlBackend (EntryTagGeneric SqlBackend)]
main = do
  args <- getArgs
  case args of
    mode_:custom_id:entered_on:updated_on:heading_:hasMath_:tags_ -> do
      let mode :: DefaultEnv = read mode_
          customId :: ÃCustomId = pack custom_id
          heading :: ÃHeading = pack heading_
          hasMath :: ÃHasMath = read hasMath_
          tags :: [ÃTag] = map pack tags_
          enteredOn :: ÃEnteredOn = parseDateUTC entered_on
          updatedOn :: ÃUpdatedOn = parseDateUTC updated_on
      text <- getContents
      let entryText = truncateWhitespace text
          in
          runDBAction mode (insertEntry customId enteredOn updatedOn heading
                            hasMath tags
                            entryText)

    _ -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++
        "mode customID enteredOn updatedOn heading [tags]"
      exitFailure

  where
    parseDateUTC :: String -> UTCTime
    parseDateUTC = readTime defaultTimeLocale "%F %T %Z"



runDBAction :: DefaultEnv
               -> SqlPersist (ResourceT IO) a
               -> IO a
runDBAction env action = do
  conf <- loadConfig $ configSettings env
  dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
            DPS.loadConfig >>= DPS.applyEnv
  pool <- DPS.createPoolConfig (dbconf :: Settings.PersistConfig)
  runResourceT $ DPS.runPool dbconf (do
                      runMigration migrateAll
                      action
                     ) pool

deleteEntry :: (PersistQuery m, PersistUnique m)
               => ÃCustomId
               -> m ()
deleteEntry customId = do
  mEntryE <- getBy $ UniqueCustomId customId
  case mEntryE of
    Just entryE -> let entryK = entityKey entryE in
      do
        deleteWhere $ [EntryTagEntryId ==. entryK]
        delete $ entryK
    Nothing -> return ()


insertEntry :: ( PersistQuery m
               , PersistUnique m
               , backend ~ (PersistMonadBackend m)
               )
               => ÃCustomId
               -> ÃEnteredOn
               -> ÃUpdatedOn
               -> ÃHeading
               -> ÃHasMath
               -> [ÃTag]
               -> ÃPost
               -> m [KeyBackend backend (EntryTagGeneric backend)]
insertEntry customId enteredOn updatedOn heading hasMath tags post = do
  deleteEntry customId
  blogPost <- insert $ Entry post customId enteredOn updatedOn heading hasMath
  sequence $ map (addTag blogPost) tags

addTag :: ( PersistUnique m
          , backend ~ (PersistMonadBackend m)
          )
          => KeyBackend backend (EntryGeneric backend)
          -> ÃTag
          -> m (Key (EntryTagGeneric backend))
addTag entryId tag = let
    normalizedTag = toLower tag
    in
  do
    maybeTagId <- getBy $ UniqueTagName normalizedTag
    case maybeTagId of
      Just tagEntity -> insert $ EntryTag entryId (entityKey tagEntity)
      Nothing -> do
        tagId <- insert $ Tag normalizedTag
        insert $ EntryTag entryId tagId
