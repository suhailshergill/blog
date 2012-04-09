import Import -- standard imports
import Settings -- database backend settings

-- we need something which can read the config/postgresql.yml
-- file. Yesod.Default.Config provides withYamlEnvironment. see it being used
-- [[file:Application.hs::dbconf%20<-%20withYamlEnvironment%20"config/postgresql.yml"%20(appEnv%20conf)][here]].

import Yesod.Default.Config

-- this also brings into scope appEnv etc. to see what else is in scope see
--   [[file:~/.haskdogs/yesod-default-0.6.1/Yesod/Default/Config.hs::module%20Yesod.Default.Config][source]].

-- next we want to do something with those connection settings (i.e, create a pool
-- etc and execute query statements). that is in Database.Persist.Store (again
--   taking cues from
--   [[file:Application.hs::getApplication%20::%20AppConfig%20DefaultEnv%20Extra%20->%20Logger%20->%20IO%20Application][getApplication]].

import qualified Database.Persist.Store as DPS

import Database.Persist.GenericSql (runMigration)

import Data.Text (toLower)

import System.Environment (getArgs, getProgName)
import System.IO
import System.Exit (exitFailure)
import Data.Text (pack, strip)

import Su.Date

-- stupid type signatures. need the following imports for them

import Data.Time.Clock (UTCTime)

truncateWhitespace :: String -> Text
truncateWhitespace = strip . pack

main :: IO [Key SqlPersist (EntryTagGeneric SqlPersist)]
main = do
  args <- getArgs
  case args of
    mode_:custom_id:entered_on:heading_:tags_ -> do
      let mode = read mode_
          customId = pack custom_id
          heading = pack heading_
          tags = map pack tags_
          enteredOn = parseDateUTC entered_on
      text <- getContents
      let entryText = truncateWhitespace text
          in
          runDBAction mode (insertEntry customId enteredOn heading tags
                            entryText)

    _ -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ "mode customID enteredOn heading [tags]"
      exitFailure




runDBAction :: DefaultEnv 
               -> SqlPersist IO a
               -> IO a
runDBAction env action = do
  conf <- loadConfig $ configSettings env
  dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
            DPS.loadConfig >>= DPS.applyEnv
  pool <- DPS.createPoolConfig (dbconf :: Settings.PersistConfig)
  DPS.runPool dbconf (do
                      runMigration migrateAll
                      action
                     ) pool

deleteEntry :: (PersistQuery backend m, PersistUnique backend m)
               => ÃCustomId
               -> backend m ()
deleteEntry customId = do
  mEntryE <- getBy $ UniqueCustomId customId
  case mEntryE of
    Just entryE -> let entryK = entityKey entryE in
      do
        deleteWhere $ [EntryTagEntryId ==. entryK]
        delete $ entryK
    Nothing -> return ()


type ÃCustomId = Text
type ÃEnteredOn = UTCTime
type ÃHeading = Text
type ÃTag = Text
type ÃPost = Text
insertEntry :: (PersistQuery backend m, PersistUnique backend m)
               => ÃCustomId
               -> ÃEnteredOn
               -> ÃHeading
               -> [ÃTag]
               -> ÃPost
               -> backend m [Key backend (EntryTagGeneric backend)]
insertEntry customId enteredOn heading tags post = do
  deleteEntry customId
  blogPost <- insert $ Entry post customId enteredOn heading
  sequence $ map (addTag blogPost) tags

addTag :: PersistUnique backend m
          => Key backend (EntryGeneric backend)
          -> ÃTag
          -> backend m (Key backend (EntryTagGeneric backend))
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
