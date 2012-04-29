{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
module Utils
       ( getBOFHExcuses
         , getBOFHExcusesC
         , findOrCreate
         , joinTables
         , joinTables3
       ) where

import Prelude
import System.Process (readProcess)
import Control.Monad.IO.Class (MonadIO)

import Data.Time.Clock (getCurrentTime, utctDayTime, DiffTime)

import System.IO.Unsafe
import Data.IORef

-- {{{ [[https://github.com/pbrisbin/renters-reality/blob/master/Helpers/Model.hs][source]]

import Yesod
import Data.Maybe (catMaybes)
import qualified Data.Map as M

findOrCreate :: ( YesodPersistBackend m ~ PersistEntityBackend v
                , YesodPersist m
                , PersistUnique (PersistEntityBackend v) (GHandler s m)
                , PersistEntity v
                )
             => v -> GHandler s m (Key (PersistEntityBackend v) v)
findOrCreate v = return . either entityKey id =<< runDB (insertBy v)

-- |
--
-- My solution to the N+1 problem:
--
-- > runDB $ do
-- > posts <- selectList [] []
-- > users <- selectList [] []
-- >
-- > let records = joinTables postUser posts users
-- >
-- > forM records $ \(post,user) -> do
-- > --
-- > -- ...
-- > --
--
joinTables :: (a -> Key (PersistEntityBackend b) b)
           -> [Entity a]
           -> [Entity b]
           -> [(Entity a, Entity b)]
joinTables f as bs = catMaybes . for as $ \a -> fmap (\b -> (a,b)) $ lookupRelation f a bs

joinTables3 :: (a -> Key (PersistEntityBackend b) b)
            -> (a -> Key (PersistEntityBackend c) c)
            -> [Entity a]
            -> [Entity b]
            -> [Entity c]
            -> [(Entity a, Entity b, Entity c)]
joinTables3 f g as bs cs = catMaybes . for as $ \a ->
    case (lookupRelation f a bs, lookupRelation g a cs) of
        (Just b, Just c) -> Just (a,b,c)
        _ -> Nothing

lookupRelation :: (a -> Key (PersistEntityBackend b) b) -> Entity a -> [Entity b] -> Maybe (Entity b)
lookupRelation f a bs = let k = f $ entityVal a
                            vs = M.fromList $ map (\(Entity k' v) -> (k',v)) bs
                        in fmap (\v -> Entity k v) $ M.lookup k vs

for :: [a] -> (a -> b) -> [b]
for xs f = map f xs

-- }}}

date :: IO DiffTime
date = fmap utctDayTime getCurrentTime

isStale :: DiffTime -> CacheWindow -> IO Bool
isStale w x = do
  t <- date
  let a = t - x
  return $! (a > w)


type BOFHExcuse = (Title, Body)
type CacheState = (DiffTime, BOFHExcuse)
type CacheWindow = DiffTime

{-# NOINLINE bofhExcuseCacheR #-}
bofhExcuseCacheR :: IORef (Maybe CacheState)
bofhExcuseCacheR = unsafePerformIO $ newIORef Nothing

getBOFHExcusesC :: (MonadIO m) => CacheWindow -> m BOFHExcuse
getBOFHExcusesC w = _getBOFHExcusesC w bofhExcuseCacheR

_getBOFHExcusesC :: (MonadIO m) => CacheWindow -> IORef (Maybe CacheState) -> m BOFHExcuse
_getBOFHExcusesC windowSize cacheR = do
  cache <- liftIO (readIORef cacheR)
  case cache of
    Just (diffTime,bofhExcuse) ->
      liftIO (
        do
          x <- isStale windowSize diffTime
          if x
            then getFreshBOFHExcuses
            else (return bofhExcuse)
        )
    Nothing -> liftIO getFreshBOFHExcuses
  where
    getFreshBOFHExcuses = do
      t <- date
      e <- getBOFHExcuses
      writeIORef cacheR (Just (t, e))
      return $! e


type Title = String
type Body = String
getBOFHExcuses :: (MonadIO m) => m (Title,Body)
getBOFHExcuses = do
  title:body <- return . filter (/= "") . lines =<< liftIO (readProcess "/usr/games/fortune"
                                                        ["bofh-excuses"] "")
  return $! (title, unlines body)


-- cacheWindow :: DiffTime
-- cacheWindow = 1

-- main = do
--   x <- getBOFHExcusesC cacheWindow
--   putStrLn (show x)
--   main
