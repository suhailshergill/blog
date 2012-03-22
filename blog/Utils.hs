module Utils
       ( getBOFHExcuses
         , getBOFHExcusesC
       ) where

import Prelude
import System.Process (readProcess)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Time.Clock (getCurrentTime, utctDayTime, DiffTime)

import System.IO.Unsafe
import Data.IORef

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
      writeIORef bofhExcuseCacheR (Just (t, e))
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
