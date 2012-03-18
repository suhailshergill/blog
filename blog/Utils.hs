{-# LANGUAGE NoMonomorphismRestriction #-}

module Utils
       ( getBOFHExcuses
       ) where

import Prelude
import System.Process (readProcess)
import Control.Monad.IO.Class

type Title = String
type Body = String
getBOFHExcuses :: (MonadIO m) => m (Title,Body)
getBOFHExcuses = do
  title:body <- return . filter (/= "") . lines =<< liftIO (readProcess "/usr/games/fortune"
                                                        ["bofh-excuses"] "")
  return $! (title, unlines body)


-- main = getBOFHExcuses
