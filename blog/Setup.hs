#!/usr/bin/env runhaskell
module Main where
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription
import System.Process (runCommand,  waitForProcess )

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks 
         { preBuild = genCompass }

runPostConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
runPostConf _ _ _ _ = return () 

genCompass :: Args -> BuildFlags -> IO HookedBuildInfo
genCompass _ _ = do
  putStrLn "Running Compass"
  waitForProcess =<< runCommand "compass compile compass"
  return emptyHookedBuildInfo
