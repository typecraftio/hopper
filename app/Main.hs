module Main where

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as ByteString
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Hopper as Hopper
import System.Directory
import System.FilePath
import Data.Time.Clock.POSIX

-- TODO: command line arguments
main :: IO ()
main = do
  before <- getPOSIXTime
  assetsDir <- Hopper.getMinecraftAssetsDirectory
  langMap <- Hopper.inspectAllLangAssets assetsDir
  currDir <- getCurrentDirectory
  let path = currDir </> "langs.json"
  let contents = Json.encode langMap
  ByteString.writeFile path contents
  after <- getPOSIXTime
  print $ "langs: " ++ path
  print $ "took: " ++ show (after - before)
