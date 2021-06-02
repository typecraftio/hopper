module Data.Hopper.AssetHeader
  ( AssetHeader (..),
    assetPath
  )
where

import qualified Data.Aeson as Json
import qualified Data.Text as Text
import System.FilePath

data AssetHeader = AssetHeader
  { hash :: String,
    size :: Int
  }
  deriving (Eq, Show)

instance Json.FromJSON AssetHeader where
  parseJSON = Json.withObject "AssetHeader" $ \v ->
    AssetHeader
      <$> v Json..: Text.pack "hash"
      <*> v Json..: Text.pack "size"

-- | Get the conventional asset path from the hash name, base directory.
-- >>> path "~/.minecraft/assets/objects" "hash123"
assetPath :: FilePath -> String -> FilePath
assetPath baseDir hashName =
  baseDir </> take 2 hashName </> hashName
