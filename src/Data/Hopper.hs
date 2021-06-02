module Data.Hopper where

import Control.Concurrent.Async
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as ByteString
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Hopper.AssetHeader (AssetHeader (..))
import qualified Data.Hopper.AssetHeader as AssetHeader
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
import System.FilePath

-- | Format to "major.minor", example: '1.16'
type Version = String

getMinecraftAssetsDirectory :: IO FilePath
getMinecraftAssetsDirectory =
  (</> "assets") . (</> ".minecraft") . takeDirectory <$> getAppUserDataDirectory ""

inspectAllLangAssets :: FilePath -> IO (HashMap Version (HashMap String (HashMap Text Text)))
inspectAllLangAssets assetsDir =
  do
    indexMap <- inspectAssetIndexes assetsDir
    currDir <- getCurrentDirectory
    let langIndexMap = HashMap.filterWithKey (\k v -> filterLangAsset $ Text.unpack k) <$> indexMap
    mapConcurrently (readAllLangAssets . HashMap.toList) langIndexMap

genMaterialsForScala2 :: [Text] -> Text
genMaterialsForScala2 ids = Text.empty

-- | Generate a source code for Scala 2
--
-- Expected input: `[item|block].minecraft.{name}`
genMaterialScala :: Text -> Text
genMaterialScala id = Text.empty

filterLangAsset :: String -> Bool
filterLangAsset = isPrefixOf "minecraft/lang/"

-- | Read all lang assets
--
-- [("lang/ko_kr.json", "/path/path")] -> IO $ [("ko_kr", [("sword", "검")])]
readAllLangAssets :: [(Text, FilePath)] -> IO (HashMap String (HashMap Text Text))
readAllLangAssets paths =
  do
    langs <- mapConcurrently (readLangAsset . snd) paths
    pure $ HashMap.fromList $ zip locales langs
  where
    locales = takeBaseName . Text.unpack . fst <$> paths

-- | Read a lang asset
--
-- The given `FilePath` should be pointed at [(String,String)] shaped json.
readLangAsset :: FilePath -> IO (HashMap Text Text)
readLangAsset path = do
  str <- ByteString.readFile path
  let map = fold (Json.decode str :: Maybe (HashMap Text Text))
  pure map

parseLangAsset :: Json.Value -> HashMap Text Text
parseLangAsset node =
  fold (Json.fromJSON node :: Json.Result (HashMap Text Text))

-- | Finds all assets path and headers in the given directory.
--
-- The path would be "~/.minecraft/assets" for official Minecraft launcher.
inspectAssetIndexes :: FilePath -> IO (HashMap Version (HashMap Text FilePath))
inspectAssetIndexes dir =
  do
    indexPaths <- listDirectory indexDir
    jsons <- mapConcurrently readJson $ (indexDir </>) <$> indexPaths
    let indexNodes = HashMap.map (AssetHeader.assetPath objectDir . AssetHeader.hash) . parseAssetIndexNode <$> jsons
    pure $ HashMap.fromList $ zip (map takeBaseName indexPaths) indexNodes
  where
    indexDir = dir </> "indexes"
    objectDir = dir </> "objects"

-- | Parse the json to get a assetname-header map.
--
-- Expected json input is:
--
-- {"objects": {"lang\/ko_kr.json": {hash: "abc", size: 123}}}
--
-- Example:
--
-- >>> let json = Char8.pack "{\"objects\": {\"lang/ko_kr.json\": {\"hash\": \"hash123\", \"size\": 123}}}"
-- >>> let node = fromMaybe Json.Null (Json.decode json :: Maybe Json.Value)
-- >>> parseAssetIndexNode node
-- fromList [("lang/ko_kr.json",AssetHeader {hash = "hash123", size = 123})]
parseAssetIndexNode :: Json.Value -> HashMap Text AssetHeader -- Either [(Text, String)] (HashMap Text AssetHeader)?
parseAssetIndexNode node =
  fromMaybe HashMap.empty $ HashMap.lookup (Text.pack "objects") m
  where
    m = fold (Json.fromJSON node :: Json.Result (HashMap Text (HashMap Text AssetHeader)))

readJson :: FilePath -> IO Json.Value
readJson path =
  do
    contents <- ByteString.readFile path
    let node = Json.decode contents :: Maybe Json.Value
    pure $ fromMaybe Json.Null node
