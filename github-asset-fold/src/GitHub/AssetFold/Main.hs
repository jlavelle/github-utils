{-# Language DeriveAnyClass  #-}
{-# Language RecordWildCards #-}

module GitHub.AssetFold.Main where

import           Prelude

import           Control.Applicative  (Alternative ((<|>)))
import qualified Control.Applicative
import           Control.DeepSeq      (NFData)
import           Control.Foldl        (FoldM)
import           Data.Aeson           (Encoding, Value, parseJSON, toEncoding, withObject, (.:))
import           Data.Aeson           (Value (..))
import           Data.Aeson.Encoding  (pair, pairs)
import qualified Data.Aeson.Encoding
import           Data.Aeson.Types     (Parser)
import           Data.Binary          (Binary)
import qualified Data.Binary          as Binary
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable
import           Data.Functor         (($>))
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           GHC.Generics         (Generic)
import           GitHub
    (AuthMethod, Error, FetchCount (..), Id, Name, Owner, Release, ReleaseAsset, Repo)
import qualified GitHub
import qualified GitHub.AssetFold

type ReleaseData a = Map Release (Map ReleaseAsset a)

data Config am = Config
  { configOwner       :: !(Name Owner)
  , configRepo        :: !(Name Repo)
  , configAuth        :: !am
  , configMaxRequests :: !Int
  , configPersistPath :: !FilePath
  , configFetchCount  :: !FetchCount
  }
  deriving (Eq, Ord, Show, Generic, NFData, Functor, Foldable,
          Traversable)

toEncodingConfig :: Config () -> Encoding
toEncodingConfig Config{..} = pairs $ Data.Foldable.fold
  [ pair "owner" $ toEncoding configOwner
  , pair "repo" $ toEncoding configRepo
  , pair "maxRequests" $ toEncoding configMaxRequests
  , pair "persistPath" $ toEncoding configPersistPath
  , pair "fetchCount" $ toEncodingFetchCount configFetchCount
  ]

parseJSONConfig :: Value -> Parser (Config ())
parseJSONConfig = withObject "Config" $ \obj -> Config
  <$> obj .: "owner"
  <*> obj .: "repo"
  <*> pure ()
  <*> obj .: "maxRequests"
  <*> obj .: "persistPath" <*> (parseJSONFetchCount =<< obj .: "fetchCount")

toEncodingFetchCount :: FetchCount -> Encoding
toEncodingFetchCount = \case
   FetchAtLeast w -> toEncoding w
   FetchAll -> Data.Aeson.Encoding.string "all"

parseJSONFetchCount :: Value -> Parser FetchCount
parseJSONFetchCount v = (FetchAtLeast <$> parseJSON v) <|> (FetchAll <$ parseAll v)
  where
    parseAll = \case
      String "all" -> pure ()
      _ -> Control.Applicative.empty

simpleMain
  :: (Binary a, AuthMethod am)
  => Config am
  -> (ReleaseData a -> IO ())
  -> FoldM IO ByteString a
  -> IO ()
simpleMain config projection fold =
  updateReleaseData config fold >>= either (error . show) projection

updateReleaseData
  :: (Binary a, AuthMethod am)
  => Config am
  -> FoldM IO ByteString a
  -> IO (Either Error (ReleaseData a))
updateReleaseData config@Config{ configPersistPath } fold = do
  existing <- loadBinary configPersistPath
  eupdated <- addNewAssets config existing fold
  traverse (\r -> writeBinary configPersistPath r $> r) eupdated

addNewAssets
  :: AuthMethod am
  => Config am
  -> ReleaseData a
  -> FoldM IO ByteString a
  -> IO (Either Error (ReleaseData a))
addNewAssets Config{..} existing fold = do
  enew <-
    GitHub.AssetFold.foldNewRepoAssets
      configMaxRequests
      configAuth
      configOwner
      configRepo
      configFetchCount
      (releaseAssetIds existing)
      fold
  pure $ merge existing <$> enew

merge :: ReleaseData a -> ReleaseData a -> ReleaseData a
merge existing new = Map.unionWith (Map.union) new existing

releaseAssetIds :: ReleaseData a -> Set (Id ReleaseAsset)
releaseAssetIds = foldMap (Set.fromList . fmap GitHub.releaseAssetId . Map.keys) . Map.elems

loadBinary :: Binary b => FilePath -> IO b
loadBinary path = Binary.decode <$> LBS.readFile path

writeBinary :: Binary b => FilePath -> b -> IO ()
writeBinary path = LBS.writeFile path . Binary.encode
