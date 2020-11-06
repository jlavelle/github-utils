{-# Language DeriveAnyClass  #-}
{-# Language QuasiQuotes     #-}
{-# Language RecordWildCards #-}

module GitHub.AssetFold.Main
  ( Codec (..)
  , Codec'
  , Config (..)
  , ReleaseMap
  , assetUrlAndSha256
  , binaryCodec
  , binaryCodecStrict
  , envTokenAuth
  , jsonCodec
  , jsonCodecStrict
  , parseJSONConfig
  , releaseMapMain
  , sha256
  , strictify
  , tagPlatform
  , toEncodingConfig
  ) where

import           Prelude

import           Control.Applicative            (Alternative (..))
import qualified Control.Concurrent.Async       as Async
import qualified Control.Concurrent.STM         as STM
import           Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import           Control.DeepSeq                (NFData)
import           Control.Foldl                  (Fold (..), FoldM (..))
import qualified Control.Foldl
import           Control.Monad                  ((>=>))
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Reader           (ReaderT (..))
import qualified Control.Monad.Reader
import qualified Crypto.Hash.SHA256             as Sha256
import           Data.Aeson
    (Encoding, FromJSON, ToJSON, Value (..), parseJSON, toEncoding, withObject, (.:))
import qualified Data.Aeson                     as Aeson
import           Data.Aeson.Encoding            (pair, pairs)
import qualified Data.Aeson.Encoding
import           Data.Aeson.Types               (Parser)
import qualified Data.Bifunctor
import           Data.Binary                    (Binary)
import qualified Data.Binary                    as Binary
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Coerce
import qualified Data.Foldable
import qualified Data.Map.Monoidal
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Proxy                     (Proxy (..))
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import qualified Data.String
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Data.Vector                    (Vector)
import qualified Database.SQLite.Simple         as Sqlite
import           GHC.Generics                   (Generic)
import           GitHub
    (Auth, AuthMethod, Error, FetchCount (..), Id, Name, Owner, Release, ReleaseAsset, Repo)
import qualified GitHub
import           GitHub.AssetFold               (AssetFold)
import qualified GitHub.AssetFold
import           SQLite.BinaryCache             (Cache (..), Codec (..))
import qualified SQLite.BinaryCache             as BinaryCache
import qualified System.Environment

-- TODO This should really be called something more descriptive
data Config am = Config
  { configOwner       :: (Name Owner)
  , configRepo        :: (Name Repo)
  , configAuth        :: am
  , configMaxRequests :: Int
  , configFetchCount  :: FetchCount
  }
  deriving (Eq, Ord, Show, Generic, NFData, Functor, Foldable,
          Traversable)

toEncodingConfig :: Config () -> Encoding
toEncodingConfig Config{..} = pairs $ Data.Foldable.fold
  [ pair "owner" $ toEncoding configOwner
  , pair "repo" $ toEncoding configRepo
  , pair "maxRequests" $ toEncoding configMaxRequests
  , pair "fetchCount" $ toEncodingFetchCount configFetchCount
  ]

parseJSONConfig :: Value -> Parser (Config ())
parseJSONConfig = withObject "Config" $ \obj -> Config
  <$> obj .: "owner"
  <*> obj .: "repo"
  <*> pure ()
  <*> obj .: "maxRequests"
  <*> (parseJSONFetchCount =<< obj .: "fetchCount")

toEncodingFetchCount :: FetchCount -> Encoding
toEncodingFetchCount = \case
   FetchAtLeast w -> toEncoding w
   FetchAll       -> Data.Aeson.Encoding.string "all"

parseJSONFetchCount :: Value -> Parser FetchCount
parseJSONFetchCount v = (FetchAtLeast <$> parseJSON v) <|> (FetchAll <$ parseAll v)
  where
    parseAll = \case
      String "all" -> pure ()
      _            -> Control.Applicative.empty

strictify :: Codec LBS.ByteString a b -> Codec ByteString a b
strictify (Codec enc dec) = Codec (LBS.toStrict . enc) (dec . LBS.fromStrict)

binaryCodec :: (Binary a, Binary b) => Codec LBS.ByteString a (Either Text b)
binaryCodec = Codec enc dec
  where
    enc = Binary.encode
    dec =
      Data.Bifunctor.bimap (\(_, _, e) -> Text.pack e) (\(_, _, a) -> a)
      . Binary.decodeOrFail

binaryCodecStrict :: (Binary a, Binary b) => Codec ByteString a (Either Text b)
binaryCodecStrict = strictify binaryCodec

jsonCodec :: (ToJSON a, FromJSON b) => Codec LBS.ByteString a (Either Text b)
jsonCodec = Codec enc dec
  where
    enc = Aeson.encode
    dec = Data.Bifunctor.first Text.pack . Aeson.eitherDecode

jsonCodecStrict :: (ToJSON a, FromJSON b) => Codec ByteString a (Either Text b)
jsonCodecStrict = strictify jsonCodec

data Platform

tagPlatform :: Text -> Name Platform
tagPlatform = GitHub.mkName (Proxy @Platform)

type ReleaseMap a = Map (Name Release) (Map (Name Platform) (Map (Name ReleaseAsset) a))

slorp :: Monad m => FoldM m a (b -> c) -> FoldM (ReaderT b m) a c
slorp = composeExtract (\f -> ReaderT $ \b -> pure $ f b) . Control.Foldl.hoists (ReaderT . const)

releaseMapFold
  :: (Release -> ReleaseAsset -> a -> ReleaseMapData)
  -> FoldM IO ByteString (Release -> ReleaseAsset -> a)
  -> AssetFold (Id ReleaseAsset, ReleaseMapData, a)
releaseMapFold parseNames = composeExtract applyParse . slorp . fmap uncurry
  where
    applyParse a = do
      (r, ra) <- Control.Monad.Reader.ask
      pure (GitHub.releaseAssetId ra, parseNames r ra a, a)

toReleaseMap :: Vector (ReleaseMapData, a) -> ReleaseMap a
toReleaseMap = Data.Coerce.coerce . foldMap
  (\((release, platform, asset), a) ->
      Data.Map.Monoidal.singleton
        release
        (Data.Map.Monoidal.singleton
          platform
          (Map.singleton asset a)
        )
  )

type ReleaseMapData = (Name Release, Name Platform, Name ReleaseAsset)

sha256 :: Fold ByteString ByteString
sha256 = Fold Sha256.update Sha256.init Sha256.finalize

assetUrlAndSha256 :: Fold ByteString (Release -> ReleaseAsset -> (Text, ByteString))
assetUrlAndSha256 =
  (\mkUrl bs _ ra -> (mkUrl ra, bs))
  <$> pure GitHub.releaseAssetBrowserDownloadUrl
  <*> sha256

type Codec' a b = Codec a b b

data ReleaseCache a = ReleaseCache
  { releaseCacheWrite    :: (Id ReleaseAsset, ReleaseMapData, a) -> IO ()
  , releaseCacheReadIds  :: IO (Set (Id ReleaseAsset))
  , releaseCacheReadData :: IO (Vector (ReleaseMapData, a))
  }

withReleaseCache
  :: FilePath
  -> Codec' ByteString (ReleaseMapData, a)
  -> (ReleaseCache a -> IO b)
  -> IO b
withReleaseCache path (Codec enc dec) action = Sqlite.withConnection path $ \conn ->
  let cache = Cache @Int conn "release"
      releaseCacheWrite (i, d, a) = BinaryCache.write cache (GitHub.untagId i) $ enc (d, a)
      -- TODO We don't want to rely on the fields being called id and data, fix this upstream
      releaseCacheReadIds =
        setFromFoldable . fmap (GitHub.mkId Proxy) <$> BinaryCache.selectField cache "id"
      releaseCacheReadData = fmap dec <$> BinaryCache.selectField cache "data"
  in action ReleaseCache{releaseCacheWrite, releaseCacheReadIds, releaseCacheReadData}

releaseMapMain
  :: AuthMethod am
  => Config am
  -> FoldM IO ByteString (Release -> ReleaseAsset -> a)
  -> Codec' ByteString (ReleaseMapData, a)
  -> (Release -> ReleaseAsset -> a -> ReleaseMapData)
  -> FilePath
  -> (ReleaseMap a -> IO r)
  -> IO r
releaseMapMain config fold codec parseNames dbPath callback =
  withReleaseCache dbPath codec $ \ReleaseCache{..} -> do
    let done = releaseCacheReadData >>= callback . toReleaseMap
    withQueueWorker releaseCacheWrite (const done) $ \queue -> do
      existing <- releaseCacheReadIds
      enew <- getNewAssets config existing $ enqueueFold queue
      case enew of
        Left err -> fail $ show err
        Right _  -> pure ()
  where
    enqueueFold queue = postEffect
      (liftIO . STM.atomically . TBQueue.writeTBQueue queue . Just)
      (releaseMapFold parseNames fold)

postEffect :: Monad m => (b -> m r) -> FoldM m a b -> FoldM m a b
postEffect f = composeExtract ((\cb b -> b <$ cb b) f)

-- TODO Does this indicate that FoldM is a right module?
composeExtract :: Monad m => (b -> m r) -> FoldM m a b -> FoldM m a r
composeExtract f (FoldM s i extract) = FoldM s i (extract >=> f)

-- Process elements added to a queue in the interim function, automatically making sure the queue is
-- empty before continuing. This addresses the eariler issues with releaseMapMain calling its
-- callback before all of the data was written to the cache, resulting in incomplete output.
withQueueWorker :: (a -> IO b) -> (x -> IO r) -> (TBQueue (Maybe a) -> IO x) -> IO r
withQueueWorker workFn done interim = do
  queue <- TBQueue.newTBQueueIO 10
  Async.withAsync (queueWorker queue workFn) $ \worker -> do
    Async.link worker
    x <- interim queue
    STM.atomically $ TBQueue.writeTBQueue queue Nothing
    Async.wait worker
    done x

-- Stops on the first Nothing it receives
queueWorker :: TBQueue (Maybe a) -> (a -> IO b) -> IO ()
queueWorker q f = loop
  where
    loop = STM.atomically (TBQueue.readTBQueue q) >>= maybe (pure ()) (\a -> f a *> loop)

-- Just wraps foldNewRepoAssets
getNewAssets
  :: AuthMethod am
  => Config am
  -> Set (Id ReleaseAsset)
  -> AssetFold a
  -> IO (Either Error (Vector (Vector a)))
getNewAssets Config{..} existing fold =
  GitHub.AssetFold.foldNewRepoAssets
    configMaxRequests
    configAuth
    configOwner
    configRepo
    configFetchCount
    existing
    fold

envTokenAuth :: String -> IO Auth
envTokenAuth n = GitHub.OAuth . Data.String.fromString <$> System.Environment.getEnv n

setFromFoldable :: (Foldable f, Ord a) => f a -> Set a
setFromFoldable = Set.fromList . Data.Foldable.toList
