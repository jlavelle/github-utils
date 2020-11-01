{-# Language DeriveAnyClass  #-}
{-# Language QuasiQuotes     #-}
{-# Language RecordWildCards #-}

module GitHub.AssetFold.Main where

import           Prelude

import           Control.Applicative            (Alternative (..))
import qualified Control.Concurrent.Async       as Async
import qualified Control.Concurrent.STM         as STM
import           Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import           Control.DeepSeq                (NFData)
import qualified Control.Exception
import           Control.Foldl                  (Fold, FoldM (..))
import qualified Control.Foldl
import           Control.Monad                  (forever, (>=>))
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Reader           (ReaderT (..))
import qualified Control.Monad.Reader
import qualified Crypto.Hash.SHA256             as Sha256
import           Data.Aeson
    (Encoding, Value, parseJSON, toEncoding, withObject, (.:))
import           Data.Aeson                     (Value (..))
import           Data.Aeson                     (FromJSON, ToJSON)
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
import           Data.Data                      (Data)
import qualified Data.Foldable
import qualified Data.Map.Monoidal              as MonoidalMap
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Profunctor                (Profunctor (..))
import           Data.Proxy                     (Proxy (..))
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Data.Vector                    (Vector)
import           Data.Void                      (absurd)
import           Database.SQLite.Simple         (Connection, NamedParam (..), Query (..))
import qualified Database.SQLite.Simple         as Sqlite
import           GHC.Generics                   (Generic)
import           GitHub
    (AuthMethod, Error, FetchCount (..), Id, Name, Owner, Release, ReleaseAsset, Repo)
import qualified GitHub
import           GitHub.AssetFold               (AssetFold)
import qualified GitHub.AssetFold
import           NeatInterpolation              (text)
import qualified Data.Foldable as Foldable

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
   FetchAll -> Data.Aeson.Encoding.string "all"

parseJSONFetchCount :: Value -> Parser FetchCount
parseJSONFetchCount v = (FetchAtLeast <$> parseJSON v) <|> (FetchAll <$ parseAll v)
  where
    parseAll = \case
      String "all" -> pure ()
      _ -> Control.Applicative.empty

data Codec i o a b = Codec
  { codecEncode :: a -> o
  , codecDecode :: i -> b
  }
  deriving (Functor, Generic)

type Codec' a b = Codec a a b b

instance Profunctor (Codec i o) where
  dimap f g (Codec enc dec) = Codec (lmap f enc) (fmap g dec)

strictify :: Codec LBS.ByteString LBS.ByteString a b -> Codec ByteString ByteString a b
strictify (Codec enc dec) = Codec (LBS.toStrict . enc) (dec . LBS.fromStrict)

binaryCodec :: (Binary a, Binary b) => Codec LBS.ByteString LBS.ByteString a (Either Text b)
binaryCodec = Codec enc dec
  where
    enc = Binary.encode
    dec =
      Data.Bifunctor.bimap (\(_, _, e) -> Text.pack e) (\(_, _, a) -> a)
      . Binary.decodeOrFail

binaryCodecStrict :: Binary a => Codec ByteString ByteString a (Either Text a)
binaryCodecStrict = strictify binaryCodec

jsonCodec :: (ToJSON a, FromJSON a) => Codec LBS.ByteString LBS.ByteString a (Either Text a)
jsonCodec = Codec enc dec
  where
    enc = Aeson.encode
    dec = Data.Bifunctor.first Text.pack . Aeson.eitherDecode

jsonCodecStrict :: (ToJSON a, FromJSON a) => Codec ByteString ByteString a (Either Text a)
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
      MonoidalMap.singleton
        release
        (MonoidalMap.singleton
          platform
          (Map.singleton asset a)
        )
  )

type ReleaseMapData = (Name Release, Name Platform, Name ReleaseAsset)

sha256 :: Fold ByteString ByteString
sha256 = Control.Foldl.Fold Sha256.update Sha256.init Sha256.finalize

assetUrlAndSha256 :: Fold ByteString (Release -> ReleaseAsset -> (Text, ByteString))
assetUrlAndSha256 =
  (\mkUrl bs _ ra -> (mkUrl ra, bs))
  <$> pure GitHub.releaseAssetBrowserDownloadUrl
  <*> sha256

releaseMapMain
  :: AuthMethod am
  => Config am
  -> FoldM IO ByteString (Release -> ReleaseAsset -> a)
  -> Codec' ByteString (ReleaseMapData, a)
  -> (Release -> ReleaseAsset -> a -> ReleaseMapData)
  -> FilePath
  -> (ReleaseMap a -> IO x)
  -> IO x
releaseMapMain config fold codec parseNames dbPath callback = do
  queue <- TBQueue.newTBQueueIO 10
  Sqlite.withConnection dbPath $ \conn ->
    let readIds = setFromFoldable . fmap (GitHub.mkId Proxy) <$> binaryDataSelectIds "release" conn
        readData = fmap (codecDecode codec) <$> binaryDataSelectData "release" conn
        encoder (i, d, a) = (GitHub.untagId i, codecEncode codec (d, a))
        writeData = STM.atomically . TBQueue.writeTBQueue queue . encoder
        writeWorker = queueWorker queue $ uncurry (binaryDataInsert "release" conn)
        fold' = releaseMapFold parseNames fold
        cli ids = assetFoldCli config fold' ids writeData readData (callback . toReleaseMap)
    in Async.withAsync writeWorker $ \a1 ->
         Async.withAsync (readIds >>= cli) $ \a2 ->
           either absurd id <$> Async.waitEither a1 a2

assetFoldCli
  :: AuthMethod am
  => Config am
  -> AssetFold a
  -> Set (Id ReleaseAsset) -- ^ The assets we already know about
  -> (a -> IO r) -- ^ how to write data about new assets
  -> IO b -- ^ how to read saved asset data
  -> (b -> IO x) -- ^ callback to run on all the data after new data is written
  -> IO x
assetFoldCli config fold existing writeData readData callback = do
  enew <- getNewAssets config existing $ postEffect (liftIO . writeData) fold
  case enew of
    Left err -> error $ show err
    Right _ -> do
      d <- readData
      callback d

postEffect :: Monad m => (b -> m r) -> FoldM m a b -> FoldM m a b
postEffect f = composeExtract ((\cb b -> b <$ cb b) f)

-- TODO Does this indicate that FoldM is a right module?
composeExtract :: Monad m => (b -> m r) -> FoldM m a b -> FoldM m a r
composeExtract f (FoldM s i extract) = FoldM s i (extract >=> f)

queueWorker :: TBQueue a -> (a -> IO b) -> IO c
queueWorker q f = forever $ f =<< STM.atomically (TBQueue.readTBQueue q)

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

data SqlFieldType = FieldInteger
  | FieldBlob
  | FieldReal
  | FieldText
  deriving (Eq, Ord, Enum, Show, Generic, Data, NFData)

binaryDataSelectData :: Text -> Connection -> IO (Vector ByteString)
binaryDataSelectData name conn = withCreateTable name conn
  $ foldQuery_ (lmap Sqlite.fromOnly $ Control.Foldl.vectorM) conn
  $ binaryDataSelectDataQuery name

binaryDataSelectDataQuery :: Text -> Query
binaryDataSelectDataQuery name = Query [text| select data from $name |]

binaryDataInsert :: Text -> Connection -> Int -> ByteString -> IO ()
binaryDataInsert name conn a b = withCreateTable name conn $ do
  let params = [ ":id" := a, ":data" := b ]
  Sqlite.executeNamed conn (binaryDataInsertQuery name) params

binaryDataInsertQuery :: Text -> Query
binaryDataInsertQuery name = Query
  [text|
    insert into $name (id, data)
    values (:id, :data)
    on conflict(id) do update set data = excluded.data
  |]

binaryDataSelectIds :: Text -> Connection -> IO (Vector Int)
binaryDataSelectIds name conn = withCreateTable name conn
  $ foldQuery_ (lmap Sqlite.fromOnly $ Control.Foldl.vectorM) conn
  $ binaryDataSelectIdsQuery name

binaryDataSelectIdsQuery :: Text -> Query
binaryDataSelectIdsQuery name = Query [text| select id from $name |]

withCreateTable :: Text -> Connection -> IO a -> IO a
withCreateTable name conn action = Control.Exception.try action >>= \case
  Right a -> pure a
  Left (Sqlite.SQLError Sqlite.ErrorError _ _) -> createTable name conn *> action
  Left e -> Control.Exception.throw e

createTable :: Text -> Connection -> IO ()
createTable name conn = Sqlite.execute_ conn $ binaryDataTableQuery name

binaryDataTableQuery :: Text -> Query
binaryDataTableQuery name = Query $ mkTableSql name FieldInteger [("data", FieldBlob)]

mkTableSql :: Text -> SqlFieldType -> [(Text, SqlFieldType)] -> Text
mkTableSql name idType fieldSpec =
  let fields = toFieldDefs fieldSpec
      idName = nameOf idType
  in [text|
       create table if not exists $name(
         id $idName primary key,
         $fields
       )
     |]

toFieldDefs :: [(Text, SqlFieldType)] -> Text
toFieldDefs =
  Text.intercalate ",\n"
  . fmap
    (\(name, sqlData) ->
      let sqlName = nameOf sqlData
      in [text| $name $sqlName not null |])

nameOf :: SqlFieldType -> Text
nameOf = \case
  FieldInteger -> "integer"
  FieldReal -> "real"
  FieldText -> "text"
  FieldBlob -> "blob"

foldQuery_ :: Sqlite.FromRow r => FoldM IO r o -> Connection -> Query -> IO o
foldQuery_ (FoldM step initial extract) conn query = do
  a <- initial
  r <- Sqlite.fold_ conn query a step
  extract r

setFromFoldable :: (Foldable f, Ord a) => f a -> Set a
setFromFoldable = Set.fromList . Foldable.toList
