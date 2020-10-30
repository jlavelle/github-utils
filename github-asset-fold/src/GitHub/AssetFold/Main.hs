{-# Language DeriveAnyClass  #-}
{-# Language QuasiQuotes     #-}
{-# Language RecordWildCards #-}

module GitHub.AssetFold.Main where

import           Prelude

import           Control.Applicative            (Alternative ((<|>)))
import           Control.Applicative            (Alternative (empty))
import qualified Control.Concurrent.Async       as Async
import qualified Control.Concurrent.STM         as STM
import           Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import           Control.DeepSeq                (NFData)
import qualified Control.Exception
import           Control.Foldl                  (FoldM (..))
import qualified Control.Foldl
import           Control.Monad                  ((>=>))
import           Control.Monad.Catch            (MonadMask)
import qualified Control.Monad.Catch
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import qualified Control.Monad.Reader
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
import           Data.Function                  ((&))
import           Data.Functor                   ((<&>))
import qualified Data.Map.Monoidal              as MonoidalMap
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Profunctor                (Profunctor (..))
import qualified Data.Proxy
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Data.Vector                    (Vector)
import           Data.Void                      (Void, absurd)
import           Database.SQLite.Simple         (Connection, NamedParam (..), Query (..))
import qualified Database.SQLite.Simple         as Sqlite
import           GHC.Generics                   (Generic)
import           GitHub
    (AuthMethod, Error, FetchCount (..), Id, Name, Owner, Release, ReleaseAsset, Repo)
import qualified GitHub
import           GitHub.AssetFold               (AssetFold)
import qualified GitHub.AssetFold
import           NeatInterpolation              (text)

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

-- | Profunctor instance over the i and o params
newtype Codecabio a b i o = Codecabio { unCodecabio :: Codec i o a b }

instance Functor (Codecabio a b i) where
  fmap f (Codecabio c) = Codecabio $ c { codecEncode = f <$> codecEncode c }

instance Profunctor (Codecabio a b) where
  dimap f g (Codecabio (Codec enc dec)) = Codecabio (Codec (fmap g enc) (lmap f dec))

instance Profunctor (Codec i o) where
  dimap f g (Codec enc dec) = Codec (lmap f enc) (fmap g dec)

binaryCodec :: Binary a => Codec LBS.ByteString LBS.ByteString a (Either Text a)
binaryCodec = Codec enc dec
  where
    enc = Binary.encode
    dec =
      Data.Bifunctor.bimap (\(_, _, e) -> Text.pack e) (\(_, _, a) -> a)
      . Binary.decodeOrFail

jsonCodec :: (ToJSON a, FromJSON a) => Codec LBS.ByteString LBS.ByteString a (Either Text a)
jsonCodec = Codec enc dec
  where
    enc = Aeson.encode
    dec = Data.Bifunctor.first Text.pack . Aeson.eitherDecode

data Persist m q r i o = Persist
  { persistRead :: q -> m o
  , persistWrite :: i -> m r
  }
  deriving (Functor, Generic)

instance Functor m => Profunctor (Persist m q r) where
  dimap f g (Persist r w) = Persist ((fmap . fmap) g r) (lmap f w)

cmapQuery :: (p -> q) -> Persist m q r i o -> Persist m p r i o
cmapQuery f p = p { persistRead = lmap f $ persistRead p }

mapResponse :: Functor m => (r -> s) -> Persist m q r i o -> Persist m q s i o
mapResponse f p = p { persistWrite = (fmap . fmap) f $ persistWrite p }

hoistPersist :: (forall a. m a -> n a) -> Persist m q r i o -> Persist n q r i o
hoistPersist n (Persist r w) = Persist (\q -> n $ r q) (\i -> n $ w i)

cached :: Monad m => Persist m a x (a, b) (Maybe b) -> (a -> m b) -> a -> m b
cached (Persist r w) f a = r a >>= maybe (f a >>= \b -> b <$ w (a, b)) pure

-- | Persistence for binary data indexed by an integer id. The data will be written into a table
-- called tableName (which will be created whenever an operation is run if it doesn't exist) with
-- the schema: (id integer primary key, data blob)
binSqliteIds :: Text -> Persist IO Connection r Void (Vector Int)
binSqliteIds table = Persist (binaryDataSelectIds table) absurd

binSqliteData :: Text -> Persist IO Connection () (Connection, (Int, ByteString)) (Vector (Int, ByteString))
binSqliteData table = Persist pread pwrite
  where
    pread conn = binaryDataSelectAll table conn
    pwrite (conn, (rid, bs)) = binaryDataInsert table conn rid bs

-- TODO Look at how the managed library handles this and what operations it can be used for
data Resource m h = Resource
  { resourceAcquire :: m h
  , resourceRelease :: h -> m ()
  , resourceWith :: forall x . (h -> m x) -> m x
  }

defaultResourceWith :: MonadMask m => m h -> (h -> m ()) -> (h -> m x) -> m x
defaultResourceWith = Control.Monad.Catch.bracket

persistWithResource
  :: Functor m
  => Resource m h
  -> Persist m (h, q) r (h, i) o
  -> (Persist m q r i o -> m x)
  -> m x
persistWithResource (Resource _ _ with) p f = with (\h -> f $ cmapQuery (h,) $ lmap (h,) p)

sqliteResource :: FilePath -> Resource IO Connection
sqliteResource dbname = Resource
  { resourceAcquire = Sqlite.open dbname
  , resourceRelease = Sqlite.close
  , resourceWith = Sqlite.withConnection dbname
  }

data Platform

tagPlatform :: Text -> Name Platform
tagPlatform = GitHub.mkName (Data.Proxy.Proxy @Platform)

type ReleaseMap a = Map (Name Release) (Map (Name Platform) (Map (Name ReleaseAsset) a))

releaseMapFold
  :: (Release -> ReleaseAsset -> a -> ReleaseMapData)
  -> FoldM IO ByteString a
  -> AssetFold (ReleaseMapData, Id ReleaseAsset, a)
releaseMapFold parseNames =
  composeExtract
    (\a -> Control.Monad.Reader.ask <&>
       \(r, ra) -> (parseNames r ra a, GitHub.releaseAssetId ra, a)
    )
  . Control.Foldl.hoists liftIO

toReleaseMap :: Vector (ReleaseMapData, Id ReleaseAsset, a) -> ReleaseMap a
toReleaseMap = Data.Coerce.coerce . foldMap
  (\((release, platform, asset), _, a) ->
      MonoidalMap.singleton
        release
        (MonoidalMap.singleton
          platform
          (Map.singleton asset a)
        )
  )

releaseMapCodec
  :: Codec' ByteString (ReleaseMapData, a)
  -> Persist IO q r (Int, ByteString) (Vector (Int, ByteString))
  -> Persist IO q r (ReleaseMapData, Id ReleaseAsset, a) (Vector (ReleaseMapData, Id ReleaseAsset, a))
releaseMapCodec (Codec enc dec) = dimap
  (\(d, i, a) -> (GitHub.untagId i, enc (d, a)))
  (fmap (\(i, bs) -> dec bs & \(r, a) -> (r, GitHub.mkId Data.Proxy.Proxy i, a)))

type ReleaseMapData = (Name Release, Name Platform, Name ReleaseAsset)

releaseMapMain
  :: AuthMethod am
  => Config am
  -> FoldM IO ByteString a
  -> Codec' ByteString (ReleaseMapData, a)
  -> (Release -> ReleaseAsset -> a -> ReleaseMapData)
  -> (ReleaseMap a -> IO x)
  -> IO x
releaseMapMain config fold codec parseNames callback = do
  queue <- TBQueue.newTBQueueIO 10
  let pids = binSqliteIds "release"
      (pdat, writeWorker) = workerize queue $ binSqliteData "release"
  resourceWith (sqliteResource "data.db") $ \conn ->
    Async.withAsync writeWorker $ \_ ->
      assetFoldCli
        config
        (releaseMapFold parseNames fold)
        (cmapQuery (const conn) pids)
        (releaseMapCodec codec (cmapQuery (const conn) $ lmap (conn,) pdat))
        (lmap toReleaseMap callback)

workerize
  :: TBQueue i
  -> Persist IO q r i o
  -> (Persist IO q () i o, IO r)
workerize queue (Persist r w) =
  let persist = Persist r (\i -> STM.atomically $ TBQueue.writeTBQueue queue i)
      worker  = persistenceWorker queue w
  in (persist, worker)

assetFoldCli
  :: AuthMethod am
  => Config am
  -> AssetFold a
  -> Persist IO () r Void (Vector Int)
  -> Persist IO () r a b
  -> (b -> IO x)
  -> IO x
assetFoldCli config fold pids pdata callback = do
  existing <- persistRead pids ()
  getNewAssets config existing $ postEffect (persistWrite $ hoistPersist liftIO pdata) fold
  d <- persistRead pdata ()
  callback d

postEffect :: Monad m => (b -> m r) -> FoldM m a b -> FoldM m a b
postEffect f = composeExtract ((\cb b -> b <$ cb b) f)

composeExtract :: Monad m => (b -> m r) -> FoldM m a b -> FoldM m a r
composeExtract f (FoldM s i extract) = FoldM s i (extract >=> f)

persistenceWorker :: TBQueue a -> (a -> IO x) -> IO x
persistenceWorker q f = f =<< STM.atomically (TBQueue.readTBQueue q)

-- Just wraps foldNewRepoAssets
getNewAssets
  :: AuthMethod am
  => Config am
  -> Vector Int
  -> AssetFold a
  -> IO (Either Error (Vector (Vector a)))
getNewAssets Config{..} existing fold =
  GitHub.AssetFold.foldNewRepoAssets
    configMaxRequests
    configAuth
    configOwner
    configRepo
    configFetchCount
    (Set.fromList $ Data.Foldable.toList $ fmap (GitHub.mkId Data.Proxy.Proxy) $ existing)
    fold

data SqlFieldType = FieldInteger
  | FieldBlob
  | FieldReal
  | FieldText
  deriving (Eq, Ord, Enum, Show, Generic, Data, NFData)

binaryDataSelectAll :: Text -> Connection -> IO (Vector (Int, ByteString))
binaryDataSelectAll name conn = withCreateTable name conn
  $ foldQuery_ Control.Foldl.vectorM conn
  $ binaryDataSelectAllQuery name

binaryDataSelectAllQuery :: Text -> Query
binaryDataSelectAllQuery name = Query [text| select id, data from $name |]

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
  $ foldQuery_ (lmap Sqlite.fromOnly Control.Foldl.vectorM) conn
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
