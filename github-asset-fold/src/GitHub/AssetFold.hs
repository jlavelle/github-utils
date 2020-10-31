module GitHub.AssetFold where

import           Prelude

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.QSem  (QSem)
import qualified Control.Concurrent.QSem  as QSem
import qualified Control.Exception
import           Control.Foldl            (FoldM (..))
import qualified Control.Foldl
import           Control.Monad.Catch      (MonadCatch, MonadThrow)
import qualified Control.Monad.Catch
import           Control.Monad.Except     (ExceptT (..))
import qualified Control.Monad.Except     as ExceptT
import qualified Data.Bool
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as ByteString
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import           Data.Vector              (Vector)
import qualified Data.Vector              as Vector
import           GitHub
    (AuthMethod, Error, FetchCount, Id, Name, Owner, Release, ReleaseAsset, Repo)
import qualified GitHub
import qualified Network.HTTP.Client      as Http
import qualified Network.HTTP.Client.TLS  as Tls
import Control.Monad.Reader (ReaderT, runReaderT)

withQSem :: QSem -> IO a -> IO a
withQSem s = Control.Exception.bracket_ (QSem.waitQSem s) (QSem.signalQSem s)

-- | Like 'mapConcurrently', but uses a semaphore to limit activity a fixed number of
-- threads at a time.
mapConcurrentlyN :: Traversable t => QSem -> (a -> IO b) -> t a -> IO (t b)
mapConcurrentlyN s f = Async.mapConcurrently (withQSem s . f)

catchHttp :: MonadCatch m => (Http.HttpException -> e) -> m a -> m (Either e a)
catchHttp h a = fmap Right a `Control.Monad.Catch.catch` (pure . Left . h)

foldBodyReader :: FoldM IO ByteString a -> IO ByteString -> IO a
foldBodyReader (FoldM step i extract) br = i >>= go
  where
    go !x = br >>= \case
      bs | ByteString.null bs -> extract x
         | otherwise          -> step x bs >>= go

foldResponseBody :: FoldM IO ByteString a -> Http.Request -> Http.Manager -> IO a
foldResponseBody f r m = Http.withResponse r m $ foldBodyReader f . Http.responseBody

httpFoldedBody :: FoldM IO ByteString a -> Http.Request -> IO a
httpFoldedBody f r = Tls.getGlobalManager >>= foldResponseBody f r

-- | Build a 'Request' for a release asset's file.
releaseAssetRequest :: MonadThrow m => ReleaseAsset -> m Http.Request
releaseAssetRequest = Http.parseUrlThrow . Text.unpack . GitHub.releaseAssetBrowserDownloadUrl

foldReleaseAsset :: FoldM IO ByteString a -> ReleaseAsset -> IO a
foldReleaseAsset f r = releaseAssetRequest r >>= httpFoldedBody f

type AssetFold a = FoldM (ReaderT (Release, ReleaseAsset) IO) ByteString a

runAssetFold :: AssetFold a -> Release -> ReleaseAsset -> IO a
runAssetFold f r a = foldReleaseAsset (Control.Foldl.hoists (`runReaderT` (r, a)) f) a

foldRelease :: QSem -> AssetFold a -> Release -> IO (Vector a)
foldRelease sem f r = mapConcurrentlyN sem (runAssetFold f r) (GitHub.releaseAssets r)

foldReleases :: Traversable f => Int -> AssetFold a -> f Release -> IO (f (Vector a))
foldReleases n f rs = do
  sem <- QSem.newQSem n
  Async.mapConcurrently (foldRelease sem f) rs

-- | Get the releases from a repository, but restrict their assets to those whose ids are not in a
-- a given set, then remove releases without any assets from the result.
getNewRepoAssets
  :: AuthMethod am
  => am
  -> Name Owner
  -> Name Repo
  -> FetchCount
  -> Set (Id ReleaseAsset) -- ^ A set of release asset ids to exclude
  -> IO (Either Error (Vector Release))
getNewRepoAssets am owner repo count old = ExceptT.runExceptT $ do
  rs <- ExceptT $ GitHub.github am $ GitHub.releasesR owner repo count
  pure $ Vector.mapMaybe (maybeNewReleaseAssets old) rs

-- | Filter a release's assets, keeping those whose ids are not in a given set, returning Nothing
-- if the resultant vector is empty.
maybeNewReleaseAssets :: Set (Id ReleaseAsset) -> Release -> Maybe Release
maybeNewReleaseAssets old r =
  let r' = filterAssets (not . assetIdInSet old) r
  in Data.Bool.bool (Just r') Nothing $ emptyRelease r'

emptyRelease :: Release -> Bool
emptyRelease = Vector.null . GitHub.releaseAssets

filterAssets :: (ReleaseAsset -> Bool) -> Release -> Release
filterAssets p r = r { GitHub.releaseAssets = Vector.filter p $ GitHub.releaseAssets r }

assetIdInSet :: Set (Id ReleaseAsset) -> ReleaseAsset -> Bool
assetIdInSet s ra = GitHub.releaseAssetId ra `Set.member` s

-- Note that the fold may be running on up to n threads at a given time, so it should be
-- written in a thread-safe manner.
foldNewRepoAssets
  :: AuthMethod am
  => Int -- ^ The maximum number of concurrent requests
  -> am
  -> Name Owner
  -> Name Repo
  -> FetchCount
  -> Set (Id ReleaseAsset) -- ^ A set of release asset ids to exclude
  -> AssetFold a
  -> IO (Either Error (Vector (Vector a)))
foldNewRepoAssets n am owner repo count old f = ExceptT.runExceptT $ do
  nrs <- ExceptT $ getNewRepoAssets am owner repo count old
  ExceptT $ catchHttp GitHub.HTTPError $ foldReleases n f nrs
