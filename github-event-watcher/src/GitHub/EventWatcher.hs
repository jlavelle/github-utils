module GitHub.EventWatcher where

import           Prelude

import qualified Control.Concurrent
import           Control.DeepSeq              (NFData)
import qualified Control.Exception
import           Control.Monad.Catch          (MonadThrow)
import           Data.Binary                  (Binary)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Lazy         as LBS
import           Data.Data                    (Data)
import qualified Data.List
import qualified Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import qualified Data.Text.Read               as Text
import           Data.Time                    (UTCTime)
import qualified Data.Time
import           Data.Vector                  (Vector)
import           Data.Vector.NonEmpty         (NonEmptyVector)
import qualified Data.Vector.NonEmpty         as NonEmptyVector
import           GHC.Generics                 (Generic)
import           GitHub                       (AuthMethod, Name, Organization, Owner, Repo, User)
import qualified GitHub
import           Network.HTTP.Client          (Manager, Request, Response)
import qualified Network.HTTP.Client          as Http
import qualified Network.HTTP.Client.Internal as Http
import qualified Network.HTTP.Types           as HttpTypes
import           Network.HTTP.Types.Header    (HeaderName)
import qualified Network.HTTP.Types.Header    as HttpTypes

data EventEndpoint = Public
  | PublicNetwork (Name Owner) (Name Repo)
  | PublicOrganization (Name Organization)
  | Repository (Name Owner) (Name Repo)
  | AuthenticatedUser (Name User)
  | AuthenticatedUserOrganization (Name User) (Name Organization)
  | PublicUser (Name User)
  | PublicRecievedUser (Name User)
  | AuthenticatedRecievedUser (Name User)
  deriving (Eq, Ord, Show, Data, Generic, Binary, NFData)

newtype ETag = ETag ByteString
  deriving (Eq, Ord, Show, Data, Generic)
  deriving anyclass (Binary, NFData)

data LogEvent = Waiting Int
  | Requesting
  | NewEvents Int
  | NoNewEvents
  deriving (Eq, Ord, Show, Data, Generic, Binary, NFData)

data LogEntry = LogEntry
  { logEntryEvent    :: LogEvent
  , logEntryDateTime :: UTCTime
  }
  deriving (Eq, Ord, Show, Data, Generic, Binary, NFData)

-- "https://api.github.com/foo/bar/baz"
githubUrl :: [Text] -> String
githubUrl ps = "https://api.github.com" <> "/" <> Text.unpack (Text.intercalate "/" ps)

urlFor :: EventEndpoint -> String
urlFor = githubUrl . \case
  Public -> [ "events" ]
  PublicNetwork no nr -> [ "networks", GitHub.toPathPart no, GitHub.toPathPart nr, "events" ]
  PublicOrganization no -> [ "orgs", GitHub.toPathPart no, "events" ]
  Repository no nr -> [ "repos", GitHub.toPathPart no, GitHub.toPathPart nr, "events" ]
  AuthenticatedUser nu -> [ "users", GitHub.toPathPart nu, "events" ]
  AuthenticatedUserOrganization nu no ->
    [ "users", GitHub.toPathPart nu, "events", "orgs", GitHub.toPathPart no ]
  PublicUser nu -> [ "users", GitHub.toPathPart nu, "events", "public" ]
  PublicRecievedUser nu -> [ "users", GitHub.toPathPart nu, "received_events" ]
  AuthenticatedRecievedUser nu -> [ "users", GitHub.toPathPart nu, "received_events", "public" ]

parseEventsRequest :: MonadThrow m => AuthMethod am => am -> EventEndpoint -> Maybe ETag -> m Request
parseEventsRequest am ep metag = do
  req <- Http.parseUrlThrow (urlFor ep)
  pure $ GitHub.setAuthRequest am $ setHeaders metag req

setHeaders :: Maybe ETag -> Request -> Request
setHeaders m = setAccept . setUserAgent . setETag m

setETag :: Maybe ETag -> Request -> Request
setETag met req = case met of
  Just (ETag bs) -> setRequestHeader HttpTypes.hIfNoneMatch bs req
  Nothing        -> req

setAccept :: Request -> Request
setAccept = setRequestHeader HttpTypes.hAccept "application/vnd.github.v3+json"

setUserAgent :: Request -> Request
setUserAgent = setRequestHeader HttpTypes.hUserAgent "github-utils/0.1.0.0"

setRequestHeader :: HeaderName -> ByteString -> Request -> Request
setRequestHeader k v r =
  r { Http.requestHeaders = (k, v) : filter (not . (==) k . fst) (Http.requestHeaders r) }


parsePollingInterval :: Response a -> Int
parsePollingInterval res = Data.Maybe.fromMaybe 0 $ do
  ibs <- Data.List.lookup "x-poll-interval" $ Http.responseHeaders res
  parseInt $ Text.decodeUtf8 ibs

logEntry :: (LogEntry -> IO ()) -> LogEvent -> IO ()
logEntry wl ev = do
  now <- Data.Time.getCurrentTime
  wl $ LogEntry { logEntryEvent = ev, logEntryDateTime = now }

-- | Can throw a @Network.HTTP.Client.HttpException@
pollEvents
  :: AuthMethod am
  => am
  -> Int -- ^ Desired polling interval (will not be respected if lower than x-poll-interval response header)
  -> Manager -- ^ The 'Network.HTTP.Client.Manager' used to make requests. It should support SSL/TLS.
  -> EventEndpoint -- ^ The endpoint to poll
  -> Maybe ETag -- ^ The last ETag received, or Nothing if one has not been received yet.
  -> (LogEntry -> IO ()) -- ^ A logging function
  -> (Maybe ETag -> Vector LBS.ByteString -> IO ()) -- ^ A callback that is executed when events are received
  -> IO ()
pollEvents auth interval manager endpoint met writeLog callback =
  let logger = logEntry writeLog
      mkReq = parseEventsRequest auth endpoint met
      loop delay maybeETag req = do
        logger $ Waiting delay
        Control.Concurrent.threadDelay $ (max interval delay) * 1000000
        logger Requesting
        res <- Control.Exception.try $ execPollRequest req manager
        case res of
          Right (delay', maybeETag', evs) -> do
            logger $ NewEvents $ length evs
            callback maybeETag' evs
            loop delay' maybeETag' $ setETag maybeETag' req
          Left (match304Status -> Just httpRes) -> do
            logger NoNewEvents
            let delay' = parsePollingInterval httpRes
            loop delay' maybeETag req
          Left err -> Control.Exception.throw err
  in mkReq >>= loop 0 met

execPollRequest
  :: Request
  -> Manager
  -> IO (Int, Maybe ETag, Vector LBS.ByteString)
execPollRequest req manager = do
  page1 <- Http.httpLbs req manager
  pages <- getAllPages page1 req manager
  let metag = getETag page1
      delay = parsePollingInterval page1
  pure (delay, metag, NonEmptyVector.toVector pages)

getETag :: Response a -> Maybe ETag
getETag res = ETag <$> Data.List.lookup "etag" (Http.responseHeaders res)

match304Status :: Http.HttpException -> Maybe (Response ())
match304Status = \case
  (Http.HttpExceptionRequest _
   (Http.StatusCodeException res _)) -> case Http.responseStatus res of
      HttpTypes.Status 304 _ -> Just res
      _                      -> Nothing
  _ -> Nothing

-- | Traverse a series of paginated responses, collecting the bodies in a vector.
getAllPages :: Response LBS.ByteString -> Request -> Manager -> IO (NonEmptyVector LBS.ByteString)
getAllPages page1 req1 manager = do
  req <- nextUrlRequest page1 req1
  let b1  = Http.responseBody page1
  NonEmptyVector.unfoldr1M unfoldPages b1 req
  where
    unfoldPages :: Maybe Request -> IO (Maybe (LBS.ByteString, Maybe Request))
    unfoldPages = \case
      Nothing -> pure Nothing
      Just req -> do
        res <- Http.httpLbs req manager
        mreq <- nextUrlRequest res req1
        pure $ Just (Http.responseBody res, mreq)

nextUrlRequest :: MonadThrow m => Response a -> Request -> m (Maybe Request)
nextUrlRequest res req = traverse (Http.setUri req) $ GitHub.getNextUrl res

parseInt :: Text -> Maybe Int
parseInt = either (const Nothing) (Just . fst) . Text.decimal
