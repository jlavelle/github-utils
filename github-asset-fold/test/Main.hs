module Main where

import           Prelude

import qualified Control.Foldl
import qualified Data.Aeson            as Aeson
import qualified Data.ByteString.Lazy  as LBS
import           Data.Proxy            (Proxy (..))
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text.Encoding
import qualified GitHub
import           GitHub.AssetFold.Main (Config (..))
import qualified GitHub.AssetFold.Main as AssetFold.Main
import           Test.Tasty            (TestTree)
import qualified Test.Tasty
import qualified Test.Tasty.Golden

main :: IO ()
main = Test.Tasty.defaultMain $ Test.Tasty.testGroup "github-asset-fold" [ integrationTest ]

integrationTest :: TestTree
integrationTest = Test.Tasty.testGroup "Integration tests"
  [ Test.Tasty.Golden.goldenVsString
      "mctesting123/github-utils-test release map"
      "test/github-utils-test-release-map.golden"
      githubUtilsTestReleaseMap
  ]

githubUtilsTestReleaseMap :: IO LBS.ByteString
githubUtilsTestReleaseMap = do
  auth <- AssetFold.Main.envTokenAuth "GITHUB_TOKEN"
  AssetFold.Main.releaseMapMain (config auth) fold codec parseNames ":memory:" callback
  where
    config auth = Config
      { configOwner = "mctesting123"
      , configRepo = "github-utils-test"
      , configAuth = auth
      , configMaxRequests = 4
      , configFetchCount = GitHub.FetchAll
      }

    fold = Control.Foldl.generalize
      $ fmap (\a _ _ -> Text.Encoding.decodeUtf8 a)
      $ Control.Foldl.foldMap id id

    codec = either (error "decode error") id <$> AssetFold.Main.binaryCodecStrict

    parseNames release asset _ =
      let (bin, plat) = case Text.splitOn "-" $ GitHub.releaseAssetName asset of
            [b, _, p] -> (b, p)
            _ -> error "Failed to parse release asset data"
      in ( GitHub.mkName Proxy $ GitHub.releaseName release
         , GitHub.mkName Proxy plat
         , GitHub.mkName Proxy bin
         )

    callback = pure . Aeson.encode
