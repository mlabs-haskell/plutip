module Test.BotInterface (test) where

import BotInterface.Setup (keysDir, pParamsFile)

-- import LocalCluster.Cluster (runUsingCluster)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import DSL
import Network.HTTP.Client (
  Response (responseStatus),
  defaultManagerSettings,
  httpNoBody,
  newManager,
  parseRequest,
 )
import Network.HTTP.Types.Status (status200)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (setEnv)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase)

test :: TestTree
test = testCase "Bot interface integration" $ do
  withTestConf . runUsingCluster $ do
    cEnv <- ask
    liftIO $
      doesDirectoryExist (keysDir cEnv)
        >>= assertBool "Required directory not found after setup run"
    liftIO $
      doesFileExist (pParamsFile cEnv)
        >>= assertBool "Protocol params file not found after setup run"
    liftIO $
      tipChainIndex
        >>= assertBool "Chain index not available" . (== status200) . responseStatus
  where
    tipChainIndex = do
      mgr <- newManager defaultManagerSettings
      req <- parseRequest "http://localhost:9083/tip" --todo: port from cEnv
      httpNoBody req mgr

withTestConf :: IO b -> IO b
withTestConf runTest = do
  setEnv "SHELLEY_TEST_DATA" "cluster-data/cardano-node-shelley"
  setEnv "NO_POOLS" "1"
  setEnv "CARDANO_NODE_TRACING_MIN_SEVERITY" "Error"
  setEnv "TESTS_TRACING_MIN_SEVERITY" "Error"
  runTest
