module Test.BotInterface (test) where

import BotInterface.Setup (keysDir, pParamsFile)
import LocalCluster.Cluster (runUsingCluster)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (setEnv)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase)

test :: TestTree
test = testCase "Bot interface integration - setup" $ do
  withTestConf . runUsingCluster $ \cEnv -> do
    doesDirectoryExist (keysDir cEnv)
      >>= assertBool "Required directory not found after setup run"
    doesFileExist (pParamsFile cEnv)
      >>= assertBool "Protocol params file not found after setup run"

withTestConf :: IO b -> IO b
withTestConf runTest = do
  setEnv "SHELLEY_TEST_DATA" "cluster-data/cardano-node-shelley"
  setEnv "NO_POOLS" "1"
  setEnv "CARDANO_NODE_TRACING_MIN_SEVERITY" "Error"
  setEnv "TESTS_TRACING_MIN_SEVERITY" "Error"
  runTest
