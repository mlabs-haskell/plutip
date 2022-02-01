module Test.Integration (test) where

import BotInterface.Wallet qualified as BW
import Cardano.Api (AssetId (AdaAssetId), Quantity (Quantity), TxOut (TxOut), UTxO (unUTxO), txOutValueToValue, valueToList)
import Data.Map qualified as Map
import LocalCluster.Cluster (runUsingCluster)
import System.Environment (setEnv)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Tools.CardanoApi (utxosAtAddress)
import Utils (ada, waitSeconds)

-- FIXME: something prints node configs polluting test outputs
test :: TestTree
test = do
  testCase "Basic integration: launch and add wallet" $ do
    withTestConf . runUsingCluster $ \cEnv -> do
      ws <-
        BW.usingEnv cEnv $
          BW.addSomeWallet (ada 101)
      case ws of
        Left e -> assertFailure $ "Error: " <> show e
        Right wallet -> checkFunds cEnv wallet
  where
    checkFunds cEnv wallet' = do
      waitSeconds 2
      res <- utxosAtAddress cEnv (BW.cardanoMainnetAddress wallet')
      let resultValue = toCombinedFlatValue <$> res
      resultValue @?= Right [(AdaAssetId, Quantity 101000000)]

withTestConf :: IO b -> IO b
withTestConf runTest = do
  setEnv "SHELLEY_TEST_DATA" "cluster-data/cardano-node-shelley"
  setEnv "NO_POOLS" "1"
  setEnv "CARDANO_NODE_TRACING_MIN_SEVERITY" "Error"
  setEnv "TESTS_TRACING_MIN_SEVERITY" "Error"
  runTest

toCombinedFlatValue :: UTxO era -> [(AssetId, Quantity)]
toCombinedFlatValue =
  mconcat
    . fmap (valueToList . txOutValueToValue . getValue)
    . Map.elems
    . unUTxO
  where
    getValue (TxOut _ v _) = v
