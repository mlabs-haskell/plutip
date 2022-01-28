module Test.Integration (test) where

import Cardano.Api (AssetId (AdaAssetId), Quantity (Quantity), TxOut (TxOut), UTxO (unUTxO), txOutValueToValue, valueToList)
import Data.Map qualified as Map
import LocalCluster.CardanoApi (utxosAtAddress)
import LocalCluster.Cluster (runUsingCluster)
import LocalCluster.Wallet (addWallet, cwAddress, someWallet)
import System.Environment (setEnv)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Utils (ada, waitSeconds)

-- FIXME: something prints node configs polluting test outputs
test :: TestTree
test = testCase "Basic integration: launch and add wallet" $ do
  withTestConf . runUsingCluster $ \cEnv -> do
    singleWallet <- addWallet cEnv $ someWallet (ada 707)
    waitSeconds 2
    res <- utxosAtAddress cEnv (cwAddress singleWallet)
    let resultValue = toCombinedFlatValue <$> res
    resultValue @?= Right [(AdaAssetId, Quantity 707000000)]

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
