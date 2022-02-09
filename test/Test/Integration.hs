module Test.Integration (test) where

import Cardano.Api (AssetId (AdaAssetId), Quantity (Quantity), TxOut (TxOut), UTxO (UTxO, unUTxO), txOutValueToValue, valueToList)
import Data.Map qualified as Map
import System.Environment (setEnv)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Tools.CardanoApi (utxosAtAddress)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import DSL (
  RunResult (RunFailed, RunSuccess),
  ada,
  addSomeWallet,
  cardanoMainnetAddress,
  runContract,
  runUsingCluster,
  waitSeconds,
 )

import DebugContract.GetUtxos qualified as DebugContract
import DebugContract.PayToWallet qualified as DebugContract

import BotInterface.Wallet (ledgerPaymentPkh)

-- FIXME: something prints node configs polluting test outputs
test :: TestTree
test = do
  testCase "Basic integration: launch, add wallet, tx from wallet to wallet" $ do
    withTestConf . runUsingCluster $ do
      w1 <- addSomeWallet (ada 101) >>= either (error . show) pure
      checkFunds w1 (ada 101)
      w2 <- addSomeWallet (ada 102) >>= either (error . show) pure
      checkFunds w2 (ada 102)

      assertSucceeds
        "Get utxos"
        (runContract w1 DebugContract.getUtxos)
      assertFails
        "Get utxos throwing error"
        (runContract w1 DebugContract.getUtxosThrowsErr)
      assertFails
        "Get utxos throwing exception"
        (runContract w1 DebugContract.getUtxosThrowsEx)
      assertFails
        "Pay negative amount"
        (runContract w1 (DebugContract.payTo (ledgerPaymentPkh w2) (-10_000_000)))

      checkAdaTxFromTo w1 w2
  where
    checkFunds wallet' expectedAmt = do
      let expectedAmt' = toInteger expectedAmt
      ask >>= \cEnv -> liftIO $ do
        waitSeconds 2
        res <- utxosAtAddress cEnv (cardanoMainnetAddress wallet')
        let resultValue = toCombinedFlatValue <$> res
        resultValue @?= Right [(AdaAssetId, Quantity expectedAmt')]

    assertSucceeds tag act = do
      act >>= liftIO . assertBool (tag <> " did not succeed") . isSuccess

    assertFails tag act = do
      act >>= liftIO . assertBool (tag <> " did not fail") . not . isSuccess

    checkAdaTxFromTo w1 w2 = do
      res <- runContract w1 (DebugContract.payTo (ledgerPaymentPkh w2) 10_000_000)
      cEnv <- ask
      liftIO $ do
        assertBool ("Wallet to wallet tx failed: " <> show res) (isSuccess res)
        waitSeconds 1 -- todo: some "wait tx processed" could be handy
        utxosAtAddress cEnv (cardanoMainnetAddress w2)
          >>= \case
            Left e ->
              assertFailure $ "Failed ot get wallet UTxO: " <> show e
            Right (UTxO utxo) ->
              let utxoCnt = Map.size utxo
               in assertBool
                    ("Should be 2 UTxO at destination wallet, but request returned " <> show utxoCnt)
                    (utxoCnt == 2)

    isSuccess = \case
      RunSuccess _ _ -> True
      RunFailed _ -> False

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
