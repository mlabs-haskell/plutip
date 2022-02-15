module Spec.Integration (test) where

import Cardano.Api (AssetId (AdaAssetId), Quantity (Quantity), TxOut (TxOut), UTxO (UTxO, unUTxO), txOutValueToValue, valueToList)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (CardanoTx, ChainIndexTxOut, PaymentPubKeyHash, TxOutRef, pubKeyHashAddress)
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Plutus.Contract (Contract, ownPaymentPubKeyHash, submitTx, utxosAt, waitNSlots)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Test.Plutip (
  ada,
  addSomeWallet,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
  runContract,
  runUsingCluster,
  waitSeconds,
  ledgerPaymentPkh,
 )
import Test.Plutip.Internal.LocalCluster.Types (isSuccess)
import Test.Plutip.Tools.CardanoApi (utxosAtAddress)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Text.Printf (printf)

-- FIXME: something prints node configs polluting test outputs even with maximum log severity
-- upd: (https://github.com/input-output-hk/cardano-node/blob/4ad6cddd40517c2eb8c3df144a6fa6737952aa92/cardano-node/src/Cardano/Node/Run.hs#L117)
test :: TestTree
test = do
  testCase "Basic integration: launch, add wallet, tx from wallet to wallet" $ do
    runUsingCluster $ do
      w1 <- addSomeWallet (ada 101)
      checkFunds w1 (ada 101)
      w2 <- addSomeWallet (ada 102)
      checkFunds w2 (ada 102)

      assertSucceeds
        "Get utxos"
        (runContract w1 getUtxos)
      assertFails
        "Get utxos throwing error"
        (runContract w1 getUtxosThrowsErr)
      assertFails
        "Get utxos throwing exception"
        (runContract w1 getUtxosThrowsEx)
      assertFails
        "Pay negative amount"
        (runContract w1 (payTo (ledgerPaymentPkh w2) (-10_000_000)))

      checkAdaTxFromTo w1 w2
  where
    checkFunds wallet' expectedAmt = do
      let expectedAmt' = toInteger expectedAmt
      ask >>= \cEnv -> do
        waitSeconds 2
        liftIO $ do
          res <- utxosAtAddress cEnv (cardanoMainnetAddress wallet')
          let resultValue = toCombinedFlatValue <$> res
          resultValue @?= Right [(AdaAssetId, Quantity expectedAmt')]

    assertSucceeds tag act = do
      act >>= liftIO . assertBool (tag <> " did not succeed") . isSuccess

    assertFails tag act = do
      act >>= liftIO . assertBool (tag <> " did not fail") . not . isSuccess

    checkAdaTxFromTo w1 w2 = do
      res <- runContract w1 (payTo (ledgerPaymentPkh w2) 10_000_000)
      cEnv <- ask
      liftIO $ do
        assertBool ("Wallet to wallet tx failed: " <> show res) (isSuccess res)
        utxosAtAddress cEnv (cardanoMainnetAddress w2)
          >>= \case
            Left e ->
              assertFailure $ "Failed ot get wallet UTxO: " <> show e
            Right (UTxO utxo) ->
              let utxoCnt = Map.size utxo
               in assertBool
                    ("Should be 2 UTxO at destination wallet, but request returned " <> show utxoCnt)
                    (utxoCnt == 2)

getUtxos :: Contract () EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxos = do
  pkh <- Contract.ownPaymentPubKeyHash
  Contract.logInfo @String $ printf "Own PKH: %s" (show pkh)
  utxosAt $ pubKeyHashAddress pkh Nothing

getUtxosThrowsErr :: Contract () EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxosThrowsErr =
  Contract.throwError $ Text.pack "This Error was thrown intentionally by Contract \n"

getUtxosThrowsEx :: Contract () EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxosThrowsEx = error "This Exception was thrown intentionally in Contract.\n"

payTo :: PaymentPubKeyHash -> Integer -> Contract () EmptySchema Text CardanoTx
payTo toPkh amt = do
  ownPkh <- ownPaymentPubKeyHash
  tx <-
    submitTx
      ( Constraints.mustPayToPubKey toPkh (Ada.lovelaceValueOf amt)
          <> Constraints.mustBeSignedBy ownPkh
      )
  void $ waitNSlots 1
  pure tx

toCombinedFlatValue :: UTxO era -> [(AssetId, Quantity)]
toCombinedFlatValue =
  mconcat
    . fmap (valueToList . txOutValueToValue . getValue)
    . Map.elems
    . unUTxO
  where
    getValue (TxOut _ v _) = v
