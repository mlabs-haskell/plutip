module Spec.Integration (test) where

import Control.Monad (void)
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (CardanoTx, ChainIndexTxOut, PaymentPubKeyHash, TxOutRef, pubKeyHashAddress)
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Plutus.Contract (Contract, ownPaymentPubKeyHash, submitTx, utxosAt, waitNSlots)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Test.Plutip.Contract (ada, ledgerPaymentPkh, shouldFail, shouldSucceed)
import Test.Plutip.LocalCluster (withCluster)
import Test.Tasty (TestTree)
import Text.Printf (printf)

-- FIXME: something prints node configs polluting test outputs even with maximum log severity
-- upd: (https://github.com/input-output-hk/cardano-node/blob/4ad6cddd40517c2eb8c3df144a6fa6737952aa92/cardano-node/src/Cardano/Node/Run.hs#L117)
test :: TestTree
test =
  withCluster
    "Basic integration: launch, add wallet, tx from wallet to wallet"
    [ shouldSucceed "Get utxos" (ada 100) $ const getUtxos
    , shouldFail "Get utxos throwing error" (ada 100) $ const getUtxosThrowsErr
    , shouldFail "Get utxos throwing exception" (ada 100) $ const getUtxosThrowsEx
    , shouldFail "Pay negative amount" (ada 300 <> ada 200) $ \[w1] ->
        payTo (ledgerPaymentPkh w1) (-10_000_000)
    ]

-- where
--   checkFunds wallet' expectedAmt = do
--     let expectedAmt' = toInteger expectedAmt
--     ask >>= \cEnv -> do
--       waitSeconds 2
--       liftIO $ do
--         res <- utxosAtAddress cEnv (cardanoMainnetAddress wallet')
--         let resultValue = toCombinedFlatValue <$> res
--         resultValue @?= Right [(AdaAssetId, Quantity expectedAmt')]

--   checkAdaTxFromTo w1 w2 = do
--     res <- wiithPlutusInterface w1 (payTo (ledgerPaymentPkh w2) 10_000_000)
--     cEnv <- ask
--     liftIO $ do
--       assertBool ("Wallet to wallet tx failed: " <> show res) (isSuccess res)
--       utxosAtAddress cEnv (cardanoMainnetAddress w2)
--         >>= \case
--           Left e ->
--             assertFailure $ "Failed ot get wallet UTxO: " <> show e
--           Right (UTxO utxo) ->
--             let utxoCnt = Map.size utxo
--              in assertBool
--                   ("Should be 2 UTxO at destination wallet, but request returned " <> show utxoCnt)
--                   (utxoCnt == 2)

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
