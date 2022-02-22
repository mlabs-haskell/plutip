module Spec.Integration (test) where

import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (CardanoTx, ChainIndexTxOut, PaymentPubKeyHash, TxOutRef, pubKeyHashAddress)
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Plutus.Contract (Contract, submitTx, utxosAt)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Test.Plutip.Contract (initAda, initAndAssertAda, shouldFail, shouldSucceed)
import Test.Plutip.LocalCluster (withCluster)
import Test.Tasty (TestTree)
import Text.Printf (printf)

-- FIXME: something prints node configs polluting test outputs even with maximum log severity
-- upd: (https://github.com/input-output-hk/cardano-node/blob/4ad6cddd40517c2eb8c3df144a6fa6737952aa92/cardano-node/src/Cardano/Node/Run.hs#L117)
test :: TestTree
test =
  withCluster
    "Basic integration: launch, add wallet, tx from wallet to wallet"
    [ shouldSucceed "Get utxos" (initAda 100) $ const getUtxos
    , shouldFail "Get utxos throwing error" (initAda 100) $ const getUtxosThrowsErr
    , shouldFail "Get utxos throwing exception" (initAda 100) $ const getUtxosThrowsEx
    , shouldFail "Pay negative amount" (initAda 300 <> initAda 200) $
        \[pkh1] -> payTo pkh1 (-10_000_000)
    , shouldSucceed "Pay from wallet to wallet" (initAda 100 <> initAndAssertAda 100 110) $
        \[pkh1] -> payTo pkh1 10_000_000
    ]

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
  submitTx (Constraints.mustPayToPubKey toPkh (Ada.lovelaceValueOf amt))
