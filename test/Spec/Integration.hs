module Spec.Integration (test) where

import Control.Lens ((^.))
import Control.Monad (replicateM_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (CardanoTx, ChainIndexTxOut, PaymentPubKeyHash, TxOutRef, Value, ciTxOutValue, pubKeyHashAddress)
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Plutus.Contract (Contract, submitTx, utxosAt)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.V1.Ledger.Ada
import Test.Plutip.Contract (assertObservableStateWith, assertYieldedResultWith, initAda, initAndAssertAda, initLovelace, shouldFail, shouldHaveObservableState, shouldSucceed, shouldYield)
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
    , assertYieldedResultWith
        "Wallet initiation creates single UTxO"
        (initAda 100)
        (\v -> Map.size v == 1)
        (const getUtxos)
    , let initFunds = 10_000_000
       in shouldYield
            "Should yield own initial Ada"
            (initLovelace $ toEnum initFunds)
            (lovelaceValueOf $ toEnum initFunds)
            (const ownValue)
    , let initFunds = 10_000_000
       in shouldHaveObservableState
            "Should have state with list of single initial Ada value"
            (initLovelace $ toEnum initFunds)
            [lovelaceValueOf $ toEnum initFunds]
            (const ownValueToState)
    , let stateLen = 2
       in assertObservableStateWith
            "Should have state with length 2"
            (initAda 101)
            ((== stateLen) . length)
            (const $ replicateM_ stateLen ownValueToState)
    ]

getUtxos :: Contract [Value] EmptySchema Text (Map TxOutRef ChainIndexTxOut)
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

ownValue :: Contract [Value] EmptySchema Text Value
ownValue =
  foldMap (^. ciTxOutValue) <$> getUtxos

ownValueToState :: Contract [Value] EmptySchema Text ()
ownValueToState =
  ownValue >>= Contract.tell . (: [])
