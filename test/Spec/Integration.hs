module Spec.Integration (test) where

import Control.Exception (ErrorCall, Exception (fromException))
import Control.Lens ((^.))
import Control.Monad (replicateM_, void)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Ledger (CardanoTx, ChainIndexTxOut, PaymentPubKeyHash, TxOutRef, Value, ciTxOutValue, pubKeyHashAddress)
import Ledger.Ada qualified as Ada
import Ledger.Constraints (MkTxError (OwnPubKeyMissing))
import Ledger.Constraints qualified as Constraints
import Plutus.Contract (Contract, ContractError (ConstraintResolutionError), submitTx, utxosAt)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Test.Plutip.Contract (ValueOrdering (VLt), assertContractError, assertFailure, assertObservableStateWith, assertYieldedResultWith, initAda, initAndAssertAda, initAndAssertAdaWith, initLovelace, shouldFail, shouldHaveObservableState, shouldSucceed, shouldThrowContractError, shouldYield, withContract, withContractAs)
import Test.Plutip.Internal.Types (
  FailureReason (CaughtException),
 )
import Test.Plutip.LocalCluster (withCluster)
import Test.Tasty (TestTree)
import Text.Printf (printf)

-- FIXME: something prints node configs polluting test outputs even with maximum log severity
-- upd: (https://github.com/input-output-hk/cardano-node/blob/4ad6cddd40517c2eb8c3df144a6fa6737952aa92/cardano-node/src/Cardano/Node/Run.hs#L117)

test :: TestTree
test =
  withCluster
    "Basic integration: launch, add wallet, tx from wallet to wallet"
    [ shouldSucceed "Get utxos" (initAda 100) $ withContract $ const getUtxos
    , shouldFail "Get utxos throwing error" (initAda 100) $ withContract $ const getUtxosThrowsErr
    , shouldFail "Get utxos throwing exception" (initAda 100) $ withContract $ const getUtxosThrowsEx
    , shouldFail "Pay negative amount" (initAda 300 <> initAda 200) $
        withContract $
          \[pkh1] -> payTo pkh1 (-10_000_000)
    , shouldSucceed "Pay from wallet to wallet" (initAda 100 <> initAndAssertAda 100 110) $
        withContract $ \[pkh1] -> payTo pkh1 10_000_000
    , shouldSucceed
        "Two contracts after each other"
        (initAndAssertAdaWith 100 VLt 100 <> initAndAssertAdaWith 100 VLt 100)
        $ do
          void $
            withContract $
              \[pkh1] -> payTo pkh1 10_000_000
          withContractAs 1 $
            \[pkh1] -> payTo pkh1 10_000_000
    , assertYieldedResultWith
        "Wallet initiation creates single UTxO"
        (initAda 100)
        (\v -> Map.size v == 1)
        (withContract $ const getUtxos)
    , let initFunds = 10_000_000
       in shouldYield
            "Should yield own initial Ada"
            (initLovelace $ toEnum initFunds)
            (lovelaceValueOf $ toEnum initFunds)
            (withContract $ const ownValue)
    , let initFunds = 10_000_000
       in shouldHaveObservableState
            "Should have state with list of single initial Ada value"
            (initLovelace $ toEnum initFunds)
            [lovelaceValueOf $ toEnum initFunds]
            (withContract $ const ownValueToState)
    , let stateLen = 2
       in assertObservableStateWith
            "Should have state with length 2"
            (initAda 101)
            ((== stateLen) . length)
            (withContract $ const $ replicateM_ stateLen ownValueToState)
    , let err = ConstraintResolutionError OwnPubKeyMissing
       in shouldThrowContractError
            "Should throw `ConstraintResolutionError OwnPubKeyMissing`"
            (initAda 100)
            err
            (withContract $ const getUtxosThrowsErr)
    , assertContractError
        "Should throw anything but `Contract.OtherError"
        (initAda 100)
        (\case Contract.OtherError _ -> False; _ -> True)
        (withContract $ const getUtxosThrowsErr)
    , let pred' = \case
            CaughtException e -> isJust @ErrorCall (fromException e)
            _ -> False
       in assertFailure
            "Should throw `ErrorCall` exception"
            (initAda 100)
            pred'
            (withContract $ const getUtxosThrowsEx)
    ]

getUtxos :: Contract [Value] EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxos = do
  pkh <- Contract.ownPaymentPubKeyHash
  Contract.logInfo @String $ printf "Own PKH: %s" (show pkh)
  utxosAt $ pubKeyHashAddress pkh Nothing

getUtxosThrowsErr :: Contract () EmptySchema ContractError (Map TxOutRef ChainIndexTxOut)
getUtxosThrowsErr =
  Contract.throwError $ ConstraintResolutionError OwnPubKeyMissing

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
