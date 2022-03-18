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
import Plutus.Contract (Contract, ContractError (ConstraintResolutionContractError), submitTx, utxosAt)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Test.Plutip.Contract (ValueOrdering (VLt), withContract, withContractAs, assertExecution, initAndAssertAdaWith, initAda, initAndAssertAda, initLovelace)
import Test.Plutip.Internal.Types (
  FailureReason (CaughtException, ContractExecutionError),
 )
import Test.Plutip.LocalCluster (withCluster)
import Test.Tasty (TestTree)
import Text.Printf (printf)
import Test.Plutip.Predicate (shouldSucceed, shouldYield, shouldFail, yieldSatisfies, errorSatisfies, shouldThrow)
import Test.Plutip.Predicate qualified as Predicate

-- FIXME: something prints node configs polluting test outputs even with maximum log severity
-- upd: (https://github.com/input-output-hk/cardano-node/blob/4ad6cddd40517c2eb8c3df144a6fa6737952aa92/cardano-node/src/Cardano/Node/Run.hs#L117)

test :: TestTree
test =
  withCluster
    "Basic integration: launch, add wallet, tx from wallet to wallet"
    [ assertExecution "Contract 1" 
        (initAda 100) 
        (withContract $ const getUtxos)
        [shouldSucceed, Predicate.not shouldFail]
    , assertExecution "Contract 2" 
        (initAda 100) 
        (withContract $ const getUtxosThrowsErr)
        [shouldFail, Predicate.not shouldSucceed]
    , assertExecution "Contract 3"
        (initAda 100) 
        (withContract $ const getUtxosThrowsEx)
        [shouldFail, Predicate.not shouldSucceed]
    , assertExecution "Pay negative amount"
        (initAda 100) 
        (withContract $ \[pkh1] -> payTo pkh1 (-10_000_000))
        [shouldFail]
    , assertExecution "Pay from wallet to wallet" 
        (initAda 100 <> initAndAssertAda 100 110)
        (withContract $ \[pkh1] -> payTo pkh1 10_000_000)
        [shouldSucceed]
    , assertExecution "Two contracts after each other"
        (initAndAssertAdaWith 100 VLt 100 <> initAndAssertAdaWith 100 VLt 100)
        (do
          void $ -- run something prior to the contract which result will be checked 
            withContract $
              \[pkh1] -> payTo pkh1 10_000_000
          withContractAs 1 $ -- run contract which result will be checked -- TODO: some explanation
            \[pkh1] -> payTo pkh1 10_000_000)
        [shouldSucceed]
    , assertExecution "Wallet initiation creates single UTxO"
        (initAda 100)
        (withContract $ const getUtxos)
        [yieldSatisfies ((==1) . Map.size)]
    , let initFunds = 10_000_000
       in assertExecution
            "Should yield own initial Ada"
            (initLovelace $ toEnum initFunds)
            (withContract $ const ownValue)
            [shouldYield (lovelaceValueOf $ toEnum initFunds)]
    , let err = ConstraintResolutionContractError OwnPubKeyMissing
       in assertExecution
            "Should throw `ConstraintResolutionError OwnPubKeyMissing`"
            (initAda 100)
            (withContract $ const getUtxosThrowsErr)
            [shouldThrow err, errorSatisfies (== err)]
    ]

-- TODO: to be ported
    -- , let initFunds = 10_000_000
    --    in shouldHaveObservableState
    --         "Should have state with list of single initial Ada value"
    --         (initLovelace $ toEnum initFunds)
    --         [lovelaceValueOf $ toEnum initFunds]
    --         (withContract $ const ownValueToState)
    -- , let stateLen = 2
    --    in assertObservableStateWith
    --         "Should have state with length 2"
    --         (initAda 101)
    --         ((== stateLen) . length)
    --         (withContract $ const $ replicateM_ stateLen ownValueToState)
    -- , assertContractError
    --     "Should throw anything but `Contract.OtherError"
    --     (initAda 100)
    --     (\case Contract.OtherContractError _ -> False; _ -> True)
    --     (withContract $ const getUtxosThrowsErr)
    -- , let pred' = \case
    --         CaughtException e -> isJust @ErrorCall (fromException e)
    --         _ -> False
    --    in assertFailure
    --         "Should throw `ErrorCall` exception"
    --         (initAda 100)
    --         pred'
    --         (withContract $ const getUtxosThrowsEx)
    -- ]

getUtxos :: Contract [Value] EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxos = do
  pkh <- Contract.ownPaymentPubKeyHash
  Contract.logInfo @String $ printf "Own PKH: %s" (show pkh)
  utxosAt $ pubKeyHashAddress pkh Nothing

getUtxosThrowsErr :: Contract () EmptySchema ContractError (Map TxOutRef ChainIndexTxOut)
getUtxosThrowsErr =
  Contract.throwError $ ConstraintResolutionContractError OwnPubKeyMissing

getUtxosThrowsEx :: Contract () EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxosThrowsEx = error "This Exception was thrown intentionally in Contract.\n"

payTo :: PaymentPubKeyHash -> Integer -> Contract () EmptySchema Text CardanoTx
payTo toPkh amt = do
  submitTx (Constraints.mustPayToPubKey toPkh (Ada.lovelaceValueOf amt))

ownValue :: Contract [Value] EmptySchema Text Value
ownValue = foldMap (^. ciTxOutValue) <$> getUtxos 

ownValueToState :: Contract [Value] EmptySchema Text ()
ownValueToState =
  ownValue >>= Contract.tell . (: [])
