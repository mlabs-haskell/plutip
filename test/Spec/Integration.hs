module Spec.Integration (test) where

import Control.Exception (ErrorCall, Exception (fromException))
import Control.Monad (void)
import Data.Default (Default (def))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Row (Row)
import Data.Text (Text, isInfixOf, pack)
import Data.Typeable (Typeable)
import Ledger.Ada (lovelaceValueOf)
import Ledger.Constraints (MkTxError (CannotSatisfyAny))
import Plutus.Contract (
  ContractError (ConstraintResolutionContractError),
  waitNSlots,
 )
import Plutus.Contract qualified as Contract
import Spec.TestContract.AdjustTx (runAdjustTest)
import Spec.TestContract.AlwaysFail (lockThenFailToSpend)
import Spec.TestContract.LockSpendMint (lockThenSpend)
import Spec.TestContract.MintAndPay (zeroAdaOutTestContract)
import Spec.TestContract.MustBeSignedBy qualified as MustBeSignedBy
import Spec.TestContract.SimpleContracts (
  getUtxos,
  getUtxosThrowsErr,
  getUtxosThrowsEx,
  ownValue,
  ownValueToState,
  payTo,
 )
import Spec.TestContract.ValidateTimeRange (failingTimeContract, successTimeContract)
import Test.Plutip.Config (PlutipConfig (extraConfig))
import Test.Plutip.Contract (
  TestWallets,
  ValueOrdering (VLt),
  assertExecution,
  assertExecutionWith,
  initAda,
  initAndAssertAda,
  initAndAssertAdaWith,
  initAndAssertLovelace,
  initLovelace,
  withCollateral,
  withContract,
  withContractAs,
 )
import Test.Plutip.Internal.Cluster.Extra.Types (ExtraConfig (ecSlotLength))
import Test.Plutip.Internal.Types (
  ClusterEnv,
  FailureReason (CaughtException, ContractExecutionError),
  isException,
 )
import Test.Plutip.LocalCluster (BpiWallet, withConfiguredCluster)
import Test.Plutip.Options (TraceOption (ShowBudgets))
import Test.Plutip.Predicate (
  assertOverallBudget,
  budgetsFitUnder,
  errorSatisfies,
  failReasonSatisfies,
  overallBudgetFits,
  policyLimit,
  scriptLimit,
  shouldFail,
  shouldSucceed,
  shouldThrow,
  shouldYield,
  stateIs,
  stateSatisfies,
  yieldSatisfies,
 )
import Test.Plutip.Predicate qualified as Predicate
import Test.Tasty (TestTree)

test :: TestTree
test =
  let config = def
      slotLen = ecSlotLength $ extraConfig config
   in withConfiguredCluster
        config
        "Basic integration: launch, add wallet, tx from wallet to wallet"
        $ [
            -- Basic Succeed or Failed tests
            assertExecution
              "Contract 1"
              (initAda (100 : replicate 10 7))
              (withContract $ const getUtxos)
              [ shouldSucceed
              , Predicate.not shouldFail
              ]
          , assertExecution
              "Contract 2"
              (initAda [100])
              (withContract $ const getUtxosThrowsErr)
              [ shouldFail
              , Predicate.not shouldSucceed
              ]
          , assertExecution
              "Pay negative amount"
              (initAda [100])
              ( withContract $
                  const $ do
                    Contract.logInfo @Text "Some contract log with Info level."
                    Contract.logDebug @Text "Another contract log with debug level." >> getUtxosThrowsEx
              )
              [ shouldFail
              , Predicate.not shouldSucceed
              ]
          , assertExecution
              "Pay negative amount"
              (initAda [100])
              (withContract $ \[pkh1] -> payTo pkh1 (-10_000_000))
              [shouldFail]
          , -- Tests with wallet's Value assertions
            assertExecution
              "Pay from wallet to wallet"
              (initAda [100] <> initAndAssertAda [100, 13] 123)
              (withContract $ \[pkh1] -> payTo pkh1 10_000_000)
              [shouldSucceed]
          , assertExecution
              "Two contracts one after another"
              ( initAndAssertAdaWith [100] VLt 100 -- own wallet (index 0 in wallets list)
                  <> initAndAssertAdaWith [100] VLt 100 -- wallet with index 1 in wallets list
              )
              ( do
                  void $ -- run something prior to the contract which result will be checked
                    withContract $
                      \[pkh1] -> payTo pkh1 10_000_000
                  withContractAs 1 $ -- run contract which result will be checked
                    \[pkh1] -> payTo pkh1 10_000_000
              )
              [shouldSucceed]
          , -- Tests with assertions on Contract return value
            assertExecution
              "Initiate wallet and get UTxOs"
              (initAda [100])
              (withContract $ const getUtxos)
              [ yieldSatisfies "Returns single UTxO" ((== 1) . Map.size)
              ]
          , let initFunds = 10_000_000
             in assertExecution
                  "Should yield own initial Ada"
                  (withCollateral $ initLovelace [toEnum initFunds])
                  (withContract $ const ownValue)
                  [ shouldYield (lovelaceValueOf $ toEnum initFunds)
                  ]
          , -- Tests with assertions on state
            let initFunds = 10_000_000
             in assertExecution
                  "Puts own UTxOs Value to state"
                  (withCollateral $ initLovelace [toEnum initFunds])
                  (withContract $ const ownValueToState)
                  [ stateIs [lovelaceValueOf $ toEnum initFunds]
                  , Predicate.not $ stateSatisfies "length > 1" ((> 1) . length)
                  ]
          , -- Tests with assertions on failure
            let expectedErr = ConstraintResolutionContractError CannotSatisfyAny
                isResolutionError = \case
                  ConstraintResolutionContractError _ -> True
                  _ -> False
             in assertExecution
                  ("Contract which throws `" <> show expectedErr <> "`")
                  (initAda [100])
                  (withContract $ const getUtxosThrowsErr)
                  [ shouldThrow expectedErr
                  , errorSatisfies "Throws resolution error" isResolutionError
                  , Predicate.not $ failReasonSatisfies "Throws exception" isException
                  ]
          , let checkException = \case
                  CaughtException e -> isJust @ErrorCall (fromException e)
                  _ -> False
             in assertExecution
                  "Contract which throws exception"
                  (initAda [100])
                  (withContract $ const getUtxosThrowsEx)
                  [ shouldFail
                  , Predicate.not shouldSucceed
                  , failReasonSatisfies "Throws ErrorCall" checkException
                  ]
          , -- tests with assertions on execution budget
            assertExecutionWith
              [ShowBudgets] -- this influences displaying the budgets only and is not necessary for budget assertions
              "Lock then spend contract"
              (initAda (replicate 3 300))
              (withContract $ const lockThenSpend)
              [ shouldSucceed
              , budgetsFitUnder
                  (scriptLimit 406250690 1016102)
                  (policyLimit 405210181 1019024)
              , assertOverallBudget
                  "Assert CPU == 1106851699 and MEM == 2694968"
                  (== 1106851699)
                  (== 2694968)
              , overallBudgetFits 1106851699 2694968
              ]
          , -- regression tests for time <-> slot conversions
            let isValidityError = \case
                  ContractExecutionError e -> "OutsideValidityIntervalUTxO" `isInfixOf` e
                  _ -> False
             in assertExecution
                  "Fails because outside validity interval"
                  (initAda [100])
                  (withContract $ const (failingTimeContract slotLen))
                  [ shouldFail
                  , failReasonSatisfies "Execution error is OutsideValidityIntervalUTxO" isValidityError
                  ]
          , assertExecution
              "Passes validation with exact time range checks"
              (initAda [100])
              (withContract $ const (successTimeContract slotLen))
              [shouldSucceed]
          , -- always fail validation test
            let errCheck e = "I always fail" `isInfixOf` pack (show e)
             in assertExecution
                  "Always fails to validate"
                  (initAda [100])
                  (withContract $ const lockThenFailToSpend)
                  [ shouldFail
                  , errorSatisfies "Fail validation with 'I always fail'" errCheck
                  ]
          , -- Test `adjustUnbalancedTx`
            runAdjustTest
          , testBugMintAndPay
          , runSimpleTest "MustBeSignedBy: Signed by Self and requires Others should succeed" MustBeSignedBy.testSignedBySelfAndRequiresOthers
          , runSimpleTest "MustBeSignedBy: Signed by none and requires Others should succeed" MustBeSignedBy.testSignedByNoneAndRequireOthers
          ]
          ++ testValueAssertionsOrderCorrectness

runSimpleTest ::
  forall (s :: Row Type) (a :: Type).
  (Show a, Typeable a) =>
  String ->
  Contract.Contract Text s Text a ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
runSimpleTest msg contract =
  assertExecution @Text
    msg
    (initAda [100])
    (withContract $ const contract)
    [shouldSucceed]

-- https://github.com/mlabs-haskell/plutip/issues/138
testBugMintAndPay :: (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
testBugMintAndPay =
  assertExecution
    "Adjustment of outputs with 0 Ada does not fail"
    (withCollateral $ initAda [1000] <> initAda [1111])
    (withContract $ \[p1] -> zeroAdaOutTestContract p1)
    [ shouldSucceed
    ]

-- Tests for https://github.com/mlabs-haskell/plutip/issues/84
testValueAssertionsOrderCorrectness ::
  [(TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)]
testValueAssertionsOrderCorrectness =
  [ -- withContract case
    let wallet0 = 100_000_000
        wallet1 = 200_000_000
        wallet2 = 300_000_000

        payFee = 146400
        payTo1Amt = 22_000_000
        payTo2Amt = 33_000_000
        wallet1After = wallet1 + payTo1Amt
        wallet2After = wallet2 + payTo2Amt
        wallet0After =
          wallet0
            - payTo1Amt
            - payFee
            - payTo2Amt
            - payFee
     in assertExecution
          "Values asserted in correct order with withContract"
          ( withCollateral $
              initAndAssertLovelace [wallet0] wallet0After
                <> initAndAssertLovelace [wallet1] wallet1After
                <> initAndAssertLovelace [wallet2] wallet2After
          )
          ( do
              withContract $ \[w1pkh, w2pkh] -> do
                _ <- payTo w1pkh (toInteger payTo1Amt)
                _ <- waitNSlots 2
                payTo w2pkh (toInteger payTo2Amt)
          )
          [shouldSucceed]
  , -- withContractAs case
    let wallet0 = 100_000_000
        wallet1 = 200_000_000
        wallet2 = 300_000_000

        payFee = 146400
        payTo0Amt = 11_000_000
        payTo1Amt = 22_000_000
        payTo2Amt = 33_000_000

        wallet0After = wallet0 + payTo0Amt
        wallet2After =
          wallet2
            + payTo2Amt
            - payTo1Amt
            - payFee

        wallet1After =
          wallet1
            + payTo1Amt
            - payTo0Amt
            - payFee
            - payTo2Amt
            - payFee
     in assertExecution
          "Values asserted in correct order with withContractAs"
          ( withCollateral $ -- Initialize all the wallets with the collateral utxo.
              initAndAssertLovelace [wallet0] wallet0After
                <> initAndAssertLovelace [wallet1] wallet1After
                <> initAndAssertLovelace [wallet2] wallet2After
          )
          ( do
              void $
                withContractAs 1 $ \[w0pkh, w2pkh] -> do
                  _ <- payTo w0pkh (toInteger payTo0Amt)
                  _ <- waitNSlots 2
                  payTo w2pkh (toInteger payTo2Amt)

              withContractAs 2 $ \[_, w1pkh] -> do
                payTo w1pkh (toInteger payTo1Amt)
          )
          [shouldSucceed]
  ]
