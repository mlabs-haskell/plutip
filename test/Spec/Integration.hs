module Spec.Integration (test) where

import BotPlutusInterface.Types (LogContext (ContractLog), LogLevel (Error), LogType (AnyLog))
import Control.Exception (ErrorCall, Exception (fromException))
import Control.Monad (void)
import Data.Default (Default (def))
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text, isInfixOf, pack)
import Ledger.Ada (lovelaceValueOf)
import Ledger.Constraints (MkTxError (OwnPubKeyMissing))
import Plutus.Contract (
  ContractError (ConstraintResolutionContractError),
  waitNSlots,
 )
import Plutus.Contract qualified as Contract
import Spec.TestContract.AlwaysFail (lockThenFailToSpend)
import Spec.TestContract.LockSpendMint (lockThenSpend)
import Spec.TestContract.SimpleContracts (
  getUtxos,
  getUtxosThrowsErr,
  getUtxosThrowsEx,
  ownValue,
  ownValueToState,
  payTo,
 )
import Spec.TestContract.ValidateTimeRange (failingTimeContract, successTimeContract)
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
import Test.Plutip.Internal.Types (
  ClusterEnv,
  FailureReason (CaughtException),
  isException,
 )
import Test.Plutip.LocalCluster (BpiWallet, withConfiguredCluster)
import Test.Plutip.Options (TraceOption (ShowBudgets, ShowTraceButOnlyContext))
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
  withConfiguredCluster
    def
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
      , assertExecutionWith
          [ShowTraceButOnlyContext ContractLog $ Error [AnyLog]]
          "Contract 3"
          (initAda [100])
          (withContract $ const $ Contract.logInfo @Text "Some contract log with Info level." >> getUtxosThrowsEx)
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
              (initLovelace [toEnum initFunds])
              (withContract $ const ownValue)
              [ shouldYield (lovelaceValueOf $ toEnum initFunds)
              ]
      , -- Tests with assertions on state
        let initFunds = 10_000_000
         in assertExecution
              "Puts own UTxOs Value to state"
              (initLovelace [toEnum initFunds])
              (withContract $ const ownValueToState)
              [ stateIs [lovelaceValueOf $ toEnum initFunds]
              , Predicate.not $ stateSatisfies "length > 1" ((> 1) . length)
              ]
      , -- Tests with assertions on failure
        let expectedErr = ConstraintResolutionContractError OwnPubKeyMissing
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
              (scriptLimit 426019962 1082502)
              (policyLimit 428879716 1098524)
          , assertOverallBudget
              "Assert CPU == 1156006922 and MEM == 2860068"
              (== 1156006922)
              (== 2860068)
          , overallBudgetFits 1156006922 2860068
          ]
      , -- regression tests for time <-> slot conversions
        assertExecution
          "Fails because outside validity interval"
          (initAda [100])
          (withContract $ const failingTimeContract)
          [shouldFail]
      , assertExecution
          "Passes validation with exact time range checks"
          (initAda [100])
          (withContract $ const successTimeContract)
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
      ]
      ++ testValueAssertionsOrderCorrectness

-- Tests for https://github.com/mlabs-haskell/plutip/issues/84
testValueAssertionsOrderCorrectness ::
  [(TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)]
testValueAssertionsOrderCorrectness =
  [ -- withContract case
    let wallet0 = 100_000_000
        wallet1 = 200_000_000
        wallet2 = 300_000_000

        payFee = 146200
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

        payFee = 146200
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
