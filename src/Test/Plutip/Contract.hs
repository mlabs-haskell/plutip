{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}

-- |
--  This module together with `Test.Plutip.Predicate` provides the way
--  to run assertions against the result of contract execution,
--  as well as funds at the wallet's UTxOs after contract being run.
--
--  Each test case starts with `assertExecution`, which accepts:
--
--    - description of test case
--    - initial funds distribution at wallets addresses (with optional Value assertions to be performed after the Contract run)
--    - contract to be tested (passed to `withContract`, more on this later)
--    - list of assertions to run against Contract return result, observable state and/or error
--
--  At least one TestWallet is required, this will be used as the own wallet for the contract. Any other
--  wallets can be used as other parties in transactions.
--
--  A TestWallet can be initialised with any positive number of lovelace, using the `initAda` or
--  `initLovelace`. In addition, the value in these wallets can be asserted after the contract
--  execution with `initAdaAssertValue` or `initAndAssertAda`. When `initAdaAssertValue` or `initAndAssertAda` used
--  to initiate wallets corresponding test case will be added automatically.
--
--  Each assertion in assertions list will become separate test case in `TestTree`,
--  however Contract will be executed only once.
--
--  E.g.:
--
--    > assertExecution
--    >   "Some Contract"                   -- Contract description
--    >   (initAda 100)                     -- wallets and initial funds for them (single wallet in this case)
--    >   (withContract $ \_ -> myContract) -- contract execution
--    >   [ shouldSucceed                   -- list of assertions
--    >   , not $ shouldYield someResult
--    >   , stateSatisfies "description" somePredicate
--    >   ]
--
--  To use multiple wallets, you can use the `Semigroup` instance of `TestWallets`. To reference the
--  wallet inside the contract, the following callback function is used together with `withContract`:
--  @[PaymentPubKeyHash] -> Contract w s e a@.
--
-- To display information useful for debugging together with test results use `assertExecutionWith`
-- and provide it with options:
--
--    - BudgetCounting, for displaying transaction execution budgets and enabling assertions for them
--    - Tracing, for displaying contract execution trace
--    - TracingButOnlyContext, like Tracing but filter what to show
--
--  Note that @[PaymentPubKeyHash]@ does not include the contract's own wallet,
--  for that you can use `Plutus.Contract.ownPaymentPubKeyHash` inside the Contract monad.
--
--  When contract supplied to test with `withContract`,
--  the 1st initiated wallet will be used as "own" wallet, e.g.:
--
--    > assertExecution  "Send some Ada"
--    >   (initAda 100 <> initAda 101 <> initAda 102)
--    >   (withContract $ \[pkh1, pkh2] ->
--    >     payToPubKey pkh1 (Ada.lovelaceValueOf amt))
--    >   [shouldSucceed]
--
--  Here:
--
--  - 3 wallets will be initialised with 100, 101 and 102 Ada respectively
--  - wallet with 100 Ada will be used as own wallet to run the contract
--  - `pkh1` - `PaymentPubKeyHash` of wallet with 101 Ada
--  - `pkh2` - `PaymentPubKeyHash` of wallet with 102 Ada
--
--
--  When contract supplied to test with `withContractAs`, wallet with provided index (0 based)
--  will be used as "own" wallet, e.g.:
--
--    > assertExecutionWith
--    >   [BudgetCounting, TracingButOnlyContext ContractLog Error]
--    >   "Send some Ada"
--    >   (initAda 100 <> initAda 101 <> initAda 102)
--    >   (withContractAs 1 $ \[pkh0, pkh2] ->
--    >     payToPubKey pkh1 (Ada.lovelaceValueOf amt))
--    >   [shouldSucceed]
--
--  Here:
--
--    - 3 wallets will be initialised with 100, 101 and 102 Ada respectively
--    - wallet with 101 Ada will be used as own wallet to run the contract
--    - `pkh0` - `PaymentPubKeyHash` of wallet with 100 Ada
--    - `pkh2` - `PaymentPubKeyHash` of wallet with 102 Ada
--    - test result will additionaly show budget calculations and execution trace (but only contract logs)
--
--
--  If you have multiple contracts depending on each other, you can chain them together using
--  `withContract` and `withContractAs`:
--
--    > assertExecution
--    >   "Two contracts one after another"
--    >   (initAda 100 <> initAda 101)
--    >   ( do
--    >       void $ -- run something prior to the contract which result will be checked
--    >         withContract $
--    >           \[pkh1] -> payTo pkh1 10_000_000
--    >       withContractAs 1 $ -- run the contract which result will be checked
--    >         \[pkh1] -> payTo pkh1 10_000_000
--    >   )
--    >   [shouldSucceed]
--
--  Here two contracts are executed one after another.
--  Note that only execution result of the second contract will be tested.
module Test.Plutip.Contract (
  withContract,
  withContractAs,
  -- Wallet initialisation
  TestWallets (TestWallets, unTestWallets),
  TestWallet (twInitDistribuition, twExpected),
  initAda,
  initAndAssertAda,
  initAndAssertAdaWith,
  initAdaAssertValue,
  initAdaAssertValueWith,
  initLovelace,
  initAndAssertLovelace,
  initAndAssertLovelaceWith,
  initLovelaceAssertValue,
  initLovelaceAssertValueWith,
  -- Helpers
  ledgerPaymentPkh,
  ValueOrdering (VEq, VGt, VLt, VGEq, VLEq),
  assertValues,
  assertExecution,
  assertExecutionWith,
  ada,
) where

import BotPlutusInterface.Types (LogContext, LogLevel, LogsList (getLogsList))
import Control.Arrow (left)
import Control.Monad (void)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT, runReaderT)
import Data.Bool (bool)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust)
import Data.Row (Row)
import Data.Tagged (Tagged (Tagged))
import Data.Text qualified as Text
import Ledger (PaymentPubKeyHash)
import Ledger.Address (pubKeyHashAddress)
import Ledger.Value (Value)
import Plutus.Contract (Contract, waitNSlots)
import PlutusPrelude (render)
import Prettyprinter (Doc, Pretty (pretty), vcat, (<+>))
import Test.Plutip.Contract.Init (
  initAda,
  initAdaAssertValue,
  initAdaAssertValueWith,
  initAndAssertAda,
  initAndAssertAdaWith,
  initAndAssertLovelace,
  initAndAssertLovelaceWith,
  initLovelace,
  initLovelaceAssertValue,
  initLovelaceAssertValueWith,
 )
import Test.Plutip.Contract.Types (
  TestContract (TestContract),
  TestContractConstraints,
  TestWallet (twExpected, twInitDistribuition),
  TestWallets (TestWallets, unTestWallets),
  ValueOrdering (VEq, VGEq, VGt, VLEq, VLt),
 )
import Test.Plutip.Contract.Values (assertValues, valueAt)
import Test.Plutip.Internal.BotPlutusInterface.Run (runContract)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (BpiWallet, ledgerPaymentPkh)
import Test.Plutip.Internal.Types (
  ClusterEnv,
  ExecutionResult (contractLogs, outcome),
  budgets,
 )
import Test.Plutip.Options (TraceOption (BudgetCounting, Tracing, TracingButOnlyContext))
import Test.Plutip.Predicate (Predicate, noBudgetsMessage, pTag)
import Test.Plutip.Tools (ada)
import Test.Plutip.Tools.Format (fmtTxBudgets)
import Test.Tasty (testGroup, withResource)
import Test.Tasty.HUnit (assertFailure, testCase)
import Test.Tasty.Providers (IsTest (run, testOptions), TestTree, singleTest, testPassed)

type TestRunner (w :: Type) (e :: Type) (a :: Type) =
  ReaderT (ClusterEnv, NonEmpty BpiWallet) IO (ExecutionResult w e (a, NonEmpty Value))

-- | When used with `withCluster`, builds `TestTree` from initial wallets distribution,
--  Contract and list of assertions (predicates). Each assertion will be run as separate test case,
--  although Contract will be executed only once.
--
-- > assertExecution
-- >   "Some Contract"                   -- Contract description
-- >   (initAda 100)                     -- wallets and initial funds for them (single wallet in this case)
-- >   (withContract $ \_ -> myContract) -- contract execution
-- >   [ shouldSucceed                   -- list of assertions
-- >   , not $ shouldYield someResult
-- >   , stateSatisfies "description" somePredicate
-- >   ]
--
-- @since 0.2
assertExecution ::
  forall (w :: Type) (e :: Type) (a :: Type).
  TestContractConstraints w e a =>
  String ->
  TestWallets ->
  TestRunner w e a ->
  [Predicate w e a] ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
assertExecution = assertExecutionWith mempty

-- | Version of assertExecution parametrised with a list of extra TraceOption's.
--
-- > assertExecutionWith [Tracing, BudgetCounting]
--
-- to print additional transaction budget estimations and contract execution logs
assertExecutionWith ::
  forall (w :: Type) (e :: Type) (a :: Type).
  TestContractConstraints w e a =>
  [TraceOption] ->
  String ->
  TestWallets ->
  TestRunner w e a ->
  [Predicate w e a] ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
assertExecutionWith options tag testWallets testRunner predicates =
  (testWallets, toTestGroup)
  where
    toTestGroup ioEnv =
      withResource (runReaderT testRunner =<< ioEnv) (const $ pure ()) $
        \ioRes ->
          testGroup tag $
            maybeAddValuesCheck
              ioRes
              testWallets
              ((toCase ioRes <$> predicates) <> ((`optionToTestTree` ioRes) <$> options))

    -- wraps IO with result of contract execution into single test
    toCase ioRes p =
      singleTest (pTag p) (TestContract p ioRes)

    optionToTestTree = \case
      BudgetCounting -> singleTest "Budget stats" . StatsReport
      Tracing -> singleTest logsName . LogsReport DisplayAllTrace
      TracingButOnlyContext logCtx logLvl ->
        singleTest logsName . LogsReport (DisplayOnlyFromContext logCtx logLvl)

    logsName = "BPI logs (PAB requests/responses)"

-- | Adds test case with assertions on values if any assertions were added
--  by `initAndAssert...` functions during wallets setup
--
-- @since 0.2
maybeAddValuesCheck ::
  Show e =>
  IO (ExecutionResult w e (a, NonEmpty Value)) ->
  TestWallets ->
  [TestTree] ->
  [TestTree]
maybeAddValuesCheck ioRes tws =
  bool id (valuesCheckCase :) (any isJust expected)
  where
    expected = twExpected <$> unTestWallets tws

    valuesCheckCase =
      testCase "Values check" $
        ioRes
          >>= either (assertFailure . Text.unpack) (const $ pure ())
            . checkValues
            . outcome

    checkValues o =
      left (Text.pack . show) o
        >>= \(_, vs) -> assertValues expected vs

-- | Run a contract using the first wallet as own wallet, and return `ExecutionResult`.
-- This could be used by itself, or combined with multiple other contracts.
--
-- @since 0.2
withContract ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  TestContractConstraints w e a =>
  ([PaymentPubKeyHash] -> Contract w s e a) ->
  TestRunner w e a
withContract = withContractAs 0

-- | Run a contract using the nth wallet as own wallet, and return `ExecutionResult`.
-- This could be used by itself, or combined with multiple other contracts.
--
-- @since 0.2
withContractAs ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  TestContractConstraints w e a =>
  Int ->
  ([PaymentPubKeyHash] -> Contract w s e a) ->
  TestRunner w e a
withContractAs walletIdx toContract = do
  (cEnv, wallets') <- ask
  let wallets@(ownWallet :| otherWallets) = reorder walletIdx wallets'
  let contract = wrapContract wallets (toContract (map ledgerPaymentPkh otherWallets))
  liftIO $ runContract cEnv ownWallet contract
  where
    reorder i xss = case NonEmpty.splitAt i xss of
      (xs, y : ys) -> y :| xs ++ ys
      _ -> error $ "Should fail: bad wallet index for own wallet: " <> show i

-- | Wrap test contracts to wait for transaction submission and
-- to get the utxo amount at test wallets and wait for transaction.
--
-- @since 0.2
wrapContract ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  TestContractConstraints w e a =>
  NonEmpty BpiWallet ->
  Contract w s e a ->
  Contract w s e (a, NonEmpty Value)
wrapContract bpiWallets contract = do
  res <- contract
  void $ waitNSlots 1
  let walletPkhs = fmap ledgerPaymentPkh bpiWallets
  values <- traverse (valueAt . (`pubKeyHashAddress` Nothing)) walletPkhs
  pure (res, values)

newtype StatsReport w e a = StatsReport (IO (ExecutionResult w e (a, NonEmpty Value)))

instance
  forall (w :: Type) (e :: Type) (a :: Type).
  TestContractConstraints w e a =>
  IsTest (StatsReport w e a)
  where
  run _ (StatsReport ioRes) _ =
    testPassed . mkDescription <$> ioRes
    where
      mkDescription runRes =
        let bs = budgets runRes
         in bool (fmtTxBudgets bs) noBudgetsMessage (null bs)

  testOptions = Tagged []

-- | Test case used internally for logs printing.
data LogsReport w e a = LogsReport LogsReportOption (IO (ExecutionResult w e (a, NonEmpty Value)))

-- | TraceOption stripped to what LogsReport wants to know.
data LogsReportOption
  = -- | Display all logs collected by BPI during contract execution.
    DisplayAllTrace
  | -- | Display filtered logs
    DisplayOnlyFromContext
      LogContext
      -- ^ upper bound on LogLevel
      LogLevel

instance
  forall (w :: Type) (e :: Type) (a :: Type).
  TestContractConstraints w e a =>
  IsTest (LogsReport w e a)
  where
  run _ (LogsReport option ioRes) _ =
    testPassed . ppShowLogs <$> ioRes
    where
      ppShowLogs = render . vcat . zipWith indexedMsg [0 ..] . map (\(_, _, msg) -> msg) . filterOrDont . getLogsList . contractLogs
      filterOrDont = case option of
        DisplayAllTrace -> id -- don't
        DisplayOnlyFromContext logCtx logLvl -> filter (\(ctx, lvl, _) -> ctx == logCtx && logLvl >= lvl)

      indexedMsg :: Int -> Doc ann -> Doc ann
      indexedMsg i msg = pretty i <> pretty ("." :: String) <+> msg

  testOptions = Tagged []
