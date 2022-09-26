{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
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
--  A wallet is named with a tag, the tag contstructor specifies what type of wallet is to be initialized.
--  Don't use the same name `k` for two wallets even with different tag constructors.
--
--  Each assertion in assertions list will become separate test case in `TestTree`,
--  however Contract will be executed only once.
--
--  E.g.:
--
--    > assertExecution
--    >   "Some Contract"                   -- Contract description
--    >   (initAda (PkhTag ()) 100)                     -- wallets and initial funds for them (single wallet in this case)
--    >   (withContract $ \_ -> myContract) -- contract execution
--    >   [ shouldSucceed                   -- list of assertions
--    >   , not $ shouldYield someResult
--    >   , stateSatisfies "description" somePredicate
--    >   ]
--
--  To use multiple wallets, you can use the `Semigroup` instance of `TestWallets`. To reference the
--  wallet inside the contract, the following callback function is used together with `withContract`:
--  @WalletLookups k -> Contract w s e a@.
--
-- To display information useful for debugging together with test results use `assertExecutionWith`
-- and provide it with options:
--
--    - ShowBudgets, for displaying transaction execution budgets
--    - ShowTrace, for displaying contract execution trace
--    - ShowTraceButOnlyContext, like ShowTrace but filter what to show
--
--  Note that @WalletLookups@ don't include the contract's own wallet,
--  for that you can use `Plutus.Contract.ownPaymentPubKeyHash` inside the Contract monad.
--
--  When contract supplied to test with `withContract`,
--  the 1st initiated wallet will be used as "own" wallet, e.g.:
--
--    > assertExecution  "Send some Ada"
--    >   (initAda (PkhTag 0) 100 <> initAda (PkhTag 1) 101 <> initAda (PkhTag 2) 102)
--    >   (withContract $ \wl -> do
--    >     PkhWallet pkh1 <- lookupWallet wl (PkhTag 1)
--    >     payToPubKey pkh1 (Ada.lovelaceValueOf amt))
--    >   [shouldSucceed]
--
--  Here:
--
--  - 3 wallets will be initialised with 100, 101 and 102 Ada respectively
--  - wallet with 100 Ada will be used as own wallet to run the contract
--  - `pkh1` - `PaymentPubKeyHash` of wallet with 101 Ada
--
--
--  When contract supplied to test with `withContractAs`, wallet with provided name
--  will be used as "own" wallet, e.g.:
--
--    > assertExecutionWith
--    >   [ShowBudgets, ShowTraceButOnlyContext ContractLog Error]
--    >   "Send some Ada"
--    >   (initAda (PkhTag "pkh0") 100 <> initAda (PkhTag "myOwnWallet") 101 <> initAda (PkhTag "pkh2") 102)
--    >   (withContractAs "myOwnWallet" $ \wl -> do
--    >     PkhWallet pkh0 <- lookupWallet wl (PkhTag "pkh0")
--    >     PkhWallet pkh2 <- lookupWallet wl (PkhTag "pkh2")
--    >     payToPubKey pkh2 (Ada.lovelaceValueOf amt))
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
--    >   (initAda (PkhTag 0) 100 <> initAda (PkhTag 1) 101)
--    >   ( do
--    >       void $ -- run something prior to the contract which result will be checked
--    >         withContract $ \wl -> 
--    >           PkhWallet pkh1 <- lookupWallet wl (PkhTag 1)
--    >           payTo pkh1 10_000_000
--    >       withContractAs 1 $ \wl -> do  -- run the contract which result will be checked
--    >         PkhWallet pkh0 <- lookupWallet wl (PkhTag 0)
--    >         payTo pkh0 10_000_000
--    >   )
--    >   [shouldSucceed]
--
--  Here two contracts are executed one after another.
--  Note that only execution result of the second contract will be tested.
module Test.Plutip.Contract (
  withContract,
  withContractAs,
  -- Wallet initialisation
  initAda,
  withCollateral,
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
  walletPaymentPkh,
  assertValues,
  assertExecution,
  assertExecutionWith,
  ada,
  TestWallets,
  ClusterTest (ClusterTest),
  -- Contract runners
  runContract,
  runContractWithLogLvl,
) where

import BotPlutusInterface.Types (
  LogContext,
  LogLevel,
  LogLine (LogLine, logLineContext, logLineLevel),
  LogsList (getLogsList),
  sufficientLogLevel,
 )

import Control.Arrow (left)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT, runReaderT, void)
import Data.Bool (bool)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Row (Row)
import Data.Tagged (Tagged (Tagged))
import Data.Text qualified as Text
import Ledger.Value (Value)
import Plutus.Contract (Contract, waitNSlots)
import PlutusPrelude (render)
import PlutusTx.These (These (That, These, This))
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
  withCollateral,
 )
import Test.Plutip.Contract.Types (
  TestContract (TestContract),
  TestContractConstraints,
 )
import Test.Plutip.Contract.Values (assertValues, valueAt)
import Test.Plutip.Internal.BotPlutusInterface.Lookups (WalletLookups, lookupsMap, makeWalletInfo, makeWalletLookups)
import Test.Plutip.Internal.BotPlutusInterface.Run
    ( runContract, runContract, runContractWithLogLvl )
import Test.Plutip.Internal.BotPlutusInterface.Types (
  BpiWallet (bwTag),
  TestWallet (twExpected, twTag),
  TestWallet' (TestWallet'),
  TestWallets (unTestWallets),
  getTag,
  ownAddress,
  WalletInfo,
 )
import Test.Plutip.Internal.BotPlutusInterface.Wallet
    ( walletPaymentPkh )
import Test.Plutip.Internal.Types (
  ClusterEnv,
  ExecutionResult (contractLogs, outcome),
  budgets,
 )
import Test.Plutip.Options (TraceOption (ShowBudgets, ShowTrace, ShowTraceButOnlyContext))
import Test.Plutip.Predicate (Predicate, noBudgetsMessage, pTag)
import Test.Plutip.Tools (ada)
import Test.Plutip.Tools.Format (fmtTxBudgets)
import Test.Tasty (testGroup, withResource)
import Test.Tasty.HUnit (assertFailure, testCase)
import Test.Tasty.Providers (IsTest (run, testOptions), TestTree, singleTest, testPassed)

type TestRunner (w :: Type) (e :: Type) (k :: Type) (a :: Type) =
  ReaderT (ClusterEnv, NonEmpty (BpiWallet k)) IO (ExecutionResult w e (a, Map k Value))

-- | A type for the output of `assertExecution`.
-- `k` is existentially quantified to allow different key types in every test case.
data ClusterTest = forall k. ClusterTest (TestWallets k, IO (ClusterEnv, NonEmpty (BpiWallet k)) -> TestTree)

-- | When used with `withCluster`, builds `TestTree` from initial wallets distribution,
--  Contract and list of assertions (predicates). Each assertion will be run as separate test case,
--  although Contract will be executed only once.
--
-- > assertExecution
-- >   "Some Contract"                   -- Contract description
-- >   (initAda (PkhTag 0) 100)                     -- wallets and initial funds for them (single wallet in this case)
-- >   (withContract $ \_ -> myContract) -- contract execution
-- >   [ shouldSucceed                   -- list of assertions
-- >   , not $ shouldYield someResult
-- >   , stateSatisfies "description" somePredicate
-- >   ]
--
-- @since 0.2
assertExecution ::
  forall (w :: Type) (e :: Type) (k :: Type) (a :: Type).
  TestContractConstraints w e k a =>
  String ->
  TestWallets k ->
  TestRunner w e k a ->
  [Predicate w e k a] ->
  ClusterTest
assertExecution = assertExecutionWith mempty

-- | Version of assertExecution parametrised with a list of extra TraceOption's.
--
-- > assertExecutionWith [ShowTrace, ShowBudgets]
--
-- to print additional transaction budget calculations and contract execution logs
assertExecutionWith ::
  forall (w :: Type) (e :: Type) (a :: Type) (k :: Type).
  TestContractConstraints w e k a =>
  [TraceOption] ->
  String ->
  TestWallets k ->
  TestRunner w e k a ->
  [Predicate w e k a] ->
  ClusterTest
assertExecutionWith options tag testWallets testRunner predicates =
  ClusterTest (testWallets, toTestGroup)
  where
    toTestGroup :: IO (ClusterEnv, NonEmpty (BpiWallet k)) -> TestTree
    toTestGroup ioEnv =
      withResource (runReaderT testRunner =<< ioEnv) (const $ pure ()) $
        \ioRes ->
          testGroup tag $
            maybeAddValuesCheck
              ioRes
              testWallets
              ((toCase ioRes <$> predicates) <> ((`optionToTestTree` ioRes) <$> options))

    -- wraps IO with result of contract execution into single test
    toCase :: IO (ExecutionResult w e (a, Map k Value)) -> Predicate w e k a -> TestTree
    toCase ioRes p =
      singleTest (pTag p) (TestContract p ioRes)

    optionToTestTree :: TraceOption -> IO (ExecutionResult w e (a, Map k Value)) -> TestTree
    optionToTestTree = \case
      ShowBudgets -> singleTest "Budget stats" . StatsReport
      ShowTrace -> singleTest logsName . LogsReport DisplayAllTrace
      ShowTraceButOnlyContext logCtx logLvl ->
        singleTest logsName . LogsReport (DisplayOnlyFromContext logCtx logLvl)

    logsName = "BPI logs (PAB requests/responses)"

-- | Adds test case with assertions on values if any assertions were added
--  by `initAndAssert...` functions during wallets setup
--
-- @since 0.2
maybeAddValuesCheck ::
  (Show e, Ord k) =>
  IO (ExecutionResult w e (a, Map k Value)) ->
  TestWallets k ->
  [TestTree] ->
  [TestTree]
maybeAddValuesCheck ioRes tws =
  bool id (valuesCheckCase :) (any isJust expected)
  where
    expected = Map.fromList $ toList $ (\(TestWallet' tw) -> (getTag (twTag tw), twExpected tw)) <$> unTestWallets tws

    valuesCheckCase :: TestTree
    valuesCheckCase =
      testCase "Values check" $
        ioRes
          >>= \res -> do
            ( either (assertFailure . Text.unpack) (const $ pure ())
                . checkValues
                . outcome
              )
              res

    checkValues o =
      left (Text.pack . show) o
        >>= \(_, vs) ->
          let theseToPair = \case
                (These b c) -> (b, c)
                _ -> error "The two maps should have the same keys as both follow from TestWallets."
              (expecs, vals) = unzip $ Map.elems $ theseToPair <$> zipMaps expected vs
           in assertValues expecs vals

    zipMaps :: Ord a => Map a b -> Map a c -> Map a (These b c)
    zipMaps mb mc =
      let f (This b) (That c) = These b c
          f _ _ = error "All left are This and all right are That."
       in Map.unionWith f (This <$> mb) (That <$> mc)

-- | Run a contract using the first wallet as own wallet, and return `ExecutionResult`.
-- This could be used by itself, or combined with multiple other contracts.
--
-- @since 0.2
withContract ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) (k :: Type).
  TestContractConstraints w e k a =>
  (WalletLookups k -> Contract w s e a) ->
  TestRunner w e k a
withContract toContract = do
  (_, wallets') <- ask
  withContractAs (bwTag $ NonEmpty.head wallets') toContract

-- | Run a contract using wallet with the given tag as own wallet, and return `ExecutionResult`.
-- This could be used by itself, or combined with multiple other contracts.
--
-- @since 0.2
withContractAs ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) (k :: Type).
  TestContractConstraints w e k a =>
  k ->
  (WalletLookups k -> Contract w s e a) ->
  TestRunner w e k a
withContractAs walletName toContract = do
  (cEnv, wallets') <- ask
  let -- pick wallet for Contract's "own PKH", other wallets PKHs will be provided
      -- to the user in `withContractAs`
      (ownWallet, otherWallets) = separateWallets walletName $ NonEmpty.toList wallets'

      -- without own wallet
      otherLookups :: Map k WalletInfo
      otherLookups = lookupsMap otherWallets

      -- to be passed to the user, without own wallet
      walletLookups = makeWalletLookups otherLookups

      -- these are `PaymentPubKeyHash`es of all wallets used in test case
      collectValuesAddr = ownAddress <$> Map.insert (bwTag ownWallet) (makeWalletInfo ownWallet) otherLookups

      -- contract that gets all the values present at the test wallets.
      valuesAtWallet :: Contract w s e (Map k Value)
      valuesAtWallet =
        void (waitNSlots 1)
          >> traverse valueAt collectValuesAddr

  -- run the test contract
  execRes <- liftIO $ runContract cEnv ownWallet (toContract walletLookups)

  -- get all the values present at the test wallets after the user given contracts has been executed.
  execValues <- liftIO $ runContract cEnv ownWallet valuesAtWallet

  case outcome execValues of
    Left e -> fail $ "Failed to get values. Error: " ++ show e
    Right values -> return $ execRes {outcome = (,values) <$> outcome execRes}
  where
    separateWallets :: k -> [BpiWallet k] -> (BpiWallet k, [BpiWallet k])
    separateWallets tag =
      let p = (== tag) . bwTag
          loop ys = \case
            (a : xs) -> if p a then (a, xs <> ys) else loop (a : ys) xs
            [] -> error $ "Should fail: bad wallet tag for own wallet: " <> show tag
       in loop []

newtype StatsReport w e k a = StatsReport (IO (ExecutionResult w e (a, Map k Value)))

instance
  forall (w :: Type) (e :: Type) (a :: Type) (k :: Type).
  TestContractConstraints w e k a =>
  IsTest (StatsReport w e k a)
  where
  run _ (StatsReport ioRes) _ =
    testPassed . mkDescription <$> ioRes
    where
      mkDescription runRes =
        let bs = budgets runRes
         in bool (fmtTxBudgets bs) noBudgetsMessage (null bs)

  testOptions = Tagged []

-- | Test case used internally for logs printing.
data LogsReport w e k a = LogsReport LogsReportOption (IO (ExecutionResult w e (a, Map k Value)))

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
  forall (w :: Type) (e :: Type) (a :: Type) (k :: Type).
  TestContractConstraints w e k a =>
  IsTest (LogsReport w e k a)
  where
  run _ (LogsReport option ioRes) _ =
    testPassed . ppShowLogs . contractLogs <$> ioRes
    where
      ppShowLogs =
        render
          . vcat
          . zipWith indexedMsg [0 ..]
          . map pretty
          . filterOrDont
          . getLogsList

      filterOrDont = case option of
        DisplayAllTrace ->
          id -- don't
        DisplayOnlyFromContext logCtx logLvl ->
          filter
            ( \LogLine {logLineContext, logLineLevel} ->
                logLineContext == logCtx
                  && sufficientLogLevel logLvl logLineLevel
            )

      indexedMsg :: Int -> Doc ann -> Doc ann
      indexedMsg i msg = pretty i <> pretty ("." :: String) <+> msg

  testOptions = Tagged []
