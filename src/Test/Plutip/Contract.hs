{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Plutip.Contract (
  -- Assertinos
  shouldSucceed,
  shouldFail,
  shouldYield,
  shouldHaveObservableState,
  assertYieldedResultWith,
  assertObservableStateWith,
  -- Wallet initialisation
  TestWallets (TestWallets, unTestWallets),
  TestWallet (twInitDistribuition, twExpected),
  initAda,
  initLovelace,
  initAndAssertAda,
  initAndAssertLovelace,
  initAdaAssertValue,
  initLovelaceAssertValue,
  -- Helpers
  ledgerPaymentPkh,
) where

import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (ToJSON)
import Data.Dynamic (Typeable)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Row (Row)
import Data.Tagged (Tagged (Tagged))
import Ledger (Address, ChainIndexTxOut (PublicKeyChainIndexTxOut, ScriptChainIndexTxOut))
import Ledger.Ada qualified as Ada
import Ledger.Address (pubKeyHashAddress)
import Ledger.Value (Value)
import Numeric.Natural (Natural)
import Plutus.Contract (AsContractError, Contract, utxosAt, waitNSlots)
import Test.Plutip.Internal.BotPlutusInterface.Run (runContract)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (BpiWallet, ledgerPaymentPkh)
import Test.Plutip.Internal.Types (ClusterEnv, FailureReason, Outcome (Failure, Success))
import Test.Tasty.Providers (IsTest (run, testOptions), TestTree, singleTest, testFailed, testPassed)

type TestContractConstraints (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) =
  ( ToJSON w
  , Monoid w
  , Show w
  , Show e
  , Show a
  , Typeable s
  , Typeable w
  , Typeable e
  , Typeable a
  , AsContractError e
  )

-- | Assert that a contract is valid
shouldSucceed ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  TestContractConstraints w s e a =>
  String ->
  TestWallets ->
  ([BpiWallet] -> Contract w s e a) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
shouldSucceed tag testWallets toContract =
  ( testWallets
  , singleTest tag
      . TestContract
        toContract
        (ExpectSuccess (const True) (const True) (twExpected <$> unTestWallets testWallets))
  )

-- | Assert that a contract should NOT validate
shouldFail ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  TestContractConstraints w s e a =>
  String ->
  TestWallets ->
  ([BpiWallet] -> Contract w s e a) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
shouldFail tag testWallets toContract =
  ( testWallets
  , singleTest tag
      . TestContract
        toContract
        (ExpectFailure (const True))
  )

-- | Assert the return value of the contract with a custom preficate
assertYieldedResultWith ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  TestContractConstraints w s e a =>
  String ->
  TestWallets ->
  (a -> Bool) ->
  ([BpiWallet] -> Contract w s e a) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
assertYieldedResultWith tag testWallets predicate toContract =
  ( testWallets
  , singleTest tag
      . TestContract
        toContract
        (ExpectSuccess predicate (const True) (twExpected <$> unTestWallets testWallets))
  )

-- | Check if the return value of the contract equals to some expected value
shouldYield ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  (TestContractConstraints w s e a, Eq a) =>
  String ->
  TestWallets ->
  a ->
  ([BpiWallet] -> Contract w s e a) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
shouldYield tag testWallets expected =
  assertYieldedResultWith tag testWallets (== expected)

-- | Assert the observable state of the contract with a custom preficate
assertObservableStateWith ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  TestContractConstraints w s e a =>
  String ->
  TestWallets ->
  (w -> Bool) ->
  ([BpiWallet] -> Contract w s e a) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
assertObservableStateWith tag testWallets predicate toContract =
  ( testWallets
  , singleTest tag
      . TestContract
        toContract
        (ExpectSuccess (const True) predicate (twExpected <$> unTestWallets testWallets))
  )

-- | Check if the observable state of the contract equals to some expected value
shouldHaveObservableState ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  (TestContractConstraints w s e a, Eq w) =>
  String ->
  TestWallets ->
  w ->
  ([BpiWallet] -> Contract w s e a) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
shouldHaveObservableState tag testWallets expected =
  assertObservableStateWith tag testWallets (== expected)

valueAt ::
  forall (w :: Type) (s :: Row Type) (e :: Type).
  AsContractError e =>
  Address ->
  Contract w s e Value
valueAt addr = do
  utxos <- utxosAt addr
  pure . mconcat . map utxoValue . Map.elems $ utxos
  where
    utxoValue :: ChainIndexTxOut -> Value
    utxoValue (PublicKeyChainIndexTxOut _ v) = v
    utxoValue (ScriptChainIndexTxOut _ _ _ v) = v

data TestContract (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) = TestContract
  { tcContract :: (ToJSON w, Monoid w, Show w, Show e, Show a) => [BpiWallet] -> Contract w s e a
  , tcExpected :: ExpectedOutcome w e a
  , tcSetup :: IO (ClusterEnv, NonEmpty BpiWallet)
  }
  deriving stock (Typeable)

-- | Expected outcome of running contract
data ExpectedOutcome w e a
  = ExpectSuccess (a -> Bool) (w -> Bool) (NonEmpty (Maybe Value))
  | ExpectFailure (FailureReason e -> Bool)
  deriving stock (Typeable)

instance
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  TestContractConstraints w s e a =>
  IsTest (TestContract w s e a)
  where
  run _ TestContract {tcContract, tcSetup, tcExpected} _ = do
    (cEnv, wallets@(ownWallet :| otherWallets)) <- tcSetup
    let contract = wrapContract wallets (tcContract otherWallets)
    result <- runReaderT (runContract cEnv ownWallet contract) cEnv

    pure $ case (tcExpected, result) of
      (ExpectSuccess assertRes assertObsSt expectedVal, Success (res, values) obsSt)
        | assertRes res
            && assertObsSt obsSt
            && assertValues expectedVal values ->
          testPassed $ show result
      (ExpectFailure _, Failure _) -> testPassed ""
      _ -> testFailed $ show result

  testOptions = Tagged []

{- | Wrap test contracts to wait for transaction submission and
 to get the utxo amount at test wallets and wait for transaction
-}
wrapContract ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  TestContractConstraints w s e a =>
  NonEmpty BpiWallet ->
  Contract w s e a ->
  Contract w s e (a, NonEmpty Value)
wrapContract bpiWallets contract = do
  res <- contract
  void $ waitNSlots 1
  let walletPkhs = fmap ledgerPaymentPkh bpiWallets
  values <- traverse (valueAt . (`pubKeyHashAddress` Nothing)) walletPkhs
  pure (res, values)

assertValues :: NonEmpty (Maybe Value) -> NonEmpty Value -> Bool
assertValues expected values =
  all assertValue $ zip (NonEmpty.toList expected) (NonEmpty.toList values)
  where
    assertValue (Nothing, _) = True
    assertValue (Just v, v') = v == v'

newtype TestWallets = TestWallets {unTestWallets :: NonEmpty TestWallet}
  deriving newtype (Semigroup)

data TestWallet = TestWallet
  { twInitDistribuition :: Natural
  , twExpected :: Maybe Value
  }

-- | Create a wallet with the given amount of lovelace
initLovelace :: Natural -> TestWallets
initLovelace initial = TestWallets $ TestWallet initial Nothing :| []

-- | Create a wallet with the given amount of Ada
initAda :: Natural -> TestWallets
initAda initial = initLovelace (initial * 1_000_000)

{- | Create a wallet with the given amount of lovelace,
 and assert values at the wallet address after contract execution
-}
initLovelaceAssertValue :: Natural -> Value -> TestWallets
initLovelaceAssertValue initial expect = TestWallets $ TestWallet initial (Just expect) :| []

{- | Create a wallet with the given amount of Ada
 and assert values at the wallet address after contract execution
-}
initAdaAssertValue :: Natural -> Natural -> TestWallets
initAdaAssertValue initial = initAndAssertLovelace (initial * 1_000_000)

{- | Create a wallet with the given amount of lovelace
 and assert the amount lovelace at the wallet address after contract execution
-}
initAndAssertLovelace :: Natural -> Natural -> TestWallets
initAndAssertLovelace initial expect =
  initLovelaceAssertValue initial (Ada.lovelaceValueOf (fromIntegral expect))

{- | Create a wallet with the given amount of Ada
 and assert the amount of Ada at the wallet address after contract execution
-}
initAndAssertAda :: Natural -> Natural -> TestWallets
initAndAssertAda initial expect =
  initAndAssertLovelace (initial * 1_000_000) (expect * 1_000_000)
