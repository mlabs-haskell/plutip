{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

{- | This module provides some contract assertions to be used with `Test.Plutip.LocalCluster.withContract`
  All assertions accept a name, some TestWallets and a contract.

  At least one TestWallet is required, this will be used as the own wallet for the contract. Any other
  wallets can be used as other parties in transactions.

  A TestWallet can be initialised with any positive number of lovelace, using the `initAda` or
  `initLovelace`. In addition, the value in these wallets can be asserted after the contract
  execution with `initAdaAssertValue` or `initAndAssertAda`.

 > shouldSucceed "Get utxos" (initAda 100) $ \_ -> do
 >   pkh <- Contract.ownPaymentPubKeyHash
 >   utxosAt $ pubKeyHashAddress pkh Nothing

  To use multiple wallets, you can use the `Semigroup` instance of `TestWallets`. To reference the
  wallet inside the contract, the following callback function is used, when
  supplying a contract toa test case: @[PaymentPubKeyHash] -> Contract w s e a@.
  Note that @[PaymentPubKeyHash]@ does not include the contract's own wallet, for that you can use `Plutus.Contract.ownPaymentPubKeyHash` inside the Contract monad.

 > shouldSucceed "Send some Ada" (initAda 100 <> initAndAssertAda 100 110) $
 >   \[pkh1] -> submitTx (Constraints.mustPayToPubKey pkh1 (Ada.lovelaceValueOf amt))
-}
module Test.Plutip.Contract (
  -- Assertions
  shouldSucceed,
  shouldFail,
  shouldYield,
  shouldHaveObservableState,
  assertYieldedResultWith,
  assertObservableStateWith,
  assertFailure,
  assertContractError,
  shouldThrowContractError,
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
) where

import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (ToJSON)
import Data.Aeson.Extras (encodeByteString)
import Data.Dynamic (Typeable)
import Data.Either (fromRight)
import Data.Kind (Type)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Row (Row)
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8')
import Ledger (Address, ChainIndexTxOut (PublicKeyChainIndexTxOut, ScriptChainIndexTxOut), PaymentPubKeyHash)
import Ledger.Ada qualified as Ada
import Ledger.Address (pubKeyHashAddress)
import Ledger.Value (CurrencySymbol (unCurrencySymbol), TokenName (unTokenName), Value)
import Ledger.Value qualified as Value
import Numeric.Positive (Positive)
import Plutus.Contract (AsContractError, Contract, utxosAt, waitNSlots)
import PlutusTx.Builtins (fromBuiltin)
import Test.Plutip.Internal.BotPlutusInterface.Run (runContract)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (BpiWallet, ledgerPaymentPkh)
import Test.Plutip.Internal.Types (
  ClusterEnv,
  ExecutionResult (ExecutionResult),
  FailureReason (ContractExecutionError),
 )
import Test.Tasty.Providers (IsTest (run, testOptions), TestTree, singleTest, testFailed, testPassed)
import Text.Show.Pretty (ppShow)

type TestContractConstraints (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) =
  ( ToJSON w
  , Monoid w
  , Show w
  , Show e
  , Show a
  , Typeable w
  , Typeable s
  , Typeable e
  , Typeable a
  , AsContractError e
  )

{- | Assert that a contract is valid.

 = Usage
 > shouldSucceed "Get utxos" (initAndAssertAda 100 100) $ \_ -> do
 >   pkh <- Contract.ownPaymentPubKeyHash
 >   utxosAt $ pubKeyHashAddress pkh Nothing

 @since 0.2
-}
shouldSucceed ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  TestContractConstraints w s e a =>
  String ->
  TestWallets ->
  ([PaymentPubKeyHash] -> Contract w s e a) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
shouldSucceed tag testWallets toContract =
  ( testWallets
  , singleTest tag
      . TestContract
        toContract
        (ExpectSuccess (const True) (const True) (twExpected <$> unTestWallets testWallets))
  )

{- | Assert that a contract should NOT validate.

 = Usage
 > shouldFail "Get utxos throwing error" (initAda 100) $ \_ ->
 >   Contract.throwError "This Error was thrown intentionally by Contract"

 @since 0.2
-}
shouldFail ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  TestContractConstraints w s e a =>
  String ->
  TestWallets ->
  ([PaymentPubKeyHash] -> Contract w s e a) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
shouldFail tag testWallets toContract =
  ( testWallets
  , singleTest tag
      . TestContract
        toContract
        (ExpectFailure (const True))
  )

{- | Assert the return value of the contract with a custom predicate.

 @since 0.2
-}
assertYieldedResultWith ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  TestContractConstraints w s e a =>
  String ->
  TestWallets ->
  (a -> Bool) ->
  ([PaymentPubKeyHash] -> Contract w s e a) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
assertYieldedResultWith tag testWallets predicate toContract =
  ( testWallets
  , singleTest tag
      . TestContract
        toContract
        (ExpectSuccess predicate (const True) (twExpected <$> unTestWallets testWallets))
  )

{- | Check if the return value of the contract equals to some expected value.

 @since 0.2
-}
shouldYield ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  (TestContractConstraints w s e a, Eq a) =>
  String ->
  TestWallets ->
  a ->
  ([PaymentPubKeyHash] -> Contract w s e a) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
shouldYield tag testWallets expected =
  assertYieldedResultWith tag testWallets (== expected)

{- | Assert the observable state of the contract with a custom predicate.

 @since 0.2
-}
assertObservableStateWith ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  TestContractConstraints w s e a =>
  String ->
  TestWallets ->
  (w -> Bool) ->
  ([PaymentPubKeyHash] -> Contract w s e a) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
assertObservableStateWith tag testWallets predicate toContract =
  ( testWallets
  , singleTest tag
      . TestContract
        toContract
        (ExpectSuccess (const True) predicate (twExpected <$> unTestWallets testWallets))
  )

{- | Check if the observable state of the contract equals to some expected value.

 @since 0.2
-}
shouldHaveObservableState ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  (TestContractConstraints w s e a, Eq w) =>
  String ->
  TestWallets ->
  w ->
  ([PaymentPubKeyHash] -> Contract w s e a) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
shouldHaveObservableState tag testWallets expected =
  assertObservableStateWith tag testWallets (== expected)

{- | Check if contract throws expected error.

 @since 0.2
-}
shouldThrowContractError ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  (TestContractConstraints w s e a, Eq e) =>
  String ->
  TestWallets ->
  e ->
  ([PaymentPubKeyHash] -> Contract w s e a) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
shouldThrowContractError tag testWallets expected =
  assertContractError tag testWallets (== expected)

{- | Assert the error thrown by contract with a custom predicate.

 @since 0.2
-}
assertContractError ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  (TestContractConstraints w s e a) =>
  String ->
  TestWallets ->
  (e -> Bool) ->
  ([PaymentPubKeyHash] -> Contract w s e a) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
assertContractError tag testWallets predicate =
  assertFailure tag testWallets (mkPredicate predicate)
  where
    mkPredicate p = \case
      ContractExecutionError e -> p e
      _ -> False

{- | Assert the failure reason of contract execution with a custom predicate.

 @since 0.2
-}
assertFailure ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  (TestContractConstraints w s e a) =>
  String ->
  TestWallets ->
  (FailureReason e -> Bool) ->
  ([PaymentPubKeyHash] -> Contract w s e a) ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
assertFailure tag testWallets predicate toContract =
  ( testWallets
  , singleTest tag
      . TestContract
        toContract
        (ExpectFailure predicate)
  )

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
  { tcContract :: (ToJSON w, Monoid w, Show w, Show e, Show a) => [PaymentPubKeyHash] -> Contract w s e a
  , tcExpected :: ExpectedOutcome w e a
  , tcSetup :: IO (ClusterEnv, NonEmpty BpiWallet)
  }
  deriving stock (Typeable)

{- | Expected outcome of running contract.

 @since 0.2
-}
data ExpectedOutcome w e a
  = ExpectSuccess (a -> Bool) (w -> Bool) (NonEmpty (Maybe (ValueOrdering, Value)))
  | ExpectFailure (FailureReason e -> Bool)
  deriving stock (Typeable)

instance
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  TestContractConstraints w s e a =>
  IsTest (TestContract w s e a)
  where
  run _ TestContract {tcContract, tcSetup, tcExpected} _ = do
    (cEnv, wallets@(ownWallet :| otherWallets)) <- tcSetup
    let contract = wrapContract wallets (tcContract (map ledgerPaymentPkh otherWallets))
    result <- runReaderT (runContract cEnv ownWallet contract) cEnv

    pure $ case (tcExpected, result) of
      (ExpectSuccess {}, ExecutionResult (Left err) _) ->
        testFailed $ "Contract failed with " ++ ppShow err
      (ExpectFailure _, ExecutionResult (Right _) _) ->
        testFailed "Expected a failing contract, but it succeeded."
      (ExpectSuccess assertRes assertObsSt expectedVal, ExecutionResult (Right (res, values)) obsSt)
        | not (assertRes res) ->
          testFailed $ "Contract result assertion failed.\nGot: " ++ ppShow res
        | not (assertObsSt obsSt) ->
          testFailed $ "Observable state assertion failed.\nGot:" ++ ppShow obsSt
        | otherwise -> case assertValues expectedVal values of
          Left err -> testFailed $ Text.unpack err
          Right () -> testPassed ""
      (ExpectFailure assertErr, ExecutionResult (Left err) _)
        | not (assertErr err) ->
          testFailed $ "Error assertion failed.\nGot:" ++ ppShow err
        | otherwise ->
          testPassed ""

  testOptions = Tagged []

{- | Wrap test contracts to wait for transaction submission and
 to get the utxo amount at test wallets and wait for transaction.

 @since 0.2
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

assertValues :: NonEmpty (Maybe (ValueOrdering, Value)) -> NonEmpty Value -> Either Text ()
assertValues expected values =
  maybe (Right ()) (Left . report) $
    find findFailing $ zip3 [0 :: Int ..] (NonEmpty.toList expected) (NonEmpty.toList values)
  where
    findFailing (_, Nothing, _) = False
    findFailing (_, Just (ord, v), v') = not (compareValuesWith ord v' v)

    report (_, Nothing, _) = ""
    report (walletIdx, Just (ord, expV), gotV) =
      Text.unlines
        [ mconcat
            [ "Value assertion failed on "
            , if walletIdx == 0 then "own wallet." else "wallet " <> Text.pack (show walletIdx) <> "."
            ]
        , mconcat ["Expected", showVOrd ord, ": ", showValue expV]
        , mconcat ["Got: ", showValue gotV]
        ]

    showVOrd VEq = ""
    showVOrd VGt = " greater than"
    showVOrd VLt = " less than"
    showVOrd VGEq = " greater than or equal to"
    showVOrd VLEq = " less than or equal to"

    showValue :: Value -> Text
    showValue =
      Text.intercalate ", " . map showFlatValue . Value.flattenValue

    showFlatValue :: (CurrencySymbol, TokenName, Integer) -> Text
    showFlatValue (curSymbol, name, amount)
      | curSymbol == Ada.adaSymbol = amountStr <> " lovelace"
      | Text.null tokenNameStr = amountStr <> " " <> curSymbolStr
      | otherwise = amountStr <> " " <> curSymbolStr <> "." <> tokenNameStr
      where
        amountStr = Text.pack $ show amount
        curSymbolStr = encodeByteString $ fromBuiltin $ unCurrencySymbol curSymbol
        tokenNameStr =
          let bs = fromBuiltin $ unTokenName name
           in fromRight (encodeByteString bs) $ decodeUtf8' bs

newtype TestWallets = TestWallets {unTestWallets :: NonEmpty TestWallet}
  deriving newtype (Semigroup)

data TestWallet = TestWallet
  { twInitDistribuition :: Positive
  , twExpected :: Maybe (ValueOrdering, Value)
  }

-- | Value doesn't have an Ord instance, so we cannot use `compare`
data ValueOrdering = VEq | VGt | VLt | VGEq | VLEq

compareValuesWith :: ValueOrdering -> Value -> Value -> Bool
compareValuesWith VEq = (==)
compareValuesWith VGt = Value.gt
compareValuesWith VLt = Value.lt
compareValuesWith VGEq = Value.geq
compareValuesWith VLEq = Value.leq

{- | Create a wallet with the given amount of lovelace.

 @since 0.2
-}
initLovelace :: Positive -> TestWallets
initLovelace initial = TestWallets $ TestWallet initial Nothing :| []

{- | Create a wallet with the given amount of lovelace, and after contract execution
 compare the values at the wallet address with the given ordering and value.

 @since 0.2
-}
initLovelaceAssertValueWith :: Positive -> ValueOrdering -> Value -> TestWallets
initLovelaceAssertValueWith initial ord expect = TestWallets $ TestWallet initial (Just (ord, expect)) :| []

{- | Create a wallet with the given amount of lovelace, and after contract execution
 check if values at the wallet address are equal to a given value.

 @since 0.2
-}
initLovelaceAssertValue :: Positive -> Value -> TestWallets
initLovelaceAssertValue initial = initLovelaceAssertValueWith initial VEq

{- | Create a wallet with the given amount of lovelace, and after contract execution
 compare the values at the wallet address with the given ordering and lovelace amount.

 @since 0.2
-}
initAndAssertLovelaceWith :: Positive -> ValueOrdering -> Positive -> TestWallets
initAndAssertLovelaceWith initial ord expect =
  initLovelaceAssertValueWith initial ord (Ada.lovelaceValueOf (fromIntegral expect))

{- | Create a wallet with the given amount of lovelace, and after contract execution
 check if values at the wallet address are equal to a given lovelace amount.

 @since 0.2
-}
initAndAssertLovelace :: Positive -> Positive -> TestWallets
initAndAssertLovelace initial expect =
  initLovelaceAssertValue initial (Ada.lovelaceValueOf (fromIntegral expect))

{- | Create a wallet with the given amount of Ada.

 @since 0.2
-}
initAda :: Positive -> TestWallets
initAda initial = initLovelace (initial * 1_000_000)

{- | Create a wallet with the given amount of Ada, and after contract execution
 compare the values at the wallet address with the given ordering and value.

 @since 0.2
-}
initAdaAssertValueWith :: Positive -> ValueOrdering -> Value -> TestWallets
initAdaAssertValueWith initial = initLovelaceAssertValueWith (initial * 1_000_000)

{- | Create a wallet with the given amount of Ada, and after contract execution
 check if values at the wallet address are equal to a given value.

 @since 0.2
-}
initAdaAssertValue :: Positive -> Value -> TestWallets
initAdaAssertValue initial = initLovelaceAssertValue (initial * 1_000_000)

{- | Create a wallet with the given amount of Ada, and after contract execution
 compare the values at the wallet address with the given ordering and ada amount.

 @since 0.2
-}
initAndAssertAdaWith :: Positive -> ValueOrdering -> Positive -> TestWallets
initAndAssertAdaWith initial ord expect =
  initAndAssertLovelaceWith (initial * 1_000_000) ord (expect * 1_000_000)

{- | Create a wallet with the given amount of Ada, and after contract execution
 check if values at the wallet address are equal to a given ada amount.

 @since 0.2
-}
initAndAssertAda :: Positive -> Positive -> TestWallets
initAndAssertAda initial expect =
  initAndAssertLovelace (initial * 1_000_000) (expect * 1_000_000)
