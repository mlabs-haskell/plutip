{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | This module provides some contract assertions to be used with `Test.Plutip.LocalCluster.withContract`
--  All assertions accept a name, some TestWallets and a contract.
--
--  At least one TestWallet is required, this will be used as the own wallet for the contract. Any other
--  wallets can be used as other parties in transactions.
--
--  A TestWallet can be initialised with any positive number of lovelace, using the `initAda` or
--  `initLovelace`. In addition, the value in these wallets can be asserted after the contract
--  execution with `initAdaAssertValue` or `initAndAssertAda`.
--
-- > shouldSucceed "Get utxos" (initAda 100) $ withContract $ \_ -> do
-- >   pkh <- Contract.ownPaymentPubKeyHash
-- >   utxosAt $ pubKeyHashAddress pkh Nothing
--
--  To use multiple wallets, you can use the `Semigroup` instance of `TestWallets`. To reference the
--  wallet inside the contract, the following callback function is used, when
--  supplying a contract toa test case: @[PaymentPubKeyHash] -> Contract w s e a@.
--  Note that @[PaymentPubKeyHash]@ does not include the contract's own wallet, for that you can use `Plutus.Contract.ownPaymentPubKeyHash` inside the Contract monad.
--
-- > shouldSucceed "Send some Ada" (initAda 100 <> initAndAssertAda 100 110) $ withContract $
-- >   \[pkh1] -> submitTx (Constraints.mustPayToPubKey pkh1 (Ada.lovelaceValueOf amt))
--
-- If you have multiple contracts depending on each other, you can chain them together using
--
-- `withContract` and `withContractAs`.
-- > shouldSucceed
-- >   "Two contracts after each other"
-- >   (initAndAssertAdaWith 100 VLt 100 <> initAndAssertAdaWith 100 VLt 100)
-- >   $ do
-- >     void $
-- >       withContract $
-- >         \[pkh1] -> payTo pkh1 10_000_000
-- >     withContractAs 1 $
-- >       \[pkh1] -> payTo pkh1 10_000_000
module Test.Plutip.Contract
  ( withContract,
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
  )
where

import Control.Arrow (left)
import Control.Monad (void)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT, runReaderT)
import Data.Aeson (ToJSON)
import Data.Aeson.Extras (encodeByteString)
import Data.Bool (bool)
import Data.Dynamic (Typeable)
import Data.Either (fromRight)
import Data.Kind (Type)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (isJust)
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
import Test.Plutip.Internal.Types
  ( ClusterEnv,
    ExecutionResult (outcome),
  )
import Test.Plutip.Predicate (Predicate (pCheck, sayIssue, whenOk))
import Test.Tasty (testGroup, withResource)
import Test.Tasty.HUnit (assertFailure, testCase)
import Test.Tasty.Providers (IsTest (run, testOptions), TestTree, singleTest, testFailed, testPassed)
import Text.Show.Pretty (ppShow)

type TestContractConstraints (w :: Type) (e :: Type) (a :: Type) =
  ( ToJSON w,
    Monoid w,
    Show w,
    Show e,
    Show a,
    Typeable w,
    Typeable e,
    Typeable a,
    AsContractError e
  )

assertExecution ::
  forall (w :: Type) (e :: Type) (a :: Type).
  TestContractConstraints w e a =>
  String ->
  TestWallets ->
  TestRunner w e a ->
  [Predicate w e a] ->
  (TestWallets, IO (ClusterEnv, NonEmpty BpiWallet) -> TestTree)
assertExecution tag testWallets testRunner predicates =
  (testWallets, toTestGroup)
  where
    toTestGroup ioEnv =
      withResource (runReaderT testRunner =<< ioEnv) (const $ pure ()) $
        \ioRes ->
          testGroup tag $
            maybeAddValuesCheck
              ioRes
              testWallets
              (toCase ioRes <$> predicates)

    -- wraps IO with result of contract execution into single test
    toCase ioRes p =
      singleTest (whenOk p) (TestContract p ioRes)

-- | Adds test case with assertions on values
--  if any assertion was added by `initAndAssert...` setup functions
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

    valuesCheckCase = testCase "Values check" $ do
      ioRes
        >>= either (assertFailure . Text.unpack) (const $ pure ())
          . checkOutcome
          . outcome

    checkOutcome o =
      left (Text.pack . show) o
        >>= \(_, vs) -> assertValues expected vs

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

-- | Test contract
data TestContract (w :: Type) (e :: Type) (a :: Type)
  = TestContract
      (Predicate w e a)
      -- ^ Predicate to test
      (IO (ExecutionResult w e (a, NonEmpty Value)))
      -- ^ Result of contract executoin
  deriving stock (Typeable)

type TestRunner (w :: Type) (e :: Type) (a :: Type) =
  ReaderT (ClusterEnv, NonEmpty BpiWallet) IO (ExecutionResult w e (a, NonEmpty Value))

instance
  forall (w :: Type) (e :: Type) (a :: Type).
  TestContractConstraints w e a =>
  IsTest (TestContract w e a)
  where
  run _ (TestContract predicate runResult) _ = do
    result <- runResult
    pure $
      bool
        (testFailed $ sayIssue predicate result)
        (testPassed "")
        (pCheck predicate result)

  testOptions = Tagged []

-- | Run a contract using the first wallet as own wallet, and return ExecutionResult.
-- This could be used by itself, or combined with multiple other contracts.
--
-- @since 0.2
withContract ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type).
  TestContractConstraints w e a =>
  ([PaymentPubKeyHash] -> Contract w s e a) ->
  TestRunner w e a
withContract = withContractAs 0

-- | Run a contract using the nth wallet as own wallet, and return ExecutionResult.
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
    reorder i xss =
      -- Allowing this to be partial intentionally
      let (xs, y : ys) = NonEmpty.splitAt i xss
       in y :| xs ++ ys

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
            [ "Value assertion failed on ",
              if walletIdx == 0 then "own wallet." else "wallet " <> Text.pack (show walletIdx) <> "."
            ],
          mconcat ["Expected", showVOrd ord, ": ", showValue expV],
          mconcat ["Got: ", showValue gotV]
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
  { twInitDistribuition :: Positive,
    twExpected :: Maybe (ValueOrdering, Value)
  }

-- | Value doesn't have an Ord instance, so we cannot use `compare`
data ValueOrdering = VEq | VGt | VLt | VGEq | VLEq

compareValuesWith :: ValueOrdering -> Value -> Value -> Bool
compareValuesWith VEq = (==)
compareValuesWith VGt = Value.gt
compareValuesWith VLt = Value.lt
compareValuesWith VGEq = Value.geq
compareValuesWith VLEq = Value.leq

-- | Create a wallet with the given amount of lovelace.
--
-- @since 0.2
initLovelace :: Positive -> TestWallets
initLovelace initial = TestWallets $ TestWallet initial Nothing :| []

-- | Create a wallet with the given amount of lovelace, and after contract execution
-- compare the values at the wallet address with the given ordering and value.
--
-- @since 0.2
initLovelaceAssertValueWith :: Positive -> ValueOrdering -> Value -> TestWallets
initLovelaceAssertValueWith initial ord expect = TestWallets $ TestWallet initial (Just (ord, expect)) :| []

-- | Create a wallet with the given amount of lovelace, and after contract execution
-- check if values at the wallet address are equal to a given value.
--
-- @since 0.2
initLovelaceAssertValue :: Positive -> Value -> TestWallets
initLovelaceAssertValue initial = initLovelaceAssertValueWith initial VEq

-- | Create a wallet with the given amount of lovelace, and after contract execution
-- compare the values at the wallet address with the given ordering and lovelace amount.
--
-- @since 0.2
initAndAssertLovelaceWith :: Positive -> ValueOrdering -> Positive -> TestWallets
initAndAssertLovelaceWith initial ord expect =
  initLovelaceAssertValueWith initial ord (Ada.lovelaceValueOf (fromIntegral expect))

-- | Create a wallet with the given amount of lovelace, and after contract execution
-- check if values at the wallet address are equal to a given lovelace amount.
--
-- @since 0.2
initAndAssertLovelace :: Positive -> Positive -> TestWallets
initAndAssertLovelace initial expect =
  initLovelaceAssertValue initial (Ada.lovelaceValueOf (fromIntegral expect))

-- | Create a wallet with the given amount of Ada.
--
-- @since 0.2
initAda :: Positive -> TestWallets
initAda initial = initLovelace (initial * 1_000_000)

-- | Create a wallet with the given amount of Ada, and after contract execution
-- compare the values at the wallet address with the given ordering and value.
--
-- @since 0.2
initAdaAssertValueWith :: Positive -> ValueOrdering -> Value -> TestWallets
initAdaAssertValueWith initial = initLovelaceAssertValueWith (initial * 1_000_000)

-- | Create a wallet with the given amount of Ada, and after contract execution
-- check if values at the wallet address are equal to a given value.
--
-- @since 0.2
initAdaAssertValue :: Positive -> Value -> TestWallets
initAdaAssertValue initial = initLovelaceAssertValue (initial * 1_000_000)

-- | Create a wallet with the given amount of Ada, and after contract execution
-- compare the values at the wallet address with the given ordering and ada amount.
--
-- @since 0.2
initAndAssertAdaWith :: Positive -> ValueOrdering -> Positive -> TestWallets
initAndAssertAdaWith initial ord expect =
  initAndAssertLovelaceWith (initial * 1_000_000) ord (expect * 1_000_000)

-- | Create a wallet with the given amount of Ada, and after contract execution
-- check if values at the wallet address are equal to a given ada amount.
--
-- @since 0.2
initAndAssertAda :: Positive -> Positive -> TestWallets
initAndAssertAda initial expect =
  initAndAssertLovelace (initial * 1_000_000) (expect * 1_000_000)
