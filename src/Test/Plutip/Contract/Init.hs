-- | Wallets initiation
module Test.Plutip.Contract.Init (
  initLovelace,
  initLovelaceAssertValueWith,
  initLovelaceAssertValue,
  initAndAssertLovelaceWith,
  initAndAssertLovelace,
  initAda,
  initAdaAssertValueWith,
  initAdaAssertValue,
  initAndAssertAdaWith,
  initAndAssertAda,
  withCollateral,
  ada,
) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty

import Data.Bifunctor (second)

import Ledger (Value)
import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value

import Numeric.Positive (Positive)

import Test.Plutip.Contract.Types (
  TestWallet (TestWallet, twExpected, twInitDistribuition),
  TestWallets (TestWallets, unTestWallets),
  ValueOrdering (VEq),
 )
import Test.Plutip.Internal.BotPlutusInterface.Run (defCollateralSize)

-- | Create a wallet with the given amounts of lovelace.
--  Each amount will be sent to address as separate UTXO.
--
-- @since 0.2
initLovelace :: [Positive] -> TestWallets
initLovelace initial = TestWallets $ TestWallet initial Nothing :| []

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- compare the values at the wallet address with the given ordering and value.
--
-- @since 0.2
initLovelaceAssertValueWith :: [Positive] -> ValueOrdering -> Value -> TestWallets
initLovelaceAssertValueWith initial ord expect = TestWallets $ TestWallet initial (Just (ord, expect)) :| []

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- check if values at the wallet address are equal to a given value.
--
-- @since 0.2
initLovelaceAssertValue :: [Positive] -> Value -> TestWallets
initLovelaceAssertValue initial = initLovelaceAssertValueWith initial VEq

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- compare the values at the wallet address with the given ordering and lovelace amount.
--
-- @since 0.2
initAndAssertLovelaceWith :: [Positive] -> ValueOrdering -> Positive -> TestWallets
initAndAssertLovelaceWith initial ord expect =
  initLovelaceAssertValueWith initial ord (Ada.lovelaceValueOf (fromIntegral expect))

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- check if values at the wallet address are equal to a given lovelace amount.
--
-- @since 0.2
initAndAssertLovelace :: [Positive] -> Positive -> TestWallets
initAndAssertLovelace initial expect =
  initLovelaceAssertValue initial (Ada.lovelaceValueOf (fromIntegral expect))

-- | Create a wallet with the given amounts of Ada.
--
-- @since 0.2
initAda :: [Positive] -> TestWallets
initAda initial = initLovelace (map ada initial)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- compare the values at the wallet address with the given ordering and value.
--
-- @since 0.2
initAdaAssertValueWith :: [Positive] -> ValueOrdering -> Value -> TestWallets
initAdaAssertValueWith initial = initLovelaceAssertValueWith (map ada initial)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- check if values at the wallet address are equal to a given value.
--
-- @since 0.2
initAdaAssertValue :: [Positive] -> Value -> TestWallets
initAdaAssertValue initial = initLovelaceAssertValue (map ada initial)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- compare the values at the wallet address with the given ordering and ada amount.
--
-- @since 0.2
initAndAssertAdaWith :: [Positive] -> ValueOrdering -> Positive -> TestWallets
initAndAssertAdaWith initial ord expect =
  initAndAssertLovelaceWith (map ada initial) ord (ada expect)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- check if values at the wallet address are equal to a given ada amount.
--
-- @since 0.2
initAndAssertAda :: [Positive] -> Positive -> TestWallets
initAndAssertAda initial expect =
  initAndAssertLovelace (map ada initial) (ada expect)

-- | Initialize all the 'TestWallets' with the collateral utxo and
--   adjust the 'twExpected' value accordingly.
withCollateral :: TestWallets -> TestWallets
withCollateral TestWallets {..} = TestWallets $ NonEmpty.map go unTestWallets
  where
    go :: TestWallet -> TestWallet
    go TestWallet {..} =
      TestWallet
        { twInitDistribuition = fromInteger defCollateralSize : twInitDistribuition
        , twExpected =
            second (Value.unionWith (+) $ Ada.lovelaceValueOf defCollateralSize) <$> twExpected
        }

-- | Library functions works with amounts in `Lovelace`.
-- This function helps to specify amounts in `Ada` easier.
ada :: Positive -> Positive
ada = (* 1_000_000)
