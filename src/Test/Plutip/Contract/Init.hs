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
) where

import Data.List.NonEmpty (NonEmpty ((:|)))

import Ledger (Value)
import Ledger.Ada qualified as Ada

import Numeric.Positive (Positive)

import Test.Plutip.Contract.Types (
  TestWallet (TestWallet),
  TestWallets (TestWallets),
  ValueOrdering (VEq),
 )

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
