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
) where


import Data.Bifunctor (second)
import Ledger (Value)
import Ledger.Value qualified as Value
import Ledger.Ada qualified as Ada

import Numeric.Positive (Positive)

import Test.Plutip.Contract.Types (
  TestWallet(twInitDistribuition, twExpected),
  initTestWallet,
  ValueOrdering (VEq),
  Wallets,
 )
import Test.Plutip.Internal.BotPlutusInterface.Run (defCollateralSize)
import Test.Plutip.Tools (ada)

-- | Create a wallet with the given amounts of lovelace.
--  Each amount will be sent to address as separate UTXO.
--
-- @since 0.2
initLovelace :: [Positive] -> TestWallet
initLovelace initial = initTestWallet initial Nothing

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- compare the values at the wallet address with the given ordering and value.
--
-- @since 0.2
initLovelaceAssertValueWith :: [Positive] -> ValueOrdering -> Value -> TestWallet
initLovelaceAssertValueWith initial ord expect = initTestWallet initial $ Just (ord, expect)

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- check if values at the wallet address are equal to a given value.
--
-- @since 0.2
initLovelaceAssertValue :: [Positive] -> Value -> TestWallet
initLovelaceAssertValue initial = initLovelaceAssertValueWith initial VEq

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- compare the values at the wallet address with the given ordering and lovelace amount.
--
-- @since 0.2
initAndAssertLovelaceWith :: [Positive] -> ValueOrdering -> Positive -> TestWallet
initAndAssertLovelaceWith initial ord expect =
  initLovelaceAssertValueWith initial ord (Ada.lovelaceValueOf (fromIntegral expect))

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- check if values at the wallet address are equal to a given lovelace amount.
--
-- @since 0.2
initAndAssertLovelace :: [Positive] -> Positive -> TestWallet
initAndAssertLovelace initial expect =
  initLovelaceAssertValue initial (Ada.lovelaceValueOf (fromIntegral expect))

-- | Create a wallet with the given amounts of Ada.
--
-- @since 0.2
initAda :: [Positive] -> TestWallet
initAda initial = initLovelace (map ada initial)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- compare the values at the wallet address with the given ordering and value.
--
-- @since 0.2
initAdaAssertValueWith :: [Positive] -> ValueOrdering -> Value -> TestWallet
initAdaAssertValueWith initial = initLovelaceAssertValueWith (map ada initial)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- check if values at the wallet address are equal to a given value.
--
-- @since 0.2
initAdaAssertValue :: [Positive] -> Value -> TestWallet
initAdaAssertValue initial = initLovelaceAssertValue (map ada initial)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- compare the values at the wallet address with the given ordering and ada amount.
--
-- @since 0.2
initAndAssertAdaWith :: [Positive] -> ValueOrdering -> Positive -> TestWallet
initAndAssertAdaWith initial ord expect =
  initAndAssertLovelaceWith (map ada initial) ord (ada expect)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- check if values at the wallet address are equal to a given ada amount.
--
-- @since 0.2
initAndAssertAda :: [Positive] -> Positive -> TestWallet
initAndAssertAda initial expect =
  initAndAssertLovelace (map ada initial) (ada expect)

-- | Initialize all the 'TestWallets' with the collateral utxo and
--   adjust the 'twExpected' value accordingly.

withCollateral :: Wallets idxs TestWallet -> Wallets idxs TestWallet
withCollateral = fmap go
  where
    go :: TestWallet -> TestWallet
    go tw = initTestWallet (fromInteger defCollateralSize : twInitDistribuition tw)
                           (second (Value.unionWith (+) $ Ada.lovelaceValueOf defCollateralSize) <$> twExpected tw)

