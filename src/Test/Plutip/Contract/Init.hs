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

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty

import Data.Bifunctor (second)

import Ledger (Value)
import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value

import Numeric.Positive (Positive)

import Test.Plutip.Internal.BotPlutusInterface.Run (defCollateralSize)
import Test.Plutip.Internal.BotPlutusInterface.Types (
  -- WalletSpec (wsExpected, wsInitDistribiution),
  TestWallet (TestWallet),
  TestWallets,
  ValueOrdering (VEq),
  WalletTag,
  mkWallet,
  twDistribution,
  twExpected,
  wsTag,
 )
import Test.Plutip.Tools (ada)

-- | Create a wallet with the given amounts of lovelace.
--  Each amount will be sent to address as separate UTXO.
--
-- @since 0.2
initLovelace :: WalletTag t -> [Positive] -> TestWallets
initLovelace tag initial = mkWallet initial Nothing tag :| []

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- compare the values at the wallet address with the given ordering and value.
--
-- @since 0.2
initLovelaceAssertValueWith :: WalletTag t -> [Positive] -> ValueOrdering -> Value -> TestWallets
initLovelaceAssertValueWith tag initial ord expect = mkWallet initial (Just (ord, expect)) tag :| []

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- check if values at the wallet address are equal to a given value.
--
-- @since 0.2
initLovelaceAssertValue :: WalletTag t -> [Positive] -> Value -> TestWallets
initLovelaceAssertValue tag initial = initLovelaceAssertValueWith tag initial VEq

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- compare the values at the wallet address with the given ordering and lovelace amount.
--
-- @since 0.2
initAndAssertLovelaceWith :: WalletTag t -> [Positive] -> ValueOrdering -> Positive -> TestWallets
initAndAssertLovelaceWith tag initial ord expect =
  initLovelaceAssertValueWith tag initial ord (Ada.lovelaceValueOf (fromIntegral expect))

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- check if values at the wallet address are equal to a given lovelace amount.
--
-- @since 0.2
initAndAssertLovelace :: WalletTag t -> [Positive] -> Positive -> TestWallets
initAndAssertLovelace tag initial expect =
  initLovelaceAssertValue tag initial (Ada.lovelaceValueOf (fromIntegral expect))

-- | Create a wallet with the given amounts of Ada.
--
-- @since 0.2
initAda :: WalletTag t -> [Positive] -> TestWallets
initAda tag initial = initLovelace tag (map ada initial)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- compare the values at the wallet address with the given ordering and value.
--
-- @since 0.2
initAdaAssertValueWith :: WalletTag t -> [Positive] -> ValueOrdering -> Value -> TestWallets
initAdaAssertValueWith tag initial = initLovelaceAssertValueWith tag (map ada initial)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- check if values at the wallet address are equal to a given value.
--
-- @since 0.2
initAdaAssertValue :: WalletTag t -> [Positive] -> Value -> TestWallets
initAdaAssertValue tag initial = initLovelaceAssertValue tag (map ada initial)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- compare the values at the wallet address with the given ordering and ada amount.
--
-- @since 0.2
initAndAssertAdaWith :: WalletTag t -> [Positive] -> ValueOrdering -> Positive -> TestWallets
initAndAssertAdaWith tag initial ord expect =
  initAndAssertLovelaceWith tag (map ada initial) ord (ada expect)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- check if values at the wallet address are equal to a given ada amount.
--
-- @since 0.2
initAndAssertAda :: WalletTag t -> [Positive] -> Positive -> TestWallets
initAndAssertAda tag initial expect =
  initAndAssertLovelace tag (map ada initial) (ada expect)

-- | Initialize all the 'TestWallets' with the collateral utxo and
--   adjust the 'twExpected' value accordingly.
withCollateral :: TestWallets -> TestWallets
withCollateral = NonEmpty.map go
  where
    go :: TestWallet -> TestWallet
    go tw@(TestWallet spec) =
      mkWallet
        (fromInteger defCollateralSize : twDistribution tw)
        (second (Value.unionWith (+) collateral) <$> twExpected tw)
        (wsTag spec)

    collateral = Ada.lovelaceValueOf defCollateralSize
