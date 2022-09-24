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
  TestWallet (TestWallet, twExpected, twInitDistribiution),
  TestWallets (TestWallets, unTestWallets),
  ValueOrdering (VEq), WalletTag, TestWallet' (TestWallet')
 )
import Test.Plutip.Tools (ada)

-- | Create a wallet with the given amounts of lovelace.
--  Each amount will be sent to address as separate UTXO.
--
-- @since 0.2
initLovelace :: WalletTag t k -> [Positive] -> TestWallets k
initLovelace tag initial = TestWallets $ TestWallet' (TestWallet initial Nothing tag) :| []

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- compare the values at the wallet address with the given ordering and value.
--
-- @since 0.2
initLovelaceAssertValueWith :: WalletTag t k -> [Positive] -> ValueOrdering -> Value -> TestWallets k
initLovelaceAssertValueWith tag initial ord expect = TestWallets $ TestWallet' (TestWallet initial (Just (ord, expect)) tag) :| []

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- check if values at the wallet address are equal to a given value.
--
-- @since 0.2
initLovelaceAssertValue :: WalletTag t k -> [Positive] -> Value -> TestWallets k
initLovelaceAssertValue tag initial = initLovelaceAssertValueWith tag initial VEq 

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- compare the values at the wallet address with the given ordering and lovelace amount.
--
-- @since 0.2
initAndAssertLovelaceWith :: WalletTag t k -> [Positive] -> ValueOrdering -> Positive -> TestWallets k
initAndAssertLovelaceWith tag initial ord expect =
  initLovelaceAssertValueWith tag initial ord (Ada.lovelaceValueOf (fromIntegral expect))

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- check if values at the wallet address are equal to a given lovelace amount.
--
-- @since 0.2
initAndAssertLovelace :: WalletTag t k -> [Positive] -> Positive -> TestWallets k
initAndAssertLovelace tag initial expect =
  initLovelaceAssertValue tag initial (Ada.lovelaceValueOf (fromIntegral expect))

-- | Create a wallet with the given amounts of Ada.
--
-- @since 0.2
initAda :: WalletTag t k -> [Positive] -> TestWallets k
initAda tag initial = initLovelace tag (map ada initial)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- compare the values at the wallet address with the given ordering and value.
--
-- @since 0.2
initAdaAssertValueWith :: WalletTag t k -> [Positive] -> ValueOrdering -> Value -> TestWallets k
initAdaAssertValueWith tag initial = initLovelaceAssertValueWith tag (map ada initial)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- check if values at the wallet address are equal to a given value.
--
-- @since 0.2
initAdaAssertValue :: WalletTag t k -> [Positive] -> Value -> TestWallets k
initAdaAssertValue tag initial = initLovelaceAssertValue tag (map ada initial)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- compare the values at the wallet address with the given ordering and ada amount.
--
-- @since 0.2
initAndAssertAdaWith :: WalletTag t k -> [Positive] -> ValueOrdering -> Positive -> TestWallets k
initAndAssertAdaWith tag initial ord expect =
  initAndAssertLovelaceWith tag (map ada initial) ord (ada expect)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- check if values at the wallet address are equal to a given ada amount.
--
-- @since 0.2
initAndAssertAda :: WalletTag t k -> [Positive] -> Positive -> TestWallets k
initAndAssertAda tag initial expect =
  initAndAssertLovelace tag (map ada initial) (ada expect)

-- | Initialize all the 'TestWallets' with the collateral utxo and
--   adjust the 'twExpected' value accordingly.
withCollateral :: TestWallets k -> TestWallets k
withCollateral TestWallets {..} = TestWallets $ NonEmpty.map go unTestWallets
  where
    go :: TestWallet' k -> TestWallet' k
    go (TestWallet' wall@TestWallet {..}) = 
      TestWallet' wall
        { twInitDistribiution = fromInteger defCollateralSize : twInitDistribiution
        , twExpected =
            second (Value.unionWith (+) $ Ada.lovelaceValueOf defCollateralSize) <$> twExpected
        }
