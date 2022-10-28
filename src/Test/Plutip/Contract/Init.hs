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

import Test.Plutip.Internal.BotPlutusInterface.Run (defCollateralSize)
import Test.Plutip.Internal.BotPlutusInterface.Types (
  ValueOrdering (VEq),
  WalletSpec (WalletSpec),
  WalletTag,
 )

-- | Create a wallet with the given amounts of lovelace.
--  Each amount will be sent to address as separate UTXO.
--
-- @since 0.2
initLovelace :: WalletTag t -> [Positive] -> NonEmpty WalletSpec
initLovelace tag initial = WalletSpec tag initial Nothing :| []

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- compare the values at the wallet address with the given ordering and value.
--
-- @since 0.2
initLovelaceAssertValueWith :: WalletTag t -> [Positive] -> ValueOrdering -> Value -> NonEmpty WalletSpec
initLovelaceAssertValueWith tag initial ord expect = WalletSpec tag initial (Just (ord, expect)) :| []

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- check if values at the wallet address are equal to a given value.
--
-- @since 0.2
initLovelaceAssertValue :: WalletTag t -> [Positive] -> Value -> NonEmpty WalletSpec
initLovelaceAssertValue tag initial = initLovelaceAssertValueWith tag initial VEq

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- compare the values at the wallet address with the given ordering and lovelace amount.
--
-- @since 0.2
initAndAssertLovelaceWith :: WalletTag t -> [Positive] -> ValueOrdering -> Positive -> NonEmpty WalletSpec
initAndAssertLovelaceWith tag initial ord expect =
  initLovelaceAssertValueWith tag initial ord (Ada.lovelaceValueOf (fromIntegral expect))

-- | Create a wallet with the given amounts of lovelace, and after contract execution
-- check if values at the wallet address are equal to a given lovelace amount.
--
-- @since 0.2
initAndAssertLovelace :: WalletTag t -> [Positive] -> Positive -> NonEmpty WalletSpec
initAndAssertLovelace tag initial expect =
  initLovelaceAssertValue tag initial (Ada.lovelaceValueOf (fromIntegral expect))

-- | Create a wallet with the given amounts of Ada.
--
-- @since 0.2
initAda :: WalletTag t -> [Positive] -> NonEmpty WalletSpec
initAda tag initial = initLovelace tag (map ada initial)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- compare the values at the wallet address with the given ordering and value.
--
-- @since 0.2
initAdaAssertValueWith :: WalletTag t -> [Positive] -> ValueOrdering -> Value -> NonEmpty WalletSpec
initAdaAssertValueWith tag initial = initLovelaceAssertValueWith tag (map ada initial)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- check if values at the wallet address are equal to a given value.
--
-- @since 0.2
initAdaAssertValue :: WalletTag t -> [Positive] -> Value -> NonEmpty WalletSpec
initAdaAssertValue tag initial = initLovelaceAssertValue tag (map ada initial)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- compare the values at the wallet address with the given ordering and ada amount.
--
-- @since 0.2
initAndAssertAdaWith :: WalletTag t -> [Positive] -> ValueOrdering -> Positive -> NonEmpty WalletSpec
initAndAssertAdaWith tag initial ord expect =
  initAndAssertLovelaceWith tag (map ada initial) ord (ada expect)

-- | Create a wallet with the given amounts of Ada, and after contract execution
-- check if values at the wallet address are equal to a given ada amount.
--
-- @since 0.2
initAndAssertAda :: WalletTag t -> [Positive] -> Positive -> NonEmpty WalletSpec
initAndAssertAda tag initial expect =
  initAndAssertLovelace tag (map ada initial) (ada expect)

-- | Initialize all the 'NonEmpty WalletSpec' with the collateral utxo and
--   adjust the 'wsExpected' value accordingly.
withCollateral :: NonEmpty WalletSpec -> NonEmpty WalletSpec
withCollateral = NonEmpty.map go
  where
    go :: WalletSpec -> WalletSpec
    go (WalletSpec tag dist expected) =
      WalletSpec
        tag
        (fromInteger defCollateralSize : dist)
        (second (Value.unionWith (+) collateral) <$> expected)

    collateral = Ada.lovelaceValueOf defCollateralSize

-- | Library functions works with amounts in `Lovelace`.
-- This function helps to specify amounts in `Ada` easier.
ada :: Positive -> Positive
ada = (* 1_000_000)
