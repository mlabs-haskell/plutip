module Test.Plutip.Internal.BotPlutusInterface.Lookups (
  WalletLookups (lookupAddress, lookupWallet),
  makeWalletInfo,
  makeWalletLookups,
  lookupsMap,
) where

import Control.Lens (withPrism)
import Control.Monad.Except (MonadError (throwError))
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Row (Row)
import Data.Text (Text)
import Ledger (Address)
import Plutus.Contract (Contract, ContractError (OtherContractError))
import Plutus.Contract.Error (AsContractError, _ContractError)
import Test.Plutip.Internal.BotPlutusInterface.Types (BaseWallet (BaseWallet), BpiWallet (bwTag), PkhWallet (PkhWallet), WalletInfo, WalletTag (EnterpriseTag, WithStakeKeysTag), ownAddress)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (walletPaymentPkh, walletStakePkh)

-- Error messages for wallet lookup fails.
expectedEnterpriseWallet, expectedWalletWithStakeKeys, badWalletIndex :: Text
expectedEnterpriseWallet = "Expected base address wallet, got one with staking keys."
expectedWalletWithStakeKeys = "Expected base address wallet, got one with staking keys."
badWalletIndex = "Index outside of range."

data WalletLookups k = WalletLookups
  { lookupWallet ::
      forall (t :: Type) (w :: Type) (s :: Row Type) (e :: Type).
      AsContractError e =>
      WalletTag t k ->
      Contract w s e t
  , lookupAddress ::
      forall (w :: Type) (s :: Row Type) (e :: Type).
      AsContractError e =>
      k ->
      Contract w s e Address
  }

makeWalletInfo :: BpiWallet k -> WalletInfo
makeWalletInfo w =
  maybe
    (Right $ PkhWallet (walletPaymentPkh w))
    (Left . BaseWallet (walletPaymentPkh w))
    (walletStakePkh w)

lookupsMap :: Ord k => [BpiWallet k] -> Map k WalletInfo
lookupsMap bpiWalls =
  Map.fromList $
    (\w -> (bwTag w, makeWalletInfo w)) <$> bpiWalls

makeWalletLookups ::
  Ord k =>
  Map k WalletInfo ->
  WalletLookups k
makeWalletLookups lookups =
  WalletLookups
    { lookupWallet = lookupTaggedWallet lookups
    , lookupAddress = \k ->
        maybe (toError badWalletIndex) pure $
          Map.lookup k $ ownAddress <$> lookups
    }
  where
    toError :: MonadError e m => AsContractError e => Text -> m a
    toError = throwError . (\e -> withPrism _ContractError $ \f _ -> f e) . OtherContractError

    lookupTaggedWallet ::
      forall (k :: Type) (w :: Type) (s :: Row Type) (e :: Type) (t :: Type).
      (Ord k, AsContractError e) =>
      Map k WalletInfo ->
      WalletTag t k ->
      Contract w s e t
    lookupTaggedWallet wl (EnterpriseTag k) = case Map.lookup k wl of
      Nothing -> toError badWalletIndex
      Just (Right res@(PkhWallet _)) -> pure res
      Just (Left (BaseWallet _ _)) -> toError expectedEnterpriseWallet
    lookupTaggedWallet wl (WithStakeKeysTag k) = case Map.lookup k wl of
      Nothing -> toError badWalletIndex
      Just (Right (PkhWallet _)) -> toError expectedWalletWithStakeKeys
      Just (Left res@(BaseWallet _ _)) -> pure res
