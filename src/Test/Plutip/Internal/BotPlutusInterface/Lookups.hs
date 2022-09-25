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
import Data.String (IsString (fromString))
import Ledger (Address)
import Plutus.Contract (Contract, ContractError (OtherContractError))
import Plutus.Contract.Error (AsContractError, _ContractError)
import Test.Plutip.Internal.BotPlutusInterface.Types (BpiWallet (bwTag), WalletInfo (EnterpriseInfo, WithStakeKeysInfo), WalletInfo' (WalletInfo'), WalletTag (EnterpriseTag, WithStakeKeysTag), WalletType, WalletTypeError (BadWalletIndex, ExpectedEnterpriseWallet, ExpectedWalletWithStakeKeys), ownAddress)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (walletPaymentPkh, walletStakePkh)

data WalletLookups k = WalletLookups
  { lookupWallet ::
      forall (t :: WalletType) (w :: Type) (s :: Row Type) (e :: Type).
      AsContractError e =>
      WalletTag t k ->
      Contract w s e (WalletInfo t)
  , lookupAddress ::
      forall (w :: Type) (s :: Row Type) (e :: Type).
      AsContractError e =>
      k ->
      Contract w s e Address
  }

makeWalletInfo :: BpiWallet k -> WalletInfo'
makeWalletInfo w =
  maybe
    (WalletInfo' $ EnterpriseInfo (walletPaymentPkh w))
    (WalletInfo' . WithStakeKeysInfo (walletPaymentPkh w))
    (walletStakePkh w)

lookupsMap :: Ord k => [BpiWallet k] -> Map k WalletInfo'
lookupsMap bpiWalls =
  Map.fromList $
    (\w -> (bwTag w, makeWalletInfo w)) <$> bpiWalls

makeWalletLookups ::
  Ord k =>
  Map k WalletInfo' ->
  WalletLookups k
makeWalletLookups lookups =
  WalletLookups
    { lookupWallet = lookupTaggedWallet lookups
    , lookupAddress = \k ->
        maybe (throwError $ toError BadWalletIndex) pure $
          Map.lookup k $ (\(WalletInfo' w) -> ownAddress w) <$> lookups
    }
  where
    toError :: AsContractError e => WalletTypeError -> e
    toError = (\e -> withPrism _ContractError $ \f _ -> f e) . OtherContractError . fromString . show

    lookupTaggedWallet ::
      forall (k :: Type) (w :: Type) (s :: Row Type) (e :: Type) (t :: WalletType).
      (Ord k, AsContractError e) =>
      Map k WalletInfo' ->
      WalletTag t k ->
      Contract w s e (WalletInfo t)
    lookupTaggedWallet wl (EnterpriseTag k) = case Map.lookup k wl of
      Nothing -> throwError $ toError BadWalletIndex
      Just (WalletInfo' res@(EnterpriseInfo _)) -> pure res
      Just (WalletInfo' (WithStakeKeysInfo _ _)) -> throwError $ toError ExpectedEnterpriseWallet
    lookupTaggedWallet wl (WithStakeKeysTag k) = case Map.lookup k wl of
      Nothing -> throwError $ toError BadWalletIndex
      Just (WalletInfo' (EnterpriseInfo _)) -> throwError $ toError ExpectedWalletWithStakeKeys
      Just (WalletInfo' res@(WithStakeKeysInfo _ _)) -> pure res
