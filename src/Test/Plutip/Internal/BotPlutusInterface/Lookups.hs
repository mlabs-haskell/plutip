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
import Test.Plutip.Internal.BotPlutusInterface.Types (BpiWallet (bwTag), EnterpriseInfo (EnterpriseInfo), WalletInfo, WalletTag (EnterpriseTag, WithStakeKeysTag), WalletTypeError (BadWalletIndex, ExpectedEnterpriseWallet, ExpectedWalletWithStakeKeys), WithStakeKeysInfo (WithStakeKeysInfo), ownAddress)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (walletPaymentPkh, walletStakePkh)

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
    (Right $ EnterpriseInfo (walletPaymentPkh w))
    (Left . WithStakeKeysInfo (walletPaymentPkh w))
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
        maybe (throwError $ toError BadWalletIndex) pure $
          Map.lookup k $ ownAddress <$> lookups
    }
  where
    toError :: AsContractError e => WalletTypeError -> e
    toError = (\e -> withPrism _ContractError $ \f _ -> f e) . OtherContractError . fromString . show

    lookupTaggedWallet ::
      forall (k :: Type) (w :: Type) (s :: Row Type) (e :: Type) (t :: Type).
      (Ord k, AsContractError e) =>
      Map k WalletInfo ->
      WalletTag t k ->
      Contract w s e t
    lookupTaggedWallet wl (EnterpriseTag k) = case Map.lookup k wl of
      Nothing -> throwError $ toError BadWalletIndex
      Just (Right res@(EnterpriseInfo _)) -> pure res
      Just (Left (WithStakeKeysInfo _ _)) -> throwError $ toError ExpectedEnterpriseWallet
    lookupTaggedWallet wl (WithStakeKeysTag k) = case Map.lookup k wl of
      Nothing -> throwError $ toError BadWalletIndex
      Just (Right (EnterpriseInfo _)) -> throwError $ toError ExpectedWalletWithStakeKeys
      Just (Left res@(WithStakeKeysInfo _ _)) -> pure res
