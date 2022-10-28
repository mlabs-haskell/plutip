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
import Test.Plutip.Contract.Types (TestWallet (getWallet), getTwTag)
import Test.Plutip.Internal.BotPlutusInterface.Types (
  BaseWallet (BaseWallet),
  EntWallet (EntWallet),
  WalletInfo,
  WalletTag (BaseTag, EntTag),
  ownAddress,
 )
import Test.Plutip.Internal.BotPlutusInterface.Wallet (walletPaymentPkh, walletStakePkh)

-- Error messages for wallet lookup fails.
expectedEnterpriseWallet, expectedWalletWithStakeKeys :: Text
expectedEnterpriseWallet = "Expected base address wallet, got one with staking keys."
expectedWalletWithStakeKeys = "Expected base address wallet, got one with staking keys."
badWalletTag :: Text -> Text
badWalletTag tag = "Wallet not found by tag '" <> tag <> "'."

-- | Type to be used for looking up wallet informations. Wallets accessed by their k typed names.
data WalletLookups = WalletLookups
  { lookupWallet ::
      forall (t :: Type) (w :: Type) (s :: Row Type) (e :: Type).
      AsContractError e =>
      WalletTag t ->
      Contract w s e t
  , lookupAddress ::
      forall (w :: Type) (s :: Row Type) (e :: Type).
      AsContractError e =>
      Text ->
      Contract w s e Address
  }

makeWalletInfo :: TestWallet -> WalletInfo
makeWalletInfo w =
  let bpiWallet = getWallet w
   in maybe
        (Right $ EntWallet (walletPaymentPkh bpiWallet))
        (Left . BaseWallet (walletPaymentPkh bpiWallet))
        (walletStakePkh bpiWallet)

lookupsMap :: [TestWallet] -> Map Text WalletInfo
lookupsMap bpiWalls =
  Map.fromList $
    (\w -> (getTwTag w, makeWalletInfo w)) <$> bpiWalls

makeWalletLookups ::
  Map Text WalletInfo ->
  WalletLookups
makeWalletLookups lookups =
  WalletLookups
    { lookupWallet = lookupTaggedWallet lookups
    , lookupAddress = \tag ->
        maybe (toError $ badWalletTag tag) pure $
          Map.lookup tag $ ownAddress <$> lookups
    }
  where
    toError :: MonadError e m => AsContractError e => Text -> m a
    toError = throwError . (\e -> withPrism _ContractError $ \f _ -> f e) . OtherContractError

    lookupTaggedWallet ::
      forall (w :: Type) (s :: Row Type) (e :: Type) (t :: Type).
      (AsContractError e) =>
      Map Text WalletInfo ->
      WalletTag t ->
      Contract w s e t
    lookupTaggedWallet ws (EntTag tag) = case Map.lookup tag ws of
      Nothing -> toError $ badWalletTag tag
      Just (Right res@(EntWallet _)) -> pure res
      Just (Left (BaseWallet _ _)) -> toError expectedEnterpriseWallet
    lookupTaggedWallet ws (BaseTag tag) = case Map.lookup tag ws of
      Nothing -> toError $ badWalletTag tag
      Just (Right (EntWallet _)) -> toError expectedWalletWithStakeKeys
      Just (Left res@(BaseWallet _ _)) -> pure res
