{-# LANGUAGE TupleSections #-}

module Test.Plutip.Internal.BotPlutusInterface.Wallet (
  BpiWallet (..),
  addSomeWallet,
  addSomeWalletDir,
  eitherAddSomeWallet,
  eitherAddSomeWalletDir,
  mkMainnetAddress,
  cardanoMainnetAddress,
  walletPaymentPkh,
  walletStakePkh,
) where

import Cardano.Api (AddressAny)
import Cardano.Api qualified as CAPI
import Cardano.Api.Shelley qualified as CAPI
import Cardano.BM.Data.Tracer (nullTracer)
import Cardano.Ledger.BaseTypes as Shelley (Network (Mainnet))
import Cardano.Ledger.Credential qualified as Shelley
import Cardano.Wallet.Primitive.Types.Coin (Coin (Coin))
import Cardano.Wallet.Shelley.Launch.Cluster (
  sendFaucetFundsTo,
 )
import Control.Arrow (ArrowChoice (left))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Bool (bool)
import Data.Either (isRight)
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash), PubKeyHash (PubKeyHash), StakePubKeyHash (StakePubKeyHash))
import PlutusTx.Builtins (toBuiltin)
import System.Directory (createDirectoryIfMissing)
import Test.Plutip.Internal.BotPlutusInterface.Keys (KeyPair (vKey), StakeKeyPair (sVKey), genKeyPair, genStakeKeyPair, writeKeyPair, writeStakeKeyPairs)
import Test.Plutip.Internal.BotPlutusInterface.Setup qualified as Setup
import Test.Plutip.Internal.BotPlutusInterface.Types (BpiError (BotInterfaceDirMissing, SignKeySaveError), BpiWallet (BpiWallet), TestWallet (twInitDistribiution, twTag), TestWallet' (TestWallet'), WalletTag (BaseTag, PkhTag), payKeys, stakeKeys)
import Test.Plutip.Internal.Types (ClusterEnv, nodeSocket, supportDir)

{-  Add wallet with arbitrary address and specified amount of Ada.
  Each value specified in funds will be sent as separate UTXO.

During wallet addition `.skey` file with required name generated and saved
 to be used by bot interface.
 Directory for files could be obtained with `Test.Plutip.BotPlutusInterface.Setup.keysDir`
-}
eitherAddSomeWallet :: MonadIO m => TestWallet' k -> ReaderT ClusterEnv m (Either BpiError (BpiWallet k))
eitherAddSomeWallet funds = eitherAddSomeWalletDir funds Nothing

-- | The same as `eitherAddSomeWallet`, but also
-- saves the key file to a separate directory.
eitherAddSomeWalletDir :: MonadIO m => TestWallet' k -> Maybe FilePath -> ReaderT ClusterEnv m (Either BpiError (BpiWallet k))
eitherAddSomeWalletDir (TestWallet' funds) wallDir = do
  bpiWallet <- createWallet (twTag funds)
  saveWallets bpiWallet wallDir
    >>= \case
      Right _ -> sendFunds bpiWallet >> pure (Right bpiWallet)
      Left err -> pure $ Left err
  where
    sendFunds wallet = do
      cEnv <- ask
      let fundAddress = mkMainnetAddress wallet
          toAmt = Coin . fromIntegral
      liftIO $
        sendFaucetFundsTo
          nullTracer -- todo: fix tracer to be not `nullTracer`
          (nodeSocket cEnv)
          (supportDir cEnv)
          [(fundAddress, toAmt v) | v <- twInitDistribiution funds]

-- | Add wallet with arbitrary address and specified amount of Ada.
-- (version of `eitherAddSomeWallet` that will throw an error in case of failure)
addSomeWallet :: MonadIO m => TestWallet' k -> ReaderT ClusterEnv m (BpiWallet k)
addSomeWallet funds =
  eitherAddSomeWallet funds >>= either (error . show) pure

-- | Version of `addSomeWallet` that also writes the
-- wallet key file to a separate directory
addSomeWalletDir :: MonadIO m => TestWallet' k -> Maybe FilePath -> ReaderT ClusterEnv m (BpiWallet k)
addSomeWalletDir funds wallDir =
  eitherAddSomeWalletDir funds wallDir >>= either (error . show) pure

createWallet :: MonadIO m => WalletTag t k -> m (BpiWallet k)
createWallet tag = do
  kp <- liftIO genKeyPair
  (skp, k) <- case tag of
    PkhTag k -> pure (Nothing, k)
    BaseTag k -> fmap ((,k) . Just) (liftIO genStakeKeyPair)
  return $ BpiWallet kp skp k

saveWallets :: MonadIO m => BpiWallet k -> Maybe FilePath -> ReaderT ClusterEnv m (Either BpiError ())
saveWallets bpiw fp = do
  cEnv <- ask
  isSet <- liftIO (Setup.directoryIsSet cEnv)
  bool
    (return $ Left BotInterfaceDirMissing)
    ( do
        case fp of
          Nothing -> pure ()
          (Just wdir) -> void $ saveWalletDir bpiw wdir
        saveWalletDir bpiw (Setup.keysDir cEnv)
    )
    isSet

-- | Save the wallet to a specific directory.
saveWalletDir :: MonadIO m => BpiWallet k -> FilePath -> m (Either BpiError ())
saveWalletDir (BpiWallet pay stake _) wallDir = do
  liftIO $ createDirectoryIfMissing True wallDir
  pLogs <- liftIO $ writeKeyPair wallDir pay
  sLogs <- maybe (pure []) (liftIO . writeStakeKeyPairs wallDir) stake

  case listToMaybe $ dropWhile isRight (pLogs ++ sLogs) of
    Nothing -> return $ pure ()
    Just e -> return $ left (SignKeySaveError . show) e

-- | Make `AnyAddress` for mainnet
cardanoMainnetAddress :: BpiWallet k -> AddressAny
cardanoMainnetAddress (BpiWallet pay stake _) =
  CAPI.toAddressAny $
    CAPI.ShelleyAddress
      Shelley.Mainnet
      ((\case (CAPI.PaymentKeyHash kh) -> Shelley.KeyHashObj kh) . CAPI.verificationKeyHash $ vKey pay)
      ( maybe
          Shelley.StakeRefNull
          ((\case (CAPI.StakeKeyHash kh) -> Shelley.StakeRefBase $ Shelley.KeyHashObj kh) . CAPI.verificationKeyHash . sVKey)
          stake
      )

-- | Get `String` representation of address on mainnet
mkMainnetAddress :: BpiWallet k -> String
mkMainnetAddress bw =
  Text.unpack
    . CAPI.serialiseAddress
    $ cardanoMainnetAddress bw

walletPaymentPkh :: BpiWallet k -> PaymentPubKeyHash
walletPaymentPkh = PaymentPubKeyHash . PubKeyHash . toBuiltin . CAPI.serialiseToRawBytes . CAPI.verificationKeyHash . vKey . payKeys

walletStakePkh :: BpiWallet k -> Maybe StakePubKeyHash
walletStakePkh wall = StakePubKeyHash . PubKeyHash . toBuiltin . CAPI.serialiseToRawBytes . CAPI.verificationKeyHash . sVKey <$> stakeKeys wall
