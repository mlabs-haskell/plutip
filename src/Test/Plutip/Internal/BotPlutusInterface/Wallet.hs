module Test.Plutip.Internal.BotPlutusInterface.Wallet (
  BpiWallet (..),
  addSomeWallet,
  addSomeWalletDir,
  eitherAddSomeWallet,
  eitherAddSomeWalletDir,
  mkMainnetAddress,
  cardanoMainnetAddress,
  ledgerPaymentPkh,
) where

import Cardano.Api (AddressAny)
import Cardano.Api qualified as CAPI
import Cardano.Api.Shelley qualified as CAPI
import Cardano.BM.Data.Tracer (nullTracer)
import Cardano.Wallet.Primitive.Types.Coin (Coin (Coin))
import Cardano.Wallet.Shelley.Launch.Cluster (
  sendFaucetFundsTo,
 )
import Cardano.Ledger.BaseTypes as Shelley ( Network(Mainnet) )
import Cardano.Ledger.Credential qualified as Shelley
import Control.Arrow (ArrowChoice (left))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Bool (bool)
import Data.Either (isRight)
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash), PubKeyHash (PubKeyHash))
import Numeric.Positive (Positive)
import PlutusTx.Builtins (toBuiltin)
import System.Directory (createDirectoryIfMissing)
import Test.Plutip.Internal.BotPlutusInterface.Keys (KeyPair (vKey), StakeKeyPair (sVKey), genKeyPair, genStakeKeyPair, writeKeyPair, writeStakeKeyPairs)
import Test.Plutip.Internal.BotPlutusInterface.Setup qualified as Setup
import Test.Plutip.Internal.BotPlutusInterface.Types (BpiError (BotInterfaceDirMissing, SignKeySaveError))
import Test.Plutip.Internal.Types (ClusterEnv, nodeSocket, supportDir)

-- | Wallet that can be used by bot interface,
--  backed by `.skey` file when added to cluster with `addSomeWallet`
data BpiWallet = BpiWallet
  { walletPkh :: !PubKeyHash
  , payKeys :: KeyPair
  , stakeKeys :: Maybe StakeKeyPair
  }
  deriving stock (Show)

{-  Add wallet with arbitrary address and specified amount of Ada.
  Each value specified in funds will be sent as separate UTXO.

During wallet addition `.skey` file with required name generated and saved
 to be used by bot interface.
 Directory for files could be obtained with `Test.Plutip.BotPlutusInterface.Setup.keysDir`
-}
eitherAddSomeWallet :: MonadIO m => [Positive] -> ReaderT ClusterEnv m (Either BpiError BpiWallet)
eitherAddSomeWallet funds = eitherAddSomeWalletDir funds Nothing

-- | The same as `eitherAddSomeWallet`, but also
-- saves the key file to a separate directory.
eitherAddSomeWalletDir :: MonadIO m => [Positive] -> Maybe FilePath -> ReaderT ClusterEnv m (Either BpiError BpiWallet)
eitherAddSomeWalletDir funds wallDir = do
  bpiWallet <- createWallet
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
          [(fundAddress, toAmt v) | v <- funds]

-- | Add wallet with arbitrary address and specified amount of Ada.
-- (version of `eitherAddSomeWallet` that will throw an error in case of failure)
addSomeWallet :: MonadIO m => [Positive] -> ReaderT ClusterEnv m BpiWallet
addSomeWallet funds =
  eitherAddSomeWallet funds >>= either (error . show) pure

-- | Version of `addSomeWallet` that also writes the
-- wallet key file to a separate directory
addSomeWalletDir :: MonadIO m => [Positive] -> Maybe FilePath -> ReaderT ClusterEnv m BpiWallet
addSomeWalletDir funds wallDir =
  eitherAddSomeWalletDir funds wallDir >>= either (error . show) pure

createWallet :: MonadIO m => m BpiWallet
createWallet = do
  kp <- liftIO genKeyPair
  skp <- liftIO genStakeKeyPair
  return $ BpiWallet (toPkh $ vKey kp) kp (Just skp)
  where
    toPkh =
      PubKeyHash
        . toBuiltin
        . CAPI.serialiseToRawBytes
        . CAPI.verificationKeyHash

saveWallets :: MonadIO m => BpiWallet -> Maybe FilePath -> ReaderT ClusterEnv m (Either BpiError ())
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
saveWalletDir :: MonadIO m => BpiWallet -> FilePath -> m (Either BpiError ())
saveWalletDir (BpiWallet _ pay stake) wallDir = do
  liftIO $ createDirectoryIfMissing True wallDir
  pLogs <- liftIO $ writeKeyPair wallDir pay
  sLogs <- maybe (pure []) (liftIO . writeStakeKeyPairs wallDir) stake

  case listToMaybe $ dropWhile isRight (pLogs ++ sLogs) of
    Nothing -> return $ pure ()
    Just e -> return $ left (SignKeySaveError . show) e

-- | Make `AnyAddress` for mainnet
cardanoMainnetAddress :: BpiWallet -> AddressAny
cardanoMainnetAddress (BpiWallet _ pay stake) =
  CAPI.toAddressAny $
    CAPI.ShelleyAddress
      Shelley.Mainnet
      ((\case (CAPI.PaymentKeyHash kh) -> Shelley.KeyHashObj kh) . CAPI.verificationKeyHash $ vKey pay)
      (maybe
        Shelley.StakeRefNull
          ((\case (CAPI.StakeKeyHash kh) -> Shelley.StakeRefBase $ Shelley.KeyHashObj kh) . CAPI.verificationKeyHash . sVKey)
        stake)
 
-- | Get `String` representation of address on mainnet
mkMainnetAddress :: BpiWallet -> String
mkMainnetAddress bw =
  Text.unpack
    . CAPI.serialiseAddress
    $ cardanoMainnetAddress bw

ledgerPaymentPkh :: BpiWallet -> PaymentPubKeyHash
ledgerPaymentPkh = PaymentPubKeyHash . walletPkh
