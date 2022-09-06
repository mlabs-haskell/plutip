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

import Cardano.Api (AddressAny, FileError, PaymentKey, SigningKey, StakeKey, VerificationKey)
import Cardano.Api qualified as CAPI
import Cardano.BM.Data.Tracer (nullTracer)
import Cardano.Wallet.Primitive.Types.Coin (Coin (Coin))
import Cardano.Wallet.Shelley.Launch.Cluster (
  sendFaucetFundsTo,
 )
import Control.Arrow (ArrowChoice (left))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Aeson.Extras (encodeByteString)
import Data.Bool (bool)
import Data.Text qualified as Text
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash), PubKeyHash (PubKeyHash))
import Numeric.Positive (Positive)
import Plutus.V1.Ledger.Api qualified as LAPI
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import Test.Plutip.Internal.BotPlutusInterface.Keys (KeyPair (sKey, vKey), StakeKeyPair (sSKey, sVKey), genWalletKeys)
import Test.Plutip.Internal.BotPlutusInterface.Setup qualified as Setup
import Test.Plutip.Internal.BotPlutusInterface.Types (BpiError (BotInterfaceDirMissing, SignKeySaveError))
import Test.Plutip.Internal.Types (ClusterEnv, nodeSocket, supportDir)

-- | Wallet that can be used by bot interface,
--  backed by `.skey` file when added to cluster with `addSomeWallet`
data BpiWallet = BpiWallet
  { walletPkh :: !PubKeyHash
  , vrfKey :: VerificationKey PaymentKey
  , signKey :: SigningKey PaymentKey
  , stakeVrfKey :: Maybe (VerificationKey StakeKey)
  , stakeSignKey :: Maybe (SigningKey StakeKey)
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
  (kp, skp) <- liftIO genWalletKeys
  return $ BpiWallet (toPkh $ vKey kp) (vKey kp) (sKey kp) (Just $ sVKey skp) (Just $ sSKey skp)
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
saveWalletDir (BpiWallet pkh _ sk _ ssk) wallDir = do
  liftIO $ createDirectoryIfMissing True wallDir
  let pkhStr = Text.unpack (encodeByteString (fromBuiltin (LAPI.getPubKeyHash pkh)))
      path = wallDir </> "signing-key-" ++ pkhStr <.> "skey"
      skhStr = Text.unpack (encodeByteString (fromBuiltin (LAPI.getPubKeyHash pkh)))
      stakePath = wallDir </> "signing-key-" ++ skhStr <.> "skey"

      foo :: Maybe (SigningKey StakeKey) -> IO (Either (FileError ()) ())
      foo Nothing = pure $ Right ()
      foo (Just stakeSK) = CAPI.writeFileTextEnvelope stakePath (Just "Delegation Signing Key") stakeSK

  res1 <- liftIO $ CAPI.writeFileTextEnvelope path (Just "Payment Signing Key") sk
  res2 <- liftIO $ foo ssk

  case res1 of
    Left _ -> return $ left (SignKeySaveError . show) res1 --todo: better error handling
    Right _ -> return $ left (SignKeySaveError . show) res2

-- | Make `AnyAddress` for mainnet
cardanoMainnetAddress :: BpiWallet -> AddressAny
cardanoMainnetAddress (BpiWallet _ vk _ Nothing _) =
  CAPI.toAddressAny $
    CAPI.makeShelleyAddress
      CAPI.Mainnet
      (CAPI.PaymentCredentialByKey (CAPI.verificationKeyHash vk))
      CAPI.NoStakeAddress
cardanoMainnetAddress (BpiWallet _ vk _ (Just stakeVK) _) =
  CAPI.toAddressAny $
    CAPI.makeShelleyAddress
      CAPI.Mainnet
      (CAPI.PaymentCredentialByKey (CAPI.verificationKeyHash vk))
      (CAPI.StakeAddressByValue (CAPI.StakeCredentialByKey $ CAPI.verificationKeyHash stakeVK)) -- FIXME: Cardano.Api (in cardano-api-1.35.3) does not re-export the constructors for StakeCredential

-- | Get `String` representation of address on mainnet
mkMainnetAddress :: BpiWallet -> String
mkMainnetAddress bw =
  Text.unpack
    . CAPI.serialiseAddress
    $ cardanoMainnetAddress bw

ledgerPaymentPkh :: BpiWallet -> PaymentPubKeyHash
ledgerPaymentPkh = PaymentPubKeyHash . walletPkh
