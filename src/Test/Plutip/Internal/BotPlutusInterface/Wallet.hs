module Test.Plutip.Internal.BotPlutusInterface.Wallet
  ( BpiWallet (..),
    addSomeWallet,
    addSomeWalletDir,
    eitherAddSomeWallet,
    eitherAddSomeWalletDir,
    mkMainnetAddress,
    cardanoMainnetAddress,
    ledgerPaymentPkh,
    showAddress,
    addSlip14Wallet,
    paymentPkh,
  )
where

-- import Cardano.Wallet.Shelley.Launch.Cluster (
--   sendFaucetFundsTo,
--  )

-- import Cardano.Wallet.Primitive.AddressDerivation.Shared
--   ( SharedKey (getKey),
--     generateKeyFromSeed,
--   )

import Cardano.Api (AddressAny, PaymentExtendedKey, PaymentKey, SigningKey (PaymentExtendedSigningKey), VerificationKey, castVerificationKey)
import Cardano.Api qualified as CAPI
import Cardano.Api.Shelley (VerificationKey (PaymentExtendedVerificationKey))
import Cardano.BM.Data.Tracer (nullTracer)
import Cardano.Mnemonic (SomeMnemonic)
import Cardano.Mnemonic qualified as CAddr
import Cardano.Wallet.Primitive.AddressDerivation (HardDerivation (deriveAccountPrivateKey, deriveAddressPrivateKey), Role (UtxoExternal), publicKey)
import Cardano.Wallet.Primitive.AddressDerivation.Shelley (ShelleyKey (getKey), generateKeyFromSeed)
import Cardano.Wallet.Primitive.Types.Coin (Coin (Coin))
import Control.Arrow (ArrowChoice (left))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Aeson.Extras (encodeByteString)
import Data.Bool (bool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text qualified as Text
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash), PubKeyHash (PubKeyHash))
import Numeric.Positive (Positive)
import Plutus.V1.Ledger.Api qualified as LAPI
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import Test.Plutip.Internal.BotPlutusInterface.Setup qualified as Setup
import Test.Plutip.Internal.BotPlutusInterface.Types (BpiError (BotInterfaceDirMissing, SignKeySaveError))
import Test.Plutip.Internal.Cluster
  ( sendFaucetFundsTo,
  )
import Test.Plutip.Internal.Types (ClusterEnv, nodeSocket, supportDir)

-- | Wallet that can be used by bot interface,
--  backed by `.skey` file when added to cluster with `addSomeWallet`
data BpiWallet = BpiWallet
  { walletPkh :: !PubKeyHash,
    vrfKey :: VerificationKey PaymentKey,
    signKey :: SigningKey PaymentExtendedKey,
    rootExtendedKey :: SigningKey PaymentExtendedKey
    -- todo: do we need something else?
  }
  deriving stock (Show)

paymentPkh :: BpiWallet -> PaymentPubKeyHash
paymentPkh = PaymentPubKeyHash . walletPkh

{-  Add wallet with arbitrary address and specified amount of Ada.
  Each value specified in funds will be sent as separate UTXO.

During wallet addition `.skey` file with required name generated and saved
 to be used by bot interface.
 Directory for files could be obtained with `Test.Plutip.BotPlutusInterface.Setup.keysDir`
-}
eitherAddSomeWallet ::
  MonadIO m =>
  SomeMnemonic ->
  [Positive] ->
  ReaderT ClusterEnv m (Either BpiError BpiWallet)
eitherAddSomeWallet mnemonic funds =
  eitherAddSomeWalletDir mnemonic funds Nothing

-- | The same as `eitherAddSomeWallet`, but also
-- saves the key file to a separate directory.
eitherAddSomeWalletDir ::
  MonadIO m =>
  SomeMnemonic ->
  [Positive] ->
  Maybe FilePath ->
  ReaderT ClusterEnv m (Either BpiError BpiWallet)
eitherAddSomeWalletDir mnemonic funds wallDir = do
  bpiWallet <- createWallet mnemonic
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
addSomeWallet funds = do
  mn <- getRandomMnemonic
  eitherAddSomeWallet mn funds >>= either (error . show) pure

slip14Mnemonic :: Text
slip14Mnemonic = "all all all all all all all all all all all all"

addSlip14Wallet :: MonadIO m => [Positive] -> ReaderT ClusterEnv m BpiWallet
addSlip14Wallet funds = do
  result <- eitherAddSomeWallet slipMnem funds
  pure $ getOrThrow result
  where
    slipMnem =
      getOrThrow $
        CAddr.mkSomeMnemonic @'[12] (T.splitOn " " slip14Mnemonic)

    getOrThrow :: Show e => Either e a -> a
    getOrThrow = either (error . show) id

-- | Version of `addSomeWallet` that also writes the
-- wallet key file to a separate directory
addSomeWalletDir :: MonadIO m => [Positive] -> Maybe FilePath -> ReaderT ClusterEnv m BpiWallet
addSomeWalletDir funds wallDir = do
  mn <- getRandomMnemonic
  eitherAddSomeWalletDir mn funds wallDir >>= either (error . show) pure

createWallet :: MonadIO m => SomeMnemonic -> m BpiWallet
createWallet mnemonic = do
  let pwd = mempty

  let rootExtKey = generateKeyFromSeed (mnemonic, Nothing) pwd
      accZeroXPrv = deriveAccountPrivateKey pwd rootExtKey minBound
      addrZeroXPrv = deriveAddressPrivateKey pwd accZeroXPrv UtxoExternal minBound
      addrZeroXPub = publicKey addrZeroXPrv

  let vrfKey = castVerificationKey $ PaymentExtendedVerificationKey $ getKey addrZeroXPub

  return $
    BpiWallet
      { walletPkh = toPkh vrfKey,
        vrfKey = vrfKey,
        signKey = PaymentExtendedSigningKey $ getKey addrZeroXPrv,
        rootExtendedKey = PaymentExtendedSigningKey $ getKey rootExtKey
      }
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
saveWalletDir (BpiWallet pkh vk sk rootExtSk) wallDir = do
  liftIO $ createDirectoryIfMissing True wallDir
  let pkhStr = Text.unpack (encodeByteString (fromBuiltin (LAPI.getPubKeyHash pkh)))
      pathVk = wallDir </> "verification-key-" ++ pkhStr <.> "vkey"
      pathSk = wallDir </> "signing-key-" ++ pkhStr <.> "skey"
      pathRootSk = wallDir </> "root-extended-key-" ++ pkhStr <.> "skey"
  resVk <- liftIO $ CAPI.writeFileTextEnvelope pathVk (Just "Payment Verification Key") vk
  resSk <- liftIO $ CAPI.writeFileTextEnvelope pathSk (Just "Payment Signing Key") sk
  resRootSk <- liftIO $ CAPI.writeFileTextEnvelope pathRootSk (Just "Root Extended Key") rootExtSk
  return $ left (SignKeySaveError . show) (resVk >> resSk >> resRootSk) --todo: better error handling

-- | Make `AnyAddress` for mainnet
cardanoMainnetAddress :: BpiWallet -> AddressAny
cardanoMainnetAddress (BpiWallet _ vk _ _) =
  CAPI.toAddressAny $
    CAPI.makeShelleyAddress
      CAPI.Mainnet
      (CAPI.PaymentCredentialByKey (CAPI.verificationKeyHash vk))
      CAPI.NoStakeAddress

-- | Get `String` representation of address on mainnet
mkMainnetAddress :: BpiWallet -> String
mkMainnetAddress =
  showAddress . cardanoMainnetAddress

showAddress :: AddressAny -> String
showAddress = Text.unpack . CAPI.serialiseAddress

ledgerPaymentPkh :: BpiWallet -> PaymentPubKeyHash
ledgerPaymentPkh = PaymentPubKeyHash . walletPkh

getRandomMnemonic :: MonadIO m => m SomeMnemonic
getRandomMnemonic = liftIO $ do
  ent <- CAddr.genEntropy @256
  let mn = CAddr.entropyToMnemonic ent
  return $ CAddr.SomeMnemonic mn

-- 1852H/1815H/0H/0/0