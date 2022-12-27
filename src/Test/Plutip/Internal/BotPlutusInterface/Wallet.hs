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
  )
where

import Cardano.Api (AddressAny, PaymentExtendedKey, PaymentKey, SigningKey (PaymentExtendedSigningKey), VerificationKey, castVerificationKey)
import Cardano.Api qualified as CAPI
import Cardano.BM.Data.Tracer (nullTracer)
-- import Cardano.Wallet.Shelley.Launch.Cluster (
--   sendFaucetFundsTo,
--  )

import Cardano.Crypto.Seed qualified as Crypto
import Cardano.Mnemonic (someMnemonicToBytes)
import Cardano.Mnemonic qualified as CADDR
-- import Cardano.Wallet.Primitive.AddressDerivation.Shared
--   ( SharedKey (getKey),
--     generateKeyFromSeed,
--   )
import Cardano.Wallet.Primitive.Types.Coin (Coin (Coin))
import Control.Arrow (ArrowChoice (left), (>>>))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Aeson.Extras (encodeByteString)
import Data.Bool (bool)
import Data.ByteArray (convert)
import Data.Text (Text)
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
import Cardano.Wallet.Primitive.AddressDerivation.Shelley (generateKeyFromSeed, ShelleyKey (getKey, ShelleyKey))
import Cardano.Address.Derivation (deriveXPrv, DerivationScheme (DerivationScheme2), Index, DerivationType (Hardened, Soft), indexFromWord32, toXPub)
import Data.Maybe
import Cardano.Api.Shelley (VerificationKey(PaymentExtendedVerificationKey))
import Cardano.Address (paymentAddress)

-- | Wallet that can be used by bot interface,
--  backed by `.skey` file when added to cluster with `addSomeWallet`
data BpiWallet = BpiWallet
  { walletPkh :: !PubKeyHash,
    vrfKey :: VerificationKey PaymentKey,
    signKey :: SigningKey PaymentKey
    -- todo: do we need something else?
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
  sKey <- liftIO $ CAPI.generateSigningKey CAPI.AsPaymentKey
  let vKey = CAPI.getVerificationKey sKey
  return $ BpiWallet (toPkh vKey) vKey sKey
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
saveWalletDir (BpiWallet pkh _ sk) wallDir = do
  liftIO $ createDirectoryIfMissing True wallDir
  let pkhStr = Text.unpack (encodeByteString (fromBuiltin (LAPI.getPubKeyHash pkh)))
      path = wallDir </> "signing-key-" ++ pkhStr <.> "skey"
  res <- liftIO $ CAPI.writeFileTextEnvelope path (Just "Payment Signing Key") sk
  return $ left (SignKeySaveError . show) res --todo: better error handling

-- | Make `AnyAddress` for mainnet
cardanoMainnetAddress :: BpiWallet -> AddressAny
cardanoMainnetAddress (BpiWallet _ vk _) =
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

-- 1852H/1815H/0H/0/0

addMnemonicWallet :: MonadIO m => [Positive] -> [Text] -> ReaderT ClusterEnv m (Either BpiError ExtWallet)
addMnemonicWallet funds mnemonic = do
  let someMnem =
        either (error . show) id (CADDR.mkSomeMnemonic @'[12] mnemonic)
      xPrv = getKey $ generateKeyFromSeed (someMnem, Nothing) mempty
      rootExtKey = PaymentExtendedSigningKey xPrv
      hIx = fromJust . indexFromWord32 @(Index 'Hardened _) . fromInteger
      sIx = fromJust . indexFromWord32 @(Index 'Soft _) . fromInteger
      derivePrv ix prv = deriveXPrv DerivationScheme2 prv ix
      deriveChild = -- FIXME: not correct
        derivePrv (hIx 1852)
        >>> derivePrv (hIx 1815)
        >>> derivePrv (hIx 0)
        >>> derivePrv (sIx 0)
        >>> derivePrv (sIx 0)

      addrSKey = PaymentExtendedSigningKey $ deriveChild xPrv
      addrVKey = castVerificationKey $ PaymentExtendedVerificationKey $ toXPub xPrv
    

     

  return . Right $ ExtWallet rootExtKey addrSKey addrVKey

data ExtWallet = ExtWallet
  { extRootKey :: SigningKey PaymentExtendedKey
  , addressSignKey :: SigningKey PaymentExtendedKey
  , addressVerKey ::  VerificationKey PaymentKey
  }

seedFromMnemonic :: [Text] -> Crypto.Seed
seedFromMnemonic mnemonic =
  let someMnem =
        either (error . show) id (CADDR.mkSomeMnemonic @'[12] mnemonic)
   in Crypto.mkSeedFromBytes $ convert $ someMnemonicToBytes someMnem

{-
genShelleyAddresses :: SomeMnemonic -> [Address]
genShelleyAddresses mw =
    let
        (seed, pwd) =
            (mw, Passphrase "encryption"
forall a. Monoid a => a
mempty)
        rootXPrv =
            Shelley.generateKeyFromSeed (seed, Nothing) pwd
        accXPrv =
            deriveAccountPrivateKey pwd rootXPrv minBound
        addrXPrv =
            deriveAddressPrivateKey pwd accXPrv UtxoExternal
    in
        [ paymentAddress @'Mainnet $ publicKey $ addrXPrv ix
        | ix <- [minBound..maxBound]
        ]

-------------

paymentAddress :: NetworkDiscriminant key -> key 'PaymentK XPub -> Address


-}