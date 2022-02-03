{- | First variant of wallet. Not used at the moment.
 Decided to move forward with bot interface for now, but maybe one day code here will be useful too.
-}
module LocalCluster.Wallet (
  ClusterWallet,
  addWallet,
  addWallets,
  encodeAddressHex,
  getRootXPrv,
  mainnetStringAddress,
  mainnetTextAddress,
  mnemonicWallet,
  cwPaymentAddress,
  paymentPubKey,
  paymentPubKeyHash,
  someWallet,
) where

import Cardano.Address.Derivation (XPrv, XPub)
import Cardano.Mnemonic (Mnemonic, SomeMnemonic (SomeMnemonic), mnemonicToText)
import Cardano.Wallet.Api.Types (
  EncodeAddress (..),
 )
import Cardano.Wallet.Primitive.AddressDerivation (
  HardDerivation (deriveAccountPrivateKey, deriveAddressPrivateKey),
  NetworkDiscriminant (..),
  PaymentAddress (paymentAddress),
  Role (UtxoExternal),
  WalletKey (publicKey),
 )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley qualified as Shelley
import Cardano.Wallet.Primitive.Types.Address (
  Address (..),
 )
import Cardano.Wallet.Primitive.Types.Coin (
  Coin (..),
 )
import Cardano.Wallet.Shelley.Launch.Cluster (sendFaucetFundsTo)
import Cardano.Wallet.Unsafe (unsafeMkMnemonic)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Control.Tracer (
  nullTracer,
 )
import Data.ByteArray.Encoding (
  Base (..),
  convertToBase,
 )
import Data.Text (
  Text,
 )
import Data.Text qualified as T
import Data.Text.Encoding as T
import Ledger qualified as LC
import LocalCluster.Types
import Numeric.Natural (Natural)
import Test.Integration.Faucet (genMnemonics)

{- | Add single wallet using cluster environment.
 (adding wallet atm means sending specified funds to `Address`)
-}
addWallet :: ClusterEnv -> ReaderT ClusterEnv m a -> m a
addWallet = flip runReaderT

{- | Add several wallets using cluster environment.
 (adding wallet atm means sending specified funds to `Address`)
-}
addWallets :: Monad m => ClusterEnv -> [ReaderT ClusterEnv m a] -> m [a]
addWallets cEnv = addWallet cEnv . sequence

{- | Action, that will build `Address` from specified `Mnemonic`
 and send specified amount of `Lovelace` to that address
-}
mnemonicWallet :: MonadIO m => [Text] -> Natural -> ReaderT ClusterEnv m ClusterWallet
mnemonicWallet mnem amt = do
  cEnv <- ask
  let mnem' = unsafeMkMnemonic mnem
      wallet = mkWallet mnem'
      fundAddress = mainnetStringAddress wallet
      amt' = toEnum . fromEnum $ amt
  liftIO $
    sendFaucetFundsTo
      nullTracer
      (nodeSocket cEnv)
      (supportDir cEnv)
      [(fundAddress, Coin amt')]
  return wallet

{- | Action, that will build `Address` from randomly generated `Mnemonic`
 and send specified amount of `Lovelace` to that address
-}
someWallet :: (MonadIO m, MonadFail m) => Natural -> ReaderT ClusterEnv m ClusterWallet
someWallet amt = do
  [mnem] <- liftIO (genMnemonics 1 :: IO [Mnemonic 15])
  mnemonicWallet (mnemonicToText mnem) amt

encodeAddressHex :: Address -> Text
encodeAddressHex = T.decodeUtf8 . convertToBase Base16 . unAddress

{- | Info of wallet, that could added to network
 under development
-}
data ClusterWallet = CWallet
  { cwMnemonic :: Mnemonic 15
  , cwPaymentAddress :: Address
  }
  deriving stock (Show)

mkWallet :: Mnemonic 15 -> ClusterWallet
mkWallet mn =
  CWallet
    mn
    -- (head $ toAddresses mn) -- TODO: not sure how many addresses we'll need
    (paymentAddress @ 'Mainnet pk)
  where
    pk = Shelley.ShelleyKey $ toPaymentXPub $ fromMnemonic mn

-- toAddresses :: Mnemonic 15 -> [Address]
-- toAddresses = genShelleyAddresses . SomeMnemonic

mainnetTextAddress :: ClusterWallet -> Text
mainnetTextAddress = encodeAddress @ 'Mainnet . cwPaymentAddress

mainnetStringAddress :: ClusterWallet -> String
mainnetStringAddress = T.unpack . mainnetTextAddress

-- TODO: maybe better include rootXPrv to wallet during creation

getRootXPrv :: ClusterWallet -> XPrv
getRootXPrv (CWallet mnem _) = fromMnemonic mnem

-- there is also `plutus-ledger.Ledger.generateFromSeed :: ByteString -> Passphrase -> XPrv`
-- maybe less code
fromMnemonic :: Mnemonic 15 -> XPrv
fromMnemonic mnem =
  Shelley.getKey $
    Shelley.generateKeyFromSeed -- ! atm uses "unsafeGenerateKeyFromSeed" version
      (SomeMnemonic mnem, Nothing)
      mempty

toPaymentXPub :: XPrv -> XPub
toPaymentXPub rootXPrv =
  let accXPrv = deriveAccountPrivateKey mempty (Shelley.ShelleyKey rootXPrv) minBound
      addrXPrv = deriveAddressPrivateKey mempty accXPrv UtxoExternal
   in Shelley.getKey $ publicKey $ addrXPrv minBound

paymentPubKey :: ClusterWallet -> LC.PubKey
paymentPubKey = LC.xPubToPublicKey . toPaymentXPub . getRootXPrv

paymentPubKeyHash :: ClusterWallet -> LC.PubKeyHash
paymentPubKeyHash = LC.pubKeyHash . paymentPubKey
