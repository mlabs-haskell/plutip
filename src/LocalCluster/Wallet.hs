module LocalCluster.Wallet (
  addWallet,
  addWallets,
  cwAddress,
  someWallet,
  mnemonicWallet,
  stringAddress,
  encodeAddressHex,
) where

import Cardano.Mnemonic (Mnemonic, SomeMnemonic (SomeMnemonic), mnemonicToText)
import Cardano.Wallet.Api.Types (
  EncodeAddress (..),
 )
import Cardano.Wallet.Primitive.AddressDerivation (
  NetworkDiscriminant (..),
 )
import Cardano.Wallet.Primitive.Types.Address (
  Address (..),
 )
import Cardano.Wallet.Primitive.Types.Coin (
  Coin (..),
 )
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (RunningNode), sendFaucetFundsTo)
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
import LocalCluster.Types
import Numeric.Natural (Natural)
import Test.Integration.Faucet (genMnemonics, genShelleyAddresses)

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
  (ClusterEnv (RunningNode socketPath _ _) dir _) <- ask
  let mnem' = unsafeMkMnemonic mnem
      wallet = mkWallet mnem'
      fundAddress = stringAddress wallet
      amt' = toEnum . fromEnum $ amt
  liftIO $ sendFaucetFundsTo nullTracer socketPath dir [(fundAddress, Coin amt')]
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
  , cwAddress :: Address
  }
  deriving stock (Show)

mkWallet :: Mnemonic 15 -> ClusterWallet
mkWallet mn =
  CWallet
    mn
    (head $ toAddresses mn) -- TODO: not sure how many addresses we'll need

toAddresses :: Mnemonic 15 -> [Address]
toAddresses = genShelleyAddresses . SomeMnemonic

textAddress :: ClusterWallet -> Text
textAddress = encodeAddress @ 'Mainnet . cwAddress

stringAddress :: ClusterWallet -> String
stringAddress = T.unpack . textAddress
