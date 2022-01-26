module LocalCluster.Wallet
  ( addWallet,
    addWallets,
    cwAddress,
    someWallet,
    mnemonicWallet,
    stringAddress,
    addrToString,
  )
where

import Cardano.Mnemonic (Mnemonic, SomeMnemonic (SomeMnemonic), mnemonicToText)
import Cardano.Wallet.Api.Types
  ( EncodeAddress (..),
  )
import Cardano.Wallet.Primitive.AddressDerivation
  ( NetworkDiscriminant (..),
  )
import Cardano.Wallet.Primitive.Types.Address
  ( Address (..),
  )
import Cardano.Wallet.Primitive.Types.Coin
  ( Coin (..),
  )
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (RunningNode), sendFaucetFundsTo)
import Cardano.Wallet.Unsafe (unsafeMkMnemonic)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Control.Tracer
  ( nullTracer,
  )
import Data.ByteArray.Encoding
  ( Base (..),
    convertToBase,
  )
import Data.Text
  ( Text,
  )
import Data.Text qualified as T
import Data.Text.Encoding as T
import LocalCluster.Types
import Numeric.Natural (Natural)
import Test.Integration.Faucet (genMnemonics, genShelleyAddresses)
import Prelude

addWallet :: ClusterEnv -> ReaderT ClusterEnv m a -> m a
addWallet = flip runReaderT

addWallets :: Monad m => ClusterEnv -> [ReaderT ClusterEnv m a] -> m [a]
addWallets cEnv = addWallet cEnv . sequence

mnemonicWallet :: MonadIO m => [Text] -> Int -> ReaderT ClusterEnv m ClusterWallet
mnemonicWallet mnem amt = do
  (ClusterEnv (RunningNode socketPath _ _) dir _) <- ask
  let mnem' = unsafeMkMnemonic mnem
      wallet = mkWallet mnem'
      fundAddress = stringAddress wallet

  liftIO $ sendFaucetFundsTo nullTracer socketPath dir [(fundAddress, Coin $ toEnum amt)]
  return wallet

someWallet :: (MonadIO m, MonadFail m) => Int -> ReaderT ClusterEnv m ClusterWallet
someWallet amt = do
  [mnem] <- liftIO (genMnemonics 1 :: IO [Mnemonic 15])
  mnemonicWallet (mnemonicToText mnem) amt

encodeAddressHex :: Address -> Text
encodeAddressHex = T.decodeUtf8 . convertToBase Base16 . unAddress

-- todo: probably, more data will be required
data ClusterWallet = CWallet
  { cwMnemonic :: Mnemonic 15,
    cwAddress :: Address
  }
  deriving stock (Show)

mkWallet :: Mnemonic 15 -> ClusterWallet
mkWallet mn =
  CWallet
    mn
    (head $ toAddresses mn) -- TODO: not sure how many addresses we'll need

toAddresses :: Mnemonic 15 -> [Address]
toAddresses = genShelleyAddresses . SomeMnemonic

addrToString :: Address -> String
addrToString = T.unpack . encodeAddress @'Mainnet

textAddress :: ClusterWallet -> Text
textAddress = encodeAddress @'Mainnet . cwAddress

stringAddress :: ClusterWallet -> String
stringAddress = T.unpack . textAddress
