module BotInterface.Wallet (
  BpiWallet,
  usingEnv,
  addSomeWallet,
  mkMainnetAddress,
  cardanoMainnetAddress,
) where

import BotInterface.Setup qualified as Setup
import BotInterface.Types
import Cardano.Api (AddressAny, AsType (AsPaymentKey), Key (VerificationKey, getVerificationKey, verificationKeyHash), NetworkId (Mainnet), PaymentCredential (PaymentCredentialByKey), PaymentKey, SerialiseAddress (serialiseAddress), SigningKey, StakeAddressReference (NoStakeAddress), generateSigningKey, makeShelleyAddress, toAddressAny, writeFileTextEnvelope)
import Cardano.BM.Data.Tracer (nullTracer)
import Cardano.Wallet.Primitive.Types.Coin (Coin (Coin))
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (RunningNode), sendFaucetFundsTo)
import Control.Arrow (ArrowChoice (left))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Bool (bool)
import Data.Text (Text, pack, unpack)
import GHC.Natural (Natural)
import LocalCluster.Types
import System.FilePath ((<.>), (</>))

{- | Wallet that can be used by bot interface,
  backed by `.skey` file when added to cluster with `addSomeWallet`
-}
data BpiWallet = BpiWallet
  { walletPkh :: Text
  , vrfKey :: VerificationKey PaymentKey
  , signKey :: SigningKey PaymentKey
  -- todo: do we need something else?
  }
  deriving stock (Show)

-- | Add bot interface compatible wallet using local cluster environment
usingEnv :: ClusterEnv -> ReaderT ClusterEnv m a -> m a
usingEnv = flip runReaderT

{- | Add wallet with arbitrary address.
 During wallet addition `.skey` file with required name generated and saved
 to be used by bot interface.
 Directory for files could be obtained with `BotInterface.Setup.keysDir`
-}
addSomeWallet :: MonadIO m => Natural -> ReaderT ClusterEnv m (Either BpiError BpiWallet)
addSomeWallet funds = do
  bpiWallet <- createWallet
  saveWallet bpiWallet
    >>= \case
      Right _ -> sendFunds bpiWallet >> pure (Right bpiWallet)
      Left err -> pure $ Left err
  where
    sendFunds wallet = do
      (ClusterEnv (RunningNode sockPath _ _) dir _) <- ask
      let fundAddress = mkMainnetAddress wallet
          amt' = Coin . toEnum . fromEnum $ funds
      liftIO $
        sendFaucetFundsTo
          nullTracer -- todo: fix tracer to be not `nullTracer`
          sockPath
          dir
          [(fundAddress, amt')]

createWallet :: MonadIO m => m BpiWallet
createWallet = do
  sKey <- liftIO $ generateSigningKey AsPaymentKey
  let vKey = getVerificationKey sKey
  return $ BpiWallet (textHash vKey) vKey sKey
  where
    textHash = pack . filter (/= '"') . show . verificationKeyHash

saveWallet :: MonadIO m => BpiWallet -> ReaderT ClusterEnv m (Either BpiError ())
saveWallet (BpiWallet pkh _ sk) = do
  cEnv <- ask
  liftIO (Setup.directoryIsSet cEnv)
    >>= bool (return $ Left BotInterfaceDirMissing) (save cEnv sk)
  where
    save cEnv key = do
      let path = Setup.keysDir cEnv </> "signing-key-" ++ unpack pkh <.> "skey"
      res <- liftIO $ writeFileTextEnvelope path (Just "Payment Signing Key") key
      return $ left (SignKeySaveError . show) res --todo: better error handling

-- | Make `AnyAddress` for mainnet
cardanoMainnetAddress :: BpiWallet -> AddressAny
cardanoMainnetAddress (BpiWallet _ vk _) =
  toAddressAny $
    makeShelleyAddress
      Mainnet
      (PaymentCredentialByKey (verificationKeyHash vk))
      NoStakeAddress

-- | Get `String` representation of address on mainnet
mkMainnetAddress :: BpiWallet -> String
mkMainnetAddress bw =
  unpack
    . serialiseAddress
    $ cardanoMainnetAddress bw
