module Test.Plutip.Internal.BotPlutusInterface.Wallet (
  BpiWallet (..),
  addSomeWallet,
  eitherAddSomeWallet,
  mkMainnetAddress,
  cardanoMainnetAddress,
  ledgerPkh,
  ledgerPaymentPkh,
) where

import Cardano.Api (AddressAny, PaymentKey, SigningKey, VerificationKey)
import Cardano.Api qualified as CAPI
import Cardano.BM.Data.Tracer (nullTracer)
import Cardano.Wallet.Primitive.Types.Coin (Coin (Coin))
import Cardano.Wallet.Shelley.Launch.Cluster (
  sendFaucetFundsTo,
 )
import Control.Arrow (ArrowChoice (left))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Bool (bool)
import Data.Text (Text, pack, unpack)
import GHC.Natural (Natural)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash), PubKey (PubKey), PubKeyHash, pubKeyHash)
import Plutus.V1.Ledger.Api qualified as LAPI
import System.FilePath ((<.>), (</>))
import Test.Plutip.Internal.BotPlutusInterface.Setup qualified as Setup
import Test.Plutip.Internal.BotPlutusInterface.Types (BpiError (BotInterfaceDirMissing, SignKeySaveError))
import Test.Plutip.Internal.Types (ClusterEnv, nodeSocket, supportDir)

{- | Wallet that can be used by bot interface,
  backed by `.skey` file when added to cluster with `addSomeWallet`
-}
data BpiWallet = BpiWallet
  { walletPkh :: !Text -- ? maybe `PubKeyHash` here will be better
  , vrfKey :: VerificationKey PaymentKey
  , signKey :: SigningKey PaymentKey
  -- todo: do we need something else?
  }
  deriving stock (Show)

-- | Add wallet with arbitrary address and specified amount of Ada.

{- During wallet addition `.skey` file with required name generated and saved
 to be used by bot interface.
 Directory for files could be obtained with `Test.Plutip.BotPlutusInterface.Setup.keysDir`
-}
eitherAddSomeWallet :: MonadIO m => Natural -> ReaderT ClusterEnv m (Either BpiError BpiWallet)
eitherAddSomeWallet funds = do
  bpiWallet <- createWallet
  saveWallet bpiWallet
    >>= \case
      Right _ -> sendFunds bpiWallet >> pure (Right bpiWallet)
      Left err -> pure $ Left err
  where
    sendFunds wallet = do
      cEnv <- ask
      let fundAddress = mkMainnetAddress wallet
          amt' = Coin . toEnum . fromEnum $ funds
      liftIO $
        sendFaucetFundsTo
          nullTracer -- todo: fix tracer to be not `nullTracer`
          (nodeSocket cEnv)
          (supportDir cEnv)
          [(fundAddress, amt')]

{- | Add wallet with arbitrary address and specified amount of Ada.
 (version of `eitherAddSomeWallet` that will throw an error in case of failure)
-}
addSomeWallet :: MonadIO m => Natural -> ReaderT ClusterEnv m BpiWallet
addSomeWallet funds =
  eitherAddSomeWallet funds >>= either (error . show) pure

createWallet :: MonadIO m => m BpiWallet
createWallet = do
  sKey <- liftIO $ CAPI.generateSigningKey CAPI.AsPaymentKey
  let vKey = CAPI.getVerificationKey sKey
  return $ BpiWallet (textHash vKey) vKey sKey
  where
    textHash = pack . filter (/= '"') . show . CAPI.verificationKeyHash

saveWallet :: MonadIO m => BpiWallet -> ReaderT ClusterEnv m (Either BpiError ())
saveWallet (BpiWallet pkh _ sk) = do
  cEnv <- ask
  liftIO (Setup.directoryIsSet cEnv)
    >>= bool (return $ Left BotInterfaceDirMissing) (save cEnv sk)
  where
    save cEnv key = do
      let path = Setup.keysDir cEnv </> "signing-key-" ++ unpack pkh <.> "skey"
      res <- liftIO $ CAPI.writeFileTextEnvelope path (Just "Payment Signing Key") key
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
mkMainnetAddress bw =
  unpack
    . CAPI.serialiseAddress
    $ cardanoMainnetAddress bw

ledgerPkh :: BpiWallet -> PubKeyHash
ledgerPkh =
  pubKeyHash
    . PubKey
    . LAPI.fromBytes
    . CAPI.serialiseToRawBytes
    . vrfKey

ledgerPaymentPkh :: BpiWallet -> PaymentPubKeyHash
ledgerPaymentPkh = PaymentPubKeyHash . ledgerPkh
