module Test.Plutip.Tools.ChainIndex (utxosAtPkh, awaitWalletFunded) where

import Cardano.Pool.Metadata (newManager)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT, ask)
import Control.Retry (constantDelay, limitRetries, recoverAll)
import Data.Default (Default (def), def)
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Ledger (PubKeyHash)
import Network.HTTP.Client (defaultManagerSettings)
import Plutus.ChainIndex (Page (Page))
import Plutus.ChainIndex.Api (UtxoAtAddressRequest (UtxoAtAddressRequest), UtxosResponse (UtxosResponse))
import Plutus.ChainIndex.Client qualified as ChainIndexClient
import Plutus.V1.Ledger.Credential (Credential (PubKeyCredential))
import Servant.Client (
  ClientError,
  mkClientEnv,
  runClientM,
 )
import Test.Plutip.Internal.BotPlutusInterface.Wallet (BpiWallet (walletPkh), mkMainnetAddress)
import Test.Plutip.Internal.Types (ClusterEnv (chainIndexUrl))
import UnliftIO (throwString)

-- | Request UTxOs at a given `PubKeyHash` using `chain-index` REST client.
utxosAtPkh ::
  MonadIO m =>
  PubKeyHash ->
  ReaderT ClusterEnv m (Either ClientError UtxosResponse)
utxosAtPkh pkh = do
  cEnv <- ask
  chIndexUrl <-
    maybe
      ( throwString
          "To perform `utxosAtPkh` request Chain Index must be launched,\
          \ but it seems to be off. \
          \ Check `ChainIndexMode` in `PlutipConfig`"
      )
      pure
      (chainIndexUrl cEnv)
  manager <- newManager defaultManagerSettings
  liftIO $ runClientM client $ mkClientEnv manager chIndexUrl
  where
    client = ChainIndexClient.getUtxoSetAtAddress req
    req = UtxoAtAddressRequest (Just def) (PubKeyCredential pkh)

-- | Waits till specified `BpiWallet` is funded using chain-index query.
-- Performs 120 tries with `retryDelay` seconds between tries.
awaitWalletFunded ::
  (MonadIO m, MonadMask m) =>
  BpiWallet ->
  NominalDiffTime ->
  ReaderT ClusterEnv m ()
awaitWalletFunded wallet retryDelay = do
  recoverAll policy $ \_ -> do
    resp <- utxosAtPkh (walletPkh wallet)
    checkResponse resp
  where
    delay = truncate $ nominalDiffTimeToSeconds retryDelay * 1000000
    retries = 120
    policy = constantDelay delay <> limitRetries retries

    checkResponse = \case
      Left e ->
        throwString $
          "Failed to check if wallet funded via chain-index query: "
            <> show e
      Right (UtxosResponse _ (Page _ _ [])) ->
        throwString $
          "Failed to check if wallet is funded via chain-index query: "
            <> "Plutip performed "
            <> show retries
            <> " chain-index queries with delay of "
            <> show delay
            <> " microseconds between them (based on slot length),"
            <> " but there are no UTxOs in chain-index response"
            <> " for the wallet with address "
            <> mkMainnetAddress wallet
      Right _ -> pure ()
