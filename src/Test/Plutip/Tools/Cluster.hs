module Test.Plutip.Tools.Cluster (
  waitSeconds,
  ada,
  -- awaitAddressFunded,
  awaitWalletFunded,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Numeric.Positive (Positive)
import Plutus.ChainIndex.Api (QueryAtAddressRequest (QueryAtAddressRequest), QueryResponse (QueryResponse))
import Plutus.ChainIndex.Client qualified as ChainIndexClient
import Plutus.V1.Ledger.Credential (Credential (PubKeyCredential))
import Servant.Client (
  mkClientEnv,
  runClientM,
 )
import Test.Plutip.Internal.BotPlutusInterface.Wallet (BpiWallet (walletPkh))
import Test.Plutip.Internal.Types (ClusterEnv (chainIndexUrl))
import Control.Retry (recoverAll, constantDelay, limitRetries)
import UnliftIO (throwString)

-- | Suspend execution for n seconds (via `threadDelay`)
waitSeconds :: Int -> IO ()
waitSeconds = threadDelay . (* 1000000)

type Delay = Int

awaitWalletFunded :: MonadIO m => BpiWallet -> Delay -> ReaderT ClusterEnv m ()
awaitWalletFunded wallet delay = do
  cEnv <- ask
  liftIO $ 
    recoverAll policy (\_ -> callChainIndex cEnv >>= checkResponse)
      where
        policy = constantDelay (delay * 1_000_000) <> limitRetries 60

        callChainIndex cEnv = do
          let client =
                ChainIndexClient.getUnspentTxOutsAtAddress
                  (QueryAtAddressRequest Nothing $ PubKeyCredential (walletPkh wallet))
          mgr <- newManager defaultManagerSettings
          liftIO $
              runClientM client $
                mkClientEnv mgr (chainIndexUrl cEnv)

        checkResponse = \case
          Left e -> throwString $ 
                    "Failed to check if wallet funded via chain-index query: "
                    <> show e
          Right (QueryResponse [] _) ->
            throwString "No UTxOs returned by chain-index after querying wallet address"
          Right _ -> pure ()

-- | Library functions works with amounts in `Lovelace`.
-- This function helps to specify amounts in `Ada` easier.
ada :: Positive -> Positive
ada = (* 1_000_000)
