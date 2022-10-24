module Test.Plutip.Tools.Cluster (
  waitSeconds,
  ada,
  awaitAddressFunded,
  awaitGodDamnedChindexSeesWalletFunded,
) where

import Cardano.Api (UTxO (UTxO))
import Cardano.Api qualified as C
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Data.Map qualified as Map
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
import Test.Plutip.Tools.CardanoApi (utxosAtAddress)

-- | Suspend execution for n seconds (via `threadDelay`)
waitSeconds :: Int -> IO ()
waitSeconds = threadDelay . (* 1000000)

type Delay = Int

awaitAddressFunded :: ClusterEnv -> Delay -> C.AddressAny -> IO ()
awaitAddressFunded cEnv delay addr = do
  utxo <- utxosAtAddress cEnv addr
  unless (utxosReceived utxo) $ do
    waitSeconds delay
    awaitAddressFunded cEnv delay addr
  where
    utxosReceived = \case
      Left _ -> False
      Right (UTxO utxo') -> not $ Map.null utxo'

awaitGodDamnedChindexSeesWalletFunded :: BpiWallet -> Delay -> ReaderT ClusterEnv IO ()
awaitGodDamnedChindexSeesWalletFunded wallet delay = do
  cEnv <- ask
  let chindexUrl = chainIndexUrl cEnv
      credential = PubKeyCredential (walletPkh wallet)
      client =
        ChainIndexClient.getUnspentTxOutsAtAddress
          (QueryAtAddressRequest Nothing credential)
          
  mgr <- liftIO $ newManager defaultManagerSettings
  res <-
    liftIO $
      runClientM client $
        mkClientEnv mgr chindexUrl
  case res of
    Left e ->
      error $
        "Awaiting for wallets finded via chain-index requests failed: "
          ++ show e
    Right utos -> case utos of
      (QueryResponse [] _) -> do
        liftIO $ waitSeconds delay
        awaitGodDamnedChindexSeesWalletFunded wallet delay
      _ -> pure ()

-- | Library functions works with amounts in `Lovelace`.
-- This function helps to specify amounts in `Ada` easier.
ada :: Positive -> Positive
ada = (* 1_000_000)
