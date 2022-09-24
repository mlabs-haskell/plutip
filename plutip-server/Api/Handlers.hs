module Api.Handlers (
  startClusterHandler,
  stopClusterHandler,
) where

import Cardano.Api (serialiseToCBOR)
import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (RunningNode))
import Control.Concurrent.MVar (isEmptyMVar, putMVar, takeMVar)
import Control.Monad (unless)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, asks)
import Data.ByteString.Base16 qualified as Base16
import Data.Default (def)
import Data.Foldable (for_)
import Data.Text.Encoding qualified as Text
import Data.Traversable (for)
import System.Directory (doesFileExist)
import System.FilePath (replaceFileName)
import Test.Plutip.Config (chainIndexPort, relayNodeLogs)
import Test.Plutip.Internal.BotPlutusInterface.Keys (KeyPair (sKey))
import Test.Plutip.Internal.BotPlutusInterface.Setup (keysDir)
import Test.Plutip.Internal.BotPlutusInterface.Types (TestWallet (TestWallet), testWallet', WalletTag(EnterpriseTag))
import Test.Plutip.Internal.BotPlutusInterface.Wallet (BpiWallet (payKeys), addSomeWallet)
import Test.Plutip.Internal.LocalCluster (startCluster, stopCluster)
import Test.Plutip.Internal.Types (ClusterEnv (runningNode))
import Test.Plutip.LocalCluster (waitSeconds)
import Types (
  AppM,
  ClusterStartupFailureReason (
    ClusterIsRunningAlready,
    NegativeLovelaces,
    NodeConfigNotFound
  ),
  ClusterStartupParameters (
    ClusterStartupParameters,
    keysDirectory,
    nodeConfigPath,
    nodeSocketPath,
    privateKeys
  ),
  Env (status),
  Lovelace (unLovelace),
  PrivateKey,
  ServerOptions (ServerOptions, nodeLogs),
  StartClusterRequest (StartClusterRequest, keysToGenerate),
  StartClusterResponse (
    ClusterStartupFailure,
    ClusterStartupSuccess
  ),
  StopClusterRequest (StopClusterRequest),
  StopClusterResponse (StopClusterFailure, StopClusterSuccess),
 )

startClusterHandler :: ServerOptions -> StartClusterRequest -> AppM StartClusterResponse
startClusterHandler
  ServerOptions {nodeLogs}
  StartClusterRequest {keysToGenerate} = interpret $ do
    -- Check that lovelace amounts are positive
    for_ keysToGenerate $ \lovelaceAmounts -> do
      for_ lovelaceAmounts $ \lovelaces -> do
        unless (unLovelace lovelaces > 0) $ do
          throwError NegativeLovelaces
    statusMVar <- asks status
    isClusterDown <- liftIO $ isEmptyMVar statusMVar
    unless isClusterDown $ throwError ClusterIsRunningAlready
    let cfg = def {relayNodeLogs = nodeLogs, chainIndexPort = Nothing}
    (statusTVar, res@(clusterEnv, _)) <- liftIO $ startCluster cfg setup
    liftIO $ putMVar statusMVar statusTVar
    let nodeConfigPath = getNodeConfigFile clusterEnv
    -- safeguard against directory tree structure changes
    unlessM (liftIO $ doesFileExist nodeConfigPath) $ throwError NodeConfigNotFound
    pure $
      ClusterStartupSuccess $
        ClusterStartupParameters
          { privateKeys = getWalletPrivateKey <$> snd res
          , nodeSocketPath = getNodeSocketFile clusterEnv
          , nodeConfigPath = nodeConfigPath
          , keysDirectory = keysDir clusterEnv
          }
    where
      setup :: ReaderT ClusterEnv IO (ClusterEnv, [BpiWallet Int])
      setup = do
        env <- ask
        wallets <- do
          for (zip [0..] keysToGenerate) $ \(idx, lovelaceAmounts) ->
            addSomeWallet (testWallet' (fromInteger . unLovelace <$> lovelaceAmounts) Nothing (EnterpriseTag idx))
        waitSeconds 2 -- wait for transactions to submit
        pure (env, wallets)
      getNodeSocketFile (runningNode -> RunningNode conn _ _ _) = nodeSocketFile conn
      getNodeConfigFile =
        -- assumption is that node.config lies in the same directory as node.socket
        flip replaceFileName "node.config" . getNodeSocketFile
      getWalletPrivateKey :: BpiWallet k -> PrivateKey
      getWalletPrivateKey = Text.decodeUtf8 . Base16.encode . serialiseToCBOR . sKey . payKeys
      interpret = fmap (either ClusterStartupFailure id) . runExceptT

stopClusterHandler :: StopClusterRequest -> AppM StopClusterResponse
stopClusterHandler StopClusterRequest = do
  statusMVar <- asks status
  isClusterDown <- liftIO $ isEmptyMVar statusMVar
  if isClusterDown
    then pure $ StopClusterFailure "Cluster is not running"
    else do
      statusTVar <- liftIO $ takeMVar statusMVar
      liftIO $ stopCluster statusTVar
      pure $ StopClusterSuccess
