module Api.Handlers
  ( startClusterHandler
  , stopClusterHandler
  )
where

import Control.Monad.Reader (ReaderT, asks, ask)
import Test.Plutip.LocalCluster (waitSeconds)
import Test.Plutip.Internal.LocalCluster (startCluster, stopCluster)
import Types
  ( StartClusterRequest(StartClusterRequest, keysToGenerate)
  , StopClusterRequest(StopClusterRequest)
  , StopClusterResponse(StopClusterSuccess, StopClusterFailure)
  , AppM
  , PrivateKey
  , StartClusterResponse
    ( ClusterStartupSuccess
    , keysDirectory
    , nodeSocketPath
    , privateKeys
    , nodeConfigPath
    , ClusterStartupFailure
    )
  , ServerOptions(ServerOptions, nodeLogs)
  , Env(status)
  , Lovelace(unLovelace)
  , ClusterStartupFailureReason(NodeConfigNotFound, ClusterIsRunningAlready)
  )
import Cardano.Api (serialiseToCBOR)
import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode(RunningNode))
import Control.Concurrent.MVar (takeMVar, isEmptyMVar, putMVar)
import Control.Monad (unless)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Base16 qualified as Base16
import Data.Default (def)
import Data.Functor ((<&>))
import Data.Text.Encoding qualified as Text
import Data.Traversable (for)
import System.Directory (doesFileExist)
import System.FilePath (replaceFileName)
import Test.Plutip.Config (relayNodeLogs, chainIndexPort)
import Test.Plutip.Internal.BotPlutusInterface.Setup (keysDir)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (BpiWallet(signKey))
import Test.Plutip.Internal.BotPlutusInterface.Wallet (addSomeWallet)
import Test.Plutip.Internal.Types (ClusterEnv(runningNode))

startClusterHandler :: ServerOptions -> StartClusterRequest -> AppM StartClusterResponse
startClusterHandler
  ServerOptions { nodeLogs }
  StartClusterRequest { keysToGenerate } = interpret $ do
  statusMVar <- asks status
  isClusterDown <- liftIO $ isEmptyMVar statusMVar
  unless isClusterDown $ throwError ClusterIsRunningAlready
  let
    cfg = def
      { relayNodeLogs = nodeLogs
      , chainIndexPort = Nothing
      }
  (statusTVar, res@(clusterEnv, _)) <- liftIO $ startCluster cfg setup
  liftIO $ putMVar statusMVar statusTVar
  let nodeConfigPath = getNodeConfigFile clusterEnv
  -- safeguard against directory tree structure changes
  unlessM (liftIO $ doesFileExist nodeConfigPath) $ throwError NodeConfigNotFound
  pure $ ClusterStartupSuccess
    { privateKeys = getKeys res
    , nodeSocketPath = getNodeSocketFile clusterEnv
    , nodeConfigPath = nodeConfigPath
    , keysDirectory = keysDir clusterEnv
    }
  where
    setup :: ReaderT ClusterEnv IO (ClusterEnv, [(Int, BpiWallet)])
    setup = do
      env <- ask
      wallets <- do
        for keysToGenerate $ traverse $ \lovelaceAmounts -> do
          addSomeWallet (fromInteger . unLovelace <$> lovelaceAmounts)
      waitSeconds 2 -- wait for transactions to submit
      pure (env, wallets)
    getNodeSocketFile (runningNode -> RunningNode conn _ _) = nodeSocketFile conn
    getNodeConfigFile =
      -- assumption is that node.config lies in the same directory as node.socket
      flip replaceFileName "node.config" . getNodeSocketFile
    getKeys :: (ClusterEnv, [(Int, BpiWallet)]) -> [(Int, PrivateKey)]
    getKeys (_, kvs) = kvs <&> \(idx, wallet) ->
      ( idx
      , Text.decodeUtf8 . Base16.encode $ serialiseToCBOR (signKey wallet)
      )
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
