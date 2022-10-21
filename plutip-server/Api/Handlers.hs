module Api.Handlers (
  startClusterHandler,
  stopClusterHandler,
) where

import Cardano.Api (serialiseToCBOR)
import Cardano.Launcher.Node (nodeSocketFile)

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
import Test.Plutip.Config (
  PlutipConfig (extraConfig),
  chainIndexPort,
  relayNodeLogs,
 )
import Test.Plutip.Tools.Cluster (awaitAddressFunded)
import Types (
  AddressType (Base, Enterprise),
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
  Key (addressType, funds),
  Lovelace (unLovelace),
  PrivateKey,
  ServerOptions (ServerOptions, nodeLogs),
  StartClusterRequest (
    StartClusterRequest,
    epochSize,
    keysToGenerate,
    slotLength
  ),
  StartClusterResponse (
    ClusterStartupFailure,
    ClusterStartupSuccess
  ),
  StopClusterRequest (StopClusterRequest),
  StopClusterResponse (StopClusterFailure, StopClusterSuccess),
 )

import Data.Text qualified as T
import Test.Plutip.Internal.BotPlutusInterface.Keys (sKey)
import Test.Plutip.Internal.BotPlutusInterface.Setup (keysDir)
import Test.Plutip.Internal.BotPlutusInterface.Types (
  BpiWallet (payKeys),
  WalletTag (BaseTag, EntTag),
 )
import Test.Plutip.Internal.BotPlutusInterface.Wallet (
  addSomeWallet,
  cardanoMainnetAddress,
 )
import Test.Plutip.Internal.Cluster.Extra.Types (
  ExtraConfig (ExtraConfig),
 )
import Test.Plutip.Internal.LocalCluster (
  startCluster,
  stopCluster,
 )
import Test.Plutip.Internal.Types (ClusterEnv (runningNode))
import Test.Plutip.LocalCluster (RunningNode (RunningNode))

startClusterHandler :: ServerOptions -> StartClusterRequest -> AppM StartClusterResponse
startClusterHandler
  ServerOptions {nodeLogs}
  StartClusterRequest {slotLength, epochSize, keysToGenerate} = interpret $ do
    -- Check that lovelace amounts are positive
    for_ keysToGenerate $ \key -> do
      for_ (funds key) $ \lovelaces -> do
        unless (unLovelace lovelaces > 0) $ do
          throwError NegativeLovelaces
    statusMVar <- asks status
    isClusterDown <- liftIO $ isEmptyMVar statusMVar
    unless isClusterDown $ throwError ClusterIsRunningAlready
    let extraConf = ExtraConfig slotLength epochSize
        cfg = def {relayNodeLogs = nodeLogs, chainIndexPort = Nothing, extraConfig = extraConf}

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
      setup :: ReaderT ClusterEnv IO (ClusterEnv, [BpiWallet])
      setup = do
        env <- ask
        let tags = T.pack . show <$> [0 ..]
        wallets <- do
          for (zip tags keysToGenerate) $ \(idx, key) -> addWallet key idx
        liftIO $ putStrLn "Waiting for wallets to be funded..."
        awaitFunds wallets 2
        pure (env, wallets)
      getNodeSocketFile (runningNode -> RunningNode conn _ _ _) = nodeSocketFile conn
      getNodeConfigFile =
        -- assumption is that node.config lies in the same directory as node.socket
        flip replaceFileName "node.config" . getNodeSocketFile
      getWalletPrivateKey :: BpiWallet -> PrivateKey
      getWalletPrivateKey =
        Text.decodeUtf8
          . Base16.encode
          . serialiseToCBOR
          . sKey
          . payKeys

      interpret = fmap (either ClusterStartupFailure id) . runExceptT
      addWallet key tag =
        let funds' = (fromInteger . unLovelace <$> funds key)
         in case addressType key of
              Base -> addSomeWallet (BaseTag tag) funds'
              Enterprise -> addSomeWallet (EntTag tag) funds'

      -- waits for the last wallet to be funded
      awaitFunds :: [BpiWallet] -> Int -> ReaderT ClusterEnv IO ()
      awaitFunds ws delay = do
        env <- ask
        let lastWallet = last ws
        liftIO $ do
          putStrLn "Waiting till all wallets will be funded..."
          awaitAddressFunded env delay (cardanoMainnetAddress lastWallet)

stopClusterHandler :: StopClusterRequest -> AppM StopClusterResponse
stopClusterHandler StopClusterRequest = do
  statusMVar <- asks status
  isClusterDown <- liftIO $ isEmptyMVar statusMVar
  if isClusterDown
    then pure $ StopClusterFailure "Cluster is not running"
    else do
      statusTVar <- liftIO $ takeMVar statusMVar
      liftIO $ stopCluster statusTVar
      pure StopClusterSuccess
