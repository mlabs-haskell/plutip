module Api.Handlers (
  startClusterHandler,
  stopClusterHandler,
) where

import Cardano.Api (serialiseToCBOR)
import Cardano.Launcher.Node (nodeSocketFile)
import Test.Plutip.Tools.CardanoApi qualified as Tools

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (isEmptyMVar, putMVar, tryTakeMVar)
import Control.Monad (unless)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, asks)
import Data.ByteString.Base16 qualified as Base16
import Data.Default (def)
import Data.Either (fromRight)
import Data.Foldable (for_)
import Data.List.Extra (firstJust)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as Text
import Data.Traversable (for)
import System.Directory (doesFileExist)
import System.FilePath (replaceFileName)
import Test.Plutip.Config (
  ChainIndexMode (NotNeeded),
  PlutipConfig (extraConfig),
  chainIndexMode,
  relayNodeLogs,
 )
import Test.Plutip.Internal.BotPlutusInterface.Setup (keysDir)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (
  BpiWallet (signKey),
  addSomeWallet,
  cardanoMainnetAddress,
 )
import Test.Plutip.Internal.Cluster (RunningNode (RunningNode))
import Test.Plutip.Internal.Cluster.Extra.Types (
  ExtraConfig (
    ExtraConfig,
    ecEpochSize,
    ecIncreasedExUnits,
    ecMaxTxSize,
    ecSlotLength
  ),
 )
import Test.Plutip.Internal.LocalCluster (startCluster, stopCluster)
import Test.Plutip.Internal.Types (ClusterEnv (plutipConf, runningNode))
import Test.Plutip.Tools.CardanoApi (
  AwaitWalletFundedError (AwaitingCapiError, AwaitingTimeoutError),
  awaitWalletFunded,
 )
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
  StartClusterRequest (
    StartClusterRequest,
    epochSize,
    increasedExUnits,
    keysToGenerate,
    maxTxSize,
    noCollateral,
    slotLength
  ),
  StartClusterResponse (
    ClusterStartupFailure,
    ClusterStartupSuccess
  ),
  StopClusterRequest (StopClusterRequest),
  StopClusterResponse (StopClusterFailure, StopClusterSuccess),
 )
import UnliftIO.Exception (throwString)

startClusterHandler :: ServerOptions -> StartClusterRequest -> AppM StartClusterResponse
startClusterHandler
  ServerOptions {nodeLogs}
  StartClusterRequest
    { keysToGenerate
    , slotLength
    , epochSize
    , maxTxSize
    , increasedExUnits
    , noCollateral
    } = interpret $ do
    -- Check that lovelace amounts are positive
    for_ keysToGenerate $ \lovelaceAmounts -> do
      for_ lovelaceAmounts $ \lovelaces -> do
        unless (unLovelace lovelaces > 0) $ do
          throwError NegativeLovelaces
    statusMVar <- asks status
    isClusterDown <- liftIO $ isEmptyMVar statusMVar
    unless isClusterDown $ throwError ClusterIsRunningAlready
    let cfg = def {relayNodeLogs = nodeLogs, chainIndexMode = NotNeeded, extraConfig = extraConf}

    (statusTVar, (clusterEnv, wallets)) <- liftIO $ startCluster cfg setup
    liftIO $ putMVar statusMVar statusTVar
    res <- liftIO $ race (threadDelay 2_000_000) $ waitForFundingTxs clusterEnv wallets extraConf
    -- throw Exception for cardano-cli errors.
    -- Ignore wait timeout error - return from this handler doesn't guarantee funded wallets immedietely.
    maybe (return ()) throwString $ fromRight Nothing res
    let nodeConfigPath = getNodeConfigFile clusterEnv
    -- safeguard against directory tree structure changes
    unlessM (liftIO $ doesFileExist nodeConfigPath) $ throwError NodeConfigNotFound
    pure $
      ClusterStartupSuccess $
        ClusterStartupParameters
          { privateKeys = getWalletPrivateKey <$> wallets
          , nodeSocketPath = getNodeSocketFile clusterEnv
          , nodeConfigPath = nodeConfigPath
          , keysDirectory = keysDir clusterEnv
          }
    where
      setup :: ReaderT ClusterEnv IO (ClusterEnv, [BpiWallet])
      setup = do
        env <- ask
        wallets <- do
          for keysToGenerate $ \lovelaceAmounts -> do
            addSomeWallet (fromInteger . unLovelace <$> lovelaceAmounts)
        return (env, wallets)

      -- wait for confirmation of funding txs, throw the first error if there's any
      waitForFundingTxs clusterEnv wallets extraConfig = do
        res <- for wallets $ \w ->
          awaitWalletFunded clusterEnv (cardanoMainnetAddress w) extraConfig
        return $
          firstJust
            ( \case
                Left (AwaitingCapiError e) -> Just $ show e
                Left AwaitingTimeoutError -> Nothing
                Right () -> Nothing
            )
            res

      getNodeSocketFile (runningNode -> RunningNode conn _ _ _) = nodeSocketFile conn
      getNodeConfigFile =
        -- assumption is that node.config lies in the same directory as node.socket
        flip replaceFileName "node.config" . getNodeSocketFile

      getWalletPrivateKey :: BpiWallet -> PrivateKey
      getWalletPrivateKey = Text.decodeUtf8 . Base16.encode . serialiseToCBOR . signKey
      interpret = fmap (either ClusterStartupFailure id) . runExceptT

      extraConf :: ExtraConfig
      extraConf =
        let defConfig = def
         in ExtraConfig
              (fromMaybe (ecSlotLength defConfig) slotLength)
              (fromMaybe (ecEpochSize defConfig) epochSize)
              (fromMaybe (ecMaxTxSize defConfig) maxTxSize)
              (fromMaybe (ecRaiseExUnitsToMax defConfig) raiseExUnitsToMax)

stopClusterHandler :: StopClusterRequest -> AppM StopClusterResponse
stopClusterHandler StopClusterRequest = do
  statusMVar <- asks status
  maybeClusterStatus <- liftIO $ tryTakeMVar statusMVar
  case maybeClusterStatus of
    Nothing -> pure $ StopClusterFailure "Cluster is not running"
    Just statusTVar -> do
      liftIO $ stopCluster statusTVar
      pure StopClusterSuccess
