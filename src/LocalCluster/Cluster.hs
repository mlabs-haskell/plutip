-- temporary measure while module under development
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
-- temporary measure while module under development
{-# OPTIONS_GHC -Wno-unused-imports #-}

module LocalCluster.Cluster (runUsingCluster, runUsingCluster') where

import BotInterface.Setup qualified as BotSetup
import Cardano.Api qualified as CAPI
import Cardano.BM.Data.Severity (
  Severity (..),
 )
import Cardano.BM.Data.Tracer (
  HasPrivacyAnnotation (..),
  HasSeverityAnnotation (..),
 )
import Cardano.CLI (
  LogOutput (..),
  withLoggingNamed,
 )
import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Startup (
  installSignalHandlers,
  setDefaultFilePermissions,
  withUtf8Encoding,
 )
import Cardano.Wallet.Api.Types (
  EncodeAddress (..),
 )
import Cardano.Wallet.Logging (
  stdoutTextTracer,
  trMessageText,
 )
import Cardano.Wallet.Primitive.AddressDerivation (
  NetworkDiscriminant (..),
 )
import Cardano.Wallet.Primitive.Types.Coin (
  Coin (..),
 )
import Cardano.Wallet.Shelley (
  SomeNetworkDiscriminant (..),
  serveWallet,
  setupTracers,
  tracerSeverities,
 )
import Cardano.Wallet.Shelley.Launch (
  withSystemTempDir,
 )
import Cardano.Wallet.Shelley.Launch.Cluster (
  ClusterLog (..),
  Credential (..),
  RunningNode (..),
  localClusterConfigFromEnv,
  moveInstantaneousRewardsTo,
  nodeMinSeverityFromEnv,
  oneMillionAda,
  sendFaucetAssetsTo,
  sendFaucetFundsTo,
  testMinSeverityFromEnv,
  tokenMetadataServerFromEnv,
  walletListenFromEnv,
  walletMinSeverityFromEnv,
  withCluster,
 )
import Control.Arrow (
  first,
 )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Monad (unless, void, when)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Tracer (
  Tracer,
  contramap,
  traceWith,
 )
import Data.Maybe (catMaybes, isJust)
import Data.Proxy (
  Proxy (..),
 )
import Data.Text (
  Text,
  pack,
 )
import Data.Text qualified as T
import Data.Text.Class (
  ToText (..),
 )
import LocalCluster.Types
import Plutus.ChainIndex.App qualified as ChainIndex
import Plutus.ChainIndex.Config (ChainIndexConfig (cicNetworkId, cicPort), cicDbPath, cicSocketPath)
import Plutus.ChainIndex.Config qualified as CI
import Plutus.ChainIndex.Logging qualified as ChainIndex.Logging
import Plutus.ChainIndex.Types (Point (..))
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import System.Directory (
  createDirectory,
  doesFileExist,
  findExecutable,
 )
import System.Exit (die)
import System.FilePath (
  (</>),
 )
import Test.Integration.Faucet (
  genRewardAccounts,
  maryIntegrationTestAssets,
  mirMnemonics,
  shelleyIntegrationTestFunds,
 )
import System.Environment (setEnv)

{- | Start cluster and run action using provided `CalusterEnv`
 under development (mostly borrowed from `cardano-wallet`)
-}
runUsingCluster :: ReaderT ClusterEnv IO () -> IO ()
runUsingCluster act = runUsingCluster' (runReaderT act)

{- Examples:
   `plutus-apps` local cluster: https://github.com/input-output-hk/plutus-apps/blob/75a581c6eb98d36192ce3d3f86ea60a04bc4a52a/plutus-pab/src/Plutus/PAB/LocalCluster/Run.hs
   `cardano-wallet` local cluster: https://github.com/input-output-hk/cardano-wallet/blob/99b13e50f092ffca803fd38b9e435c24dae05c91/lib/shelley/exe/local-cluster.hs
-}
runUsingCluster' :: (ClusterEnv -> IO ()) -> IO ()
runUsingCluster' action = do
  -- current setup requires `cardano-node` and `cardano-cli` as external processes
  checkProcessesAvailable ["cardano-node", "cardano-cli"]

  withLocalClusterSetup $ \dir clusterLogs _walletLogs -> do
    withLoggingNamed "cluster" clusterLogs $ \(_, (_, trCluster)) -> do
      let tr' = contramap MsgCluster $ trMessageText trCluster
      clusterCfg <- localClusterConfigFromEnv
      withCluster
        tr'
        dir
        clusterCfg
        (const $ pure ()) -- faucet setup was here in `cardano-wallet` version
        (\rn -> runActionWthSetup rn dir trCluster action)
  where
    runActionWthSetup rn dir trCluster userActon = do
      let tracer' = trMessageText trCluster
      awaitSocketCreated tracer' rn
      ciPort <- launchChainIndex rn dir
      traceWith tracer' (ChaiIndexStartedAt ciPort)
      let cEnv =
            ClusterEnv
              { runningNode = rn
              , chainIndexUrl = BaseUrl Http "localhost" ciPort mempty
              , networkId = CAPI.Mainnet
              , supportDir = dir
              , tracer = trCluster
              }

      BotSetup.runSetup cEnv -- run preparations to use `bot-plutus-interface`
      userActon cEnv -- executing user action on cluster

-- Do all the program setup required for running the local cluster, create a
-- temporary directory, log output configurations, and pass these to the given
-- main action.
withLocalClusterSetup ::
  (FilePath -> [LogOutput] -> [LogOutput] -> IO a) ->
  IO a
withLocalClusterSetup action = do
  -- Setting required environment variables
  setEnv "NO_POOLS" "1"
  setEnv "SHELLEY_TEST_DATA" "cluster-data"

  -- Handle SIGTERM properly
  installSignalHandlers (putStrLn "Terminated")

  -- Ensure key files have correct permissions for cardano-cli
  setDefaultFilePermissions

  -- Set UTF-8, regardless of user locale
  withUtf8Encoding $
    -- This temporary directory will contain logs, and all other data
    -- produced by the local test cluster.
    withSystemTempDir stdoutTextTracer "test-cluster" $ \dir -> do
      let logOutputs name minSev =
            [ LogToFile (dir </> name) (min minSev Info)
            , LogToStdStreams minSev
            ]

      clusterLogs <- logOutputs "cluster.log" <$> testMinSeverityFromEnv
      walletLogs <- logOutputs "wallet.log" <$> walletMinSeverityFromEnv

      action dir clusterLogs walletLogs

checkProcessesAvailable :: [String] -> IO ()
checkProcessesAvailable requiredProcesses = do
  results <- mapM findExecutable requiredProcesses
  unless (isJust `all` results) $
    -- TODO: maybe some better way throwing needed?
    die $
      "This processes should be available in the environment:\n " <> show requiredProcesses
        <> "\n but only these were found:\n "
        <> show (catMaybes results)

-- Logging

data TestsLog
  = MsgBaseUrl Text Text Text -- wallet url, ekg url, prometheus url
  | MsgSettingUpFaucet
  | MsgCluster ClusterLog
  | WaitingSocketCreated
  | ChaiIndexStartedAt Int
  deriving stock (Show)

instance ToText TestsLog where
  toText = \case
    MsgBaseUrl walletUrl ekgUrl prometheusUrl ->
      mconcat
        [ "Wallet url: "
        , walletUrl
        , ", EKG url: "
        , ekgUrl
        , ", Prometheus url:"
        , prometheusUrl
        ]
    MsgSettingUpFaucet -> "Setting up faucet..."
    MsgCluster msg -> toText msg
    WaitingSocketCreated -> "Awaiting for node socket to be created"
    ChaiIndexStartedAt ciPort -> "Chain-index started at port " <> pack (show ciPort)

instance HasPrivacyAnnotation TestsLog

instance HasSeverityAnnotation TestsLog where
  getSeverityAnnotation = \case
    MsgSettingUpFaucet -> Notice
    MsgBaseUrl {} -> Notice
    MsgCluster msg -> getSeverityAnnotation msg
    WaitingSocketCreated -> Notice
    ChaiIndexStartedAt {} -> Notice

awaitSocketCreated :: Tracer IO TestsLog -> RunningNode -> IO ()
awaitSocketCreated trCluster rn@(RunningNode socket _ _) = do
  let sock = nodeSocketFile socket
  socketReady <- doesFileExist sock
  unless socketReady (waitASecond >> awaitSocketCreated trCluster rn)
  where
    waitASecond =
      traceWith trCluster WaitingSocketCreated
        >> threadDelay 1000000

-- | Launch the chain index in a separate thread.

-- TODO: add ability to set custom port (if needed)
launchChainIndex :: RunningNode -> FilePath -> IO Int
launchChainIndex (RunningNode sp _block0 (_gp, _vData)) dir = do
  config <- ChainIndex.Logging.defaultConfig
  let dbPath = dir </> "chain-index.db"
      chainIndexConfig =
        CI.defaultConfig
          { cicSocketPath = nodeSocketFile sp
          , cicDbPath = dbPath
          , cicNetworkId = CAPI.Mainnet
          }
  void . async $ void $ ChainIndex.runMain config chainIndexConfig
  return $ cicPort chainIndexConfig
