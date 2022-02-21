module Test.Plutip.Internal.LocalCluster.Cluster
  ( runUsingCluster,
    runUsingCluster',
    runUsingClusterConf,
  )
where

import Cardano.Api qualified as CAPI
import Cardano.BM.Data.Severity qualified as Severity
import Cardano.BM.Data.Tracer
  ( HasPrivacyAnnotation,
    HasSeverityAnnotation,
    Tracer,
    contramap,
    getSeverityAnnotation,
    traceWith,
  )
import Cardano.CLI (LogOutput (LogToFile, LogToStdStreams), withLoggingNamed)
import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Startup
  ( installSignalHandlers,
    setDefaultFilePermissions,
    withUtf8Encoding,
  )
import Cardano.Wallet.Logging
  ( stdoutTextTracer,
    trMessageText,
  )
import Cardano.Wallet.Shelley.Launch
  ( withSystemTempDir,
  )
import Cardano.Wallet.Shelley.Launch.Cluster
  ( ClusterLog,
    RunningNode (RunningNode),
    localClusterConfigFromEnv,
    testMinSeverityFromEnv,
    walletMinSeverityFromEnv,
    withCluster,
  )
import Control.Concurrent.Async (async)
import Control.Exception.Safe (IOException, catchIO, throw, throwIO)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Retry (constantDelay, limitRetries, recoverAll)
import Data.Default (def)
import Data.Foldable (for_)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text
  ( Text,
    pack,
  )
import Data.Text.Class (ToText (toText))
import GHC.Stack (HasCallStack)
import Plutus.ChainIndex.App qualified as ChainIndex
import Plutus.ChainIndex.Config (ChainIndexConfig (cicNetworkId, cicPort), cicDbPath, cicSocketPath)
import Plutus.ChainIndex.Config qualified as CI
import Plutus.ChainIndex.Logging qualified as ChainIndex.Logging
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import System.Directory
  ( copyFile,
    findExecutable,
  )
import System.Environment (setEnv)
import System.Exit (die)
import System.FilePath
  ( (</>),
  )
import Test.Plutip.Internal.BotPlutusInterface.Setup qualified as BotSetup
import Test.Plutip.Internal.LocalCluster.Config (Config (clusterDataDir, relayNodeLogs))
import Test.Plutip.Internal.LocalCluster.Types (ClusterEnv (ClusterEnv, chainIndexUrl, networkId, runningNode, supportDir, tracer))
import Test.Plutip.Tools.CardanoApi qualified as Tools
import Text.Printf (printf)

-- | Start cluster and run action using provided `CalusterEnv`
-- under development
runUsingCluster :: ReaderT ClusterEnv IO () -> IO ()
runUsingCluster = runUsingClusterConf def

runUsingClusterConf :: Config -> ReaderT ClusterEnv IO () -> IO ()
runUsingClusterConf conf act =
  runUsingCluster'
    conf
    (runReaderT act)

{- Examples:
   `plutus-apps` local cluster: https://github.com/input-output-hk/plutus-apps/blob/75a581c6eb98d36192ce3d3f86ea60a04bc4a52a/plutus-pab/src/Plutus/PAB/LocalCluster/Run.hs
   `cardano-wallet` local cluster: https://github.com/input-output-hk/cardano-wallet/blob/99b13e50f092ffca803fd38b9e435c24dae05c91/lib/shelley/exe/local-cluster.hs
-}
runUsingCluster' :: Config -> (ClusterEnv -> IO ()) -> IO ()
runUsingCluster' conf action = do
  -- current setup requires `cardano-node` and `cardano-cli` as external processes
  checkProcessesAvailable ["cardano-node", "cardano-cli"]

  withLocalClusterSetup conf $ \dir clusterLogs _walletLogs ->
    do
      withLoggingNamed "cluster" clusterLogs $ \(_, (_, trCluster)) -> do
        let tr' = contramap MsgCluster $ trMessageText trCluster
        clusterCfg <- localClusterConfigFromEnv
        withCluster
          tr'
          dir
          clusterCfg
          (const $ pure ()) -- faucet setup was here in `cardano-wallet` version
          (\rn -> runActionWthSetup rn dir trCluster action)
      >> handleLogs dir conf
  where
    runActionWthSetup rn dir trCluster userActon = do
      let tracer' = trMessageText trCluster
      waitForRelayNode tracer' rn
      ciPort <- launchChainIndex rn dir
      traceWith tracer' (ChaiIndexStartedAt ciPort)
      let cEnv =
            ClusterEnv
              { runningNode = rn,
                chainIndexUrl = BaseUrl Http "localhost" ciPort mempty,
                networkId = CAPI.Mainnet,
                supportDir = dir,
                tracer = trCluster
              }

      BotSetup.runSetup cEnv -- run preparations to use `bot-plutus-interface`
      userActon cEnv -- executing user action on cluster

handleLogs :: HasCallStack => FilePath -> Config -> IO ()
handleLogs clusterDir conf =
  copyRelayLog `catchIO` (error . printf "Failed to save relay node log: %s" . show)
  where
    copyRelayLog = for_ (relayNodeLogs conf) $ \toFile ->
      copyFile
        {- We're heavily depending on cardano-wallet local cluster tooling atm.
          Path partially hardcoded in Cardano.Wallet.Shelley.Launch.Cluster by
         `withRelayNode` ("node" subdir) and `genConfig` (file name)
        -}
        (clusterDir </> "node" </> "cardano-node.log")
        toFile

-- Do all the program setup required for running the local cluster, create a
-- temporary directory, log output configurations, and pass these to the given
-- main action.
withLocalClusterSetup ::
  Config ->
  (FilePath -> [LogOutput] -> [LogOutput] -> IO a) ->
  IO a
withLocalClusterSetup conf action = do
  -- Setting required environment variables
  setEnv "NO_POOLS" "1"
  setClusterDataDir

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
            [ LogToFile (dir </> name) (min minSev Severity.Info),
              LogToStdStreams minSev
            ]

      clusterLogs <- logOutputs "cluster.log" <$> testMinSeverityFromEnv
      walletLogs <- logOutputs "wallet.log" <$> walletMinSeverityFromEnv

      action dir clusterLogs walletLogs
  where
    setClusterDataDir =
      setEnv "SHELLEY_TEST_DATA" $
        fromMaybe "./cluster-data" (clusterDataDir conf)

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
  | WaitingRelayNode
  | ChaiIndexStartedAt Int
  deriving stock (Show)

instance ToText TestsLog where
  toText = \case
    MsgBaseUrl walletUrl ekgUrl prometheusUrl ->
      mconcat
        [ "Wallet url: ",
          walletUrl,
          ", EKG url: ",
          ekgUrl,
          ", Prometheus url:",
          prometheusUrl
        ]
    MsgSettingUpFaucet -> "Setting up faucet..."
    MsgCluster msg -> toText msg
    WaitingRelayNode -> "Waiting for relay node up and running"
    ChaiIndexStartedAt ciPort -> "Chain-index started at port " <> pack (show ciPort)

instance HasPrivacyAnnotation TestsLog

instance HasSeverityAnnotation TestsLog where
  getSeverityAnnotation = \case
    MsgSettingUpFaucet -> Severity.Notice
    MsgBaseUrl {} -> Severity.Notice
    MsgCluster msg -> getSeverityAnnotation msg
    WaitingRelayNode -> Severity.Notice
    ChaiIndexStartedAt {} -> Severity.Notice

waitForRelayNode :: Tracer IO TestsLog -> RunningNode -> IO ()
waitForRelayNode trCluster rn = do
  liftIO $ recoverAll policy (const getTip)
  where
    policy = constantDelay 500000 <> limitRetries 5
    getTip = trace >> void (Tools.queryTip rn)
    trace = traceWith trCluster WaitingRelayNode

-- | Launch the chain index in a separate thread.
-- TODO: add ability to set custom port (if needed)
launchChainIndex :: RunningNode -> FilePath -> IO Int
launchChainIndex (RunningNode sp _block0 (_gp, _vData)) dir = do
  config <- ChainIndex.Logging.defaultConfig
  let dbPath = dir </> "chain-index.db"
      chainIndexConfig =
        CI.defaultConfig
          { cicSocketPath = nodeSocketFile sp,
            cicDbPath = dbPath,
            cicNetworkId = CAPI.Mainnet
          }
  void . async $ void $ ChainIndex.runMain config chainIndexConfig
  return $ cicPort chainIndexConfig
