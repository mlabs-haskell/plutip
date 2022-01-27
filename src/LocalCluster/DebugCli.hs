module LocalCluster.DebugCli (
  debugCli,
  utxoAtAddress,
) where

import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (RunningNode))
import Data.ByteString.Lazy.Char8 qualified as BS
import GHC.IO.Exception (ExitCode (ExitSuccess))
import LocalCluster.Types
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitFailure))
import System.Process.Typed (
  proc,
  readProcess,
  setEnv,
 )

{- | Call `cardano-cli` as external process.
 `cardano-cli` must be available in shell
-}
debugCli ::
  ClusterEnv ->
  [String] ->
  IO BS.ByteString
debugCli (ClusterEnv (RunningNode conn _ _) _ _) args = do
  env <- getEnvironment
  let process =
        setEnv (("CARDANO_NODE_SOCKET_PATH", nodeSocketFile conn) : env) $
          proc "cardano-cli" args
  (st, out, err) <- readProcess process
  pure $ case st of
    ExitSuccess -> out
    ExitFailure _ -> err

{- | Get UTxOs at address using `cardano-cli` as external process.
 `cardano-cli` must be available in shell
-}
utxoAtAddress :: ClusterEnv -> String -> IO ()
utxoAtAddress ce addr = do
  res <- debugCli ce ["query", "utxo", "--mainnet", "--address", addr]
  BS.putStrLn $ "UTxO at " <> BS.pack addr <> ": " <> res
