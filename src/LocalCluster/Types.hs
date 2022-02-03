module LocalCluster.Types (
  ClusterEnv (..),
  socketPath,
) where

import Cardano.BM.Tracing (Trace)
import Cardano.Launcher.Node (CardanoNodeConn)
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (RunningNode))
import Data.Text (Text)

data ClusterEnv = ClusterEnv
  { runningNode :: RunningNode
  , -- | this directory atm used to store all node related files
    -- and files created by `cardano-cli`
    supportDir :: FilePath
  , tracer :: Trace IO Text -- not really used anywhere now, but probably should be =)
  }

-- | Helper function to get socket path from
socketPath :: ClusterEnv -> CardanoNodeConn
socketPath (ClusterEnv (RunningNode sp _ _) _ _) = sp
