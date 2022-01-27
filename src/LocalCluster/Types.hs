module LocalCluster.Types (
  ClusterEnv (..),
) where

import Cardano.BM.Tracing (Trace)
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode)
import Data.Text (Text)

data ClusterEnv = ClusterEnv
  { runningNode :: RunningNode
  , -- | this directory atm used to store all node related files
    -- and files created by `cardano-cli`
    supportDir :: FilePath
  , tracer :: Trace IO Text -- not really used anywhere now, but probably should be =)
  }
