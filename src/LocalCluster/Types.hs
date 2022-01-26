module LocalCluster.Types (
  ClusterEnv(..)
) where

import Prelude  
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode)
import Cardano.BM.Tracing (Trace)
import Data.Text (Text)

data ClusterEnv = ClusterEnv
  { runningNode :: RunningNode
  , supportDir :: FilePath
  , tracer :: Trace IO Text
  }