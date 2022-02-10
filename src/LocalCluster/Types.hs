module LocalCluster.Types (
  ClusterEnv (..),
  RunResult (..),
  FailReason (..),
  nodeSocket,
) where

import BotPlutusInterface.Types (ContractState)
import Cardano.Api (NetworkId)
import Cardano.BM.Tracing (Trace)
import Cardano.Launcher.Node (CardanoNodeConn)
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (RunningNode))
import Data.Text (Text)
import Servant.Client (BaseUrl)

data ClusterEnv = ClusterEnv
  { runningNode :: RunningNode
  , chainIndexUrl :: !BaseUrl
  , networkId :: !NetworkId
  , -- | this directory atm used to store all node related files,
    -- files created by `cardano-cli`, `chain-index` and `bot-plutus-interface`
    supportDir :: FilePath
  , tracer :: Trace IO Text -- not really used anywhere now, but probably should be =)
  }

-- | Helper function to get socket path from
nodeSocket :: ClusterEnv -> CardanoNodeConn
nodeSocket (ClusterEnv (RunningNode sp _ _) _ _ _ _) = sp

data FailReason e
  = ContractErr e
  | CaughtExcpetion
  | OtherErr Text
  deriving stock (Show)

data RunResult w e a
  = RunSuccess
      { contractResult :: a
      , contractState :: ContractState w
      }
  | RunFailed {reason :: FailReason e}
  deriving stock (Show)
