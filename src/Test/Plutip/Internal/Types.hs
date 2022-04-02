module Test.Plutip.Internal.Types (
  ClusterEnv (..),
  ExecutionResult (..),
  FailureReason (..),
  RunningNode (..),
  nodeSocket,
  nodeConfig,
  isExecutionError,
  isException,
  isSuccessful,
) where

import Paths_plutip ( getDataFileName )
import Cardano.Api (NetworkId)
import Cardano.BM.Tracing (Trace)
import Cardano.Launcher.Node (CardanoNodeConn)
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (RunningNode))
import Control.Exception (SomeException)
import Data.Either (isRight)
import Data.Text (Text)
import Servant.Client (BaseUrl)

-- | Environment for actions that use local cluster
data ClusterEnv = ClusterEnv
  { runningNode :: RunningNode
  , chainIndexUrl :: !BaseUrl
  , networkId :: !NetworkId
  , -- | this directory atm used to store all node related files,
    -- files created by `cardano-cli`, `chain-index` and `bot-plutus-interface`
    supportDir :: FilePath
  , tracer :: Trace IO Text -- not really used anywhere now
  , -- | set the budget estimation to a constant
    bpiForceBudget :: Maybe (Integer, Integer)
  }

-- | Helper function to get socket path from
nodeSocket :: ClusterEnv -> CardanoNodeConn
nodeSocket (ClusterEnv (RunningNode sp _ _) _ _ _ _ _) = sp

-- | Helper function to get node config path from
nodeConfig :: IO FilePath
nodeConfig = getDataFileName "cluster-data/node.config"

-- | Result of `Contract` execution. Returns contract observable state
--    and either `Contract` return value, or error of type `FailureReason`.
--    In case of failure observable state will hold changes up to the failure point.
data ExecutionResult w e a = ExecutionResult
  { -- | outcome of running contract.
    outcome :: Either (FailureReason e) a
  , -- | `Contract` observable state after execution (or up to the point where it failed)
    contractState :: w
  }
  deriving stock (Show)

isSuccessful :: ExecutionResult w e b -> Bool
isSuccessful = isRight . outcome

-- | The reason of `Contract` execution failure
data FailureReason e
  = -- | error thrown by `Contract` (via `throwError`)
    ContractExecutionError e
  | -- | exception caught during contract execution
    CaughtException SomeException
  deriving stock (Show)

isExecutionError :: FailureReason e -> Bool
isExecutionError = \case
  ContractExecutionError _ -> True
  _ -> False

isException :: FailureReason e -> Bool
isException = \case
  CaughtException _ -> True
  _ -> False
