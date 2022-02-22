module Test.Plutip.Internal.Types (
  ClusterEnv (..),
  ExecutionResult (..),
  Outcome (..),
  FailureReason (..),
  RunningNode (..),
  nodeSocket,
  isSuccess,
  isException,
  isContractError,
) where

import Cardano.Api (NetworkId)
import Cardano.BM.Tracing (Trace)
import Cardano.Launcher.Node (CardanoNodeConn)
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (RunningNode))
import Control.Exception (SomeException)
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
  }

-- | Helper function to get socket path from
nodeSocket :: ClusterEnv -> CardanoNodeConn
nodeSocket (ClusterEnv (RunningNode sp _ _) _ _ _ _) = sp

-- | Result of `Contract` execution
data ExecutionResult w e a = ExecutionResult
  { -- | outcome of running contract (success or failure)
    outcome :: Outcome w e a
  , -- | `Contract` observable state after execution (or up to the point where it failed)
    contractState :: w
  }
  deriving stock (Show)

-- | Outcome of running contract
data Outcome w e a
  = Success
      { -- | return value of `Contract`
        contractResult :: a
      }
  | Failure
      { -- | reason of `Contract` execution failure
        reason :: FailureReason e
      }
  deriving stock (Show)

-- | Reason of `Contract` execution failure
data FailureReason e
  = -- | error thrown by `Contract` (via `throwError`)
    ContractExecutionError e
  | -- | exception caught during contract run
    CaughtException SomeException
  | OtherErr Text
  deriving stock (Show)

-- | Check if outcome of contract execution result is `Success`
isSuccess :: ExecutionResult w e a -> Bool
isSuccess = \case
  ExecutionResult (Success _) _ -> True
  ExecutionResult (Failure _) _ -> False

-- | Check if Contract error was thrown during execution
isContractError :: ExecutionResult w e a -> Bool
isContractError = \case
  ExecutionResult (Failure (ContractExecutionError _)) _ -> True
  _ -> False

-- | Check if Exception was thrown during execution
isException :: ExecutionResult w e a -> Bool
isException = \case
  ExecutionResult (Failure (CaughtException _)) _ -> True
  _ -> False

-- -- | Pretty print (temporary impl)
-- prettyResult :: (Show a, Show w, Show e) => Outcome w e a -> Text
-- prettyResult res@(ExecutionResult tag outc) =
--   intercalate "\n" [header, prettyOut outc, ""]
--   where
--     header =
--       mconcat
--         [ maybe "Contract" (\t -> "\'" <> t <> "\'") tag
--         , " execution "
--         , if isSuccess res then "succeeded" else "failed"
--         ]

-- prettyOut :: (Show a, Show w, Show e) => Outcome w e a -> Text
-- prettyOut = \case
--   (Success cRes cState) ->
--     intercalate
--       "\n"
--       [ " Contract returned: " <> toText cRes
--       , " Contract state: " <> toText cState
--       ]
--   (Failure e) -> " The error is: " <> toText e

-- toText :: Show a => a -> Text
-- toText = pack . show
