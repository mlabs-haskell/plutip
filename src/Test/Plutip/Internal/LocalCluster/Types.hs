module Test.Plutip.Internal.LocalCluster.Types (
  ClusterEnv (..),
  Outcome (..),
  FailReason (..),
  nodeSocket,
  isSuccess,
) where

import BotPlutusInterface.Types (ContractState)
import Cardano.Api (NetworkId)
import Cardano.BM.Tracing (Trace)
import Cardano.Launcher.Node (CardanoNodeConn)
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (RunningNode))
import Control.Exception (SomeException)
import Data.Text (Text, intercalate, pack)
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

-- | Outcome of running contract
data Outcome w e a
  = Success
      { -- | return value of `Contract`
        contractResult :: a
      , -- | `Contract` state after execution
        contractState :: ContractState w
      }
  | Fail
      { -- | reason of `Contract` execution failure
        reason :: FailReason e
      }
  deriving stock (Show)

-- | Reason of `Contract` execution failure
data FailReason e
  = -- | error thrown by `Contract` (via `throwError`)
    ContractExecutionError e
  | -- | exception caught during contract run
    CaughtException SomeException
  | OtherErr Text
  deriving stock (Show)

-- | Check if outcome of contract execution result is `Success`
isSuccess :: Outcome w e a -> Bool
isSuccess = \case
  Success _ _ -> True
  Fail _ -> False

-- -- | Pretty print (temporary impl)
-- prettyResult :: (Show a, Show w, Show e) => Outcome w e a -> Text
-- prettyResult res@(RunResult tag outc) =
--   intercalate "\n" [header, prettyOut outc, ""]
--   where
--     header =
--       mconcat
--         [ maybe "Contract" (\t -> "\'" <> t <> "\'") tag
--         , " execution "
--         , if isSuccess res then "succeeded" else "failed"
--         ]

prettyOut :: (Show a, Show w, Show e) => Outcome w e a -> Text
prettyOut = \case
  (Success cRes cState) ->
    intercalate
      "\n"
      [ " Contract returned: " <> toText cRes
      , " Contract state: " <> toText cState
      ]
  (Fail e) -> " The error is: " <> toText e

toText :: Show a => a -> Text
toText = pack . show
