module Plutip.Types (
  ClusterEnv (..),
  RunningNode (..),
  nodeSocket,
  keysDir,
) where

import Cardano.Api (NetworkId)
import Cardano.Launcher.Node (CardanoNodeConn)
import Plutip.Config (PlutipConfig)
import Plutip.Launch.Cluster (RunningNode (RunningNode), ClusterEra)
import System.FilePath ((</>))

-- | Environment for actions that use local cluster
data ClusterEnv = ClusterEnv
  { runningNode :: RunningNode
  , -- , chainIndexUrl :: !(Maybe BaseUrl)
    networkId :: !NetworkId
  , -- | this directory atm used to store all node related files,
    -- files created by `cardano-cli`, `chain-index` and `bot-plutus-interface`
    supportDir :: FilePath
  , plutipConf :: !PlutipConfig
  , clusterEra :: !ClusterEra
  }

-- | Helper function to get socket path from
nodeSocket :: ClusterEnv -> CardanoNodeConn
nodeSocket (ClusterEnv (RunningNode sp _ _) _ _ _ _) = sp

keysDir' :: FilePath
keysDir' = "signing-keys"

-- | Directory for `.skey`'s of crated wallets for current cluster environment
keysDir :: ClusterEnv -> FilePath
keysDir cEnv = supportDir cEnv </> keysDir'
