module Plutip.Types (
  ClusterEnv (..),
  RunningNode (..),
  nodeSocket,
  keysDir,
) where

import Cardano.Api (NetworkId)
import Cardano.Launcher.Node (CardanoNodeConn)
import Plutip.Config (PlutipConfig)
import Plutip.Launch.Cluster (RunningNode (RunningNode))
import System.FilePath ((</>))

-- | Environment for actions that use local cluster
data ClusterEnv = ClusterEnv
  { -- | A wrapper over a node socket plus some node information.
    runningNode :: RunningNode
  , networkId :: !NetworkId
  , -- | This directory is used to store all node related files,
    -- plus files created by `cardano-cli`.
    supportDir :: FilePath
  , plutipConf :: !PlutipConfig
  }

-- | Helper function to get socket path from
nodeSocket :: ClusterEnv -> CardanoNodeConn
nodeSocket (ClusterEnv (RunningNode sp _ _ _) _ _ _) = sp

keysDir' :: FilePath
keysDir' = "signing-keys"

-- | Directory for `.skey`'s of crated wallets for current cluster environment
keysDir :: ClusterEnv -> FilePath
keysDir cEnv = supportDir cEnv </> keysDir'
