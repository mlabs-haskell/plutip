module Plutip.Launch.Extra.Utils (
  localClusterConfigWithExtraConf,
) where

import Plutip.Launch.Cluster (LocalClusterConfig (LocalClusterConfig), clusterEraFromEnv, clusterEraToString, logFileConfigFromEnv)
import Plutip.Launch.Extra.Types (ExtraConfig)
import Plutip.Launch.PoolConfigs (defaultPoolConfigs)

localClusterConfigWithExtraConf :: ExtraConfig -> IO LocalClusterConfig
localClusterConfigWithExtraConf ec = do
  era <- clusterEraFromEnv
  logConf <- logFileConfigFromEnv (Just $ clusterEraToString era)
  pure $ LocalClusterConfig defaultPoolConfigs era logConf ec
