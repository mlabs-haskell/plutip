module Plutip.Launch.Extra.Utils (
  localClusterConfigWithExtraConf,
) where

import Plutip.Launch.Cluster (ClusterEra, LocalClusterConfig (LocalClusterConfig), clusterEraToString, logFileConfigFromEnv, defaultPoolConfigs)
import Plutip.Launch.Extra.Types (ExtraConfig)


localClusterConfigWithExtraConf :: ClusterEra -> ExtraConfig -> IO LocalClusterConfig
localClusterConfigWithExtraConf era ec = do
  logConf <- logFileConfigFromEnv (Just $ clusterEraToString era)
  pure $ LocalClusterConfig defaultPoolConfigs era logConf ec
