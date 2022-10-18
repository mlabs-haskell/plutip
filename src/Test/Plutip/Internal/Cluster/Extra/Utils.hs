module Test.Plutip.Internal.Cluster.Extra.Utils (
  localClusterConfigWithExtraConf,
) where

import Test.Plutip.Internal.Cluster (LocalClusterConfig (LocalClusterConfig), clusterEraFromEnv, clusterEraToString, defaultPoolConfigs, logFileConfigFromEnv)
import Test.Plutip.Internal.Cluster.Extra.Types (ExtraConfig)

localClusterConfigWithExtraConf :: ExtraConfig -> IO LocalClusterConfig
localClusterConfigWithExtraConf ec = do
  era <- clusterEraFromEnv
  logConf <- logFileConfigFromEnv (Just $ clusterEraToString era)
  pure $ LocalClusterConfig defaultPoolConfigs era logConf ec

-- setSlotLen ::  NominalDiffTime -> LocalClusterConfig -> LocalClusterConfig
-- setSlotLen sl lc = lc {slotLength = sl}

-- setEpochLen :: EpochSize -> LocalClusterConfig -> LocalClusterConfig
-- setEpochLen el lc = lc {epochSize = el}

-- type PtpConf = (NominalDiffTime, EpochSize)
