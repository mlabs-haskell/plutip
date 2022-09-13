# Tweaking private network

It is possible to change some settings of local network that Plutip starts. By default Plutip uses node config, genesis files and etc. from `cluster-data` directory.

It is not advised to change anything in `cluster-data`. Better way will be to copy `cluster-data` to desired location, change what is needed and then point Plutip  to this custom directory via `PlutipConfig.clusterDataDir` field when calling `withConfiguredCluster` or `startCluster`.
