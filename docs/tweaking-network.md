# Tweaking private network

## Setting slot length and epoch size

It is possible to set slot length and epoch size while starting network from haskell via `PlutipConfig` - `extraConfig :: ExtraConfig` holds corresponding fields.

For setting parameters while launching `local-cluster` executable see `--slot-len` and `--epoch-size` options in [documentation](../local-cluster/README.md).

## Tweaking cluster config files

It is possible to change some settings of local network that Plutip starts. By default Plutip uses node config, genesis files and etc. from `cluster-data` directory.

It is not advised to change anything in `cluster-data`. Better way will be to copy `cluster-data` to desired location, change what is needed and then point Plutip  to this custom directory via `PlutipConfig.clusterDataDir` field when calling `withConfiguredCluster` or `startCluster`.
