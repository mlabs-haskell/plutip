# Tweaking private network

## Setting slot length and epoch size

It is possible to set slot length and epoch size when starting network from Haskell via `PlutipConfig` -- `extraConfig :: ExtraConfig` holds corresponding fields.

For setting parameters when launching `local-cluster` executable see `--slot-len` and `--epoch-size` options in the [local-cluster documentation](../local-cluster/README.md).

## Tweaking cluster config files

It is possible to change some settings of a local network that Plutip starts. By default Plutip uses node config, genesis files and etc. from the `cluster-data` directory.

It is not advised to change anything in `cluster-data`. Better way would be to copy `cluster-data` to a desired location, change what is needed and then point Plutip to this custom directory by modifying the `clusterDataDir` field of a `PlutipConfig` parameter to `withConfiguredCluster` or `startCluster`.
