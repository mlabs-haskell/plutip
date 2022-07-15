# Changing cluster settings

Not too convenient atm, but possible.

## Changing things that could be set via genesis files or node config

1. Copy [cluster-data dir](../cluster-data) from plutip to some location
2. In that copied dir change whatever you need (e.g. you can change slot length [here](../cluster-data/shelley-genesis.yaml#L53))
3. Set path to that copied dir in config via [clusterDataDir](../src/Test/Plutip/Config.hs#30)
