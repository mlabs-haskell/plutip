# Cluster launcher update

[cardano-wallet-cluster-hs-1952de1]: https://github.com/input-output-hk/cardano-wallet/blob/1952de13f1cd954514cfa1cb02e628cfc9fde675/lib/shelley/src/Cardano/Wallet/Shelley/Launch/Cluster.hs

`Plutip` relies heavily on a local cluster testing framework from `cardano-wallet`.
See [Cardano.Wallet.Launch.Cluster](https://github.com/input-output-hk/cardano-wallet/blob/master/lib/wallet/api/http/Cardano/Wallet/Launch/Cluster.hs) module, and originally [Cardano.Wallet.Shelley.Launch.Cluster@1952de1][cardano-wallet-cluster-hs-1952de1] (this is the one that is used in Plutip currently as of 06.05.2023).

Initially, framework was used as-is, but in order to add ability to set slot length and epoch size to Plutip, module `Cluster.hs` was copied from `cardano-wallet` to Plutip's codebase and adjusted to make this settings possible. So in case of updating `cardano-wallet` dependency be sure that original `Cluster.hs` and Plutip's one differs only in expected way.

At the moment all changes are related to adding `ExtraConfig` to necessary ADTs and functions in Plutip's version of `Cluster.hs` and difference with the original is pretty small.

The [Cardano.Wallet.Shelley.Launch.Cluster@1952de1][cardano-wallet-cluster-hs-1952de1] module was split into:
1. [Plutip.Launch.PoolConfigs](./src/Plutip/Launch/PoolConfigs.hs)
2. [Plutip.Launch.Cluster](./src/Plutip/Launch/Cluster.hs)
3. [Plutip.Launch.FaucetFunds](./src/Plutip/Launch/FaucetFunds.hs)

All modified typess and functions are marked with the "altered" comment for easier search, e.g.:
```haskell
-- altered: `def :: ExtraConfig` added
localClusterConfigFromEnv :: IO LocalClusterConfig
localClusterConfigFromEnv = do
    era <- clusterEraFromEnv
    logConf <- logFileConfigFromEnv (Just $ clusterEraToString era)
    pure $ LocalClusterConfig defaultPoolConfigs era logConf def
```
