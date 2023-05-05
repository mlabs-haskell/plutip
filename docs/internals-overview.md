# A more detailed overview of how Plutip works

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [A more detailed overview of how Plutip works](#a-more-detailed-overview-of-how-plutip-works)
    - [Plutip Haskell API](#plutip-haskell-api)
        - [`withCluster`](#withcluster)
        - [`withFundedCluster`](#withfundedcluster)
        - [`startCluster` and `stopCluster`](#startcluster-and-stopcluster)
            - [`StopClusterRef`](#stopclusterref)
        - [`ClusterEnv`](#clusterenv)
        - [`PlutipConfig`](#plutipconfig)
            - [`ExtraConfig`](#extraconfig)
    - [What does `withCluster` do?](#what-does-withcluster-do)

<!-- markdown-toc end -->

As a Haskell library Plutip exports five functions plus one datatype for referencing launched clusters (see a [Plutip.Cluster module](./src/Plutip/Cluster.hs).
All cluster setup functions boil down to the `withCluster` function.

The `local-cluster` executable (more info [here](./local-cluster/README.md)) is no exception as it uses the `withFundedCluster` function (the user action prints and creates files containing keys, useful information, etc.). 

First this document will give an [overview](#plutip-haskell-api) of all functions that Plutip provides and datatypes which you might need to use with them.

Next we'll describe what goes on [underneath](#what-does-withcluster-do) the `withCluster` function.

## Plutip Haskell API

### `withCluster`

```haskell
withCluster :: PlutipConfig -> (ClusterEnv -> IO a) -> IO a
withCluster conf action
```

How to use: see the [library section](./README.md#as-a-library) of the main README.
This function is used for launching a local cluster.
In a nutshell you supply cluster configuration and a user action to be launched after the cluster is created.

See [this section](#what-does-withcluster-do) for an explanation of what this actually entails.

### `withFundedCluster`

```haskell
withFundedCluster :: PlutipConfig -> [[Lovelace]] -> (ClusterEnv -> [KeyPair] -> IO a) -> IO a
withFundedCluster conf distribution action
```

How to use: consult the [library section](./README.md#as-a-library) of the main README.

This function is used for launching a local cluster and funding a collection of addresses.
See [this section](#...) for an explanation of where the money come from.

User supplies:
* a cluster configuration,
* a distribution of Ada UTxOs for each wallet that is to be created,
* an action which has access for wallet key pairs.

Money distribution can specify creating several different UTxOs in each wallet, e.g. `[[ada 1], [ada 2, ada 4]]` will create 1 UTxO with 1 Ada in wallet 1 and 2 UTxOs with 2 and 4 Ada in wallet 2.

`withFundedCluster` is implemented in terms of `withCluster` with the following user action:
1. generate required amount of key pairs,
2. [fund](#...) each key with the specified Ada UTxOs,
3. optionally save public and private keys to a user-specified folder,
4. wait for funds to arrive to the wallets,
5. execute user action supplied to the `withFundedCluster`

### `startCluster` and `stopCluster`

#### `StopClusterRef`

### `ClusterEnv`

###  `PlutipConfig`

Defined in a [`Plutip.Config` module](./src/Plutip/Config.hs).

Provides a `Default` instance so you can use `def` from `Data.Default` for a default configuration (default `cluster-data` folder, temporary working directory and default `ExtraConfig`).

#### `ExtraConfig`

Defined in a [`Plutip.Launch.Extra.Types` module](./src/Plutip/Launch/Extra/Types.hs).

Provides a `Default` instance so you can use `def` from `Data.Default` for a default configuration (...).

## What does `withCluster` do?

### ... faucet ...
