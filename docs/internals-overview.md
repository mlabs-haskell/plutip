# A more detailed overview of how Plutip works

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [A more detailed overview of how Plutip works](#a-more-detailed-overview-of-how-plutip-works)
    - [Plutip Haskell API](#plutip-haskell-api)
        - [`withCluster`](#withcluster)
        - [`withFundedCluster`](#withfundedcluster)
        - [`startCluster`, `startFundedCluster` and `stopCluster`](#startcluster-startfundedcluster-and-stopcluster)
            - [`StopClusterRef`](#stopclusterref)
        - [`ClusterEnv`](#clusterenv)
        - [`PlutipConfig`](#plutipconfig)
            - [`ExtraConfig`](#extraconfig)
    - [What does `withCluster` do?](#what-does-withcluster-do)
        - [`Plutip.Cluster.withCluster`](#plutipclusterwithcluster)
        - [`Plutip.Launch.Cluster.withCluster`](#plutiplaunchclusterwithcluster)
            - [Stake pools setup](#stake-pools-setup)
            - [Faucets setup](#faucets-setup)

<!-- markdown-toc end -->

As a Haskell library Plutip exports five functions plus one datatype for referencing launched clusters (see a [Plutip.Cluster module](../src/Plutip/Cluster.hs).
All cluster setup functions boil down to the `withCluster` function.

The `local-cluster` executable (more info [here](./local-cluster/README.md)) is no exception as it uses the `withFundedCluster` function (the user action prints and creates files containing keys, useful information, etc).

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

How to use: consult the [library section](../README.md#as-a-library) of the main README.

This function is used for launching a local cluster and funding a collection of addresses.
See [this section](#faucets-setup) for an explanation of where the money come from.

User supplies:
* a cluster configuration,
* a distribution of Ada UTxOs for each wallet that is to be created,
* an action which has access for wallet key pairs.

Money distribution can specify creating several different UTxOs in each wallet, e.g. `[[ada 1], [ada 2, ada 4]]` will create 1 UTxO with 1 Ada in wallet 1 and 2 UTxOs with 2 and 4 Ada in wallet 2.

`withFundedCluster` is implemented in terms of `withCluster` with the following user action:
1. generate required amount of key pairs,
2. [fund](#faucets-setup) each key with the specified Ada UTxOs,
3. optionally save public and private keys to a user-specified folder,
4. wait for funds to arrive to the wallets,
5. execute user action supplied to the `withFundedCluster`

### `startCluster`, `startFundedCluster` and `stopCluster`

```haskell
startCluster ::
  forall (a :: Type).
  PlutipConfig ->
  (ClusterEnv -> IO a) ->
  IO (StopClusterRef, a)
startCluster conf action

startFundedCluster ::
  forall (a :: Type).
  PlutipConfig ->
  [[Lovelace]] ->
  (ClusterEnv -> [KeyPair] -> IO a) ->
  IO (StopClusterRef, a)
startFundedCluster conf distribution action

stopCluster :: StopClusterRef -> IO ()
stopCluster (StopClusterRef status)
```

These functions can be used for launching and stopping local cluster, also optionally funding a collection of addresses.

`startCluster` returns a tuple of a started cluster reference (`StopClusterRef`, name is due to it being used for stopping the cluster) and the result of a user action.
`startFundedCluster` additionally receives a distribution of Ada amounts to be created in each wallet.
Both functions are using `withCluster` to pass user action that doesn't terminate till `stopCluster` is called, plus extra bookkeeping in order for that to work (see [below](#stopclusterref) for more info). 

`stopCluster` stops the user action passed to `withCluster` by `startCluster` allowing for cluster tearown and cleanup to proceed.

A `{start,stop}Cluster` usage example:
```haskell
...
  (clusterRef, userActionResult) <- startCluster def $ \cenv -> do
    doSomething

  -- optionally access result of the user action (doSomething in this case)
  doSomethingElse userActionResult
  stopCluster clusterRef
```

#### `StopClusterRef`

`start/stopCluster` functions utilize cluster status reference to keep track of it:

```haskell
data ClusterStatus (a :: Type)
  = ClusterStarting
  | ClusterStarted a
  | ClusterClosing
  | ClusterClosed

data StopClusterRef = forall a. StopClusterRef (TVar (ClusterStatus a))
```

`startCluster`:
1. creates a new `TVar` for keeping cluster status,
2. launches `withCluster` call with the user action that executes the initial action passed to `startCluster` (via `forkFinally` with status cleanups):
  1. initial action is executed, then a cluster status is updated to `ClusterStarted`
  2. an `STM` action is executed which:
    * checks whether cluster status was changed to `ClusterClosing` after which it
    * either exits, thus allowing `withCluster` cleanup to proceed,
    * or blocks till cluster status `TVar` changes; via `retrySTM`,
5. `startCluster` then tries to read result of the user action from the cluster status
6. and finally returns `StopClusterRef` and a user action result.

`stopCluster` then just writes a `ClusterClosing` status to the `TVar` in `StopClusterRef`, thus triggering user action termination and subsequent cluster shutdown and cleanup.

### `ClusterEnv`

This is a datatype that is passed to each user action, it's mostly a wrapper over a node socket, belonging to one of the nodes in the local cluster.
You don't need to construct `ClusterEnv` as `withCluster` handles that.

Functions from the [`Plutip.CardanoApi` module](../src/Plutip/CardanoApi.hs) which are usually useful inside the user action expect a `ClusterEnv` argument (you can also use these functions as examples for writing your own with `Cardano.Api`).

Defined in the [`Plutip.Types` module](../src/Plutip/Types.hs).

### `PlutipConfig`

Contains Plutip configuration options that need to be passed to `withCluster` and other similar functions.
When using Plutip as a Haskell library users usually construct it directly, when using `local-cluster` executable `PlutipConfig` is constructed from the CLI options.

The options include
* override for the `cluster-data` folder location,
* working directory of the cluster and an option for keeping node-related files after cluster shutdown,
* [extra configuration](#extraconfig) for setting slot length, epoch size, max tx size and max ex units.

Defined in the [`Plutip.Config` module](../src/Plutip/Config.hs).

Provides a `Default` instance so you can use `def` from `Data.Default` for a default configuration (default `cluster-data` folder, temporary working directory and default `ExtraConfig`).

#### `ExtraConfig`

Defined in a [`Plutip.Launch.Extra.Types` module](../src/Plutip/Launch/Extra/Types.hs).

Provides a `Default` instance so you can use `def` from `Data.Default` for a default configuration (0.1 second slot size, 80 slot epoch size, 16KiB tx size, standard ex units limit).

**NOTE:** Currently (as of 08.05.2023) if you change the epoch size from its default value (80) then rewards for staking pool delegation stop working. See #149.

## What does `withCluster` do?

There are actually two `withCluster` functions:
1. `Plutip.Cluster.withCluster` which is supposed to be used by plutip users;
  * it sets up the environment (more of this [here](#plutipclusterwithcluster)),
  * sets up loggers, tracers, retries, etc.,
  * and calls `Plutip.Launch.Cluster.withCluster`
2. `Plutip.Launch.Cluster.withCluster`
  * it is based on `cardano-wallet`'s local cluster framework,
  * Plutip adds extra configurability to `cardano-wallet`'s version;
  * look [here](#plutiplaunchclusterwithcluster) for more information.

### `Plutip.Cluster.withCluster`

So what happens after you pass `PlutipConfig` and a user action to `withCluster`?

First it sets up the environment:
* checking that required binaries (`cardano-node`, `cardano-cli`) are available, checking RTS options
* change default file creation permissions (needed for `cardano-cli`), install SIGTERM handler with cleanup,
* set a folder with cluster data (default is project's `cluster-data`),
* set up the cluster directory which will contain logs and other data produced by the cluster (uses a temporary folder by default).

Afterwards `withCluster` sets up logging (mostly hides a long node configuratio log from stdout), creates a cluster configuration based on the [`ExtraConfig`](#extraconfig), and tries to proceed with cluster launching (several attempts will be made in case `cardano-node` process exits; this usually happens when node tries to start on a port that is in use).

The cluster configuration (`LocalClusterConfig`) includes an era which will be used (as of 09.05.2023 it was Babbage, after the Vasil hard fork), `ExtraConfig`, logging configuration and configuration of the pools that will be created (more on this [here](#stake-pools-setup)).

Then `Plutip.Launch.Cluster.withCluster` is finally called, which starts the cluster, waits till a node from the cluster is available (checks are done via retrying chain tip requests) and then executes the user action.

### `Plutip.Launch.Cluster.withCluster`

This function does the heavy lifting for starting a cluster.
The cluster consists of N (currently 4) block-producing Cardano nodes each operating a stake pool.

It is based on `cardano-wallet`'s local cluster testing framework, see [maintenance notes](./cardano-wallet-update.md) for more information.

The overview is as follows:
0. based on the era the cluster will start in procedure is quite different
  * before Babbage we needed to federalize network (basically change a param in genesis files) and one extra BFT node, so cluster will consist of N+1 nodes
  * with Babbage we can add stake pool information to genesis files and launch with less initial setup
  <!-- whether we can add stake pools to genesis files pre-Babbage (I think it works at least in Alonzo? Though `genesis.alonzo.json` file may have been misnamed) remains to be checked; it could be just a quirk of cardano-wallet's setup -->
1. launch a server which hosts pool metadata,
2. configure stake pools that will be launched,
3. generate genesis files, also adding faucet funds and stake pools to them,
4. generate node configs and launch each stake pool on an available random port,
5. wait till all nodes are operational (shutdown cluster in case not all node was able to launch),
6. use the first node to launch for subsequent operations:
  1. make a tx registering stake address on the chain,
  2. finally launch the user action,
7. shutdown the cluster

You can find more details about important steps below.

#### Stake pools setup

The [`Plutip.Launch.PoolConfigs` module](../src/Plutip/Launch/PoolConfigs.hs) describes stake pools that will launched, containing information like pledge amount, pool metadata, operator keys and so on.

Currently (as of 11.05.2023) Plutip defines 4 stake pools.

Pool metadata json files are then created and hosted by a webserver as required for [registering](https://github.com/input-output-hk/cardano-node/blob/master/doc/stake-pool-operations/8_register_stakepool.md) and operating a public stake pool.

Pools are then configured -- per-pool folder are created, key pairs, operational certificates are generated and we add pre-fund pledge addresses via an entry to genesis file and add stake pools themselves to the genesis file.
Customizations to slot length, etc. are also done by modifying genesis files.

Then we actually spin up the pools:
* allocate N free ports and assign them to pools,
* generate topology files by listing 3 other nodes on localhost with the ports above for each pool,
* generate node configurations,
* launch N `cardano-node`s, wait for all nodes to start and possibly terminate the cluster in case any node crashed,
* then finally launch the user action on the first available pool.

Nodes are configured to automatically to go through a series of hard forks up to Babbage during the startup via the special configuration options:
```yaml
TestAllegraHardForkAtEpoch: 0
TestAlonzoHardForkAtEpoch: 0
TestBabbageHardForkAtEpoch: 0
TestMaryHardForkAtEpoch: 0
TestShelleyHardForkAtEpoch: 0
```

These hard forks don't come through the update proposals, so the node startup it's much faster.

#### Faucets setup

The [`Plutip.Launch.FaucetFunds` module](../src/Plutip/Launch/FaucetFunds.hs) defines 200 addresses with 1 billion Ada in each.
These addresses match key files in the [`cluster-data/faucet-addrs` folder](../cluster-data/faucet-addrs).

We add entries from `FaucetFunds` to the genesis files during pool configuration stage, so when the network spins up we have 1 UTxO with Ada in each faucet address.

Faucets are then used to pay for any transactions, fund user wallets defined by Plutip, etc.
Faucets are used on a once-per-use throwaway basis (via the `takeFaucet` function), so you can expect a bit less than `200 - 1 - N*2 - 1 = 200 - 1 - 4*2 - 1 = 190` calls to [`fundKey` function](../src/Plutip/DistributeFunds.hs) to work in your user action.
This is usually more than enough, as you can create up to 80 UTxOs with a single faucet when using `fundKey` (if you need more than 80 outputs extra faucets are used automatically).
