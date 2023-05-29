# Plutip

[![Hercules-ci][herc badge]][herc link]

[herc badge]: https://img.shields.io/badge/ci--by--hercules-green.svg
[herc link]: https://hercules-ci.com/github/mlabs-haskell/plutip

Plutip is a Cardano tool for spawning local clusters.
You can use it to start up disposable private network with an arbitrary amount of funded addresses (Plutip will provide corresponding PKHs and key pairs as well).

For smart contract testing see [CTL integration with Plutip](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutip-testing.md).

**TL;DR**: plutip gets you a cardano node socket where the node belongs to a small cluster on a private network, optionally you can prefund addresses with ADA.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Plutip](#plutip)
    - [Prerequisites](#prerequisites)
        - [When using Plutip as a Haskell library](#when-using-plutip-as-a-haskell-library)
    - [Quick start](#quick-start)
        - [Run as an executable](#run-as-an-executable)
        - [Use in a Haskell program](#use-in-a-haskell-program)
    - [Overview](#overview)
        - [As a library](#as-a-library)
        - [As an executable](#as-an-executable)
        - [Via CTL for contract testing](#via-ctl-for-contract-testing)
    - [Tutorials](#tutorials)
    - [Advanced network setup](#advanced-network-setup)
    - [Useful links](#useful-links)
    - [Plutip for integration testing of smart contracts](#plutip-for-integration-testing-of-smart-contracts)
    - [Maintenance](#maintenance)

<!-- markdown-toc end -->

## Prerequisites

1. [Nix](https://nix.dev/tutorials/install-nix)
2. [Set up haskell.nix binary cache](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started#setting-up-the-binary-cache)

### When using Plutip as a Haskell library

If your project is importing and making use of `Plutip`s library you will need to make sure that the following executables are present in your `PATH`:

* `cardano-cli` executable available in the environment
* `cardano-node` executable available in the environment

The following GHC flags must be used in order for Plutip to run: `-threaded -rtsopts`.

**NOTE:** This branch launches local network in `Vasil`.
It was tested with node `1.35.4` (this node version is used in the Nix environment as well).
Please use an appropriate node version when setting up own binaries in `PATH`.

## Quick start

### Run as an executable

```bash
nix run github:mlabs-haskell/plutip#plutip-core:exe:local-cluster -- --help

# start local network with 2 funded addresses 10,000 ADA each
nix run github:mlabs-haskell/plutip#plutip-core:exe:local-cluster -- -n 2

# or if you want to use the local version, clone the repo and then
nix run .#plutip-core:exe:local-cluster -- --help
```

### Use in a Haskell program

Launch local cluster with:
```haskell
withCluster :: PlutipConfig -> (ClusterEnv -> IO a) -> IO a
withCluster conf action
```
Use `withFundedCluster` to additionaly receive pre-funded keys.

Cluster shuts down when the user action (second argument to `withCluster`) completes.
Use `startCluster`/`startFundedCluster` and `stopCluster` variants to keep the cluster running.

[internals-tutorial]: ./docs/internals-overview.md

See a more detailed [overview](#as-a-library) or read an [in-depth tutorial][internals-tutorial] for more information.

## Overview

Plutip is in essence a simpler wrapper over some `cardano-wallet` code for spawning private disposable Cardano clusters.

It can be used in a few ways:
  1. as a library,
  2. as an executable,
  3. indirectly via cardano-transaction-lib (CTL) smart contract tests. This is a facility for testing contracts in an isolated environment: with wallet mocks and a private plutip cluster. See [CTL](https://github.com/Plutonomicon/cardano-transaction-lib/) and their [documentation](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/testing.md#testing-with-plutip) on Plutip tests. That's very much the recommended way if you're a CTL user.
  4. Historical mention: you could test PAB `Contract`s with Plutip itself, but this functionality is unmantained and was removed as most users switched to [CTL](https://github.com/Plutonomicon/cardano-transaction-lib/). If you're interested check out the archive branch [`plutip-bpi`](https://github.com/mlabs-haskell/plutip/tree/plutip-bpi) and the old [tutorial](https://github.com/mlabs-haskell/plutip/blob/plutip-bpi/docs/interactive-plutip.md).

### As a library

Launch local cluster with 
```haskell
withCluster :: PlutipConfig -> (ClusterEnv -> IO a) -> IO a
withCluster conf action
```
where:
 - `conf :: PlutipConfig` specifies the working directory of a spawned cluster (can be temporary) and in some capacity the parameters of the cluster. Use `Data.Default (def)` to spawn default cluster in a temporary directory.
 - `ClusterEnv` is essentially a wrapper around the node socket. The socket belongs to one of the nodes.
 - `action :: ClusterEnv -> IO a` is a user action which has access to a `cardano-node` in a cluster via `Cardano.Api`.

Use
```haskell
withFundedCluster :: PlutipConfig -> [[Lovelace]] -> (ClusterEnv -> [KeyPair] -> IO a) -> IO a
```
to additionaly receive keys prefunded with specified fund distributions (e.g. Key 1 with `[1 Lovelace]` and Key 2 with `[2 Lovelace, 4 Lovelace]`).

Additionaly there are helpers `startCluster`, `startFundedCluster`, `stopCluster` which are useful when you want your cluster to keep running, instead of shutting down after the IO action is completed.

All those functions are exported from the [Plutip.Cluster module](./src/Plutip.Cluster.hs).
<!-- TODO: add haddocks -->
Read an [in-depth tutorial][internals-tutorial] for more information.

Example:
```haskell
import Data.Default (Default (def))
import Plutip.CardanoApi (currentBlock)
import Plutip.Cluster (withFundedCluster)
import Plutip.Keys (cardanoMainnetAddress)

main = withFundedCluster def [[ada 1], [ada 2, ada 4]] $ \cenv [key1, key2] -> do
  -- You have a default cluster using a temporary directory for storage and 7 Ada to use wisely.
  -- You can query the node for block number with
  res <- currentBlock cenv
  ...
  -- See Plutip.CardanoApi for example queries and construct your own queries with Cardano.Api.
  -- To make use of your keys, also use Cardano.Api.

ada = (*) 1_000_000
```

How to use `{start,stop}Cluster`:
```haskell
...
  (clusterRef, userActionResult) <- startCluster def $ \cenv -> do
    doSomething

  -- optionally access result of the user action (doSomething in this case)
  doSomethingElse userActionResult
  stopCluster clusterRef
```

Another useful example of using `startFundedCluster` and `stopCluster` is [`plutip-server`](https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/plutip-server), see its [`Api.Handlers`](https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/plutip-server/src/Api/Handlers.hs) module.

### As an executable

Plutip provides a `local-cluster` executable.
You can build it and run with Nix:
```bash
nix run github:mlabs-haskell/plutip#plutip-core:exe:local-cluster -- --help
```

Available options mostly match the `withFundedCluster` interface, see `--help` and [local-cluster README](local-cluster/README.md) for detailed description of the arguments.

### Via CTL for contract testing

[CTL](https://github.com/Plutonomicon/cardano-transaction-lib) is a PureScript SDK for creating DApps.
One of its features is the ability to test contracts on disposable private networks which Plutip sets up, see [plutip-testing](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutip-testing.md).
CTL provides (via Nix) a runtime environment containing several services, including [plutip-server](https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/plutip-server) which allows to control Plutip via HTTP.
As long as you are using CTL's Nix environment (or your setup is based on it) there's no need to install Plutip separately.
<!-- See a full working example of a CTL-based project with smart contract tests is [here](...). You can base your project's structure on it. -->

## Note on running multiple clusters

There is one caveat you need to be aware of when trying to run multiple Plutip instances.
Internally Plutip uses the `randomUnusedTCPPorts` from `cardano-wallet` (see the definition [here](https://github.com/input-output-hk/cardano-wallet/blob/af82118b5cd5addc60c68dc4fdaf59cb1d228be7/lib/wallet/src/Cardano/Wallet/Network/Ports.hs#L119)) to get a collection of ports in later assignes to nodes in the cluster.

There is a race condition (via `isPortOpen`) that may result in nodes from separate clusters getting assigned the same port, then these two nodes will race for the port and in the result one cluster will start fine and another will fail, currently the only way to deal with it is to start Plutip again.
The probability of this is not huge, but it can happen.

## Tutorials

* [Running disposable local network and building custom runners](./local-cluster/README.md)
* [Overview of how Plutip works][internals-tutorial]
<!-- * [CTL-based project with smart contract tests example](...) -->

## Advanced network setup

* [Tweaking local network](./docs/tweaking-network.md)
* [How to (re)generate network configs from node config and genesis files](./docs/regenerate-network-configs.md)

## Useful links

* [Template for setting up a Nix flake that includes Plutip](https://github.com/MitchyCola/plutip-flake). Kudos to @MitchyCola

## Plutip for integration testing of smart contracts

If your goal is to:
* run tests with the `tasty` Haskell framework where user can run Plutus contracts (`Contract w s e a`) using the disposable private network set up by Plutip,
* run contracts in REPL on a local network,

then check out [CTL](https://github.com/Plutonomicon/cardano-transaction-lib) or a legacy Plutip revision ([`plutip-bpi`](https://github.com/mlabs-haskell/plutip/tree/plutip-bpi)) or Plutip v1.3.1 and older releases.

## Maintenance

* [Important notes on updating the `cardano-wallet` dependency](./docs/cardano-wallet-update.md)
