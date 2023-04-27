# Plutip

[![Hercules-ci][herc badge]][herc link]

[herc badge]: https://img.shields.io/badge/ci--by--hercules-green.svg
[herc link]: https://hercules-ci.com/github/mlabs-haskell/plutip

Plutip is Cardano tool for spawning local clusters.
Use it to start up disposable private network with arbitrary amount of funded addresses (providing keys for that addresses as well). 

TLDR: `plutip :: IO CardanoNodeSocket` where the node belongs to a small cluster, optionally you can ask for prefunded keys.

## Requirements

Best way of building and launching Plutip libraries is using `Nix` and `cabal`. E.g. to start local network with two funded addresses run

```bash
nix build .#plutip-core:exe:local-cluster  
./result/bin/local-cluster -n 2
```

or

```bash
nix develop
cabal run local-cluster -- -n 2
```

If your project is importing and making use of `Plutip`s library you will need to make sure that the following executables are present in your `PATH`:

* `cardano-cli` executable available in the environment
* `cardano-node` executable available in the environment

And the following ghc flag must to be set for the test execution: `-Wall -threaded -rtsopts`

## NOTES

⚠️ This branch launches local network in `Vasil`. It was tested with node `1.35.4` (this node version used in nix environment as well). Please use appropriate node version when setting up own binaries in `PATH`.

## Quick start

Launch local cluster with:
```
withCluster :: PlutipConfig -> (ClusterEnv -> IO a) -> IO a
```
Use `withFundedCluster` to additionaly receive pre-funded keys.

Cluster shutdowns when the user action completes. Use `startCluster`/`startFundedCluster` and `stopCluster` variants to keep cluster running.

## Overview

Plutip is in essence a simpler wrapper on some cardano-wallet code for spawning private disposable cardano clusters.
It can be used in few ways:
  1. as a library
  2. as an executable
  3. indirectly via cardano-transaction-lib e2e plutip tests. This is a facility for testing contracts e2e in isolated environment: with wallet mocks and a private plutip cluster. See [ctl](https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop) and their documentation on e2e tests. That's very much the recommended way if you're a ctl user.
  4. Historical mention: you could test pab `Contract`s with plutip itself, but this functionality is unmantained and was removed. If you're interested check out the archive branch `plutip-bpi`.

### As a library

Launch local cluster with 
```
withCluster :: PlutipConfig -> (ClusterEnv -> IO a) -> IO a
```
where
 - `PlutipConfig` specifies the working directory of a spawned cluster (can be temporary) and in some capacity the parameters of the cluster. Use `Data.Default (def)` to spawn default cluster in temporary directory.
 - `ClusterEnv` is essentially a wrapper around the node socket. The socket belongs to one of the nodes.

Use
```
withFundedCluster :: PlutipConfig -> [[Lovelace]] -> (ClusterEnv -> [KeyPair] -> IO a) -> IO a
```
to additionaly receive keys prefunded with specified fund distributions (i.e. Key 1 with [1 Lovelace] and Key 2 with [2 Lovelace, 4 Lovelace]).

Additionaly there's helpers `startCluster`, `startFundedCluster`, `stopCluster` useful when you want your cluster to keep running, instead of close after the IO action is completed.

Full example: 
```
import Data.Default (Default (def))
import Plutip.CardanoApi (currentBlock)
import Plutip.Cluster (withFundedCluster)
import Plutip.Keys (cardanoMainnetAddress)

main = withFundedCluster def [[ada 1], [ada 2, ada 4]] $ \cenv [key1, key2] -> do
  -- You have a default cluster using a temporary directory for storage and 7 Ada to use wisely.
  -- You can i.e. query the node for block number with 
  res <- currentBlock cenv
  ...
  -- See Plutip.CardanoApi for example queries and construct your own queries with Cardano.Api.
  -- To make use of your keys, also use Cardano.Api.

ada = (*) 1_000_000

```

### As an executable

Plutip provides a `local-cluster` executable. You can build it and run with nix package manager:
```
nix run github:mlabs-haskell/plutip#plutip-core:exe:local-cluster -- --help
```
Available options mostly match the `withFundedCluster` interface, see `--help` and [README](local-cluster/README.md).

### Via CTL

CTL is a purescript sdk for creating dapps. One of its features is ability to test contracts with private networks, see [plutip-testing](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutip-testing.md).

## Tutorials

* [Running disposable local network and building own runners](./local-cluster/README.md)
* [Running Contracts is REPL](./docs/interactive-plutip.md)

## Advanced network setup

* [Tweaking local network](./docs/tweaking-network.md)
* [Regenerating network configs](./docs/regenerate-network-configs.md)

## Examples

* [Starting private network from Haskell and executing contract](./contract-execution/Main.hs)
* [Template for setting a Nix flake that includes Plutip](https://github.com/MitchyCola/plutip-flake). Kudos to @MitchyCola

## Plutip for integration testing smart contracts

If your goal is to:
* build tests with `tasty` Haskell framework where user can run Plutus contracts (`Contract w s e a`) using mentioned above private network
* run contracts in REPL on local network

then check out [CTL](https://github.com/Plutonomicon/cardano-transaction-lib) or legacy plutip revision at branch `plutip-bpi`.

## Maintenance

* [Important notes on updating `cardano-wallet` dependency](./docs/cardano-wallet-update.md)
