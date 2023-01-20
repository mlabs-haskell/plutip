# Plutip

[![Hercules-ci][herc badge]][herc link]

[herc badge]: https://img.shields.io/badge/ci--by--hercules-green.svg
[herc link]: https://hercules-ci.com/github/mlabs-haskell/plutip

Plutip is Cardano tool for spawning local clusters.
Use it to start up disposable private network with arbitrary amount of funded addresses (providing keys for that addresses as well).

## Requirements

Best way of building and launching Plutip libraries is using `Nix` and `cabal`. E.g. to start local network with two funded addresses run

```bash
nix build .#plutip:exe:local-cluster  
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

then check out [CTL](https://github.com/Plutonomicon/cardano-transaction-lib) or legacy plutip revision.

## Maintenance

* [Important notes on updating `cardano-wallet` dependency](./docs/cardano-wallet-update.md)
