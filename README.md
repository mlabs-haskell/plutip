# Plutip

Plutip is Cardano tool that aims to help dApp developers with integration testing and contracts debugging.

Plutip can serve several purposes:

* start up disposable private network with arbitrary amount of funded addresses (providing keys for that addresses as well)
* build tests with `tasty` Haskell framework where user can run Plutus contracts (`Contract w s e a`) using mentioned above private network
* with a bit of additional instrumentation run contracts in REPL on local network

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

NOTE: This branch launches local network in `Vasil`. It was tested with node `1.35.3` (this node version used in nix environment as well). Please use appropriate node version when setting up own binaries in `PATH`.

## Tutorials

* [Running disposable local network and building own runners](./local-cluster/README.md)
* [Tasty integration](./docs/tasty-integration.md)
* [Running Contracts is REPL](./docs/interactive-plutip.md)
* [Providing constant keys](./docs/constant-keys.md)
* [Tweaking local network](./docs/tweaking-network.md)
