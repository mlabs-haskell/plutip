# Plutip

Plutip is Cardano tool that aims to help dApp developers with integration testing and contracts debugging.

Plutip can serve several purposes:

* start up disposable private network with arbitrary amount of funded addresses (providing keys for that addresses as well)
* build tests with `tasty` Haskell framework where user can run Plutus contracts (`Contract w s e a`) using mentioned above private network

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

* [Running disposable local network](./local-cluster/README.md)


----- TO DO Line -----
Plutip provides a tasty interface for executing Plutus contracts on a local cluster.
depend on each other.

```haskell
tests :: TestTree
tests =
  withCluster
    "Integration tests"
    [ assertExecution "Get utxos" (initAndAssertAda 100 100)
        (withContract $ const GetUtxos.getUtxos)
        [shouldSucceed]
    , assertExecution "Throws Contract error" (initAda 100)
        (withContract $ const GetUtxos.getUtxosThrowsErr)
        [shouldFail]
    , assertExecution "Throws Exception" (initAda 100)
        (withContract $ const GetUtxos.getUtxosThrowsEx)
        [shouldFail]
    , assertExecution
        "Pay wallet-to-wallet"
        (initAda 300 <> initAndAssertAda 100 110)
        (withContract $ \[w1] ->
          PayToWallet.payTo (ledgerPaymentPkh w1) 10_000_000)
        [shouldSuceed]
    , assertExecution
        "Lock at script then spend - budget overspend"
        (initAda 100)
        (withContract $
          const LockUnlock.lockThenSpend)
        [shouldFail]
    , assertExecutionWith
        [ShowTrace, ShowBudgets]
        "Lock at script then spend - validation fail"
        (initAda 100)
        (withContract $
          const LockUnlockValidationFail.lockThenSpend)
        [shouldFail]
    ]
```

For more, see `Test.Plutip.LocalCluster` `Test.Plutip.Contract`.

More examples could be found [here](test/Spec/Integration.hs).
