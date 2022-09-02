# Plutip

A Cardano tool to spin up private network and run Plutus contracts on it

## Requirements

If your project is importing and making use of `Plutip`s library you will need to make sure that the following executables are present in your `PATH`:

- `cardano-cli` executable available in the environment
- `cardano-node` executable available in the environment

And the following ghc flag must to be set for the test execution: `-Wall -threaded -rtsopts`

NOTE: This branch launches local network in Vasil. Please use appropriate node version if running w/o Nix. It was tested with node `1.35.3`.

## Usage

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
