
> ## For vasil hard-fork see `vasil-hardfork.md`

# Plutip

A Cardano tool to spin up private network and run Plutus contracts on it

## Requirements

If your project is importing and making use of `Plutip`s library you will need to make sure that the following executables are present in your `PATH`:

- `cardano-cli` executable available in the environment
- `cardano-node` executable available in the environment

And the following ghc flag must to be set for the test execution: `-Wall -threaded -rtsopts`

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

## Known limitations

At the moment underlying mechanisms that execute contract do not support `awaitTxConfirmed`. As one possible solution, `waitNSlots n` can be used instead. We are working on it.
