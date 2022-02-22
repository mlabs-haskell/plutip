# Plutip

A Cardano tool to spin up private network and run Plutus contracts on it

## Requirements

Current version of `Plutip` requires some initial setup to be prepared to function properly:

- `cardano-cli` executable available in the environment
- `cardano-node` executable available in the environment
- data required by local cluster in `./cluster-data` directory

## Usage

Plutip provides a tasty interface for executing Plutus contracts on a local cluster.
depend on each other.

```haskell
tests :: TestTree
tests =
  withCluster
    "Integration tests"
    [ shouldSucceed "Get utxos" (initAndAssertAda 100 100) $ const GetUtxos.getUtxos
    , shouldFail "Throws Contract error" (initAda 100) $ const GetUtxos.getUtxosThrowsErr
    , shouldFail "Throws Exception" (initAda 100) $ const GetUtxos.getUtxosThrowsEx
    , shouldSucceed
        "Pay wallet-to-wallet"
        (initAda 300 <> initAndAssertAda 100 110)
        $ \[w1] ->
          PayToWallet.payTo (ledgerPaymentPkh w1) 10_000_000
    , shouldFail "Lock at script then spend - budget overspend" (initAda 100) $
        const LockUnlock.lockThenSpend
    , shouldFail "Lock at script then spend - validation fail" (initAda 100) $ const LockUnlockValidationFail.lockThenSpend
    ]
```

For more, see `Test.Plutip.LocalCluster` `Test.Plutip.Contract`.

More examples could be found [here](example/Main.hs).

## Known limitations

At the moment underlying mechanisms that execute contract do not support `awaitTxConfirmed`. As one possible solution, `waitNSlots n` can be used instead. We are working on it.
