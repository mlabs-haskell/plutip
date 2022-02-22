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

`withCluster` will spin up the local cluster and create a test group with the contracts inside it.
The cluster is reused by all the test cases, but the wallets are isolated, so contracts won't
depend on each other (note that time related issues might still occur).

All assertions accept a name, some TestWallets and a contract.

At least one TestWallet is required, this will be used as the own wallet for the contract. Any other
wallets can be used as other parties in transactions.

A TestWallet can be initialised with any positive number of lovelace, using the `initAda` or
`initLovelace`. In addition, the value in these wallets can be asserted after the contract
execution with `initAdaAssertValue` or `initAndAssertAda`. For more, see `Test.Plutip.Contract`.

To reference the wallet inside the contract, the following callback function is used, when
supplying a contract toa test case: `[PaymentPubKeyHash] -> Contract w s e a`.
Note that `[PaymentPubKeyHash]` does not include the contract's own wallet, for that you can use `ownPaymentPubKeyHash` from `Plutus.Contract` inside the Contract monad.

More examples could be found [here](example/Main.hs).

## Known limitations

At the moment underlying mechanisms that execute contract do not support `awaitTxConfirmed`. As one possible solution, `waitNSlots n` can be used instead. We are working on it.
