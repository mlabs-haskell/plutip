# Plutip

A Cardano tool to spin up private network and run Plutus contracts on it

## Requirements

Current version of `Plutip` requires some initial setup to be prepared to function properly:

- `cardano-cli` executable available in the environment
- `cardano-node` executable available in the environment

## Usage

Current version provides brief eDSL to spin up private network and execute `Contract`. Minimal example could be something like:

```haskell
runUsingCluster $ do -- spins up private local network
    testW1 <- addSomeWallet (ada 101) -- creates wallet and sends 101 Ada to it
    testW2 <- addSomeWallet (ada 202) -- creates wallet and sends 101 Ada to it
    waitSeconds 2 -- wait for wallet funding transactions to complete
    runContractTagged 
      "Pay wallet-to-wallet" -- short description of the Contract
      testW1 -- wallet that will act as "own wallet" (e.g., will provide own `PaymentPubKeyHash`)
      (payTo (ledgerPaymentPkh testW2) 10_000_000) -- `Contract` to execute

    where
      payTo :: PaymentPubKeyHash -> Integer -> Contract () EmptySchema Text CardanoTx
      payTo toPkh amt = do
        ownPkh <- ownPaymentPubKeyHash
        tx <- submitTx 
                (Constraints.mustPayToPubKey toPkh (Ada.lovelaceValueOf amt) 
                  <> Constraints.mustBeSignedBy ownPkh
                )
      void $ waitNSlots 1
      pure tx
```

`runContract` and `runContractTagged` return result of contract execution in form of `RunResult` which can be pretty printed to terminal with `report` function or used in asserions in tests (there is `isSuccess` function to check that execution did not fail).

If exception will be thrown during `Contract` execution, `RunResult` with error will be returned.

More examples could be found [here](example/Main.hs).

## Known limitations

At the moment underlying mechanisms that execute contract do not support `awaitTxConfirmed`. As one possible solution, `waitNSlots n` can be used instead. We are working on it.
