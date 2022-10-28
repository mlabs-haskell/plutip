# Address types

There are [several types of addresses](https://docs.cardano.org/learn/cardano-addresses) on Cardano network. Currently Plutip supports two types of addresses in both [interactive](./interactive-plutip.md) and [tasty](tasty-integration.md) modes:

* ***Enterprise Address*** - carry no stake rights, backed only by payment keys
* ***Base Address*** - directly specifies the staking key, backed by both payment and staking keys

To pick which type of address to create for wallet, Plutip provides some options depending on what type of tool user want to use - local cluster eDSL or tasty integration.

## Setting address type for local cluster runners

As shown in [conctract-execution example](../contract-execution/Main.hs) or in [interactive plutip tutorial](./interactive-plutip.md) new wallet can be added to the local cluster with `addSomeWallet` or `eitherAddSomeWalletDir` function. As first argument this functions accepts `AddressType` type of argument, which have to constructions:

* `Base` for Base Address
* `Enterprise` for EnterpriseAddress

E.g.:

```haskell
main :: IO ()
main = do
  (st, _) <- startCluster def $ do
    w <- addSomeWallet Enterprise [10_000_000]
    awaitWalletFunded w 1
    doSomething w
  stopCluster st
```

Creation of staking keys for Base Address will be handled automatically. Staking keys will be stored in the same directory as verification keys.

## Setting address type in tasty integration

In case of tasty integration, type of address is determined by `WalletTag` when initializing wallets with `init...` functions like `initAda`. `WalletTag` has two constructors:

* `BaseTag Text` - will create wallet with `Base Address`
* `EntTag Text` - will create wallet with `Enterprise Address`

Besides determining address type, tag gives test wallet it's name (the `Text` argument of constructor) and can be used to lookup desired wallet by name via special lookups machinery. E.g.:

```haskell
assertExecution
  "Some test"
  ( initAda (EntTag "w0") [100]
    <> initAda (EntTag "w1") [100]
  )
  ( do
  void $ withContract $ \ws -> do
    -- lookup wallet by it's tag
    EntWallet pkh0 <- lookupWallet ws (EntTag "w0")
    payToPubKey pkh0 10_000_000
  -- select wallet with tag "w1" to be "own" wallet
  withContractAs "w1" $ \ws -> do
    -- lookup address by wallet tag
    addr1 <- lookupAddress ws "w1"
    payToPubKeyAddress addr1 10_000_000
  )
  [shouldSucceed]

```

For more examples see [tasty integration tutorial](./tasty-integration.md).
