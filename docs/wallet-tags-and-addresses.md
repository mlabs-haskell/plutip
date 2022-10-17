# Address types and `WalletTag`

There are [several types of addresses](https://docs.cardano.org/learn/cardano-addresses) on Cardano network. Currently Plutip supports two types of addresses in both [interactive](./interactive-plutip.md) and [tasty](tasty-integration.md) modes:

* ***Enterprise Address*** - carry no stake rights, backed only by payment keys
* ***Base Address*** - directly specifies the staking key, backed by both payment and staking keys

To pick which type of address to create for wallet, Plutip provides `WalletTag`. `WalletTag` has two constructors:

* `BaseTag Text` - will create wallet with `Base Address`
* `EntTag Text` - will create wallet with `Enterprise Address`

Wallet tag also gives wallet it's name (the `Text` argument of constructor) and can be used to lookup desired wallet by name. There is special functionality for `WalletTag` available tasty framework, but lets see simple example with local cluster eDSL first.

## `WalletTag` in interactive mode and local cluster

Whenever you use eDSL function like `addSomeWallet` ([example 1](interactive-plutip.md), [example 2](../local-cluster/README.md)), from now on you can specify `WalletTag` as first argument to pick what type of address wallet will have. The `Text` argument of constructor will be added as textual tag to created wallet. E.g.:

```haskell
main :: IO ()
main = do
  (st, _) <- startCluster def $ do
    wallet1 <- addSomeWallet (BaseTag "wallet1") [100_000_000]
    waitSeconds 2
  stopCluster st
```

`wallet1` has type `BpiWallet`, and textual tag can be obtained with

```haskell
wTag :: Text
wTag = bwTag wallet1
```

It could useful if you want to print some info about wallet and distinguish output by tag, or when you initialize several wallets with something like `mapM` - can use tags to enable some lookups:

```haskell
  let mkTag idx = EntTag $ T.pack $ "wallet" <> show idx
  wallets <- 
    for [0..42] $ \idx ->
      addSomeWalletDir (mkTag idx) [100_000_000]
  ...
  let maybeMyWallet = find ((== "wallet13") . bwTag) wallets
```

## `WalletTag` and tasty Plutip

In tasty integration it is also possible to pick address type now with functions like `initAda`. And on top of that new lookup system was added, that lets you find initialized wallets easier (no more pattern matching on lists and figuring out correct indexes):

```haskell
assertExecution
  "Some test"
  ( initAndAda (EntTag "w0") [100]
    <> initAnd (EntTag "w1") [100]
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
