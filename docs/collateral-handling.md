# Collateral handling

Before running *any* contract `Plutip` under the hood creates dedicated UTxO at "own" wallet address to be used *only* as collateral. This UTxO is created by submitting transaction and spending "own" wallet's funds . For collateral `Plutip` always uses **10 Ada** at this point and if wallet address already has UTxO with 10 Ada on it, then Plutip will use it as collateral.

UTxO that was created or picked for collateral is stored in memory, so during Contract execution `Plutip` will always use this exact same UTxO. Collateral UTxO has special properties - to guard it from being consumed by accident it is not accessible from Contract. I.e. calls like `utxosAt` ***will not return*** UTxO used for collateral. This means, that users don't have to care really about collateral UTxO during contract execution.

The only place where collateral "sticks out" is the moment of wallet creation. Ii is also visible for `cardano-cli` queries.

## Cluster runner

With [cluster runners](../local-cluster/README.md), when creating wallet with `addSomeWallet [100_000_000]`, if you want to have UTxO with exactly 100 Ada while running the Contract, you should add 10 Ada more to wallet's initial distribution, or UTxO with 100 Ada will be used to create collateral.

E.g.:

```haskell
main :: IO ()
main = do
  let executeContract wallet contract =
     ask >>= \cEnv -> runContract cEnv wallet contract
     
  (st, _) <- startCluster def $ do
    w <- addSomeWallet [100_000_000, 10_000_000] -- 10 Ada will be used as collateral
    awaitWalletFunded w 1
    result <- executeContract w someContract
    doSomething result
  stopCluster st
```

Or just make helper function:

```haskell
addSomeWalletWithCollateral funds =
  addSomeWallet (toAda 10 : funds)
```

## Tasty integration

For collateral handling in tasty integration see [Collateral handling section](./tasty-integration.md#collateral-handling).
