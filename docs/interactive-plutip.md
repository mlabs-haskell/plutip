# Running contracts in an executable and interactive environment with Plutip

by [Chase](https://github.com/TotallyNotChase)

This article will showcase how you can utilize [Plutip](https://github.com/mlabs-haskell/plutip) to effortlessly run contracts in an executable environment. Which may be helpful for presenting/recording a demo for your project, or for debugging a contract flow, testing edge cases manually, and even completely interactively in the repl!

## The basic executable environment

Firstly, we'll need a few functions that are exported from Plutip's internal modules. These will allow us to spin up the local node and set up everything - the same way it works for your Plutip tests!

```haskell
import Test.Plutip.Internal.LocalCluster (startCluster, stopCluster)
```

Here's the types for those functions:

```haskell
startCluster :: PlutipConfig -> ReaderT ClusterEnv IO a -> IO (TVar (ClusterStatus a), a)

stopCluster :: TVar (ClusterStatus a) -> IO ()
```

Let's declutter it a bit by monomorphizing the `a`. Since we're using Plutip with [BPI (bot-plutus-interface)](https://github.com/mlabs-haskell/bot-plutus-interface) (i.e Haskell written contracts), and we'll be using a single wallet to run all contracts in this executable environment, `a` should be: `(ClusterEnv, BpiWallet)`

So, given a config and a setup function (we'll get to this shortly), `startCluster` yields a `TVar` which you can use to gracefully stop the local node and related services. It also returns a pair containing the `ClusterEnv` and a `BpiWallet`. The `ClusterEnv` is necessary configuration to run contracts, and `BpiWallet` is the newly generated **own wallet**. That is, this is the wallet that you should use to run the transactions.

Now, we need to get to the aforementioned setup function. Simply put, this function allows you to do run some setup transactions after the node is setup and return any value that will be of use later.

Of course, in this case, we'll just use the `setup` function to generate a wallet and fund it with some ada. Then we'll return the `ClusterEnv` and the wallet that you'll be able to use later.

```haskell
import Test.Plutip.Internal.BotPlutusInterface.Wallet (BpiWallet, addSomeWallet)
import Test.Plutip.LocalCluster (waitSeconds)
import Test.Plutip.Internal.Types

setup :: ReaderT ClusterEnv IO (ClusterEnv, BpiWallet)
setup = do
  env <- ask
  -- Gotta have all those utxos for the collaterals.
  ownWallet <- addWalletWithAdas $ 100 : replicate 20 10
  -- Wait for faucet funds to be added.
  waitSeconds 2
  pure (env, ownWallet)

addWalletWithAdas :: [Ada] -> ReaderT ClusterEnv IO (BpiWallet k)
addWalletWithAdas funds = addSomeWallet $ testWallet'
  (map (fromInteger . Ada.toLovelace) funds)
  Nothing
  (EntTag ())
```

> Aside: Feel free to choose the amount of ada you want to fund your wallet with. Just remember: `addSomeWallet` takes a list of _lovelace_ amounts. Here, I've actually made my custom `Ada` type as well some helper utilities (not the same as `Plutus.V1.Ledger.Ada` as that is removed in newer `plutus-ledger-api` versions).

As promised: just creating one wallet and funding it with ada, that's all!

Now, you can choose the `PlutipConfig` as you prefer, we'll just be using `def` from `Data.Default` (the default config) in this example:

```haskell
main :: IO ()
main = do
  -- Start the node.
  (clusterStat, (cEnv, ownWallet)) <- startCluster def setup

  -- Do stuff.

  -- Stop the node.
  stopCluster clusterStat
```

That's what the whole setup and teardown dance looks like! But what about actually running contracts?

That's just as simple! First, you need a contract of course:

```haskell
import qualified Plutus.Contract as Contract
import qualified Ledger.Constraints as Constraints

-- | Pay 5 ada to yourself ...very useful thing to do
payMyself :: AsContractError e => Contract w s e ()
payMyself = do
  ownPkh <- Contract.ownPaymentPubKeyHash
  let tx = Constraints.mustPayToPubKey ownPkh $ Ada.toValue 5
  ledgerTx <- Contract.submitTxConstraintsWith @Void mempty tx
  void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
```

> Aside: Remember that your `Contract` may be comprised of several transactions. In fact, if you're using this to present a demo, for example, I recommend having a single function: `demoFlow`, that yields a `Contract` monad and does all the transactions necessary for the whole flow!

Once you have that, you can simply use `runContract` from `import Test.Plutip.Internal.BotPlutusInterface.Run`:

```haskell
runContract ::
  (ToJSON w, Monoid w, MonadIO m) =>
  ClusterEnv ->
  BpiWallet k ->
  Contract w s e a ->
  m (ExecutionResult w e a)
```

There it is! It simply takes a `Contract`. But what about the other two arguments? Well, the `ClusterEnv` is simply the one you obtained before from `startCluster`, and so is `BpiWallet`! `runContract` needs to know what wallet is running the contract and therefore submitting the transactions - so we use the wallet we created exactly for this purpose.

> Aside: `runContract` may raise ambiguous type variable errors if your `Contract` also uses type variables in place of its type parameters (`w`, `s`, `e`, `a`). You can use type applications on your `runContract` to specify any valid types. I often use `()` for `w`, `EmptySchema` for `s`, and `Text` for `e`.

In the end, it yields the `ExecutionResult`, which is [defined like so](https://github.com/mlabs-haskell/plutip/blob/017fbaf9a080c592a8882abd03e331c1a05be3f2/src/Test/Plutip/Internal/Types.hs#L43):

```haskell
data ExecutionResult w e a = ExecutionResult
  { -- | outcome of running contract.
    outcome :: Either (FailureReason e) a
  , -- | stats returned by bot interface after contract being run
    txStats :: ContractStats
  , -- | `Contract` observable state after execution (or up to the point where it failed)
    contractState :: w
  }
  deriving stock (Show)
```

More often than not, you'll only be interested in the `outcome` field. This simply contains the result returned by your `Contract` in case of success, and a reason for failure in case of failure:

```haskell
data FailureReason e
  = -- | error thrown by `Contract` (via `throwError`)
    ContractExecutionError e
  | -- | exception caught during contract execution
    CaughtException SomeException
  deriving stock (Show)
```

The `e` here will be same as the `e` used by your `Contract`. In our example, we haven't chosen a concrete `e`, so we'll just use a type application to set it to `Text`.

Now, let's run that contract from above!

```haskell
ExecutionResult exOutcome _ _ <- runContract @() @EmptySchema @Text cEnv ownWallet payMyself
```

As mentioned before, you'll usually be interested in the `outcome` only; so we bind it to `exOutcome` and handle it as necessary:

```haskell
case exOutcome of
  Left (ContractExecutionError e) -> putStrLn "Contract failed" >> print e
  Left (CaughtException e) -> putStrLn "Unexpected exception" >> print e
  Right _ -> pure ()
```

Feel free to handle the outcome as it makes sense for your contract!

Assembling all of that together, your `main` should look like:

```haskell
main :: IO ()
main = do
  -- Start the node.
  (clusterStat, (cEnv, ownWallet)) <- startCluster def setup

  -- Do stuff.
  ExecutionResult exOutcome _ _ <- runContract @() @EmptySchema @Text cEnv ownWallet payMyself
  case exOutcome of
    Left (ContractExecutionError e) -> putStrLn "Contract failed" >> print e
    Left (CaughtException e) -> putStrLn "Unexpected exception" >> print e
    Right _ -> pure ()

  -- Stop the node.
  stopCluster clusterStat
```

## The interactive executable environment

Alright, you now know how to utilize Plutip outside of just tests. Can we take this further? Could we do all of this in ghci? That'd facilitate easy debugging of contracts because you can simply write, modify and run contracts on demand - all in the repl!

As a matter of fact, that's not too different from what you've seen above. All you really have to do is:

- Run `startCluster` with the necessary arguments in the repl and bind its results.
- Run whatever contracts you want using `runContract` and play with the results in the repl.
- When you're done, use `stopCluster`.

I like to short circuit some of this work with utility functions:

```haskell
import Test.Plutip.Contract.Types (TestContractConstraints)

newtype ContractRunner = ContrRunner
  { runContr ::
      forall w e a.
      TestContractConstraints w e Int a =>
      Contract w EmptySchema e a ->
      IO (Either (FailureReason e) a)
  }

begin :: IO (ContractRunner, IO ())
begin = do
  (clusterStat, (cEnv, ownWallet)) <- startCluster def setup
  pure (ContrRunner $ fmap outcome . runContract cEnv ownWallet, stopCluster clusterStat)
  where
    setup = do
      env <- ask
      -- Gotta have all those utxos for the collaterals.
      ownWallet <- addWalletWithAdas $ testWallet' (300 :  replicate 50 10) Nothing (EntTag 0)
      -- Wait for faucet funds to be added.
      waitSeconds 2
      pure (env, ownWallet)
```

This function starts the cluster, and yields 2 functions for you to use in the repl. One of them is just `runContract` with the `cEnv` and `ownWallet` pre-set, and the `ExOutcome` result is mapped to just the `outcome` field. The second function returned by `begin` is just `stopCluster` with the `clusterStat` pre-set.

This means you can effectively use it like so in your `cabal repl`:

```haskell
> (ContrRunner{runContr}, end) <- begin
```

Now, you can use `runContr` to run contracts on demand in the repl however you want and whenever you want:

```haskell
> runContr @() @EmptySchema @Text payMyself
Right ()
```

And once you're all done, you can simply type in end to stop the cluster:

```haskell
> end
```

That's it!

## Troubleshooting

### No UTxO to be used as collateral

This isn't directly related to this guide, however - you might encounter [this issue](https://github.com/mlabs-haskell/bot-plutus-interface/issues/89) while running your contracts either in the executable environment, or, more likely: when playing around in the interactive environment.

As a workaround, you can simply run a separate `distributeAda` transaction in between your larger transactions, which simply creates a bunch of small Ada only UTxOs.

```haskell
import qualified Ledger.Constraints as Constraints
import Plutus.Contract
import qualified Plutus.Contract as Contract
import Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Value as Value

adaToValue :: Integer -> Value
adaToValue x = Value.singleton adaSymbol adaToken lovelaceAmount
  where
    lovelaceAmount = x * 1_000_000

distributeAda :: AsContractError e => [Integer] -> Contract w s e ()
distributeAda amounts = do
  ownPkh <- Contract.ownPaymentPubKeyHash
  let tx = foldMap (Constraints.mustPayToPubKey ownPkh . adaToValue) amounts
  ledgerTx <- Contract.submitTxConstraintsWith @Void mempty tx
  void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
```
