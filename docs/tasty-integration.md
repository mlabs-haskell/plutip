# Tasty integration

Plutip `tasty` integration provides eDSL for building test suites that are capable to start local private network, run contracts on that network and then make assertions about contract execution results.

Simple test case which starts private network, funds some addresses (or `wallets` in eDSL terminology) and runs `Contract`'s can be described like this:

```haskell
test :: TestTree
test =
  withConfiguredCluster def -- 1
    "Basic integration: launch, add wallet, tx from wallet to wallet" -- 2
    $ [ assertExecution "Contract 1" -- 3
          (initAda [100,200] <> initLovelace 10_000_000) -- 3.1
          (withContract $ \[wallet2pkh] -> someContract) -- 3.2
          [ shouldSucceed -- 3.3
          ]
      ]
```

1. Will start local network with default config (more on configuring below)
2. Description of test group that will be run on current instance of network
3. Test scenario that will be performed on local network with it's description. Scenario includes:
   1. (3.1.) Initialization of `wallets`. Two addresses will be funded: first will have 2 UTxOs with 100 and 200 Ada, second - single UTxO with 10 Ada.
   2. (3.2) Execution of "`someContract :: Contract w s e a`". `PaymentPubKeyHash` of *first wallet* will be accessible in `someContract` as "own PaymentPubKeyHash". e.g with `ownFirstPaymentPubKeyHash`. `PaymentPubKeyHash` of *second* initiated wallet is brought to scope by `wallet2pkh` during pattern match on list (more on that below).
   3. (3.3) Check (or predicate) that will be run for result returned by execution `someContract`.

## Initializing wallets

It is possible to initialize arbitrary number of `wallets` in second argument of `assertExecution`. `PaymentPubKeyHash` of 1st wallet will always be used as "own" `PaymentPubKeyHash` for contract being executed inside `withContract`. `PaymentPubKeyHash`'es of rest `wallets` could be accessed via 1st argument of lambda inside `withContract`.

E.g. if `wallets` initialized like

```haskell
(initAda [100] <> initAda [200] <> initAda [300])
```

we will get 3 funded addresses represented by 3 corresponding `wallets`: 

* `PaymentPubKeyHash` of wallet `initAda [100]` will be "own" `PaymentPubKeyHash` for contract executed it test case.
* `PaymentPubKeyHash` of `wallets` `initAda [200]` and `initAda [300]` will be available via lambda argument. I.e.:


```haskell
withContract $ \[pkh1, pkh2] -> someContract
```

where

* `pkh1` is `PaymentPubKeyHash` of `wallet` `initAda [200]`
* `pkh2` is `PaymentPubKeyHash` of `wallet` `initAda [300]`

`PaymentPubKeyHash` of `wallet` `initAda [100]` is meant to be `pkh0` and not presented in the list.

## Executing contracts

It is possible to run arbitrary number of contracts in 3d argument of `assertExecution` using it monadic nature. E.g.:

```haskell
  assertExecution "Some description"
          ( initAda [100])
          ( do
              void $
                withContract $
                  \pkhs -> contract1
              withContractAs 1 $
                \pkhs -> contract2
          )
          [shouldSucceed]
```

Be aware though, that only result of last contract will be asserted in `shouldSucceed` (note usage of `void` for `contract1`). This pattern where `contract1` is executed before `contract2` can be useful if `contract2` requires some on-chain initialization before execution.

### Changing own `PaymentPubKeyHash`

There is no way to rearrange `wallets` after initialization, but test scenario (e.g.) can require running some setup contracts with different wallets. To make it possible `withContractAs` can be used instead of `withContract`. As first argument `withContractAs` accepts index of `wallet` in initial setup, `PaymentPubKeyHash` of this wallet will be used as "own" `PaymentPubKeyHash` inside the contract.

For example, tets describe following scenario:

* initiate 3 `wallets`: `walletA`, `walletB`, `walletC`
* run some setup contract with `walletB`
* run another setup contract with `walletC`
* run final contract with `walletA` and assert it's result

```haskell
  assertExecution "Some description"
          ( initAda [100] -- walletA
            <> initAda [200] -- walletB
            <> initAda [300] -- walletC
          )
          ( do
              void $
                withContractAs 1 $ -- running contract with walletB
                  \[walletA_PKH, walletC_PKH] -> setupContract1
              void $
                withContractAs 2 $  -- running contract with walletC
                  \[walletA_PKH, walletB_PKH] -> setupContract2
              withContract $
                \pkhs -> theContract
          )
          [shouldSucceed]
```

Under the hood, test runner builds list of wallets like this `[walletA, walletB, walletC]` and by calling `withContractAs` we can refer to an index (0 based) of specific wallet in this list. In that case, `PaymentPubKeyHash` of referenced `wallet` becomes "own" `PaymentPubKeyHash` of the contract, and `PaymentPubKeyHash`'s in argument of lambda  will be rearranged. E.g. i case of  `withContractAs 1`:

* `PaymentPubKeyHash` of `walletB` will become own `PaymentPubKeyHash`
* argument of lambada will contain `PaymentPubKeyHash`'es of `walletA` and `walletC`.

Actually, `withContract` is just shortcut for `withContractAs 0`.

## Assertions

To assert the result of contract execution user specifies list of checks or `predicates` as 4th argument of `assertExecution`. There are several `predicates` provided by the library that could be found in `Test.Plutip.Contract` module. Existing `predicates` allows to make assertions on Contracts state (`w`), error (`e`) and result (`a`) (consider type `Contract w s e a`). 

There are also predicates for making assertions on scripts execution budgets (e.g. `budgetsFitUnder` or `assertOverallBudget`). But be aware, that budget of script submitted to private network can differ from testnet or mainnet, at least because different amount of input UTxOs could be added during balancing, so this assertions are mostly useful for rough estimation and regression testing.

Users can also define their own predicates by making instances of `Predicate` type from `Test.Plutip.Contract` module.

Each `predicate` will be rendered as separate test case in output log.

### Asserting `wallet` final `Value`

To assert the final `Value` which `wallet` will have after contract execution special syntax for `wallet` initialization is required. Currently it is done by group of "init-and-assert" functions from `Test.Plutip.Contract` module. E.g.:

* `initAdaAssertValue [100] 133` - initialize `wallet` with single UTxO with 100 Ada and check that after contract execution final `Value` of all `wallet`'s UTxOs is equal to 133 Ada.
* `initAndAssertLovelaceWith [1_000_000] VGt 2_000_000` - initialize `wallet` with single UTxO with 1000000 Lovelace and check that after contract execution final `Value` of all `wallet`'s UTxOs is *greater than* 2000000 Lovelace.

***One important note*** is that Plutip handles creation of collateral UTxO under the hood. So when using assertions for `Value` it is advised to wrap `wallet`'s initialization with `withCollateral` function. E.g.:

```haskell
( withCollateral $
    initAndAssertLovelace [111] 555
    <> initAndAssertLovelace [222] 666
    <> initAndAssertLovelace [333] 777
)
```

`withCollateral` will initiate `wallet`'s with collateral UTxO and make sure that `Value` assertions will work in expected way.

## Tracing

Plutip test runner has ability to trace contract execution and provide collected information in test logs.

To enable tracing test scenario should start with `assertExecutionWith` instead of `assertExecution`. As 1st argument `assertExecutionWith` accepts list of trace options of type `TraceOption`.

E.g., scenario like this

```haskell
        assertExecutionWith
          [ShowBudgets]
          "Lock then spend contract"
          (initAda (replicate 3 300))
          (withContract $ const lockThenSpend)
          [ shouldSucceed
          ]
```

will log out something like

```bash
    Lock then spend contract
      Contract should succeed: OK
      Budget stats:            OK
        Budget for TxId 8b0182f0d178d1ce2e50d50eeb060e54025d41d52cf879a7b378434a3cedfdd5
         Empty
        Budget for TxId 965a9e2a713c1d9665be1a9e196e8e158a46ce6b3a440d093d762df74f6d0982
         TxOutRef dc551ff9b26c4dd5e30e9b608801caa9b6d660d90daac1271aedd9ed754e4374!0
          (cpu 406250690 | mem 1016102)
         TxOutRef dc551ff9b26c4dd5e30e9b608801caa9b6d660d90daac1271aedd9ed754e4374!1
          (cpu 295390828 | mem 659842)
         PolicyHash a28a933e16146b7f9acc863e557dc05c9d72e515459a18d7da9a1810
          (cpu 405210181 | mem 1019024)
        
        Budget for TxId e949c1965a7d55156b713e1ee5ef55a9d708eaa2d1dfa1abf8b3331b815f334c
         Empty
```

`TraceOption` has 3 options now:

* collect and show execution budgets
* show all possible logs (including `Contract` logs, contract interpretation debug logs, balancing logs, coin selection logs and many more)
* configurable option, where log level and type of desired logs can be specified (for more details see `LogContext` and `LogLevel` from `BotPlutusInterface.Types` module)

## Caveats

`withConfiguredCluster` launches single instance of private network which keeps producing blocks till all tests done. There is no isolation between `assertExecution` calls, all contracts run on same network, using same chain of blocks. If several `assertExecution` will use exact same script under single `withConfiguredCluster`, address of that script won't be "cleared" from UTxOs between `assertExecution` calls, no blocks will be "rolled back".

## Configuring network runner

`withConfiguredCluster` accepts `PlutipConfig` as first argument. While some of the options made for Plutip debugging, there are couple of them that can be useful during testing:

* `relayNodeLogs :: Maybe FilePath` - if set to `Just path` will save node logs to specified directory after test run finishes
* `chainIndexPort :: Maybe Natural` - run `chain-index` on custom port, default is `9083`
* `clusterWorkingDir` - by default all network related files (node logs, sockets, databases, genesis files and etc.) are stored in temporary directory which will be wiped out after private network stops. With this option it is possible to set custom directory that won't be cleared after private networks stops (but it still will be cleared on the next launch before network starts).

## More examples

Plutip uses tasty integration for it's own testing as well, so more examples can be found in [Plutip's integration spec](../test/Spec/Integration.hs).
