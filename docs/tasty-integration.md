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
          (withContract $ \[_wallet2pkh] -> someContract) -- 3.2
          [ shouldSucceed -- 3.3
          ]
      ]
```

1. Will start local network with default config (more on configuring below)
2. Description of test group that will be run on current instance of network
3. Single "unit of work" performed on local network with it's description, which includes:
   1. Initialization of `wallets`. Two addresses will be funded: first will have 2 UTxOs with 100 and 200 Ada, second - single UTxO with 10 Ada.
   2. Execution of "`someContract :: Contract w s e a`". `PaymentPubKeyHash` will be accessible in `someContract` as "own PaymentPubKeyHash". e.g with `ownFirstPaymentPubKeyHash`. `PaymentPubKeyHash` of second initiated wallet is brought to scope by `_wallet2pkh`.
   3. Check (or predicate) that will be run for result returned by execution `someContract`
  