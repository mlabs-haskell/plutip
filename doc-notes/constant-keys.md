# Providing constant keys to contracts

At the moment, when user adds new wallets during tasty test with `initAda...` or `initLovelace...`, or during interactive cluster usage with `addSomeWallet`, Plutip will generate unique `PubKey`(s) every time. Although some time users need to have constant keys, which will produce same `PubKeyHash` and `Address` every run, e.g. if `PaymentPubKeyHash` is hardcoded in validator. For this case Plutip provides mechanism to add constant keys.

First user will need to generate those keys. It can be done in several ways:

* Use simple test: run test via `withConfiguredCluster` and provide [fixed working directory](../src/Test/Plutip/Config.hs#L19) with `shouldKeep = True` in `PlutipConfig`. `initAda...` and `Contract` returning `()` will be enough. When test finish, you will be able to pick generated keys from specified directory from `bot-plutus-interface` subdir. Move them to separate location, as working directory will be wiped at next cluster launch.
* Start cluster like in the [example](../local-cluster/Main.hs), use `cardano-cli` to generate keys. NOTE: If you want to get address as well with `cardano-cli` you will need to use `--mainnet` as network option.
* Generate `SigningKey` using `Cardano.Api`.

When you will get the keys, you can set them via [PlutipConfig.extraSigners](../src/Test/Plutip/Config.hs#L42).
