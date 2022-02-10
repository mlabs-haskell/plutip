# Channels of info about contract being run

## Node logs
During run, both logs for BFT and relay nodes are available in temp directory. See [example](cardano-node-example.log), so we can pick what to trace in [node config](../cluster-data/cardano-node-shelley/node.config) and parse logs

Although, doc for `withLoggingNamed` says:

```
Run an action with logging available and configured. When the action is finished (normally or otherwise), log messages are flushed.
```

Is there a chance that we'll have to check how everything went only after cluster stopped?

Or we can add log checking action after `withLoggingNamed` inside our `runUsingCluster'`.

## What we can get now after `runContract` was executed
- `runContract` of `bot-plutus-interface` returns `Ether e a` of `Contract`, so we can get:
  - return value of `Contract`
  - Error thrown by `Contract`

- `runContract` of DSL captures expecptions, so it can return errors thrown by `cardano-cli` during bot logic run (a well as all other exceptions) in `RunResult`


## Thoughts
Will we ever see TX submission fail with `bot-interface`? Probably, any submission error will be caught and thrown by `cardano-cli` inside `bot interface`

How are we going to process `cardano-cli` errors, e.g. when validation fails, like this one:
```
Command failed: transaction build  Error: The following scripts have execution failures:
the script for transaction input 0 (in the order of the TxIds) failed with: 
The Plutus script evaluation failed: An error has occurred:  User error:
The provided Plutus code called 'error'.
Script debugging logs: Some fail message
PT5
```
Failed call to `cardano-cli` will be returned as `RunResult`, but it will be something like:
```
RunFailed {reason = OtherErr "readCreateProcess: cardano-cli \"transaction\" \"build\" \"--alonzo-era\" \"--tx-in\" \"5ed2fac098f5829154bf75a85a22166b2bbbdd5821da4791d474a3b4c59c359e#1\" \"--tx-in-script-file\" \"/tmp/nix-shell.WvINXH/test-cluster-12c86716c56edd5d/bot-plutus-interface/result-scripts/validator-97f74220cf779c781c86add56019cedaf365abcbe985b4222ac5d162.plutus\" \"--tx-in-datum-file\" \"/tmp/nix-shell.WvINXH/test-cluster-12c86716c56edd5d/bot-plutus-interface/result-scripts/datum-923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec.json\" \"--tx-in-redeemer-file\" \"/tmp/nix-shell.WvINXH/test-cluster-12c86716c56edd5d/bot-plutus-interface/result-scripts/redeemer-923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec.json\" \"--tx-in-collateral\" \"5ed2fac098f5829154bf75a85a22166b2bbbdd5821da4791d474a3b4c59c359e#0\" \"--required-signer\" \"/tmp/nix-shell.WvINXH/test-cluster-12c86716c56edd5d/bot-plutus-interface/signing-keys/signing-key-ac6cc9375be49d4b3dffad9e01575f6cd44783bc8144ba340da28f3b.skey\" \"--change-address\" \"addr1vxkxejfht0jf6jeal7keuq2htakdg3urhjq5fw35pk3g7wcd9v3ra\" \"--mainnet\" \"--protocol-params-file\" \"/tmp/nix-shell.WvINXH/test-cluster-12c86716c56edd5d/bot-plutus-interface/pparams.json\" \"--out-file\" \"/tmp/nix-shell.WvINXH/test-cluster-12c86716c56edd5d/bot-plutus-interface/txs/tx-a5e838379a7b3da7fdda2ea280a85e3e8aeb573ddb0b2f00e5e4c7467f3afa7d.raw\" (exit 1): failed"}
```

## Node logs notes
