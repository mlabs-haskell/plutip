# Channels of info about contract being run

## Node logs
During run, both logs for BFT and relay nodes are available in temp directory. See [example](cardano-node-example.log), so we can parse them

Although, doc for `withLoggingNamed` says:

```
Run an action with logging available and configured. When the action is finished (normally or otherwise), log messages are flushed.
```

Is there a chance that we'll have to check how everything went only after cluster stopped?

Or we can add log checking action after `withLoggingNamed` inside our `runUsingCluster'`.

## What we can get now after `runContract` was executed
- `runContract` of `bot-plutus-interface` returns `Ether e a` of `Contract`, so:
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

## Node logs notes
