# Starting local cluster with chain-index

This example starts a local cluster (BFT node + relay node) and by default adds a single wallet with some Ada on it's address.

As long as the cluster is not stopped, the relay node can be used for arbitrary actions that require network communication.

The node socket path can be obtained from console output.

NOTE: This branch launches local network in Vasil. Please use appropriate node version if running w/o Nix. It was tested with node `1.35.3`.

The `Main.hs` module can also serve as an example of how to make your own executable for starting local cluster with funded wallets. Note that when wallet is added with `addSomeWallet` it is recommended to wait some time (1 or 2 seconds) with `waitSeconds` while funding transaction is sent and confirmed.

## Usage 

The local cluster can be started by running

```
cabal run local-cluster -- <arguments>
```

When you see in terminal message like "`Cluster is running. Press Enter to stop.`" it means that local cluster started successfully and all desired wallets created and funded.

### Available arguments

```
--wallets NUM
-n NUM
```

This creates `NUM` wallets and saves info about them into a default directory
and also to a user-specified directory if the `--wallet-dir` argument is used.
This defaults to 1 if not specified, and can be set to 0 if you do not wish to
create any wallets.

```
--wallet-dir /path/
-d /path/
```

If specified, this saves the wallet information to an extra directory specified by
the user. This is useful if you wish to have wallet information easily accessible
by other code.

```
--ada AMOUNT
-a AMOUNT
```

This puts `AMOUNT` Ada into each UTxO in every wallet created. This defaults to
10,000 ADA. If you wish to specify an additional amount in Lovelace, you can use
the `--lovelace` argument below. Note that if you want to specify the amount to 
create entirely in Lovelace, you'll have to use `-a0 --lovelace AMOUNT`.

```
--lovelace AMOUNT
-l AMOUNT
```

This puts `AMOUNT` Lovelace into each UTxO in every wallet created, in addition to
the amount specified by the `--ada` argument. Note that if you don't specify the
amount of ADA to add, the total amount will be 10,000 ADA + `AMOUNT` lovelace.

Note that both `--ada` and `--lovelace` can not be 0 at the same time.

```
--utxos NUM
-u NUM
```

Create `NUM` UTxOs in each wallet created. Note that each UTxO created has the amount
of ADA determined by the `--ada` and `--lovelace` arguments.

```
--working-dir /path/
-w /path/
```

This determines where the node database, chain-index database, and bot-plutus-interface
files will be stored for a running cluster. If specified, this will store cluster
data in the provided path (can be relative or absolute), the files will be deleted
on cluster shutdown by default. Otherwise, the cluster data is stored in a temporary
directory and will be deleted on cluster shutdown.
