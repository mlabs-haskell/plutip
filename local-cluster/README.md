# Starting local cluster with chain-index

This example starts a local cluster (BFT node + relay node) and by default adds a single wallet with some Ada on it's address.

As long as the cluster is not stopped, the relay node can be used for arbitrary actions that require network communication.

The node socket path can be obtained from console output.

Note that when wallet is added with `addSomeWallet` it is recommended to wait some time (1 or 2 seconds) with `waitSeconds` while funding transaction is sent and confirmed.

## Usage 

The local cluster can be started by running

```
cabal run local-cluster -- <arguments>
```

The available arguments are as follows:

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


Note that for both `--ada` and `--lovelace`, the values' absolute values are taken
and then added together to get the total amount to use for the UTxOs. Note that this
means that if you use a command like `local-cluster --ada 5 --lovelace -1`, the final
amount will be 5.000001 ADA instead of 4.999999 ADA.

```
--utxos NUM
-u NUM
```

Create `NUM` UTxOs in each wallet created. Note that each UTxO created has the amount
of ADA determined by the `--ada` and `--lovelace` arguments. 




