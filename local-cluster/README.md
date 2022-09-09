# Disposable private network with `chain-index`

Local cluster executable can start local network of several nodes (4 currently) and provide access to socket of one of the nodes for arbitrary interactions. User can also fund several addresses to work with and get `.vkey`'s and `.skey`'s for them.

User can interact with cluster until executable is stopped. After that network will be disposed and new launch will start network from scratch.

The node socket path as well as funded addresses and their public key hashes can be obtained from console output.

NOTE: This branch launches local network in Vasil and was tested with node `1.35.3`. Please use appropriate node version if running w/o Nix.

The [Main.hs](Main.hs) module can also serve as an example of how to make your own executable for starting local cluster with funded wallets. Note that when wallet is added with `addSomeWallet` it is recommended to wait some time (2-3 seconds) with `waitSeconds` while funding transactions are sent and confirmed.

## Usage

The local cluster can be started by running

```bash
nix build .#plutip:exe:local-cluster  
./result/bin/local-cluster
```

or

```bash
nix develop
cabal run local-cluster
```

When you see in terminal message like "`Cluster is running. Press Enter to stop.`" it means that local cluster started successfully and all desired wallets are created and funded.

### Available arguments

Full | Short | Description
--- | ---  | ---
--wallets NUM | -n NUM | Creates `NUM` wallets and saves info about them into a default directory and also to a user-specified directory if the `--wallet-dir` argument is used. Defaults to 1 if not specified, and can be set to 0 if you do not wish to create any wallets.
--wallet-dir /path/ | -d /path/ | If specified, saves the wallet information to an extra directory specified by the user. This is useful if you wish to have wallet information easily accessible by other code or tools.
--ada AMOUNT | -a AMOUNT | Puts `AMOUNT` Ada into each UTxO in every wallet created. This defaults to 10,000 ADA. If you wish to specify an additional amount in Lovelace, you can use the `--lovelace` argument below. Note that if you want to specify the amount to create entirely in Lovelace, you'll have to use `-a0 --lovelace AMOUNT`.
--lovelace AMOUNT | -l AMOUNT | Puts `AMOUNT` Lovelace into each UTxO in every wallet created, in addition to the amount specified by the `--ada` argument. Note that if you don't specify the amount of ADA to add, the total amount will be 10,000 ADA + `AMOUNT` lovelace. <br /> Note that both `--ada` and `--lovelace` can not be 0 at the same time.
--utxos NUM | -u NUM | Create `NUM` UTxOs in each wallet created. Note that each UTxO created has the amount of ADA determined by the `--ada` and `--lovelace` arguments.
--working-dir /path/ | -w /path/ | This determines where the node database, chain-index database, and bot-plutus-interface files will be stored for a running cluster. If specified, this will store cluster data in the provided path (can be relative or absolute), the files will be deleted on cluster shutdown by default. Otherwise, the cluster data is stored in a temporary directory and will be deleted on cluster shutdown.
