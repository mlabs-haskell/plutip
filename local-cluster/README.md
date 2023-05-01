# Creating a disposable private network

Plutip's `local-cluster` executable can start a local network of several nodes (4 currently) and provide access to the socket of one of the nodes for arbitrary interactions. Users can also fund several addresses and get their `.vkey`'s and `.skey`'s for further usage (these addresses may also be referenced as "wallets" in the documentation).

User can interact with the network until the executable is stopped. After that the network will be disposed of, another launch will set up a new network from scratch.

The node socket path as well as funded addresses and their public key hashes can be obtained from the console output (stdout).

**NOTE:** This branch launches local network in Vasil and was tested with node `1.35.4`. Please use an appropriate node version if running without Nix.

## Usage

The local cluster can be started by running

```bash
nix run .#plutip-core:exe:local-cluster
```

or

```bash
nix develop
cabal run local-cluster
```

When you see a message like `"Cluster is running. Press Enter to stop."` in the terminal -- it means that the local network has started successfully and all desired wallets have been created and funded.

Wallets' public key hashes and addresses are printed in stdout, if you want to get public and private key pair you can use the `--wallet-dir FILEPATH` option to specify directory where Plutip will create generated `.vkey` and `.skey` files.

### Available arguments

| Full                      | Short          | Description |
|---------------------------|----------------|-------------|
| --num-wallets NUM_WALLETS | -n NUM_WALLETS | Creates `NUM_WALLETS` wallets and saves keys into a default directory and also to a user-specified directory if the `--wallet-dir` argument is used. Defaults to 1 if not specified, and can be set to 0 if you do not wish to create any wallets           |
| --wallets NUM_WALLETS     | See above      | See above   |
| --wallet-dir FILEPATH     | -d FILEPATH    | If specified, saves the wallet keys to an extra directory at the `FILEPATH`. This is useful if you wish to have wallet keys easily accessible by other code or tools.           |
| --wallets-dir FILEPATH    | See above      | See above   |
| --ada ADA                 | -a ADA         | Puts `ADA` Ada into each UTxO in every created wallet. Defaults to 10,000 ADA. If you wish to specify an additional amount in Lovelace, you can use the `--lovelace` argument below. Note that if you want to specify the amount entirely in Lovelace, you'll have to use `-a0 --lovelace AMOUNT`.            |
| --lovelace Lovelace       | -l Lovelace    |  Puts `Lovelace` lovelace into each UTxO in the every created wallet in addition to the amount specified by the `--ada` argument. Note that if you don't specify the amount of ADA to be added, then the total amount will be 10,000 ADA + `Lovelace` lovelace. <br /> Note that `--ada` and `--lovelace` can not be 0 at the same time.           |
| --utxos NUM_UTXOS         | -u NUM_UTXOS       | Create `NUM_UTXOS` UTxOs in each created wallet. Note that each new UTxO has the amount of ADA determined by the `--ada` and `--lovelace` arguments.           |
| --working-dir FILEPATH    | -w FILEPATH    | This determines where the node database will be stored for the running cluster. If set, this will store cluster data in the provided path (can be relative or absolute), the files will be deleted upon a cluster shutdown by default. Otherwise (if not set), the cluster data is stored in a temporary directory and will also be deleted after cluster shutdown.            |
| --slot-len SLOT_LEN       | -s SLOT_LEN    |  Sets a slot length of the created network, in seconds, e.g. `--slot-len 1s`, `-s 0.2s`. <br /> Parser expects an `s` at the end of the value.           |
| --epoch-size EPOCH_SIZE   | -e EPOCH_SIZE  |  Sets an epoch size of the created network, measured in slots.           |
| --dump-info-json FILEPATH |                |  Sets a destination for writing a JSON file with some useful runtime information (wallets, node socket path, etc.). Defaults to `./local-cluster-info.json`           |

## Making a custom local network launcher

The [Main.hs](./Main.hs) module can serve as an example of how to make your own executable for starting a local cluster with funded wallets (via the `withFundedCluster` function).

Example for `{start,stop}Cluster`:
```haskell
main :: IO ()
main = do
  (st, _) <- startCluster def $ \cenv -> do
    doSomething result
  stopCluster st
```
