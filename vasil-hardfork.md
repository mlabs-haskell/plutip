# Starting cluster and hard forking - cabal build version

Plutip has executable 'local-cluster' starting a cluster of 3 pools and a relay node.

## Environment prerequisites

To build executable only with `cabal` some environment setup is required:

* Make sure you have correct patched `libSodium` on your path.
* Install `secp256k1` from source, follow this [guide](https://forum.cardano.org/t/cardano-crypto-class-secp256k1-support-issue-on-arm64-with-cardano-node-1-35-0-solution/103765).
* Make sure you have ghc version `8.10.7` as default ghc of your system.
* Make sure you have `cardano-cli` and `cardano-node` binaries added to `PATH` (setup was tested with ver. 1.35.0; alternatively, exact version used by `local-cluster` executable can be built with `cabal install cardano-node` and `cabal install cardano-cli`)

If everything set correctly, executable can be built/run with `cabal`.

## Hard-forking

1. prepare the repository:

    ```bash
    git clone https://github.com/mlabs-haskell/plutip.git
    cd plutip
    git checkout vasil-local-cluster-cabal-build
    ```

2. start cluster with `cabal new-run local-cluster [CLUSTER_DIR]` and copy shown node socket
3. Initiate hard-fork
   1. Start in plutip directory. `cardano-cli` should be in `PATH`.
   2. Set socket acquired during cluster start (step 2 above)

       ```export CARDANO_NODE_SOCKET_PATH=<socket path>```
   3. submit update-proposal bumping major version to 7 (babbage)

       ```bash cluster-data/update-proposal-major-version.sh```

   4. submit update-proposal updating the cost model (cost model in cluster-data/cost-models-data)

      ```bash cluster-data/update-proposal-cost-model.sh```

      It waits first for new epoch in babbage era.

Notes:

* Above commands submits two transactions if the first one fails. Seeing one "transaction submit error" for every "Transaction submitted successfully" is fine.
* One can use the started cardano node for submitting transactions, queries etc. Use node socket with `--mainnet` flag.
* Wallet address displayed by local-cluster is for key in `$CLUSTER_WORK_DIR/bot-plutus-interface/signing-keys`. One can ignore it.

## Funded keys

One can freely use the key `cluster-data/utxo-keys/utxo1.*key`, it will be funded.

To fund another key run:

```
cardano-cli address info --address $(cardano-cli address build --mainnet --verification-key-file $VKEYFILE ) | jq '.base16'
```
to get the address in hex and then list the address in `initialFunds` field in `cluster-data/shelley-genesis.yaml`.

## Configs

All the configuration files for cluster are kept in cluster-data:

* `byron-genesis.yaml`
* `shelley-genesis.yaml`
* `alonzo-genesis.yaml`

Cost model with PlutusV2 is in `cost-models-data/cost-model-update.json`.

## Contents

So things of interest for vasil testing are:

* `local-cluster` executable, can be built/run with `cabal`
* scripts - `cluster-data/update-proposal*.sh`
* keys - `cluster-data/utxo-keys/*`
* configs - `cluster-data/*.yaml`
* cost model - `cluster-data/cost-models-data/cost-model-update.json`
