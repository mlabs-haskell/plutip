### starting cluster and forking

Plutip has executable 'local-cluster' starting a cluster of 3 pools and a relay node.

To witness a fork one should:
 - prepare the repository:
 ```
 git clone https://github.com/mlabs-haskell/plutip.git
 cd plutip
 git checkout vasil-local-cluster
 nix develop
 ```
 - start cluster with `cabal new-run local-cluster [CLUSTER_DIR]` and copy shown node socket
 - to hard-fork run: 
 ```
 # Start in plutip directory and have cardano-cli in path.
 # You can obtain it by entering the nix shell with `nix develop`
 
 export CARDANO_NODE_SOCKET_PATH=<socket path>

 # submit update-proposal bumping major version to 7 (babbage)
 bash cluster-data/update-proposal-major-version.sh

 # submit update-proposal updating the cost model (cost model in cluster-data/cost-models-data)
 # it waits first for new epoch in babbage era
 bash cluster-data/update-proposal-cost-model.sh
 ```
 
Above commands submits two transactions if the first one fails. Seeing one "transaction submit error" for every "Transaction submitted succesfully" is fine.

One can use the started cardano node for submiting transactions, queries etc. Use node socket with `--mainnet` flag.

Wallet address displayed by local-cluster is for key in `$CLUSTER_WORK_DIR/bot-plutus-interface/signing-keys`. One can ignore it.

### funded keys

One can freely use the key `cluster-data/utxo-keys/utxo1.*key`, it will be funded.
To also fund another key run:
```
cardano-cli address info --address $(cardano-cli address build --mainnet --verification-key-file $VKEYFILE ) | jq '.base16'
```
to get the address in hex and then list the address in `initialFunds` field in `cluster-data/shelley-genesis.yaml`.

### configs

All the configuration files for cluster are kept in cluster-data:
 - `byron-genesis.yaml`
 - `shelley-genesis.yaml`
 - `alonzo-genesis.yaml`

Cost model with PlutusV2 is in `cost-models-data/cost-model-update.json`.

### contents

So things of interest for vasil testing are:
 - local-cluster executable
 - scripts `cluster-data/update-proposal*.sh`
 - keys `cluster-data/utxo-keys/*`
 - configs `cluster-data/*.yaml`
 - cost model `cluster-data/cost-models-data/cost-model-update.json`

### Cabal only build

* Make sure you have patched `libSodium` on your path.
* Install `secp256k1` from source, follow this [guide](https://forum.cardano.org/t/cardano-crypto-class-secp256k1-support-issue-on-arm64-with-cardano-node-1-35-0-solution/103765).
* Make sure you have ghc version `8.10.7` as default ghc of your system.
* After everything is done, then build plutip using `cabal build`.
