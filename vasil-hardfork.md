### starting cluster and forking

Plutip has executable 'local-cluster' starting a cluster of 3 pools and a relay node.

To witness a fork one should:
 - start cluster with `cabal new-run local-cluster [CLUSTER_DIR]` and copy shown node socket
 - to hard-fork run: 
 ```
 export CARDANO_NODE_SOCKET_PATH=<socket path>

 # submit update-proposal bumping major version to 7 (babbage)
 bash cluster-data/update-proposal-major-version.sh

 # submit update-proposal updating the cost model (cost model in cluster-data/cost-models-data)
 # it waits first for new epoch in babbage era
 bash cluster-data/update-proposal-cost-model.sh
 ```
Use node socket with `--mainnet` flag.

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
