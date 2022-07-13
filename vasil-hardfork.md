Plutip has executable 'local-cluster' starting a cluster of 3 pools and a relay node.
One should:
 - start cluster with `cabal new-run local-cluster` and copy shown node socket
 - to hard-fork run: 
 ```
 export CARDANO_NODE_SOCKET_PATH=<socket path>

 # submit update-proposal bumping major version to 7 (babbage)
 bash cluster-data/update-proposal-major-version.sh

 # submit update-proposal updating the cost model (cost model in cluster-data/cost-models-data)
 # it waits first for new epoch in babbage era
 bash cluster-data/update-proposal-cost-model.sh
 ```