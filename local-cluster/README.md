# Starting local cluster with chain-index

[This example](Main.hs) shows how to start local cluster (BFT node + relay node) and add single wallet with some Ada on it's address.

As long as the cluster is not stopped, the relay node can be used for arbitrary actions that require network communication.

Node socket path can be obtained from console output. It also possible now to put all node related data including node socket to user specified directory via [clusterWorkingDir option](../src/Test/Plutip/Config.hs#L38). This directory gets cleared _before_ each cluster start and user can choose whether or not to keep it _after_ cluster stops with [shouldKeep option](../src/Test/Plutip/Config.hs#L22).

Note that when wallet is added with `addSomeWallet` it is recommended to wait some time (1 or 2 seconds) with `waitSeconds` while funding transaction is sent and confirmed.
