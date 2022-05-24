# Starting local cluster with chain-index

This example shows how to start local cluster (BFT node + relay node) and add single wallet with some Ada on it's address.

As long as the cluster is not stopped, the relay node can be used for arbitrary actions that require network communication.

Node socket path can be obtained from console output.

Note that when wallet is added with `addSomeWallet` it is recommended to wait some time (1 or 2 seconds) with `waitSeconds` while funding transaction is sent and confirmed.
