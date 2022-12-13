# Cluster launcher update

`Plutip` heavily relies on local cluster testing framework from `cardano-wallet`.

Initially, framework was used as-is, but in order to add to Plutip ability to set slot length and epoch size, module `Cluster.hs` was copied from `cardano-wallet` to Plutip's codebase and adjusted to make this settings possible. So in case of updating `cardano-wallet` dependency be sure that original `Cluster.hs` and Plutip's one differs only in expected way.

At the moment all changes are related to adding `ExtraConfig` to necessary ADTs and functions in Plutip's version of `Cluster.hs` and difference with the original is pretty small.
