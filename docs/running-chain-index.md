# Running chain index

It is possible to launch private network with or without `chain-index`.

This can be configured via `PlutipConfig` using `chainIndexMode :: ChainIndexMode` field. BE AWARE, that `chain-index` is required to run contracts with `Plutip` in [tasty integration](./tasty-integration.md), [interactive mode](./interactive-plutip.md) or with [custom runner](../contract-execution/Main.hs).

In case of `local-cluster` launch of `chain-index` can be controlled via options, see [readme](../local-cluster/README.md#available-arguments) for details.
