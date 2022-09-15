# How to generate cluster-data from scratch

Start inside the cluster-data directory with files:
 - cluster-data/alonzo-genesis.yaml
 - cluster-data/shelley-genesis.yaml
 - cluster-data/byron-genesis.yaml
 - cluster-data/node.config
 - cluster-data/gen-byron-funds.sh
 - cluster-data/regenerate-byron.sh
 - cluster-data/regenerate.sh

Steps:

 0. Create `genesis.alonzo.spec.json` with `yq . alonzo-genesis.yaml > genesis.alonzo.spec.json`.
 1. Run `regenerate-byron.sh`.
 2. It created `byron-genesis-init.yaml`, update `bootStakeholder` and `heavyDelegation` fields in `byron-genesis.yaml` to match it.
 5. Run `regenerate.sh`.
 6. Substitute output from previous command into `genDelegs` field in `shelley-genesis.yaml`.
 8. Run `mkdir faucet-addrs && bash gen-byron-funds.sh`.
 9. Previous command generated addresses than need to be filled with money. List the addresses in `nonAvvmBalances` field in `byron-genesis.yaml`.

Directories `utxo-keys`, `genesis-keys`, `delegate-keys`, `tmp` and json genesis files are irrelevant for local-cluster, can be removed. Note that some of the genesis delegate keys were renamed to bft-leader* keys in `regenerate.sh`.

If you want to use utxo-keys, then fill them with money listing them in `initialFunds` in `shelley-genesis.yaml`. You can get address in hex with

```bash
cardano-cli address info --address $(cardano-cli address build --mainnet --verification-key-file utxo-keys/utxo1.vkey ) | jq '.base16'
```
