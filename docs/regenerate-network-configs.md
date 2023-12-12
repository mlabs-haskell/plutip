# How to generate `cluster-data` from node config and genesis files

Requires `cardano-cli`, `jq` and `yq` to be available in `$PATH`.

Either use an existing `cluster-data` folder (e.g. from Plutip sources) and remove all generated files, or create a new folder by copying setup scripts from Plutip's `cluster-data` and downloading genesis files and a node config that you need.

Now your `cluster-data` should look like this:

- cluster-data/alonzo-genesis.yaml
- cluster-data/shelley-genesis.yaml
- cluster-data/byron-genesis.yaml
- cluster-data/node.config
- cluster-data/gen-byron-funds.sh
- cluster-data/regenerate-byron.sh
- cluster-data/regenerate.sh

Steps:

0. Prepare `cluster-data` (see above).
1. Create `genesis.alonzo.spec.json` with `yq . alonzo-genesis.yaml > genesis.alonzo.spec.json`.
2. Run `regenerate-byron.sh`.
3. It created `byron-genesis-init.yaml`, update `bootStakeholder` and `heavyDelegation` fields in `byron-genesis.yaml` to match it.
4. Run `regenerate.sh`.
5. Substitute output from previous command into `genDelegs` field in `shelley-genesis.yaml`.
6. Run `mkdir faucet-addrs && bash gen-byron-funds.sh`.
7. Previous command generated addresses than need to be filled with money. List the addresses in `nonAvvmBalances` field in `byron-genesis.yaml`.

Directories `utxo-keys`, `genesis-keys`, `delegate-keys`, `tmp` and json genesis files are irrelevant for local-cluster and can be removed. Note that some of the genesis delegate keys were renamed to bft-leader* keys in `regenerate.sh`.

If you want to use utxo-keys, then fill them with money by listing them in `initialFunds` in `shelley-genesis.yaml`. You can get an address in hex with

```bash
cardano-cli address info --address $(cardano-cli address build --mainnet --verification-key-file utxo-keys/utxo1.vkey ) | jq '.base16'
```
