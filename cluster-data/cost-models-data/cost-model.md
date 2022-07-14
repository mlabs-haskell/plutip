Cost model `cost-model-update.json` combines
 - PlutusV1 cost model from https://hydra.iohk.io/build/7578887/download/1/index.html
 - PlutusV2 cost model from plutus 1.0.0.0, made combining `plutus/plutus-core/cost-model/data/builtinCostModel.json` and `plutus/plutus-core/cost-model/data/cekMachineCosts.json` and then flattening with `FlattenJson.hs` exec.