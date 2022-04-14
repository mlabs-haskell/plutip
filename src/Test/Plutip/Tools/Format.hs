module Test.Plutip.Tools.Format where

import BotPlutusInterface.Types (TxBudget (TxBudget))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import PlutusPrelude (pretty)
import Data.Bifunctor (bimap)
import Ledger (ExBudget (ExBudget), ExCPU (ExCPU), ExMemory (ExMemory), TxId, TxOutRef, MintingPolicyHash)

formatTxBudgtes :: Map TxId TxBudget -> String
formatTxBudgtes budgets =
  let preformattedList = map glue (Map.toList budgets)
  in
    Text.unpack
    . Text.intercalate "\n"
    . map Text.pack
    $ preformattedList
  where
    glue :: (TxId, TxBudget) -> String
    glue (txId, bds) = mconcat
      (("Budget for " ++ txIdFmt txId ++ "\n") : if null r then ["  Empty"] else r) -- TODO:refactor, hlint did that
      where
        bs' :: [(String, String)]
        bs' = formatBudget bds

        f :: (String, String) -> String
        f (refOrHash, budget) = "  " ++ refOrHash ++ "\n" ++
                                "   " ++ budget ++ "\n"
        r = map f bs'


formatBudget :: TxBudget -> [(String, String)]
formatBudget (TxBudget spend mint) =
  let spending = bimap orefFmt sayExBudget <$> Map.toList spend
      minting = bimap policyFmt sayExBudget <$> Map.toList mint
  in spending ++ minting

sayExBudget :: ExBudget -> String
sayExBudget (ExBudget (ExCPU cpu) (ExMemory mem)) =
  "CPU " ++ show cpu ++ " | " ++ "MEM " ++ show mem

orefFmt :: TxOutRef -> [Char]
orefFmt = ("TxOutRef " ++) . show . pretty

policyFmt :: MintingPolicyHash -> [Char]
policyFmt =("PolicyHash " ++) . show . pretty

txIdFmt :: TxId -> [Char]
txIdFmt = ("TxId " ++) . show . pretty


fmtWith f = Map.toList . Map.mapKeys f