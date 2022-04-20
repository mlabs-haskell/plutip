module Test.Plutip.Tools.Format (
  fmtExBudget,
  fmtTxBudgets,
) where

import BotPlutusInterface.Types (TxBudget (TxBudget))
import Data.Bifunctor (bimap)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Ledger (ExBudget (ExBudget), ExCPU (ExCPU), ExMemory (ExMemory), MintingPolicyHash, TxId, TxOutRef)
import PlutusPrelude (pretty)
import Data.Bool (bool)

fmtTxBudgets :: Map TxId TxBudget -> String
fmtTxBudgets budgets =
  let preformattedList = map glue (Map.toList budgets)
   in Text.unpack
        . Text.intercalate "\n"
        . map Text.pack
        $ preformattedList
  where
    glue :: (TxId, TxBudget) -> String
    glue (txId, bds) =
        mconcat
          [ "Budget for ", txIdFmt txId, "\n"
          , bool  budget' " Empty" (null budget') 
          ]
      where
        budget' = formatBudget bds

formatBudget :: TxBudget -> String
formatBudget (TxBudget spend mint) =
  foldMap combine stringified
  where 
    combine (refOrHash, budget) =
      mconcat
            [ pad 1 ++ refOrHash ++ "\n"
            , pad 2 ++ budget ++ "\n"
            ] 

    spending = bimap orefFmt fmtExBudget <$> Map.toList spend
    minting = bimap policyFmt fmtExBudget <$> Map.toList mint
    stringified = spending ++ minting

    pad n = replicate n ' '
  

fmtExBudget :: ExBudget -> String
fmtExBudget (ExBudget (ExCPU cpu) (ExMemory mem)) =
  "(cpu " ++ show cpu ++ " | " ++ "mem " ++ show mem ++ ")"

orefFmt :: TxOutRef -> [Char]
orefFmt = ("TxOutRef " ++) . show . pretty

policyFmt :: MintingPolicyHash -> [Char]
policyFmt = ("PolicyHash " ++) . show . pretty

txIdFmt :: TxId -> [Char]
txIdFmt = ("TxId " ++) . show . pretty
