module Test.Plutip.Options (
  TxBudgetsReporting (..),
) where

import Data.Tagged (Tagged (Tagged))
import Test.Tasty.Options (
  IsOption (
    defaultValue,
    optionHelp,
    optionName,
    parseValue,
    showDefaultValue
  ),
 )

data TxBudgetsReporting
  = OmitReport
  | VerboseReport
  deriving stock (Show)

instance IsOption TxBudgetsReporting where
  defaultValue = OmitReport
  parseValue = const Nothing
  optionName = Tagged "print-budgets"
  optionHelp = Tagged "no cli parsing yet"
  showDefaultValue = Just . show
