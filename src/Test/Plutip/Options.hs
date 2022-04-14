module Test.Plutip.Options (
  TxBudgetsLog (..),
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

data TxBudgetsLog -- TODO: better naming
  = Omit
  | Verbose
  deriving stock (Show)

instance IsOption TxBudgetsLog where
  defaultValue = Omit
  parseValue = const Nothing
  optionName = Tagged "print-budgets"
  optionHelp = Tagged "no cli parsing yet"
  showDefaultValue = Just . show
