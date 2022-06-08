module Test.Plutip.Options (
  TraceOption (..),
) where

import BotPlutusInterface.Types (LogContext, LogLevel)

-- | Extra options for `assertExecutionWith`.
data TraceOption
  = -- | Display all logs collected by BPI during contract execution.
    Tracing
  | -- | Like `Tracing` but choose which context and log level to display.
    TracingButOnlyContext
      LogContext
      -- ^ Can be `ContractLog` or `BpiLog` for internal bpi requests/responses
      LogLevel
      -- ^ Upper bound on log level, can be Error | Warn | Notice | Info | Debug.
      -- | Display transaction execution budgets.
  | BudgetCounting
