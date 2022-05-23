module Test.Plutip.Tools (
  waitSeconds,
  ada,
) where

import Control.Concurrent (threadDelay)
import Numeric.Positive (Positive)

-- | Suspend execution for n seconds (via `threadDelay`)
waitSeconds :: Int -> IO ()
waitSeconds = threadDelay . (* 1000000)

-- | Library functions works with amounts in `Lovelace`.
-- This function helps to specify amounts in `Ada` easier.
ada :: Positive -> Positive
ada = (* 1_000_000)
