module Test.Plutip.Tools (
  ada,
  waitSeconds,
) where

import Control.Concurrent (threadDelay)
import Numeric.Natural (Natural)

-- | Library functions works with amounts in `Lovelace`.
-- This function helps to specify amounts in `Ada` easier.
ada :: Natural -> Natural
ada = (* 1_000_000)

-- | Suspend execution for n seconds (via `threadDelay`)
waitSeconds :: Int -> IO ()
waitSeconds = threadDelay . (* 1000000)
