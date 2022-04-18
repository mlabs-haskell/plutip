module Test.Plutip.Tools (
  waitSeconds,
) where

import Control.Concurrent (threadDelay)

-- | Suspend execution for n seconds (via `threadDelay`)
waitSeconds :: Int -> IO ()
waitSeconds = threadDelay . (* 1000000)
