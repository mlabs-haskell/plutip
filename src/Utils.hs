module Utils (
  ada,
  waitSeconds
) where
import Numeric.Natural ( Natural )
import Control.Concurrent (threadDelay)

-- | Library functions works with amounts in `Lovelace`. 
-- This function helps to specify amounts in `Ada` easier. 
ada :: Natural -> Natural
ada = (* 1_000_000)

-- | Suspend execution for n seconds (via `threadDelay`)
waitSeconds :: Int -> IO ()
waitSeconds = threadDelay . (* 1000000)
