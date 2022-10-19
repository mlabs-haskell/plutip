module Test.Plutip.Tools.Cluster (
  waitSeconds,
  ada,
  awaitAddressFunded,
) where

import Cardano.Api (UTxO (UTxO))
import Cardano.Api qualified as C
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Map qualified as Map
import Numeric.Positive (Positive)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (showAddress)
import Test.Plutip.Internal.Types (ClusterEnv)
import Test.Plutip.Tools.CardanoApi (utxosAtAddress)

-- | Suspend execution for n seconds (via `threadDelay`)
waitSeconds :: Int -> IO ()
waitSeconds = threadDelay . (* 1000000)

awaitAddressFunded :: ClusterEnv -> Int -> C.AddressAny -> IO ()
awaitAddressFunded cEnv delay addr = do
  utxo <- utxosAtAddress cEnv addr
  unless (utxosReceived utxo) $ do
    putStrLn $ "No funds at " <> showAddress addr <> " yet. Awaiting UTxOs..."
    waitSeconds delay
    awaitAddressFunded cEnv delay addr
  where
    utxosReceived = \case
      Left _ -> False
      Right (UTxO utxo') -> not $ Map.null utxo'

-- | Library functions works with amounts in `Lovelace`.
-- This function helps to specify amounts in `Ada` easier.
ada :: Positive -> Positive
ada = (* 1_000_000)
