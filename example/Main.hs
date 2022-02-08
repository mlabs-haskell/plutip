module Main (main) where

import BotInterface.Wallet qualified as BW
import Control.Monad (forever, replicateM_, void)
import Data.Text (Text, unpack)
import LocalCluster.Cluster (runUsingCluster)
import System.Environment (setEnv)
import Tools.DebugCli qualified as CLI
import Utils (ada, waitSeconds)

import Data.Either (fromRight)
import LocalCluster.Types (ClusterEnv (chainIndexUrl), supportDir)

import BotInterface.Run qualified as Bot
import DebugContract.DebugGet qualified as DebugContract

-- debug stuff

import DebugContract.DebugGet qualified as DebugContract
import Tools.ChainIndex qualified as CIX

-- debug stuff - END

main :: IO ()
main = do
  -- todo: maybe some better configuring procedure should be introduced
  setEnv "SHELLEY_TEST_DATA" "cluster-data/cardano-node-shelley"
  setEnv "NO_POOLS" "1"
  setEnv "CARDANO_NODE_TRACING_MIN_SEVERITY" "Debug"

  runUsingCluster $ \cEnv -> do
    ws <- -- ? maybe it will be more ergonomic to get rid of `Ether` and just fail hard
      BW.usingEnv cEnv . fmap sequence . sequence $
        [ BW.addSomeWallet (ada 101)
        , BW.addSomeWallet (ada 202)
        , BW.addSomeWallet (ada 303)
        ]

    debugWallets ws cEnv

    let wallet = head $ tail $ fromRight (error "Ouch") ws
    Bot.runWrapped cEnv wallet DebugContract.getUtxos >>= print
    Bot.runWrapped cEnv wallet DebugContract.getUtxosThrowsErr >>= print
    Bot.runWrapped cEnv wallet DebugContract.getUtxosThrowsEx >>= print

    putStrLn "Done. Debug awaiting - interrupt to exit" >> forever (waitSeconds 60)
  where
    debugWallets ws cEnv = do
      putStrLn "\nDebug check:"
      putStrLn $ "Cluster dir: " <> show (supportDir cEnv)
      waitSeconds 2
      either
        (error . ("Err: " <>) . show)
        (mapM_ (CLI.utxoAtAddress cEnv . BW.mkMainnetAddress))
        ws

testMnemonic :: [Text]
testMnemonic =
  [ "radar"
  , "scare"
  , "sense"
  , "winner"
  , "little"
  , "jeans"
  , "blue"
  , "spell"
  , "mystery"
  , "sketch"
  , "omit"
  , "time"
  , "tiger"
  , "leave"
  , "load"
  ]
