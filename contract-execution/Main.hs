{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (ReaderT), ask)
import Data.Default (def)
import Data.Monoid (Last)
import Data.Text.Lazy qualified as T
import Test.Plutip.Config (
  PlutipConfig (extraConfig),
 )
import Test.Plutip.Internal.BotPlutusInterface.Run ( runContract )
import Test.Plutip.Internal.BotPlutusInterface.Wallet
    ( ledgerPaymentPkh,
      BpiWallet,
      addSomeWallet,
      mkMainnetAddress,
      walletPkh )
import Test.Plutip.Internal.Cluster.Extra.Types (
  ExtraConfig (ecSlotLength),
 )
import Test.Plutip.Internal.LocalCluster (
  startCluster,
  stopCluster,
 )
import Test.Plutip.Internal.Types (
  ClusterEnv,
  ExecutionResult (outcome),
  nodeSocket,
 )
import Test.Plutip.Tools.ChainIndex qualified as CI
import Text.Pretty.Simple (pShow)
import Currency (mintContract, OneShotCurrency, CurrencySchema, CurrencyError)
import Plutus.Contract (Contract)
import Control.Monad (void)

main :: IO ()
main = do
  let slotLen = 1
      extraConf = def {ecSlotLength = slotLen}
      plutipConfig = def {extraConfig = extraConf}

  putStrLn "Starting cluster..."
  (st, _) <- startCluster plutipConfig $ do
    w <- addSomeWallet [toAda 1000]
    liftIO $ putStrLn "Waiting for wallets to be funded..."
    CI.awaitWalletFunded w slotLen

    separate
    printWallet (w, 1)
    printNodeRelatedInfo

    separate
    let contract :: Contract (Maybe (Last OneShotCurrency)) CurrencySchema CurrencyError OneShotCurrency
        contract = mintContract (ledgerPaymentPkh w)
                      [("test1", 3), ("test2", 2)]
    res <- executeContract w contract
    liftIO $ putStrLn $ pShow' (outcome res)

    separate
  putStrLn "Cluster is running. Press Enter to stop."
        >> void getLine
  putStrLn "Stopping cluster"

  stopCluster st
  where
    printNodeRelatedInfo = ReaderT $ \cEnv -> do
      putStrLn $ "Node socket: " <> show (nodeSocket cEnv)

    separate = liftIO $ putStrLn "\n------------\n"

    printWallet :: (BpiWallet, Int) -> ReaderT ClusterEnv IO ()
    printWallet (w, n) = liftIO $ do
      putStrLn $ "Wallet " ++ show n ++ " PKH: " ++ show (walletPkh w)
      putStrLn $ "Wallet " ++ show n ++ " mainnet address: " ++ show (mkMainnetAddress w)

    toAda = (* 1_000_000)

    executeContract wallet contract =
      ask >>= \cEnv -> runContract cEnv wallet contract

    pShow' :: Show a => a -> String
    pShow' = T.unpack . pShow
