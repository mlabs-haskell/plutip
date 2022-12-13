{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (ReaderT), ask)
import Data.Default (def)
import Data.Monoid (Last (getLast))
import Data.Text.Lazy qualified as T
import ExampleContracts (ownValueToState)
import Test.Plutip.Config (
  PlutipConfig (extraConfig),
 )
import Test.Plutip.Contract (runContract)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (
  BpiWallet,
  addSomeWallet,
  mkMainnetAddress,
  walletPkh,
 )
import Test.Plutip.Internal.Cluster.Extra.Types (
  ExtraConfig (ecSlotLength),
 )
import Test.Plutip.Internal.LocalCluster (
  startCluster,
  stopCluster,
 )
import Test.Plutip.Internal.Types (
  ClusterEnv,
  ExecutionResult (contractState, outcome),
  nodeSocket,
 )
import Test.Plutip.Tools.ChainIndex qualified as CI
import Text.Pretty.Simple (pShow)

main :: IO ()
main = do
  let slotLen = 1
      extraConf = def {ecSlotLength = slotLen}
      plutipConfig = def {extraConfig = extraConf}

      addSomeWalletWithCollateral funds =
        addSomeWallet (toAda 10 : funds)

  putStrLn "Starting cluster..."
  (st, _) <- startCluster plutipConfig $ do
    w <- addSomeWalletWithCollateral [toAda 100]
    liftIO $ putStrLn "Waiting for wallets to be funded..."
    CI.awaitWalletFunded w slotLen

    separate
    printWallet (w, 1)
    printNodeRelatedInfo

    separate
    res <- executeContract w ownValueToState
    printResult res

    separate

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

    printResult res = do
      liftIO . putStrLn $ "Execution outcome:\n" <> pShow' (outcome res)
      liftIO . putStrLn $
        "Contract state:\n"
          <> pShow' (getLast $ contractState res)

    pShow' :: Show a => a -> String
    pShow' = T.unpack . pShow
