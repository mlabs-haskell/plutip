{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Default (def)
import Ledger.Crypto (pubKeyHash)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (walletPubKey)
import Test.Plutip.Internal.Types (nodeSocket)
import Test.Plutip.LocalCluster (
  addSomeWallet,
  mkMainnetAddress,
  startCluster,
  stopCluster,
  waitSeconds,
 )

main :: IO ()
main = do
  (st, _) <- startCluster def $ do
    w <- addSomeWallet [toAda 10000]
    waitSeconds 2 -- let wallet Tx finish
    separate
    liftIO $ do
      putStrLn $ "Wallet public key: " ++ show (walletPubKey w)
      putStrLn $ "Wallet PKH: " ++ show (pubKeyHash (walletPubKey w))
      putStrLn $ "Wallet mainnet address: " ++ show (mkMainnetAddress w)
    prtNodeRelatedInfo
    separate

  putStrLn "Cluster is running. Press Enter to stop."
    >> void getLine
  putStrLn "Stopping cluster"

  stopCluster st
  where
    prtNodeRelatedInfo = ReaderT $ \cEnv -> do
      putStrLn $ "Node socket: " <> show (nodeSocket cEnv)

    separate = liftIO $ putStrLn "\n------------\n"

    toAda = (* 1_000_000)
