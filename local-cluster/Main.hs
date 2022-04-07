{-# LANGUAGE NumericUnderscores #-}
module Main (main) where

import Test.Plutip.LocalCluster
    ( addSomeWallet,
      mkMainnetAddress,
      startCluster,
      stopCluster,
      waitSeconds )
import Data.Default (def)
import Control.Monad (void)
import Control.Monad.Reader (ReaderT (ReaderT))
import Test.Plutip.Internal.Types ( nodeSocket )
import Control.Monad.IO.Class (liftIO)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (walletPkh)

main :: IO ()
main = do
  (st, _) <- startCluster def $ do
    w <- addSomeWallet (toAda 10000)
    waitSeconds 2 -- let wallet Tx finish

    separate
    liftIO $ do
      putStrLn $ "Wallet PKH: " ++ show (walletPkh w)
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