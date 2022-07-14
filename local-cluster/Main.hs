{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Default (def)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (walletPkh)
import Test.Plutip.Internal.Types (nodeSocket)
import Test.Plutip.LocalCluster (
  addSomeWallet,
  mkMainnetAddress,
  startCluster,
  stopCluster,
  waitSeconds,
 )
import Test.Plutip.Config (PlutipConfig(..), WorkingDirectory (..))
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let clusterWorkingDir' = case args of
        [fp] -> Fixed fp True
        _ -> Temporary
  let config = def { clusterWorkingDir = clusterWorkingDir'}
  
  (st, _) <- startCluster config $ do
    w <- addSomeWallet [toAda 10000]
    waitSeconds 2 -- let wallet Tx finish, it can take more time with bigger slot length
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
