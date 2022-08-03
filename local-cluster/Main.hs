{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import Control.Applicative (optional, (<**>))
import Control.Monad (void, replicateM, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Default (def)
import Options.Applicative qualified as Options
import Options.Applicative (Parser, info, helper)
import Test.Plutip.Config qualified as Config
import Test.Plutip.Internal.BotPlutusInterface.Wallet (walletPkh, addSomeWalletDir)
import Test.Plutip.Internal.Types (nodeSocket)
import Test.Plutip.LocalCluster (
  mkMainnetAddress,
  startCluster,
  stopCluster,
  waitSeconds,
 )

main :: IO ()
main = do
  args <- Options.execParser (info (pClusterConfig <**> helper) mempty)
  (st, _) <- startCluster def $ do
    let nWall = numWallets args
        wPath = dirWallets args
        adaAmt = (toAda $ fromInteger $ abs $ adaAmount args) + (fromInteger $ abs $ lvlAmount args)
        nUtxos = numUtxos args
    ws <- replicateM (max 0 nWall) $ addSomeWalletDir (replicate nUtxos adaAmt) wPath
    waitSeconds 2 -- let wallet Tx finish, it can take more time with bigger slot length
    separate
    forM_ (zip ws [(1 :: Int)..]) $ \(w,n) -> liftIO $ do
      putStrLn $ "Wallet " ++ show n ++ " PKH: " ++ show (walletPkh w)
      putStrLn $ "Wallet " ++ show n ++ " mainnet address: " ++ show (mkMainnetAddress w)
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


pnumWallets :: Parser Int
pnumWallets = Options.option Options.auto
  (  Options.long "num-wallets"
  <> Options.long "wallets"
  <> Options.short 'n'
  <> Options.metavar "NUM_WALLETS"
  <> Options.value 1
  )

pdirWallets :: Parser (Maybe FilePath)
pdirWallets = optional $ Options.strOption
  (  Options.long "wallets-dir"
  <> Options.long "wallet-dir"
  <> Options.short 'd'
  <> Options.metavar "FILEPATH"
  )

padaAmount :: Parser Integer
padaAmount = Options.option Options.auto
  (  Options.long "ada"
  <> Options.short 'a'
  <> Options.metavar "ADA"
  <> Options.value 10_000
  )

plvlAmount :: Parser Integer
plvlAmount = Options.option Options.auto
  (  Options.long "lovelave"
  <> Options.short 'l'
  <> Options.metavar "Lovelace"
  <> Options.value 0
  )

pnumUtxos :: Parser Int
pnumUtxos = Options.option Options.auto
  (  Options.long "utxos"
  <> Options.short 'u'
  <> Options.metavar "NUM_UTXOS"
  <> Options.value 1
  )

pClusterConfig :: Parser CWalletConfig
pClusterConfig = CWalletConfig 
  <$> pnumWallets 
  <*> pdirWallets 
  <*> padaAmount 
  <*> plvlAmount
  <*> pnumUtxos

-- | Basic info about the cluster, to
-- be used by the command-line
data CWalletConfig = CWalletConfig
  { numWallets :: Int
  , dirWallets :: Maybe FilePath
  , adaAmount  :: Integer
  , lvlAmount  :: Integer
  , numUtxos :: Int
  } deriving stock (Show, Eq)

