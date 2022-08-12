{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Control.Applicative (optional, (<**>))
import Control.Monad (forM_, replicateM, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Default (def)
import Numeric.Positive (Positive)
import Options.Applicative (Parser, helper, info)
import Options.Applicative qualified as Options
import Test.Plutip.Config
  ( PlutipConfig (clusterWorkingDir),
    WorkingDirectory (Fixed, Temporary),
  )
import Test.Plutip.Internal.BotPlutusInterface.Wallet (addSomeWalletDir, walletPkh)
import Test.Plutip.Internal.Types (nodeSocket)
import Test.Plutip.LocalCluster
  ( mkMainnetAddress,
    startCluster,
    stopCluster,
    waitSeconds,
  )
import GHC.Natural (Natural)

main :: IO ()
main = do
  config <- Options.execParser (info (pClusterConfig <**> helper) mempty)
  case totalAmount config of
    Left e -> error e
    Right amt -> do
      let CWalletConfig {numWallets, dirWallets, numUtxos, workDir} = config
          workingDir = maybe Temporary (`Fixed` False) workDir
          plutipConfig = def {clusterWorkingDir = workingDir}

      (st, _) <- startCluster plutipConfig $ do
        ws <- initWallets numWallets numUtxos amt dirWallets
        waitSeconds 2 -- let wallet Tx finish, it can take more time with bigger slot length

        separate
        liftIO $ forM_ (zip ws [(1 :: Int) ..]) printWallet
        printNodeRelatedInfo
        separate

      putStrLn "Cluster is running. Press Enter to stop."
        >> void getLine
      putStrLn "Stopping cluster"

      stopCluster st
  where
    printNodeRelatedInfo = ReaderT $ \cEnv -> do
      putStrLn $ "Node socket: " <> show (nodeSocket cEnv)

    separate = liftIO $ putStrLn "\n------------\n"

    totalAmount :: CWalletConfig -> Either String Positive
    totalAmount cwc =
      case toAda (adaAmount cwc) + lvlAmount cwc of
        0 -> Left "One of --ada or --lovelace arguments should not be 0"
        amt -> Right $ fromInteger . toInteger $ amt

    initWallets numWallets numUtxos amt dirWallets = do
      replicateM (max 0 numWallets) $
        addSomeWalletDir (replicate numUtxos amt) dirWallets

    printWallet (w, n) = do
      putStrLn $ "Wallet " ++ show n ++ " PKH: " ++ show (walletPkh w)
      putStrLn $ "Wallet " ++ show n ++ " mainnet address: " ++ show (mkMainnetAddress w)

    toAda = (* 1_000_000)

pnumWallets :: Parser Int
pnumWallets =
  Options.option
    Options.auto
    ( Options.long "num-wallets"
        <> Options.long "wallets"
        <> Options.short 'n'
        <> Options.metavar "NUM_WALLETS"
        <> Options.value 1
    )

pdirWallets :: Parser (Maybe FilePath)
pdirWallets =
  optional $
    Options.strOption
      ( Options.long "wallets-dir"
          <> Options.long "wallet-dir"
          <> Options.short 'd'
          <> Options.metavar "FILEPATH"
      )

padaAmount :: Parser Natural
padaAmount =
  Options.option
    Options.auto
    ( Options.long "ada"
        <> Options.short 'a'
        <> Options.metavar "ADA"
        <> Options.value 10_000
    )

plvlAmount :: Parser Natural
plvlAmount =
  Options.option
    Options.auto
    ( Options.long "lovelace"
        <> Options.short 'l'
        <> Options.metavar "Lovelace"
        <> Options.value 0
    )

pnumUtxos :: Parser Int
pnumUtxos =
  Options.option
    Options.auto
    ( Options.long "utxos"
        <> Options.short 'u'
        <> Options.metavar "NUM_UTXOS"
        <> Options.value 1
    )

pWorkDir :: Parser (Maybe FilePath)
pWorkDir =
  optional $
    Options.strOption
      ( Options.long "working-dir"
          <> Options.short 'w'
          <> Options.metavar "FILEPATH"
      )

pClusterConfig :: Parser CWalletConfig
pClusterConfig =
  CWalletConfig
    <$> pnumWallets
    <*> pdirWallets
    <*> padaAmount
    <*> plvlAmount
    <*> pnumUtxos
    <*> pWorkDir

-- | Basic info about the cluster, to
-- be used by the command-line
data CWalletConfig = CWalletConfig
  { numWallets :: Int,
    dirWallets :: Maybe FilePath,
    adaAmount :: Natural,
    lvlAmount :: Natural,
    numUtxos :: Int,
    workDir :: Maybe FilePath
  }
  deriving stock (Show, Eq)
