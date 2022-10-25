{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Cardano.Ledger.Slot (EpochSize (EpochSize))
import Control.Applicative (optional, (<**>), (<|>))
import Control.Monad (forM_, replicateM, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Default (def)
import Data.Time (NominalDiffTime)
import GHC.Natural (Natural)
import GHC.Word (Word64)
import Numeric.Positive (Positive)
import Options.Applicative (Parser, helper, info)
import Options.Applicative qualified as Options
import Test.Plutip.Config (
  PlutipConfig (clusterWorkingDir, extraConfig, chainIndexMode),
  WorkingDirectory (Fixed, Temporary), ChainIndexMode (DefaultPort, CustomPort, NotNeeded)
 )
import Test.Plutip.Internal.BotPlutusInterface.Wallet (
  addSomeWalletDir,
  cardanoMainnetAddress,
  walletPkh,
 )
import Test.Plutip.Internal.Cluster.Extra.Types (
  ExtraConfig (ExtraConfig),
 )
import Test.Plutip.Internal.Types (nodeSocket)
import Test.Plutip.LocalCluster (
  mkMainnetAddress,
  startCluster,
  stopCluster,
 )
import Test.Plutip.Tools.CardanoApi (awaitAddressFunded)

main :: IO ()
main = do
  config <- Options.execParser (info (pClusterConfig <**> helper) mempty)
  case totalAmount config of
    Left e -> error e
    Right amt -> do
      let ClusterConfig {numWallets, dirWallets, numUtxos, workDir, slotLength, epochSize, cIndexMode} = config
          workingDir = maybe Temporary (`Fixed` False) workDir

          extraConf = ExtraConfig slotLength epochSize
          plutipConfig = def {clusterWorkingDir = workingDir, extraConfig = extraConf, chainIndexMode = cIndexMode}

      putStrLn "Starting cluster..."
      (st, _) <- startCluster plutipConfig $ do
        ws <- initWallets numWallets numUtxos amt dirWallets
        liftIO $ putStrLn "Waiting for wallets to be funded..."
        awaitFunds ws slotLength

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

    totalAmount :: ClusterConfig -> Either String Positive
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

    -- waits for the last wallet to be funded
    awaitFunds ws delay = do
      let lastWallet = last ws
      liftIO $ putStrLn "Waiting till all wallets will be funded..."
      awaitAddressFunded (cardanoMainnetAddress lastWallet) delay

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

pSlotLen :: Parser NominalDiffTime
pSlotLen =
  Options.option
    Options.auto
    ( Options.long "slot-len"
        <> Options.short 's'
        <> Options.metavar "SLOT_LEN"
        <> Options.value 0.2
    )

pEpochSize :: Parser EpochSize
pEpochSize =
  EpochSize <$> wordParser
  where
    wordParser :: Parser Word64
    wordParser =
      Options.option
        Options.auto
        ( Options.long "epoch-size"
            <> Options.short 'e'
            <> Options.metavar "EPOCH_SIZE"
            <> Options.value 160
        )

pChainIndexMode :: Parser ChainIndexMode
pChainIndexMode = 
  withIndex <|> withIndexPort <|> pure NotNeeded
  where
    withIndex = Options.flag' DefaultPort
      ( Options.long "with-index"
       <> Options.help "Start cluster with chain-index on default port"
      )
    withIndexPort = CustomPort <$> portParser

    portParser = 
      Options.option
        Options.auto
        (Options.long "with-index-port"
          <> Options.metavar "PORT"
          <> Options.help "Strat cluster with chain-index on custom port")


pClusterConfig :: Parser ClusterConfig
pClusterConfig =
  ClusterConfig
    <$> pnumWallets
    <*> pdirWallets
    <*> padaAmount
    <*> plvlAmount
    <*> pnumUtxos
    <*> pWorkDir
    <*> pSlotLen
    <*> pEpochSize
    <*> pChainIndexMode

-- | Basic info about the cluster, to
-- be used by the command-line
data ClusterConfig = ClusterConfig
  { numWallets :: Int
  , dirWallets :: Maybe FilePath
  , adaAmount :: Natural
  , lvlAmount :: Natural
  , numUtxos :: Int
  , workDir :: Maybe FilePath
  , slotLength :: NominalDiffTime
  , epochSize :: EpochSize
  , cIndexMode :: ChainIndexMode
  }
  deriving stock (Show, Eq)
