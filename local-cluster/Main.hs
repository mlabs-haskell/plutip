{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Cardano.Launcher.Node (CardanoNodeConn, nodeSocketFile)
import Control.Applicative (optional, (<**>))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, encodeFile)
import Data.Default (def)
import Data.Foldable (for_)
import Data.Time (NominalDiffTime)
import GHC.Conc (threadDelay)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Word (Word64)
import Options.Applicative (Parser, helper, info)
import Options.Applicative qualified as Options
import Plutip.Cluster (
  dieOnError,
  withFundedCluster,
 )
import Plutip.Config (
  EpochSize (EpochSize),
  PlutipConfig (clusterWorkingDir, extraConfig),
  WorkingDirectory (Fixed, Temporary),
  ecEpochSize,
  ecSlotLength,
 )
import Plutip.DistributeFunds (Lovelace)
import Plutip.Keys (KeyPair, mainnetAddress, saveKeyPair, showPkh)
import Plutip.Types (nodeSocket)

main :: IO ()
main = do
  config <- Options.execParser (info (pClusterConfig <**> helper) mempty)
  case totalAmount config of
    Left e -> error e
    Right amt -> do
      let ClusterConfig {numWallets, dirWallets, numUtxos, workDir, slotLength, epochSize} = config
          workingDir = maybe Temporary (`Fixed` False) workDir
          -- todo: if needed pipe remaining extraConfig options from command line args
          extraConf = def {ecSlotLength = slotLength, ecEpochSize = epochSize}
          plutipConfig = def {clusterWorkingDir = workingDir, extraConfig = extraConf}

      putStrLn "Starting cluster..."
      withFundedCluster plutipConfig (replicate numWallets $ replicate numUtxos amt) $ \cenv keys -> do
        -- Save keys to requested directory
        forM_ dirWallets $ \dir -> for_ keys (fmap dieOnError . saveKeyPair dir)

        -- print info
        separate
        liftIO $ forM_ (zip keys [(1 :: Int) ..]) printWallet
        printNodeRelatedInfo cenv
        separate

        -- Dump cluster info to local-cluster.info
        forM_ (dumpInfo config) $ \dInfo -> do
          dumpClusterInfo
            dInfo
            (nodeSocket cenv)
            keys

        putStrLn "Cluster is running. Ctrl-C to stop."
        loopThreadDelay
  where
    loopThreadDelay = threadDelay 100000000 >> loopThreadDelay

    printNodeRelatedInfo = \cEnv -> do
      putStrLn $ "Node socket: " <> show (nodeSocket cEnv)

    separate = liftIO $ putStrLn "\n------------\n"

    totalAmount :: ClusterConfig -> Either String Lovelace
    totalAmount cwc =
      case toAda (adaAmount cwc) + lvlAmount cwc of
        0 -> Left "One of --ada or --lovelace arguments should not be 0"
        amt -> Right $ fromInteger . toInteger $ amt

    dumpClusterInfo :: FilePath -> CardanoNodeConn -> [KeyPair] -> IO ()
    dumpClusterInfo fp nodeConn ws = do
      encodeFile
        fp
        ( ClusterInfo
            { ciWallets = [(showPkh w, show . mainnetAddress $ w) | w <- ws]
            , ciNodeSocket = nodeSocketFile nodeConn
            }
        )

    printWallet (w, n) = do
      putStrLn $ "Wallet " ++ show n ++ " PKH: " ++ showPkh w
      putStrLn $ "Wallet " ++ show n ++ " mainnet address: " ++ mainnetAddress w

    toAda = (* 1_000_000)

data ClusterInfo = ClusterInfo
  { ciWallets :: [(String, String)]
  , ciNodeSocket :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

pnumWallets :: Parser Int
pnumWallets =
  Options.option
    Options.auto
    ( Options.long "num-wallets"
        <> Options.long "wallets"
        <> Options.short 'n'
        <> Options.metavar "NUM_WALLETS"
        <> Options.value 1
        <> Options.help "Create NUM_WALLETS wallets, defaults to 1, can be set to 0 to avoid creating any wallets. Use --walet-dir to save keys to a folder."
    )

pdirWallets :: Parser (Maybe FilePath)
pdirWallets =
  optional $
    Options.strOption
      ( Options.long "wallets-dir"
          <> Options.long "wallet-dir"
          <> Options.short 'd'
          <> Options.metavar "FILEPATH"
          <> Options.help "Save the wallet keys to FILEPATH"
      )

padaAmount :: Parser Natural
padaAmount =
  Options.option
    Options.auto
    ( Options.long "ada"
        <> Options.short 'a'
        <> Options.metavar "ADA"
        <> Options.value 10_000
        <> Options.help "Create UTxO with ADA amount of Ada in each wallet, defaults to 10,000. Use --lovelace to specify an additional amount in Lovelace. To specify amount only in Lovelace use \"--ada 0 --lovelace AMOUNT\". --ada and --lovelace can't be both zero."
    )

plvlAmount :: Parser Natural
plvlAmount =
  Options.option
    Options.auto
    ( Options.long "lovelace"
        <> Options.short 'l'
        <> Options.metavar "LOVELACE"
        <> Options.value 0
        <> Options.help "Create UTxO with LOVELACE amount of Lovelace in each wallet, defaults to 0. Use --ada to specify an additional amount in Ada. Use \"--ada 0 --lovelace AMOUNT\" to specify amount only in Lovelace. --ada and --lovelace can't be both zero.
    )

pnumUtxos :: Parser Int
pnumUtxos =
  Options.option
    Options.auto
    ( Options.long "utxos"
        <> Options.short 'u'
        <> Options.metavar "NUM_UTXOS"
        <> Options.value 1
        <> Options.help "Create NUM_UTXOS UTxOs in each wallet, defaults to 1. Amount is determinted by --ada and --lovelace options.
    )

pWorkDir :: Parser (Maybe FilePath)
pWorkDir
  optional $
    Options.strOption
      ( Options.long "working-dir"
          <> Options.short 'w'
          <> Options.metavar "FILEPATH"
          <> Options.help "Determines where the node database will be stored for the running cluster. Temporary directory will be used if this option is not set."
      )

pSlotLen :: Parser NominalDiffTime
pSlotLen =
  Options.option
    Options.auto
    ( Options.long "slot-len"
        <> Options.short 's'
        <> Options.metavar "SLOT_LEN"
        <> Options.value 0.2
        <> Options.help "Set a slot length of the created network in seconds, e.g. --slot-len 1s, -s 0.2s. Note that parser expects an 's' at the end of the value. Defaults to 0.2s"
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
            <> Options.value 80
            <> Options.help "Set an epoch size of the created network in slots. Defaults to 80."
        )

pInfoJson :: Parser (Maybe FilePath)
pInfoJson =
  optional $
    Options.strOption
      ( Options.long "dump-info-json"
          <> Options.metavar "FILEPATH"
          <> Options.help "Write some useful runtime information to a JSON file (wallets, node socket path, etc.) after starting the cluster"
          <> Options.value "local-cluster-info.json"
      )

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
    <*> pInfoJson

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
  , dumpInfo :: Maybe FilePath
  }
  deriving stock (Show, Eq)
