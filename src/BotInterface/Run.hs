{-# LANGUAGE AllowAmbiguousTypes #-}

module BotInterface.Run (activateAndThen, execute) where

import BotInterface.Setup qualified as BIS
import BotInterface.Wallet (BpiWallet, ledgerPkh)
import BotPlutusInterface (runPAB)
import BotPlutusInterface.Types (
  CLILocation (Local),
  HasDefinitions,
  LogLevel (Info),
  PABConfig (
    PABConfig,
    pcChainIndexUrl,
    pcCliLocation,
    pcDryRun,
    pcLogLevel,
    pcNetwork,
    pcOwnPubKeyHash,
    pcPort,
    pcProtocolParams,
    pcProtocolParamsFile,
    pcScriptFileDir,
    pcSigningKeyFileDir,
    pcTxFileDir
  ),
 )
import Cardano.Api (FromJSON)
import Control.Concurrent.Async (async)
import Control.Monad (void)
import Data.Aeson (ToJSON, eitherDecodeFileStrict', encode)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Kind (Type)
import Data.Text (pack)
import LocalCluster.Types (ClusterEnv (chainIndexUrl, networkId))
import System.Exit (die)
import System.Process.Typed (proc, readProcess)
import Test.Integration.Framework.DSL (json)
import Utils (waitSeconds)

goPab ::
  forall (t :: Type).
  (HasDefinitions t, FromJSON t) =>
  ClusterEnv ->
  BpiWallet ->
  IO ()
goPab cEnv wallet = do
  pparams <-
    eitherDecodeFileStrict' (BIS.pParamsFile cEnv)
      >>= either die pure
  let pabConf =
        PABConfig
          { pcCliLocation = Local
          , pcChainIndexUrl = chainIndexUrl cEnv
          , pcNetwork = networkId cEnv
          , pcProtocolParams = pparams
          , pcScriptFileDir = pack $ BIS.scriptsDir cEnv
          , pcSigningKeyFileDir = pack $ BIS.keysDir cEnv
          , pcTxFileDir = pack $ BIS.txsDir cEnv
          , pcDryRun = True
          , pcProtocolParamsFile = pack $ BIS.pParamsFile cEnv
          , pcLogLevel = Info
          , pcOwnPubKeyHash = ledgerPkh wallet
          , pcPort = 9080
          }
  void . async $ runPAB @t pabConf

-- Very work in progress variant
-- it would be nice to not be able to execute contracts with wrong type, I guess
activateAndThen ::
  forall (t :: Type).
  (HasDefinitions t, FromJSON t) =>
  ClusterEnv ->
  BpiWallet ->
  IO () ->
  IO ()
activateAndThen cEnv wallet action = do
  goPab @t cEnv wallet
  waitSeconds 2 -- todo: not sure how to handle startup awaiting better
  action

-- Very work in progress variant
-- it would be nice to not be able to execute contracts with wrong type, I guess
execute :: (ToJSON t) => t -> IO ()
execute cntr = do
  let data' = BS.unpack $ encode [json| {caID: #{cntr}} |]
      pr =
        proc
          "curl"
          [ "-s"
          , "-XPOST"
          , "localhost:9080/api/contract/activate"
          , "-H"
          , "Content-Type: application/json"
          , "-d"
          , data'
          ]
  res <- readProcess pr
  BS.putStrLn $ "Activate contract: " <> BS.pack (show res)
