{-# LANGUAGE AllowAmbiguousTypes #-}

module BotInterface.Run (runContract) where

import BotInterface.Setup qualified as BIS
import BotInterface.Wallet (BpiWallet, ledgerPkh)
import BotPlutusInterface (runPAB)
import BotPlutusInterface.Types
  ( CLILocation (Local),
    HasDefinitions,
    LogLevel (Info),
    PABConfig
      ( PABConfig,
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
import Data.Aeson (eitherDecodeFileStrict')
import Data.Kind (Type)
import Data.Text (pack)
import LocalCluster.Types ( ClusterEnv(chainIndexUrl, networkId) )
import System.Exit (die)

runContract ::
  forall (t :: Type).
  (HasDefinitions t, FromJSON t) =>
  ClusterEnv ->
  BpiWallet ->
  IO ()
runContract cEnv wallet = do
  pparams <-
    eitherDecodeFileStrict' (BIS.pParamsFile cEnv)
      >>= either die pure
  let pabConf =
        PABConfig
          { pcCliLocation = Local,
            pcChainIndexUrl = chainIndexUrl cEnv,
            pcNetwork = networkId cEnv,
            pcProtocolParams = pparams,
            pcScriptFileDir = pack $ BIS.scriptsDir cEnv,
            pcSigningKeyFileDir = pack $ BIS.keysDir cEnv,
            pcTxFileDir = pack $ BIS.txsDir cEnv,
            pcDryRun = True,
            pcProtocolParamsFile = pack $ BIS.pParamsFile cEnv,
            pcLogLevel = Info,
            pcOwnPubKeyHash = ledgerPkh wallet,
            pcPort = 9080
          }
  runPAB @t pabConf