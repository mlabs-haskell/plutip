module Test.Plutip.Internal.BotPlutusInterface.Setup (
  runSetup,
  keysDir,
  directoryIsSet,
  pParamsFile,
  scriptsDir,
  txsDir,
  metadataDir,
) where

import Cardano.Launcher.Node (nodeSocketFile)
import Data.Aeson (encodeFile)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Environment (setEnv)
import System.FilePath ((</>))
import Test.Plutip.Internal.Types (ClusterEnv (supportDir, plutipConf), nodeSocket)
import Test.Plutip.Tools.CardanoApi (queryProtocolParams)
import Cardano.Api (AsType (AsPaymentKey, AsSigningKey), Error (displayError))
import Cardano.Api qualified as CAPI
import Data.Foldable (traverse_)
import Plutus.V1.Ledger.Api (PubKeyHash (PubKeyHash))
import PlutusTx.Builtins qualified as PlutusTx
import Test.Plutip.Config (PlutipConfig(extraSigners))

workDir' :: FilePath
workDir' = "bot-plutus-interface"

keysDir' :: FilePath
keysDir' = workDir' </> "signing-keys"

scriptsDir' :: FilePath
scriptsDir' = workDir' </> "result-scripts"

txsDir' :: FilePath
txsDir' = workDir' </> "txs"

metadataDir' :: FilePath
metadataDir' = workDir' </> "metadata"

-- | Creates directories necessary for bot interface
runSetup :: ClusterEnv -> IO ()
runSetup cEnv = do
  createRequiredDirs
  saveProtocolParams
  setSocketPathEnv
  let extraSigners' = extraSigners $ plutipConf cEnv
  traverse_ addExtraSigner extraSigners'
  where
    setSocketPathEnv =
      -- required by `cardano-cli` used by bot interface
      setEnv "CARDANO_NODE_SOCKET_PATH" (nodeSocketFile $ nodeSocket cEnv)
    createRequiredDirs =
      mapM_
        (createDirectoryIfMissing True . ($ cEnv))
        [ keysDir
        , scriptsDir
        , txsDir
        , metadataDir
        ]
    saveProtocolParams = do
      ps <- queryProtocolParams cEnv
      case ps of
        Left e -> error $ show e
        Right params -> encodeFile (pParamsFile cEnv) params
    
    addExtraSigner = \case
      (Left sKeyPath) -> do
        res <- CAPI.readFileTextEnvelope (AsSigningKey AsPaymentKey) sKeyPath
        case res of
          Left fileError -> error $ displayError fileError
          Right sKey -> addExtraSigner $ Right sKey
      (Right sKey) -> do
        let vKey = CAPI.getVerificationKey sKey
            pkh = PubKeyHash . PlutusTx.toBuiltin . CAPI.serialiseToRawBytes $ CAPI.verificationKeyHash vKey
            keyFilename = "signing-key-" <> show pkh <> ".skey"
        g <-
          CAPI.writeFileTextEnvelope
            (keysDir cEnv </> keyFilename)
            Nothing
            sKey
        case g of
          Left fileError -> error $ displayError fileError
          Right _ -> pure ()

-- | Get directory for `.skey`'s of crated wallets for current cluster environment
keysDir :: ClusterEnv -> FilePath
keysDir cEnv = supportDir cEnv </> keysDir'

scriptsDir :: ClusterEnv -> FilePath
scriptsDir cEnv = supportDir cEnv </> scriptsDir'

txsDir :: ClusterEnv -> FilePath
txsDir cEnv = supportDir cEnv </> txsDir'

metadataDir :: ClusterEnv -> FilePath
metadataDir cEnv = supportDir cEnv </> metadataDir'

-- | Check if required by bot interface directories exist
directoryIsSet :: ClusterEnv -> IO Bool
directoryIsSet cEnv = doesDirectoryExist $ keysDir cEnv

-- | Protocol parameters file required for bot interface
pParamsFile :: ClusterEnv -> FilePath
pParamsFile cEnv = supportDir cEnv </> workDir' </> "pparams.json"
