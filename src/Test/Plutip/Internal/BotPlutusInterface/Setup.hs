module Test.Plutip.Internal.BotPlutusInterface.Setup (
  runSetup,
  keysDir,
  directoryIsSet,
  pParamsFile,
  scriptsDir,
  txsDir,
) where

import Cardano.Api (AsType (AsPaymentKey, AsSigningKey), Error (displayError), PaymentKey, SigningKey)
import Cardano.Api qualified as CAPI
import Cardano.Launcher.Node (nodeSocketFile)
import Data.Aeson (encodeFile)
import Data.Foldable (traverse_)
import Plutus.V1.Ledger.Api (PubKeyHash (PubKeyHash))
import PlutusTx.Builtins qualified as PlutusTx
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Environment (setEnv)
import System.FilePath ((</>))
import Test.Plutip.Internal.Types (ClusterEnv (supportDir), nodeSocket)
import Test.Plutip.Tools.CardanoApi (queryProtocolParams)

workDir' :: FilePath
workDir' = "bot-plutus-interface"

keysDir' :: FilePath
keysDir' = workDir' </> "signing-keys"

scriptsDir' :: FilePath
scriptsDir' = workDir' </> "result-scripts"

txsDir' :: FilePath
txsDir' = workDir' </> "txs"

-- | Creates directories necessary for bot interface, and add given extra signing keys.
runSetup :: ClusterEnv -> [Either FilePath (SigningKey PaymentKey)] -> IO ()
runSetup cEnv extraSigners = do
  createRequiredDirs
  saveProtocolParams
  setSocketPathEnv
  traverse_ addExtraSigner extraSigners
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
        ]
    saveProtocolParams = do
      ps <- queryProtocolParams cEnv
      case ps of
        Left e -> error $ show e
        Right params -> encodeFile (pParamsFile cEnv) params
    addExtraSigner (Left sKeyPath) = do
      res <- CAPI.readFileTextEnvelope (AsSigningKey AsPaymentKey) sKeyPath
      case res of
        Left fileError -> error $ displayError fileError
        Right sKey -> addExtraSigner $ Right sKey
    addExtraSigner (Right sKey) = do
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

-- | Check if required by bot interface directories exist
directoryIsSet :: ClusterEnv -> IO Bool
directoryIsSet cEnv = doesDirectoryExist $ keysDir cEnv

-- | Protocol parameters file required for bot interface
pParamsFile :: ClusterEnv -> FilePath
pParamsFile cEnv = supportDir cEnv </> workDir' </> "pparams.json"
