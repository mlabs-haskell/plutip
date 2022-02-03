module BotInterface.Setup (
  runSetup,
  keysDir,
  directoryIsSet,
  pParamsFile,
  scriptsDir,
  txsDir,
) where

import Data.Aeson (encodeFile)
import LocalCluster.Types (ClusterEnv (supportDir))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import Tools.CardanoApi (queryProtocolParams)

workDir' :: FilePath
workDir' = "bot-plutus-interface"

keysDir' :: FilePath
keysDir' = workDir' </> "signing-keys"

scriptsDir' :: FilePath
scriptsDir' = workDir' </> "result-scripts"

txsDir' :: FilePath
txsDir' = workDir' </> "txs"

-- | Creates directories necessary for bot interface
runSetup :: ClusterEnv -> IO ()
runSetup cEnv = do
  createRequiredDirs
  saveProtocolParams
  where
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
