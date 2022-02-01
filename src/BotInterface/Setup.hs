module BotInterface.Setup (
  setUpDir,
  keysDir,
  directoryIsSet,
) where

import LocalCluster.Types (ClusterEnv (supportDir))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))

workDir' :: FilePath
workDir' = "bot-plutus-interface"

keysDir' :: FilePath
keysDir' = workDir' </> "signing-keys"

-- | Creates directories necessary for bot interface
setUpDir :: ClusterEnv -> IO ()
setUpDir cEnv =
  createDirectoryIfMissing
    True
    (keysDir cEnv)

-- | Get directory for `.skey`'s of crated wallets for current cluster environment
keysDir :: ClusterEnv -> FilePath
keysDir cEnv = supportDir cEnv </> keysDir'

-- | Check if required by bot interface directories exist
directoryIsSet :: ClusterEnv -> IO Bool
directoryIsSet cEnv = doesDirectoryExist $ keysDir cEnv
