module Main (main) where

import Control.Applicative ((<**>))
import Options.Applicative (Parser, helper, info)
import Options.Applicative qualified as Options
import SomeScript

main :: IO ()
main = do
  config <- Options.execParser (info (pClusterConfig <**> helper) mempty)
  let envelopedScriptString = envelopeScript (nonce config)
  writeFile (out config) envelopedScriptString

pNonce :: Parser String
pNonce =
  Options.strOption
    ( Options.long "nonce"
        <> Options.short 'n'
        <> Options.metavar "NONCE"
    )

pOut :: Parser FilePath
pOut =
  Options.strOption
    ( Options.long "out-file"
        <> Options.short 'o'
        <> Options.metavar "OUTPUT_FILE"
    )

pClusterConfig :: Parser ClusterConfig
pClusterConfig =
  ClusterConfig
    <$> pNonce
    <*> pOut

-- | Basic info about the cluster, to
-- be used by the command-line
data ClusterConfig = ClusterConfig
  { nonce :: String,
    out :: FilePath
  }
  deriving stock (Show, Eq)
