module DebugContract.LogPkh (LogPkhContract (..), logPkh) where

import BotPlutusInterface.Types (
  HasDefinitions (
    getContract,
    getDefinitions,
    getSchema
  ),
  SomeBuiltin (SomeBuiltin),
  endpointsToSchemas,
 )
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger (pubKeyHashAddress)
import Plutus.Contract (Contract, utxosAt)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Text.Printf (printf)

logPkh :: Contract () EmptySchema Text ()
logPkh = do
  pkh <- Contract.ownPaymentPubKeyHash
  Contract.logInfo @String $ printf "Own PPKH: %s" (show pkh)
  utxos <- utxosAt $ pubKeyHashAddress pkh Nothing
  Contract.logInfo @String $ printf "Own UTxOs: %s" (show utxos)
  error $
    mconcat
      [ "Logs are ignored now, so take this error as proof that I was run.\n"
      , "--> Own PKH: " <> show pkh <> "\n"
      , "--> Own UTxOs: " <> show utxos
      ]

-- PAB stuff

data LogPkhContract
  = LogPkhContract
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HasDefinitions LogPkhContract where
  getDefinitions = []

  getSchema _ = endpointsToSchemas @EmptySchema

  getContract = \case
    LogPkhContract ->
      SomeBuiltin logPkh
