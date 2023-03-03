{-# HLINT ignore "Eta reduce" #-}
module SomeScript (script, envelopeScript) where

import Cardano.Api (PlutusScriptV2, serialiseToJSON, serialiseToTextEnvelope)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Codec.Serialise (serialise)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (toShort)
import Data.Maybe (Maybe (Nothing))
import Data.String (String)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Ledger (ScriptContext)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators (mkUntypedValidator)
import Plutus.V1.Ledger.Api (Script, fromCompiledCode)
import Plutus.V1.Ledger.Scripts qualified as Plutus
import PlutusTx qualified
import PlutusTx.Prelude (Bool (True), Integer)
import Prelude (Int, (.))

newtype MyCustomDatum = MyCustomDatum Integer

PlutusTx.unstableMakeIsData ''MyCustomDatum

newtype MyCustomRedeemer = MyCustomRedeemer Integer

PlutusTx.unstableMakeIsData ''MyCustomRedeemer

mkValidator :: Int -> MyCustomDatum -> MyCustomRedeemer -> ScriptContext -> Bool
mkValidator _ _ _ _ = True

validator :: Int -> Plutus.Validator
validator d =
  Plutus.mkValidatorScript
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedValidator (mkValidator d)

script :: Int -> Script
script i = fromCompiledCode $$(PlutusTx.compile [||validator i||])

envelopeScript :: Int -> String
envelopeScript =
  Text.unpack . Text.decodeUtf8 . serialiseToJSON
    . serialiseToTextEnvelope Nothing
    . PlutusScriptSerialised @PlutusScriptV2
    . toShort
    . toStrict
    . serialise
    . script