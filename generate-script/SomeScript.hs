{-# HLINT ignore "Eta reduce" #-}
module SomeScript (script, envelopeScript) where

import Cardano.Api (serialiseToJSON, serialiseToTextEnvelope)
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Plutus.V1.Ledger.Scripts qualified as Plutus
import PlutusTx qualified
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))
import Prelude hiding (($))

{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinString -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator bs _ _ _ = trace bs ()

validator :: BuiltinString -> Plutus.Validator
validator b =
  Plutus.mkValidatorScript
    ( $$(PlutusTx.compile [||mkValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode b
    )

script :: BuiltinString -> Plutus.Script
script b = Plutus.unValidatorScript (validator b)

alwaysSucceedsScriptShortBs :: BuiltinString -> SBS.ShortByteString
alwaysSucceedsScriptShortBs b = SBS.toShort . LBS.toStrict $ serialise (script b)

alwaysSucceedsScript :: BuiltinString -> PlutusScript PlutusScriptV1
alwaysSucceedsScript b = PlutusScriptSerialised (alwaysSucceedsScriptShortBs b)

envelopeScript :: String -> String
envelopeScript i =
  Text.unpack . Text.decodeUtf8 . serialiseToJSON
    . serialiseToTextEnvelope Nothing
    $ alwaysSucceedsScript (decodeUtf8 $ stringToBuiltinByteString i)