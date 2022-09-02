-- | Helper contract for collecting `Value` at wallet's address
-- and assertion to perform on collected `Value`.
-- Both used by other modules of framework.
module Test.Plutip.Contract.Values (
  valueAt,
  assertValues,
) where

import Data.Aeson.Extras (encodeByteString)
import Data.Either (fromRight)
import Data.Kind (Type)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Row (Row)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8')
import Ledger (Address, ChainIndexTxOut (PublicKeyChainIndexTxOut, ScriptChainIndexTxOut))
import Ledger.Ada qualified as Ada
import Ledger.Value (CurrencySymbol (unCurrencySymbol), TokenName (unTokenName), Value)
import Ledger.Value qualified as Value
import Plutus.Contract (AsContractError, Contract, utxosAt)
import PlutusTx.Builtins (fromBuiltin)

import Test.Plutip.Contract.Types (
  ValueOrdering (VEq, VGEq, VGt, VLEq, VLt),
  compareValuesWith,
 )

valueAt ::
  forall (w :: Type) (s :: Row Type) (e :: Type).
  AsContractError e =>
  Address ->
  Contract w s e Value
valueAt addr = do
  utxos <- utxosAt addr
  pure . mconcat . map utxoValue . Map.elems $ utxos
  where
    utxoValue :: ChainIndexTxOut -> Value
    utxoValue (PublicKeyChainIndexTxOut _ v _ _) = v
    utxoValue (ScriptChainIndexTxOut _ v _ _ _) = v

assertValues :: NonEmpty (Maybe (ValueOrdering, Value)) -> NonEmpty Value -> Either Text ()
assertValues expected values =
  maybe (Right ()) (Left . report) $
    find findFailing $
      zip3 [0 :: Int ..] (NonEmpty.toList expected) (NonEmpty.toList values)
  where
    findFailing (_, Nothing, _) = False
    findFailing (_, Just (ord, v), v') = not (compareValuesWith ord v' v)

    report (_, Nothing, _) = ""
    report (walletIdx, Just (ord, expV), gotV) =
      Text.unlines
        [ mconcat
            [ "Value assertion failed on "
            , if walletIdx == 0 then "own wallet." else "wallet " <> Text.pack (show walletIdx) <> "."
            ]
        , mconcat ["Expected", showVOrd ord, ": ", showValue expV]
        , mconcat ["Got: ", showValue gotV]
        ]

    showVOrd VEq = ""
    showVOrd VGt = " greater than"
    showVOrd VLt = " less than"
    showVOrd VGEq = " greater than or equal to"
    showVOrd VLEq = " less than or equal to"

    showValue :: Value -> Text
    showValue =
      Text.intercalate ", " . map showFlatValue . Value.flattenValue

    showFlatValue :: (CurrencySymbol, TokenName, Integer) -> Text
    showFlatValue (curSymbol, name, amount)
      | curSymbol == Ada.adaSymbol = amountStr <> " lovelace"
      | Text.null tokenNameStr = amountStr <> " " <> curSymbolStr
      | otherwise = amountStr <> " " <> curSymbolStr <> "." <> tokenNameStr
      where
        amountStr = Text.pack $ show amount
        curSymbolStr = encodeByteString $ fromBuiltin $ unCurrencySymbol curSymbol
        tokenNameStr =
          let bs = fromBuiltin $ unTokenName name
           in fromRight (encodeByteString bs) $ decodeUtf8' bs
