module Spec.TestContract.MustBeSignedBy (test) where

import BotPlutusInterface.Constraints (submitBpiTxConstraintsWith)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import Ledger (
  Address,
  CardanoTx,
  Language (PlutusV2),
  PaymentPubKeyHash (PaymentPubKeyHash),
  TxId,
  Versioned (Versioned),
  fromCompiledCode,
  getCardanoTxId,
  scriptHashAddress,
 )
import Ledger.Ada (adaValueOf)
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Plutus.Contract (Contract, awaitTxConfirmed, waitNSlots)
import Plutus.Contract qualified as Contract
import Plutus.Script.Utils.V2.Scripts qualified as ScriptUtils
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (mkUntypedValidator)
import Plutus.V2.Ledger.Api (PubKeyHash (PubKeyHash), ScriptContext (scriptContextTxInfo), Validator, mkValidatorScript)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx qualified
import PlutusTx.Prelude (traceIfFalse)
import PlutusTx.Prelude qualified as Plutus
import Prelude

test :: Contract w s Text (TxId, CardanoTx)
test = do
  let constr =
        Constraints.mustPayToOtherScriptWithDatumInTx
          (ScriptUtils.validatorHash validator)
          Scripts.unitDatum
          (adaValueOf 10)
  tx <- submitBpiTxConstraintsWith @Void mempty constr []
  awaitTxConfirmed $ getCardanoTxId tx
  _ <- waitNSlots 5
  mustBeSignedBy

mustBeSignedBy :: Contract w s Text (TxId, CardanoTx)
mustBeSignedBy = do
  valOuts <- Contract.utxosAt validatorAddr

  let constraints =
        mconcat (Constraints.mustBeSignedBy . PaymentPubKeyHash <$> testPubKeyHashes)
          <> mconcat ((`Constraints.mustSpendScriptOutput` Scripts.unitRedeemer) <$> Map.keys valOuts)
      lookups =
        Constraints.unspentOutputs valOuts
          <> Constraints.otherScript (Versioned validator PlutusV2)

  tx <- submitBpiTxConstraintsWith @Void lookups constraints []
  awaitTxConfirmed $ getCardanoTxId tx
  return (getCardanoTxId tx, tx)

{-# INLINEABLE testPubKeyHashes #-}
testPubKeyHashes :: [PubKeyHash]
testPubKeyHashes =
  [ "72cae61f85ed97fb0e7703d9fec382e4973bf47ea2ac9335cab1e3fe"
  , "2b0c9f64145896b8da237926a9ee664aed9923b455c7866fa241d218"
  , "bcd761c6fb451e78b604aaaba3d3fb4e61e218dc986dd4131c1e9958"
  ]

{-# INLINEABLE mkValidator #-}
mkValidator :: [PubKeyHash] -> () -> () -> ScriptContext -> Bool
mkValidator keys _ _ ctx =
  let info = scriptContextTxInfo ctx
      allSigsPresent = Plutus.all (txSignedBy info) keys
   in traceIfFalse
        "mustBeSignedBy validator error"
        allSigsPresent

validator :: Validator
validator =
  mkValidatorScript $
    $$(PlutusTx.compile [||wrap . mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode testPubKeyHashes
  where
    wrap = mkUntypedValidator

validatorAddr :: Address
validatorAddr = scriptHashAddress $ ScriptUtils.validatorHash validator
