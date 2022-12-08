module Spec.TestContract.VasilFeatures (
  testSendInlineDatum,
  testSpendInlineDatum,
  testSpendReferenceInput,
  testSendReferenceScript,
  testSpendReferenceScript,
) where

import BotPlutusInterface.Constraints (submitBpiTxConstraintsWith)
import Control.Lens ((^.), (^?))
import Control.Monad (void)
import Data.List (partition)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import Ledger (
  Address,
  DatumFromQuery (DatumInline),
  DecoratedTxOut,
  Language (PlutusV2),
  Versioned (Versioned),
  decoratedTxOutReferenceScript,
  decoratedTxOutScriptDatum,
  getCardanoTxId,
  scriptHashAddress,
  unPaymentPubKeyHash,
 )
import Ledger.Ada (adaValueOf)
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Plutus.Contract (Contract, awaitTxConfirmed, waitNSlots)
import Plutus.Contract qualified as Contract
import Plutus.Script.Utils.V2.Scripts qualified as ScriptUtils
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (mkUntypedValidator)
import Plutus.V2.Ledger.Api (
  Datum (Datum),
  OutputDatum (OutputDatum),
  PubKeyHash,
  ScriptContext (scriptContextTxInfo),
  TxInInfo,
  TxInfo (txInfoReferenceInputs),
  Validator (Validator),
  mkValidatorScript,
  toBuiltinData,
  txInInfoResolved,
  txOutDatum,
 )
import Plutus.V2.Ledger.Contexts (findOwnInput)
import PlutusTx qualified
import PlutusTx.Prelude (traceIfFalse)
import PlutusTx.Prelude qualified as Plutus
import Prelude

-- Tests
-- send reference script
-- spend reference script

assertContract :: Text -> Bool -> Contract w s Text ()
assertContract _ True = pure ()
assertContract err _ = Contract.throwError err

{-# INLINEABLE expectedDatum #-}
expectedDatum :: Datum
expectedDatum = Datum $ toBuiltinData (10 :: Integer)

sendNInlineDatums :: (PubKeyHash -> Validator) -> Int -> Contract w s Text Validator
sendNInlineDatums mkValidator count = do
  pkh <- unPaymentPubKeyHash <$> Contract.ownFirstPaymentPubKeyHash

  let validator = mkValidator pkh
      constraints =
        mconcat $
          replicate count $
            Constraints.mustPayToOtherScriptWithInlineDatum
              (ScriptUtils.validatorHash validator)
              expectedDatum
              (adaValueOf 10)
  tx <- submitBpiTxConstraintsWith @Void mempty constraints []
  awaitTxConfirmed $ getCardanoTxId tx
  void $ waitNSlots 5
  pure validator

testSendInlineDatum :: Contract w s Text ()
testSendInlineDatum = do
  validator <- sendNInlineDatums inlineDatumValidator 1

  valOuts <- Contract.utxosAt $ validatorAddress validator
  let correctDatumCheck :: DecoratedTxOut -> Bool
      correctDatumCheck txOut =
        Just expectedDatum == do
          DatumInline d <- snd <$> txOut ^? decoratedTxOutScriptDatum
          pure d
  assertContract "Missing expected inline datum" $ any correctDatumCheck valOuts

testSpendInlineDatum :: Contract w s Text ()
testSpendInlineDatum = do
  validator <- sendNInlineDatums inlineDatumValidator 1

  valOuts <- Contract.utxosAt $ validatorAddress validator

  let constraints = mconcat ((`Constraints.mustSpendScriptOutput` Scripts.unitRedeemer) <$> Map.keys valOuts)
      lookups =
        Constraints.unspentOutputs valOuts
          <> Constraints.otherScript (Versioned validator PlutusV2)
  tx <- submitBpiTxConstraintsWith @Void lookups constraints []
  awaitTxConfirmed $ getCardanoTxId tx
  void $ waitNSlots 5

testSpendReferenceInput :: Contract w s Text ()
testSpendReferenceInput = do
  -- Put 2 inputs with inline datums at our new validator
  -- We will spend one, and use the other as a reference input, allowing onchain code to verify the ref
  validator <- sendNInlineDatums referenceInputValidator 2
  valOuts <- Contract.utxosAt $ validatorAddress validator

  (ref1, ref2) <- case Map.keys valOuts of
    [ref1, ref2] -> pure (ref1, ref2)
    _ -> Contract.throwError "Wrong number of utxos"

  let constraints =
        Constraints.mustSpendScriptOutput ref1 Scripts.unitRedeemer
          <> Constraints.mustReferenceOutput ref2
      lookups =
        Constraints.unspentOutputs valOuts
          <> Constraints.otherScript (Versioned validator PlutusV2)
  tx <- submitBpiTxConstraintsWith @Void lookups constraints []
  awaitTxConfirmed $ getCardanoTxId tx
  void $ waitNSlots 5

sendReferenceScript :: (PubKeyHash -> Validator) -> Contract w s Text Validator
sendReferenceScript mkValidator = do
  pkh <- unPaymentPubKeyHash <$> Contract.ownFirstPaymentPubKeyHash

  let validator = mkValidator pkh
      constraints =
        Constraints.mustPayToAddressWithReferenceValidator
          (validatorAddress validator)
          (ScriptUtils.validatorHash validator)
          Nothing
          (adaValueOf 10)
      lookups =
        Constraints.otherScript (Versioned validator PlutusV2)
  tx <- submitBpiTxConstraintsWith @Void lookups constraints []
  awaitTxConfirmed $ getCardanoTxId tx
  void $ waitNSlots 5
  pure validator

hasRefScriptCheck :: Validator -> DecoratedTxOut -> Bool
hasRefScriptCheck validator txOut =
  Just validator == do
    Validator . Scripts.unversioned <$> txOut ^. decoratedTxOutReferenceScript

testSendReferenceScript :: Contract w s Text ()
testSendReferenceScript = do
  validator <- sendReferenceScript inlineDatumValidator

  valOuts <- Contract.utxosAt $ validatorAddress validator
  assertContract "Missing expected reference script" $ any (hasRefScriptCheck validator) valOuts

testSpendReferenceScript :: Contract w s Text ()
testSpendReferenceScript = do
  validator <- sendReferenceScript inlineDatumValidator
  void $ sendNInlineDatums inlineDatumValidator 1

  valOuts <- Contract.utxosAt $ validatorAddress validator
  (scriptRefTxOutRef, spendTxOutRef) <- case partition (hasRefScriptCheck validator . snd) $ Map.toList valOuts of
    ([(scriptRefTxOutRef, _)], [(spendTxOutRef, _)]) -> pure (scriptRefTxOutRef, spendTxOutRef)
    _ -> Contract.throwError "Cound't separate script utxos"

  let constraints =
        Constraints.mustSpendScriptOutputWithReference spendTxOutRef Scripts.unitRedeemer scriptRefTxOutRef
      lookups =
        -- Note specifically the LACK of otherScript
        Constraints.unspentOutputs valOuts
  tx <- submitBpiTxConstraintsWith @Void lookups constraints []
  awaitTxConfirmed $ getCardanoTxId tx
  void $ waitNSlots 5

--- Validators

validatorAddress :: Validator -> Address
validatorAddress = scriptHashAddress . ScriptUtils.validatorHash

-- Inline datum validator
{-# INLINEABLE mkInlineDatumValidator #-}
mkInlineDatumValidator :: PubKeyHash -> Datum -> () -> ScriptContext -> Bool
mkInlineDatumValidator _ _ _ ctx =
  traceIfFalse "Incorrect datum" $
    (txOutDatum . txInInfoResolved <$> findOwnInput ctx) Plutus.== Just (OutputDatum expectedDatum)

-- Parameterised on pkh to be unique per caller
inlineDatumValidator :: PubKeyHash -> Validator
inlineDatumValidator pkh =
  mkValidatorScript $
    $$(PlutusTx.compile [||wrap . mkInlineDatumValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode pkh
  where
    wrap = mkUntypedValidator

-- Reference input validator (with optional inline datum check)
{-# INLINEABLE mkReferenceInputValidator #-}
mkReferenceInputValidator :: PubKeyHash -> Datum -> () -> ScriptContext -> Bool
mkReferenceInputValidator _ _ _ ctx =
  traceIfFalse "Missing reference input" $ (txOutDatum . txInInfoResolved <$> firstRefInput) Plutus.== Just (OutputDatum expectedDatum)
  where
    firstRefInput :: Maybe TxInInfo
    firstRefInput = case txInfoReferenceInputs $ scriptContextTxInfo ctx of
      [i] -> Just i
      _ -> Nothing

-- Parameterised on pkh to be unique per caller
referenceInputValidator :: PubKeyHash -> Validator
referenceInputValidator pkh =
  mkValidatorScript $
    $$(PlutusTx.compile [||wrap . mkReferenceInputValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode pkh
  where
    wrap = mkUntypedValidator
