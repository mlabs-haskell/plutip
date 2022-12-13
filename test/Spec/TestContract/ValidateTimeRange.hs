-- | Purpose of this Contract is to test that "POSIXTime -> Slot -> POSIXTime"
-- conversion works propperly by exercising `POSIXTimaRange` in validator script
module Spec.TestContract.ValidateTimeRange (
  failingTimeContract,
  successTimeContract,
) where

import BotPlutusInterface.Constraints (submitBpiTxConstraintsWith)
import BotPlutusInterface.Constraints qualified as Constraints
import Control.Monad (void)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime)
import Ledger (
  Address,
  Extended (Finite),
  Interval (Interval),
  LowerBound (LowerBound),
  POSIXTime (POSIXTime),
  POSIXTimeRange,
  Redeemer (Redeemer),
  ScriptContext (scriptContextTxInfo),
  TxInfo (txInfoValidRange),
  UpperBound (UpperBound),
  Validator,
  Versioned,
  always,
  getCardanoTxId,
  lowerBound,
  scriptHashAddress,
  strictUpperBound,
  unitDatum,
 )
import Ledger.Ada qualified as Ada
import Ledger.Constraints (otherData)
import Ledger.Constraints qualified as Constraints
import Ledger.TimeSlot (nominalDiffTimeToPOSIXTime)
import Ledger.Typed.Scripts (mkUntypedValidator)
import Plutus.Contract (Contract, currentNodeClientTimeRange)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators qualified as Validators
import Plutus.Script.Utils.V2.Typed.Scripts (validatorHash)
import Plutus.V1.Ledger.Interval (member)
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Hask

data TestTime

data TimeRedeemer = TimeRedeemer
  { start :: POSIXTime
  , end :: POSIXTime
  , range :: POSIXTimeRange
  }

PlutusTx.unstableMakeIsData ''TimeRedeemer
PlutusTx.makeLift ''TimeRedeemer

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> TimeRedeemer -> ScriptContext -> Bool
mkValidator _ timeRmr ctx =
  rangeIsBound
    && startInRange
    && endInNotRange
    && lowerBoundsSame
    && upperBoundsSame
    && rangesAreSame
    && boundsTimesCheck
  where
    info = scriptContextTxInfo ctx
    vRange = txInfoValidRange info
    (TimeRedeemer rmrStart rmrEnd rmrRange) = timeRmr

    (Interval ctxLb ctxUb) = vRange
    (Interval rmrLb rmrUb) = rmrRange

    rangeIsBound =
      traceIfFalse "Range should be bounded" $
        vRange /= always

    startInRange =
      traceIfFalse "Start not in range" $
        rmrStart `member` vRange

    endInNotRange =
      traceIfFalse "End in range (shouldn't be)" $
        not $
          rmrEnd `member` vRange

    lowerBoundsSame =
      traceIfFalse "Lower bounds not same" $ ctxLb == rmrLb

    upperBoundsSame =
      traceIfFalse "Upper bounds not same" $ ctxUb == rmrUb

    rangesAreSame =
      traceIfFalse "Ranges not equal" $
        vRange == rmrRange

    boundsTimesCheck =
      -- True
      traceIfFalse "Bounds times check failed" $
        let (LowerBound (Finite startTime) _) = ctxLb
            (UpperBound (Finite endTime) _) = ctxUb
         in startTime == rmrStart && endTime == rmrEnd

instance Validators.ValidatorTypes TestTime where
  type DatumType TestTime = ()
  type RedeemerType TestTime = TimeRedeemer

typedValidator :: Validators.TypedValidator TestTime
typedValidator =
  Validators.mkTypedValidator @TestTime
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedValidator @() @TimeRedeemer

validator :: Versioned Validator
validator = Validators.vValidatorScript typedValidator

validatorAddr :: Address
validatorAddr = scriptHashAddress (validatorHash typedValidator)

------------------------------------------
{- Number of slots to wait was picked empirically.
  With dafeult Plutip's slot length 0.2 waiting less slots behaves buggy,
  could be because Tx stays in node mempool longer than set validation period.
-}
slotsTowait :: Integer
slotsTowait = 20

failingTimeContract :: NominalDiffTime -> Contract () EmptySchema Text Hask.String
failingTimeContract slotLen = do
  (_, startTime) <- currentNodeClientTimeRange
  let timeDiff =
        let (POSIXTime t) = nominalDiffTimeToPOSIXTime slotLen
         in (POSIXTime $ t * slotsTowait)
      endTime = startTime + timeDiff

      validInterval = Interval (lowerBound startTime) (strictUpperBound endTime)

  -- WARN: mustPayToOtherScript doesn't work with DatumNotFound
  let constr = Constraints.mustPayToOtherScriptWithDatumInTx (validatorHash typedValidator) unitDatum (Ada.adaValueOf 4)
      lookups = otherData unitDatum
  void $ Contract.awaitTime endTime
  tx <- submitBpiTxConstraintsWith @TestTime lookups constr (Constraints.mustValidateInFixed validInterval)
  Contract.awaitTxConfirmed $ getCardanoTxId tx
  pure "Light debug done"

successTimeContract :: NominalDiffTime -> Contract () EmptySchema Text ()
successTimeContract slotLen = lockAtScript >> unlockWithTimeCheck slotLen

lockAtScript :: Contract () EmptySchema Text ()
lockAtScript = do
  let constr =
        Constraints.mustPayToOtherScriptWithDatumInTx -- WARN: at the moment `mustPayToOtherScript` causes `DatumNotFound` error during constraints resolution
          (validatorHash typedValidator)
          unitDatum
          (Ada.adaValueOf 10)
      lookups = otherData unitDatum
  tx <- submitBpiTxConstraintsWith @TestTime lookups constr []
  Contract.awaitTxConfirmed $ getCardanoTxId tx

unlockWithTimeCheck :: NominalDiffTime -> Contract () EmptySchema Text ()
unlockWithTimeCheck slotLen = do
  (_, startTime) <- currentNodeClientTimeRange
  let timeDiff =
        let (POSIXTime t) = nominalDiffTimeToPOSIXTime slotLen
         in (POSIXTime $ t * slotsTowait)
      endTime = startTime + timeDiff

  -- Hask.error $ "Time: " <> Hask.show timeDiff
  utxos <- Map.toList <$> Contract.utxosAt validatorAddr
  case utxos of
    [(oref, _)] -> do
      let rmrInterval = Interval (lowerBound startTime) (strictUpperBound endTime)
          rmr' = TimeRedeemer startTime endTime rmrInterval
          rmr = Redeemer $ PlutusTx.toBuiltinData rmr'

      let txc =
            Hask.mconcat
              [ Constraints.mustSpendScriptOutput oref rmr
              ]

          lkps =
            Hask.mconcat
              [ Constraints.otherScript validator
              , Constraints.unspentOutputs (Map.fromList utxos)
              ]
      tx <- submitBpiTxConstraintsWith @TestTime lkps txc (Constraints.mustValidateInFixed rmrInterval)
      Contract.awaitTxConfirmed (getCardanoTxId tx)
    rest -> Contract.throwError $ "Unlocking error: Unwanted set of utxos: " Hask.<> Text.pack (Hask.show rest)
