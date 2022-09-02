-- | Purpose of this Contract is to test that "POSIXTime -> Slot -> POSIXTime"
-- conversion works propperly by exercising `POSIXTimaRange` in validator script
module Spec.TestContract.ValidateTimeRange (
  failingTimeContract,
  successTimeContract,
) where

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
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
  always,
  getCardanoTxId,
  lowerBound,
  strictUpperBound,
  unitDatum,
  validatorHash,
 )
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (mkUntypedValidator)
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators qualified as Validators
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

validator :: Validator
validator = Validators.validatorScript typedValidator

validatorAddr :: Address
validatorAddr = mkValidatorAddress validator

------------------------------------------
failingTimeContract :: Contract () EmptySchema Text Hask.String
failingTimeContract = do
  startTime <- Contract.currentTime
  -- amount of seconds was picked empirically
  -- it is relatively small, but big enough so Tx won't be silently dropped
  -- from the node mempool coz it stayed there longer than validation range
  let timeDiff = POSIXTime 5_000
      endTime = startTime + timeDiff

      validInterval = Interval (lowerBound startTime) (strictUpperBound endTime)

  let constr =
        Constraints.mustPayToOtherScript (validatorHash validator) unitDatum (Ada.adaValueOf 4)
          <> Constraints.mustValidateIn validInterval

  void $ Contract.awaitTime (endTime - POSIXTime 1_000)
  tx <- Contract.submitTx constr
  Contract.awaitTxConfirmed $ getCardanoTxId tx
  pure "Light debug done"

successTimeContract :: Contract () EmptySchema Text ()
successTimeContract = lockAtScript >> unlockWithTimeCheck

lockAtScript :: Contract () EmptySchema Text ()
lockAtScript = do
  let constr =
        Constraints.mustPayToOtherScript
          (validatorHash validator)
          unitDatum
          (Ada.adaValueOf 10)
  tx <- Contract.submitTx constr
  Contract.awaitTxConfirmed $ getCardanoTxId tx

unlockWithTimeCheck :: Contract () EmptySchema Text ()
unlockWithTimeCheck = do
  startTime <- Contract.currentTime
  let timeDiff = POSIXTime 5_000
      endTime = startTime + timeDiff

  utxos <- Map.toList <$> Contract.utxosAt validatorAddr
  case utxos of
    [(oref, _)] -> do
      let rmrInterval = Interval (lowerBound startTime) (strictUpperBound endTime)
          rmr' = TimeRedeemer startTime endTime rmrInterval
          rmr = Redeemer $ PlutusTx.toBuiltinData rmr'

      let txc =
            Hask.mconcat
              [ Constraints.mustSpendScriptOutput oref rmr
              , Constraints.mustValidateIn rmrInterval
              ]

          lkps =
            Hask.mconcat
              [ Constraints.plutusV1OtherScript validator
              , Constraints.unspentOutputs (Map.fromList utxos)
              ]
      tx <- Contract.submitTxConstraintsWith @TestTime lkps txc
      Contract.awaitTxConfirmed (getCardanoTxId tx)
    rest -> Contract.throwError $ "Unlocking error: Unwanted set of utxos: " Hask.<> Text.pack (Hask.show rest)
