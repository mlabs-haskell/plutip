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
  validatorHash,
 )
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.TimeSlot (nominalDiffTimeToPOSIXTime)
import Ledger.Typed.Scripts (mkUntypedValidator)
import Plutus.Contract (Contract, currentNodeClientTimeRange)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
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

validator :: Versioned Validator
validator = Validators.vValidatorScript typedValidator

validatorAddr :: Address
validatorAddr = scriptHashAddress (validatorHash validator)

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

  let constr =
        Constraints.mustPayToOtherScript (validatorHash validator) unitDatum (Ada.adaValueOf 4)
          <> Constraints.mustValidateIn validInterval

  void $ Contract.awaitTime endTime
  tx <- Contract.submitTx constr
  Contract.awaitTxConfirmed $ getCardanoTxId tx
  pure "Light debug done"

successTimeContract :: NominalDiffTime -> Contract () EmptySchema Text ()
successTimeContract slotLen = lockAtScript >> unlockWithTimeCheck slotLen

lockAtScript :: Contract () EmptySchema Text ()
lockAtScript = do
  let constr =
        Constraints.mustPayToOtherScript
          (validatorHash validator)
          unitDatum
          (Ada.adaValueOf 10)
  tx <- Contract.submitTx constr
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
              , Constraints.mustValidateIn rmrInterval
              ]

          lkps =
            Hask.mconcat
              [ Constraints.otherScript validator
              , Constraints.unspentOutputs (Map.fromList utxos)
              ]
      tx <- Contract.submitTxConstraintsWith @TestTime lkps txc
      Contract.awaitTxConfirmed (getCardanoTxId tx)
    rest -> Contract.throwError $ "Unlocking error: Unwanted set of utxos: " Hask.<> Text.pack (Hask.show rest)
