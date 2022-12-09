module Spec.TestContract.AlwaysFail (lockThenFailToSpend) where

import BotPlutusInterface.Constraints (submitBpiTxConstraintsWith)
import Control.Monad (void)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Ledger (
  Address,
  Language (PlutusV1),
  ScriptContext,
  Validator (Validator),
  Versioned (Versioned),
  fromCompiledCode,
  getCardanoTxId,
  scriptHashAddress,
  unitDatum,
  unitRedeemer,
 )
import Ledger.Ada qualified as Ada
import Ledger.Constraints (otherData)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (TypedValidator, ValidatorTypes (DatumType, RedeemerType))
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Contract, awaitTxConfirmed, submitTxConstraintsWith)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.Script.Utils.V1.Scripts qualified as ScriptUtils
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Hask

lockThenFailToSpend :: Contract () EmptySchema Text ()
lockThenFailToSpend = do
  _ <- lockAtScript
  wait 1
  void spendFromScript
  where
    wait = void . Contract.waitNSlots

lockAtScript :: Contract () EmptySchema Text ()
lockAtScript = do
  let constr =
        Constraints.mustPayToOtherScriptWithDatumInTx -- WARN: at the moment `mustPayToOtherScript` causes `DatumNotFound` error during constraints resolution
          (ScriptUtils.validatorHash validator)
          unitDatum
          (Ada.adaValueOf 10)
      lookups = otherData unitDatum
  tx <- submitBpiTxConstraintsWith @Void lookups constr []
  awaitTxConfirmed $ getCardanoTxId tx

spendFromScript :: Contract () EmptySchema Text ()
spendFromScript = do
  utxos <- Map.toList <$> Contract.utxosAt validatorAddr
  case utxos of
    [(oref, _)] -> do
      let txc = Constraints.mustSpendScriptOutput oref unitRedeemer

          lkps =
            Hask.mconcat
              [ Constraints.plutusV1OtherScript validator
              , Constraints.unspentOutputs (Map.fromList utxos)
              ]
      tx <- submitTxConstraintsWith @AlwaysFail lkps txc
      Contract.awaitTxConfirmed (getCardanoTxId tx)
    rest -> Contract.throwError $ "Unlocking error: Unwanted set of utxos: " Hask.<> Text.pack (Hask.show rest)

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ _ = traceError "I always fail"

data AlwaysFail

instance ValidatorTypes AlwaysFail where
  type DatumType AlwaysFail = ()
  type RedeemerType AlwaysFail = ()

typedValidator :: TypedValidator AlwaysFail
typedValidator =
  Scripts.mkTypedValidator @AlwaysFail
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @() @()

compiled :: Validator
compiled = Validator $ fromCompiledCode $$(PlutusTx.compile [||mkValidator||])

validator :: Validator
validator = Scripts.validatorScript typedValidator

validatorAddr :: Address
validatorAddr = scriptHashAddress (ScriptUtils.validatorHash validator)
