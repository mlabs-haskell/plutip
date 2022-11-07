module Spec.TestContract.AlwaysFail (lockThenFailToSpend) where

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (
  Address,
  ScriptContext,
  Validator,
  Versioned,
  getCardanoTxId,
  scriptHashAddress,
  unitDatum,
  unitRedeemer,
  validatorHash,
 )
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (TypedValidator, ValidatorTypes (DatumType, RedeemerType))
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Contract, awaitTxConfirmed, submitTx, submitTxConstraintsWith)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
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
        Constraints.mustPayToOtherScript
          (validatorHash validator)
          unitDatum
          (Ada.adaValueOf 10)
  tx <- submitTx constr
  awaitTxConfirmed $ getCardanoTxId tx

spendFromScript :: Contract () EmptySchema Text ()
spendFromScript = do
  utxos <- Map.toList <$> Contract.utxosAt validatorAddr
  case utxos of
    [(oref, _)] -> do
      let txc = Constraints.mustSpendScriptOutput oref unitRedeemer

          lkps =
            Hask.mconcat
              [ Constraints.otherScript validator
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

validator :: Versioned Validator
validator = Scripts.vValidatorScript typedValidator

validatorAddr :: Address
validatorAddr = scriptHashAddress (validatorHash validator)
