module Test.Plutip.DebugContract.LockUnlock (
  lockAtScript,
  spendFromScript,
  lockThenSpend,
  validatorAddr,
) where

import Data.Map qualified as Map
import Data.Text (Text)
import Ledger (Address, CardanoTx, ScriptContext, TxId, Validator, getCardanoTxId, scriptAddress, unitDatum, unitRedeemer, validatorHash)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts.Validators qualified as Validators
import Plutus.Contract (Contract, awaitTxConfirmed, submitTx, submitTxConstraintsWith)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.V1.Ledger.Ada qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude qualified as PP

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ _ =
  PP.traceIfFalse "Some error message" check
  where
    -- cpu budget was overspent
    check =
      let l :: [Integer] = 1 : l
       in PP.length (PP.take 1_000_000_000 l) PP.== 0

data TestLock

instance Validators.ValidatorTypes TestLock where
  type DatumType TestLock = ()
  type RedeemerType TestLock = ()

typedValidator :: Validators.TypedValidator TestLock
typedValidator =
  Validators.mkTypedValidator @TestLock
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Validators.wrapValidator @() @()

validator :: Validator
validator = Validators.validatorScript typedValidator

validatorAddr :: Address
validatorAddr = scriptAddress validator

lockAtScript :: Contract () EmptySchema Text (TxId, CardanoTx)
lockAtScript = do
  let constr =
        Constraints.mustPayToOtherScript
          (validatorHash validator)
          unitDatum
          (Value.adaValueOf 10)
  tx <- submitTx constr
  awaitTxConfirmed $ getCardanoTxId tx
  pure (getCardanoTxId tx, tx)

spendFromScript :: Contract () EmptySchema Text (TxId, CardanoTx)
spendFromScript = do
  utxos <- Map.toList <$> Contract.utxosAt validatorAddr
  case utxos of
    [] -> Contract.throwError "No UTxOs at script address"
    (oref, _) : _ -> spendUtxo oref utxos
  where
    spendUtxo oref utxos = do
      let txc = Constraints.mustSpendScriptOutput oref unitRedeemer
          lookups =
            Constraints.unspentOutputs (Map.fromList utxos)
              <> Constraints.otherScript validator
      tx <- submitTxConstraintsWith @TestLock lookups txc
      awaitTxConfirmed $ getCardanoTxId tx
      pure (getCardanoTxId tx, tx)

lockThenSpend :: Contract () EmptySchema Text (TxId, CardanoTx)
lockThenSpend =
  lockAtScript >> Contract.waitNSlots 1 >> spendFromScript
