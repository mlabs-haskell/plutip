module Spec.TestContract.LockSpendMint (lockThenSpend) where

import Control.Monad (void)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Text (Text)
import Ledger (
  Address,
  CardanoTx,
  ChainIndexTxOut,
  CurrencySymbol,
  PaymentPubKeyHash (PaymentPubKeyHash),
  ScriptContext (scriptContextTxInfo),
  TxId,
  TxInfo (txInfoMint),
  TxOutRef,
  getCardanoTxId,
  scriptHashAddress,
 )
import Ledger.Ada (adaValueOf)
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts (TypedValidator, Validator, ValidatorTypes, mkUntypedMintingPolicy)
import Ledger.Typed.Scripts qualified as TypedScripts
import Ledger.Value (flattenValue, tokenName)
import Plutus.Contract (Contract, awaitTxConfirmed, submitTx, submitTxConstraintsWith)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.Script.Utils.V1.Scripts qualified as ScriptUtils
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude qualified as PP
import Prelude

lockThenSpend :: Contract () EmptySchema Text [(TxOutRef, ChainIndexTxOut)]
lockThenSpend = do
  _ <- lockAtScript
  wait 1
  _ <- spendFromScript
  wait 1
  addr <- NonEmpty.head <$> Contract.ownAddresses
  Map.toList <$> Contract.utxosAt addr
  where
    wait = void . Contract.waitNSlots

lockAtScript :: Contract () EmptySchema Text (TxId, CardanoTx)
lockAtScript = do
  let constr =
        Constraints.mustPayToOtherScript
          (ScriptUtils.validatorHash validator)
          Scripts.unitDatum
          (adaValueOf 10)
  let constr2 =
        Constraints.mustPayToOtherScript
          (ScriptUtils.validatorHash $ validator2 2)
          Scripts.unitDatum
          (adaValueOf 10)
  tx <- submitTx (constr <> constr2)
  awaitTxConfirmed $ getCardanoTxId tx
  pure (getCardanoTxId tx, tx)

spendFromScript :: Contract () EmptySchema Text (TxId, CardanoTx)
spendFromScript = do
  utxos1 <- Map.toList <$> Contract.utxosAt validatorAddr
  utxos2 <- Map.toList <$> Contract.utxosAt (validatorAddr2 2)
  case (utxos1, utxos2) of
    ([], _) -> Contract.throwError "No UTxOs at script address"
    (_, []) -> Contract.throwError "No UTxOs at script address"
    ((oref1, _) : _, (oref2, _) : _) -> spendUtxo oref1 utxos1 oref2 utxos2
  where
    spendUtxo oref1 utxos1 oref2 utxos2 = do
      let token = Value.singleton currencySymbol (tokenName "ff") 1
          txc1 =
            Constraints.mustSpendScriptOutput oref1 Scripts.unitRedeemer
              <> Constraints.mustMintValueWithRedeemer Scripts.unitRedeemer token
          lookups1 =
            Constraints.unspentOutputs (Map.fromList utxos1)
              <> Constraints.otherScript validator
              <> Constraints.mintingPolicy mintingPolicy

      let txc2 =
            Constraints.mustSpendScriptOutput oref2 Scripts.unitRedeemer
              <> Constraints.mustPayToPubKey
                (PaymentPubKeyHash "72cae61f85ed97fb0e7703d9fec382e4973bf47ea2ac9335cab1e3fe")
                (adaValueOf 200)
          lookups2 =
            Constraints.unspentOutputs (Map.fromList utxos2)
              <> Constraints.otherScript (validator2 2)

      tx <-
        submitTxConstraintsWith @TestLockSpend
          (lookups1 <> lookups2)
          (txc1 <> txc2)
      awaitTxConfirmed $ getCardanoTxId tx
      pure (getCardanoTxId tx, tx)

-- Always true Script and spending contract

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ _ = PP.traceIfFalse "validator 1 error" True

data TestLockSpend

instance ValidatorTypes TestLockSpend where
  type DatumType TestLockSpend = ()
  type RedeemerType TestLockSpend = ()

typedValidator :: TypedValidator TestLockSpend
typedValidator =
  TypedScripts.mkTypedValidator @TestLockSpend
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = TypedScripts.mkUntypedValidator @() @()

validator :: Validator
validator = TypedScripts.validatorScript typedValidator

validatorAddr :: Address
validatorAddr = scriptHashAddress $ ScriptUtils.validatorHash validator

{-# INLINEABLE mkValidator2 #-}
mkValidator2 :: Integer -> () -> () -> ScriptContext -> Bool
mkValidator2 i _ _ _ =
  if i PP./= 1
    then PP.traceIfFalse "looooooooooooong" check
    else PP.traceIfFalse "short" check
  where
    someWork = PP.sort $ PP.reverse [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Integer]
    check = PP.length someWork PP.== 10

data TestLockSpend2

instance ValidatorTypes TestLockSpend2 where
  type DatumType TestLockSpend2 = ()
  type RedeemerType TestLockSpend2 = ()

typedValidator2 :: Integer -> TypedValidator TestLockSpend
typedValidator2 uid =
  TypedScripts.mkTypedValidator @TestLockSpend
    ($$(PlutusTx.compile [||mkValidator2||]) `PlutusTx.applyCode` PlutusTx.liftCode uid)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = TypedScripts.mkUntypedValidator @() @()

validator2 :: Integer -> Validator
validator2 = TypedScripts.validatorScript . typedValidator2

validatorAddr2 :: Integer -> Address
validatorAddr2 = scriptHashAddress . ScriptUtils.validatorHash . validator2

-- minting policy
{-# INLINEABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy _ ctx =
  PP.traceIfFalse "Let me mint" check
  where
    info = scriptContextTxInfo ctx
    check =
      PP.length (flattenValue PP.$ txInfoMint info) PP.== 1
        PP.&& PP.length someWork PP.== 10

    someWork = PP.sort [9, 8, 7, 6, 5, 4, 3, 2, 1, 0] :: [Integer]

mintingPolicy :: TypedScripts.MintingPolicy
mintingPolicy =
  Scripts.mkMintingPolicyScript
    $$(PlutusTx.compile [||mkUntypedMintingPolicy mkPolicy||])

currencySymbol :: CurrencySymbol
currencySymbol = ScriptUtils.scriptCurrencySymbol mintingPolicy
