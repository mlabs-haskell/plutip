module Spec.TestContract.MintAndPay (zeroAdaOutTestContract) where

import Data.Text (Text)
import Ledger (
  CurrencySymbol,
  PaymentPubKeyHash,
  ScriptContext,
  getCardanoTxId,
 )
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts (mkUntypedMintingPolicy)
import Ledger.Typed.Scripts qualified as TypedScripts
import Ledger.Value (tokenName)
import Plutus.Contract (Contract, adjustUnbalancedTx, awaitTxConfirmed, mkTxConstraints, submitTxConfirmed, submitTxConstraintsWith)

import Data.Void (Void)
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.Script.Utils.V1.Scripts qualified as ScriptUtils
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude qualified as PP
import Prelude

{- This test contract checks that outputs with 0 Ada are hadled properly.
 BPI does adjustment of ouptupt even w/o explicit `adjustUnbalancedTx`,
 so this test contract checks bot cases - with implicit and explicit adjustment.
-}
zeroAdaOutTestContract :: PaymentPubKeyHash -> Contract () EmptySchema Text ()
zeroAdaOutTestContract pkh =
  mintAndPayWithAdjustment 0 pkh
    >> mintAndPayNoAdjustment 0 pkh
    >> mintAndPayWithAdjustment 7 pkh
    >> mintAndPayNoAdjustment 7 pkh

mintAndPayWithAdjustment :: Integer -> PaymentPubKeyHash -> Contract () EmptySchema Text ()
mintAndPayWithAdjustment tokensAmt pkh = do
  let token = Value.singleton currencySymbol (tokenName "ff") tokensAmt
      txc1 =
        Constraints.mustMintValueWithRedeemer Scripts.unitRedeemer token
          <> Constraints.mustPayToPubKey pkh token
      lookups1 = Constraints.plutusV1MintingPolicy mintingPolicy

  utx <- mkTxConstraints @Void lookups1 txc1
  tx <- adjustUnbalancedTx utx
  submitTxConfirmed tx

mintAndPayNoAdjustment :: Integer -> PaymentPubKeyHash -> Contract () EmptySchema Text ()
mintAndPayNoAdjustment tokensAmt pkh = do
  let token = Value.singleton currencySymbol (tokenName "ff") tokensAmt
      txc1 =
        Constraints.mustMintValueWithRedeemer Scripts.unitRedeemer token
          <> Constraints.mustPayToPubKey pkh token
      lookups1 = Constraints.plutusV1MintingPolicy mintingPolicy

  tx <- submitTxConstraintsWith @Void lookups1 txc1
  awaitTxConfirmed (getCardanoTxId tx)

-- minting policy
{-# INLINEABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy _ _ =
  PP.traceIfFalse "Mint only check" check
  where
    check = PP.length someWork PP.== 10
    someWork = PP.sort [9, 8, 7, 6, 5, 4, 3, 2, 1, 0] :: [Integer]

mintingPolicy :: TypedScripts.MintingPolicy
mintingPolicy =
  Scripts.mkMintingPolicyScript
    $$(PlutusTx.compile [||mkUntypedMintingPolicy mkPolicy||])

currencySymbol :: CurrencySymbol
currencySymbol = ScriptUtils.scriptCurrencySymbol mintingPolicy
