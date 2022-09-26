module Spec.TestContract.MintAndPay (mintAndPayTokens) where

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
import Plutus.Contract (Contract, mkTxConstraints, adjustUnbalancedTx, submitTxConfirmed)
-- import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.Script.Utils.V1.Scripts qualified as ScriptUtils
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude qualified as PP
import Prelude
import Data.Void (Void)


mintAndPayTokens :: PaymentPubKeyHash -> Contract () EmptySchema Text ()
mintAndPayTokens pkh = do
      let token = Value.singleton currencySymbol (tokenName "ff") 10
          txc1 = Constraints.mustMintValueWithRedeemer Scripts.unitRedeemer token
                  <> Constraints.mustPayToPubKey pkh token
          lookups1 = Constraints.plutusV1MintingPolicy mintingPolicy

      utx <- mkTxConstraints @Void lookups1 txc1
      tx <- adjustUnbalancedTx utx
      submitTxConfirmed tx

-- minting policy
{-# INLINEABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy _ _ =
  PP.traceIfFalse "Mint only check" check
  where
    -- info = scriptContextTxInfo ctx
    check = PP.length someWork PP.== 10
    someWork = PP.sort [9, 8, 7, 6, 5, 4, 3, 2, 1, 0] :: [Integer]

mintingPolicy :: TypedScripts.MintingPolicy
mintingPolicy =
  Scripts.mkMintingPolicyScript
    $$(PlutusTx.compile [||mkUntypedMintingPolicy mkPolicy||])

currencySymbol :: CurrencySymbol
currencySymbol = ScriptUtils.scriptCurrencySymbol mintingPolicy
