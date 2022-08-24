module Spec.TestContract.AdjustTx (
  runAdjustTest,
) where

import BotPlutusInterface.Contract (runContract)
import Control.Lens.Operators ((^.))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Ledger (
  PaymentPubKeyHash,
  Tx (..),
  TxOut (..),
  getCardanoTxId,
 )
import Ledger qualified
import Ledger.Ada (adaValueOf)
import Ledger.Ada qualified as Ada

-- import Ledger.Constraints (mkTx)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OffChain qualified as OffChain
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts (TypedValidator, Validator, ValidatorTypes, mkUntypedMintingPolicy)
import Ledger.Typed.Scripts qualified as TypedScripts
import Ledger.Value (Value, flattenValue, tokenName)
import Ledger.Value qualified
import Plutus.Contract (
  Contract,
  ContractError (ConstraintResolutionContractError),
  adjustUnbalancedTx,
  awaitTxConfirmed,
  mkTxConstraints,
  submitTx,
  submitTxConstraintsWith,
  submitUnbalancedTx,
  utxosAt,
 )
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.Script.Utils.V1.Scripts qualified as ScriptUtils
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude qualified as PP
import Test.Plutip.Contract
import Test.Plutip.Predicate (
  shouldSucceed,
  yieldSatisfies,
 )
import Prelude

adjustTx :: PaymentPubKeyHash -> Contract () EmptySchema Text [Value]
adjustTx toPkh = do
  ownPkh <- Contract.ownPaymentPubKeyHash
  let ownAddr = Ledger.pubKeyHashAddress ownPkh Nothing
  utxos <- Contract.utxosAt ownAddr
  let consts =
        Constraints.mustPayToPubKey toPkh (Ada.lovelaceValueOf 50)
      lkups =
        Constraints.ownPaymentPubKeyHash ownPkh
          <> Constraints.unspentOutputs utxos
  unbalancedTx <- mkTxConstraints @Void lkups consts
  -- Adjust the Tx so that all UTxOs have the minimum ADA.
  adjustedTx <- adjustUnbalancedTx unbalancedTx
  let rawTx = adjustedTx ^. OffChain.tx
      vals = map txOutValue $ txOutputs rawTx
  -- crdTx <- submitUnbalancedTx adjustedTx
  balTx <- Contract.balanceTx adjustedTx
  crdTx <- Contract.submitBalancedTx balTx
  _ <- Contract.awaitTxConfirmed (getCardanoTxId crdTx)
  pure vals

adjustTx' :: [PaymentPubKeyHash] -> Contract () EmptySchema Text [Value]
adjustTx' [] = do
  pkh <- Contract.ownPaymentPubKeyHash
  adjustTx pkh
adjustTx' (pkh : _) = adjustTx pkh

-- The actual test
runAdjustTest =
  assertExecution
    "Adjust Unbalanced Tx Contract"
    (initAda [1000] <> initAda [1000])
    (withContract adjustTx')
    [ shouldSucceed
    , yieldSatisfies
        "All UTxOs have minimum ADA."
        (all (\val -> 2_000_000 <= Ada.getLovelace (Ada.fromValue val)))
    ]
