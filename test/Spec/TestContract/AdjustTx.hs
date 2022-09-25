module Spec.TestContract.AdjustTx (
  runAdjustTest,
) where

import Control.Lens.Operators ((^.))
import Data.Text (Text)
import Data.Void (Void)
import Ledger (
  PaymentPubKeyHash,
  Tx (..),
  TxOut (..),
  getCardanoTxId,
 )
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OffChain qualified as OffChain
import Ledger.Value (Value)
import Plutus.Contract (
  Contract,
  adjustUnbalancedTx,
  awaitTxConfirmed,
  mkTxConstraints,
 )
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Test.Plutip.Contract (
  ClusterTest,
  TestWallets,
  assertExecution,
  initAda,
  withContract,
 )
import Test.Plutip.Internal.BotPlutusInterface.Lookups (WalletLookups (lookupWallet))
import Test.Plutip.Internal.BotPlutusInterface.Types (WalletInfo (EnterpriseInfo), WalletTag (EnterpriseTag))
import Test.Plutip.Internal.BotPlutusInterface.Wallet (BpiWallet)
import Test.Plutip.Internal.Types (ClusterEnv)
import Test.Plutip.Predicate (
  shouldSucceed,
  yieldSatisfies,
 )
import Test.Tasty (TestTree)
import Prelude

adjustTx :: PaymentPubKeyHash -> Contract () EmptySchema Text [Value]
adjustTx toPkh = do
  ownPkh <- Contract.ownFirstPaymentPubKeyHash
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
  balTx <- Contract.balanceTx adjustedTx
  crdTx <- Contract.submitBalancedTx balTx
  _ <- awaitTxConfirmed (getCardanoTxId crdTx)
  pure vals

adjustTx' :: [PaymentPubKeyHash] -> Contract () EmptySchema Text [Value]
adjustTx' [] = do
  pkh <- Contract.ownFirstPaymentPubKeyHash
  adjustTx pkh
adjustTx' (pkh : _) = adjustTx pkh

-- | Tests whether `adjustUnbalancedTx` actually tops up the
-- UTxO to get to the minimum required ADA.
runAdjustTest :: ClusterTest
runAdjustTest =
  assertExecution
    "Adjust Unbalanced Tx Contract"
    (initAda (EnterpriseTag (0 :: Int)) [1000] <> initAda (EnterpriseTag 1) [1000])
    ( withContract $ \wl -> do
        EnterpriseInfo pkh <- lookupWallet wl (EnterpriseTag (1 :: Int))
        adjustTx' [pkh]
    )
    [ shouldSucceed
    , yieldSatisfies
        "All UTxOs have minimum(?) ADA."
        (all (\val -> 500_000 <= Ada.getLovelace (Ada.fromValue val)))
    ]
