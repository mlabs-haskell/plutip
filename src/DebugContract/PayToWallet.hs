module DebugContract.PayToWallet (payTo) where

import Data.Text (Text)
import Ledger (CardanoTx, PaymentPubKeyHash)
import Ledger.Ada qualified as Ada
import Plutus.Contract (Contract, ownPaymentPubKeyHash, submitTx, waitNSlots)
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)

import Control.Monad (void)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints qualified as Contract

payTo :: PaymentPubKeyHash -> Integer -> Contract () EmptySchema Text CardanoTx
payTo toPkh amt = do
  uwnPkh <- ownPaymentPubKeyHash
  tx <- submitTx (Constraints.mustPayToPubKey toPkh (Ada.lovelaceValueOf amt) <> Contract.mustBeSignedBy uwnPkh)
  void $ waitNSlots 1
  pure tx
