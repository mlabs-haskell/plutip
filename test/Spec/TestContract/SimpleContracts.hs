{-# LANGUAGE AllowAmbiguousTypes #-}
module Spec.TestContract.SimpleContracts (
  getUtxos,
  getUtxosThrowsErr,
  getUtxosThrowsEx,
  payTo,
  ownValue,
  ownValueToState,
) where

import Plutus.Contract (
  Contract,
  ContractError (ConstraintResolutionContractError),
  submitTx,
  utxosAt,
 )
import Plutus.Contract qualified as Contract

import Ledger (
  CardanoTx,
  ChainIndexTxOut,
  TxOutRef,
  Value,
  ciTxOutValue,
  pubKeyHashAddress,
 )

import Data.Map (Map)
import Ledger.Ada qualified as Ada

import Control.Monad.Reader (MonadReader (ask), runReaderT, void, lift)
import Control.Lens ((^.))
import Data.Text (Text)
import Ledger.Constraints (MkTxError (OwnPubKeyMissing))
import Ledger.Constraints qualified as Constraints
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)

import Test.Plutip.Internal.BotPlutusInterface.Wallet (ledgerPaymentPkh)
import Test.Plutip.Contract.Types (NthWallet(..))
import Test.Plutip.Contract (WrappedContract)

getUtxos :: Contract [Value] EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxos = do
  pkh <- Contract.ownPaymentPubKeyHash
  utxosAt $ pubKeyHashAddress pkh Nothing

getUtxosThrowsErr :: Contract () EmptySchema ContractError (Map TxOutRef ChainIndexTxOut)
getUtxosThrowsErr =
  Contract.throwError $ ConstraintResolutionContractError OwnPubKeyMissing

getUtxosThrowsEx :: Contract () EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxosThrowsEx = error "This Exception was thrown intentionally in Contract.\n"

payTo :: forall idx idxs. (NthWallet idx idxs) => Integer -> WrappedContract () EmptySchema Text CardanoTx idxs
payTo amt = do
  wallets <- ask
  let payToWallet = ledgerPaymentPkh $ nthWallet @idx wallets
  lift $ submitTx (Constraints.mustPayToPubKey payToWallet (Ada.lovelaceValueOf amt))

ownValue :: Contract [Value] EmptySchema Text Value
ownValue = foldMap (^. ciTxOutValue) <$> getUtxos

-- this Contract fails, but state should change in expected way
ownValueToState :: Contract [Value] EmptySchema Text ()
ownValueToState = do
  ownValue >>= Contract.tell . (: [])
  void $ Contract.throwError "Intentional fail"
  ownValue
    >>= Contract.tell
      . (: []) -- should not be in state
