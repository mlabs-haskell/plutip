module ExampleContracts
  ( ownValueToState,
    payTo,
  )
where

import Control.Lens ((^.))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Monoid (Last (Last))
import Data.Text (Text)
import Ledger
  ( CardanoTx,
    ChainIndexTxOut,
    PaymentPubKeyHash,
    TxId,
    TxOutRef,
    Value,
    ciTxOutValue,
    getCardanoTxId,
  )
import Ledger.Ada qualified as Ada
import Ledger.Constraints.TxConstraints qualified as Constraints
import Plutus.Contract
  ( Contract,
    submitTx,
    utxosAt,
  )
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)

getUtxos :: Monoid m => Contract m EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxos = do
  addr <- NonEmpty.head <$> Contract.ownAddresses
  utxosAt addr

ownValue :: Monoid m => Contract m EmptySchema Text Value
ownValue = foldMap (^. ciTxOutValue) <$> getUtxos

-- this Contract fails, but state should change in expected way
ownValueToState :: Contract (Last Value) EmptySchema Text Value
ownValueToState = do
  ow <- ownValue
  Contract.tell (Last $ Just ow)
  return ow

payTo :: PaymentPubKeyHash -> Integer -> Contract () EmptySchema Text TxData
payTo toPkh amt = do
  tx <- submitTx (Constraints.mustPayToPubKey toPkh (Ada.lovelaceValueOf amt))
  _ <- Contract.awaitTxConfirmed (getCardanoTxId tx)
  pure $
    TxData tx (getCardanoTxId tx)

data TxData = TxData
  { cardanoTx :: CardanoTx,
    txId :: TxId
  }
  deriving stock (Show)