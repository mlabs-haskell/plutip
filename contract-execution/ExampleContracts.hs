module ExampleContracts (
  ownValueToState,
) where

import Control.Lens (view)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Monoid (Last (Last))
import Data.Text (Text)
import Ledger (
  DecoratedTxOut,
  TxOutRef,
  Value,
  decoratedTxOutValue,
 )
import Plutus.Contract (
  Contract,
  utxosAt,
 )
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)

getUtxos :: Monoid m => Contract m EmptySchema Text (Map TxOutRef DecoratedTxOut)
getUtxos = do
  addr <- NonEmpty.head <$> Contract.ownAddresses
  utxosAt addr

ownValue :: Monoid m => Contract m EmptySchema Text Value
ownValue = foldMap (view decoratedTxOutValue) <$> getUtxos

-- this Contract fails, but state should change in expected way
ownValueToState :: Contract (Last Value) EmptySchema Text Value
ownValueToState = do
  ow <- ownValue
  Contract.tell (Last $ Just ow)
  return ow
