module ExampleContracts (
  ownValueToState,
) where

import Plutus.Contract (
  Contract,
  utxosAt,
 )
import Plutus.Contract qualified as Contract

import Ledger (
  ChainIndexTxOut,
  TxOutRef,
  Value,
  ciTxOutValue,
 )

import Data.Map (Map)

import Control.Lens ((^.))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Monoid (Last (Last))
import Data.Text (Text)
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
