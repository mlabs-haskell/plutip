-- | Cardano-Api/Ledger/Cardano-Wallet address conversions
module Tools.Address (
  walletToCardano,
  walletToCardanoAny,
  walletToLedger,
) where

import Cardano.Api qualified as CAPI
import Cardano.Wallet.Primitive.Types.Address qualified as Wallet
import Control.Arrow (left)
import Data.Data (Proxy)
import Data.Proxy (Proxy (Proxy))
import Ledger.Tx.CardanoAPI qualified as Ledger
import Plutus.V1.Ledger.Address qualified as Ledger

data AddressConversionError
  = WalletToCardanoDeserializationError
  | WalletToLedgerError Ledger.FromCardanoError
  deriving stock (Show)

-- walletToCardano :: Wallet.Address -> Maybe (CAPI.Address CAPI.ShelleyAddr)
walletToCardano :: Wallet.Address -> Either AddressConversionError (CAPI.Address CAPI.ShelleyAddr)
walletToCardano (Wallet.Address rawBytes) =
  let px = Proxy :: Proxy (CAPI.Address CAPI.ShelleyAddr)
   in maybe
        (Left WalletToCardanoDeserializationError)
        Right
        (CAPI.deserialiseFromRawBytes (CAPI.proxyToAsType px) rawBytes)

walletToCardanoAny :: Wallet.Address -> Either AddressConversionError CAPI.AddressAny
walletToCardanoAny = fmap CAPI.AddressShelley . walletToCardano

walletToLedger :: Wallet.Address -> Either AddressConversionError Ledger.Address
walletToLedger wAddr =
  walletToCardano wAddr
    >>= left WalletToLedgerError . convert
  where
    convert =
      Ledger.fromCardanoAddress
        . CAPI.shelleyAddressInEra @CAPI.AlonzoEra
