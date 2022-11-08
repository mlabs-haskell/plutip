-- | Cardano-Api/Ledger/Cardano-Wallet address conversions
module Test.Plutip.Tools.Address (
  walletToCardano,
  walletToCardanoAny,
  walletToLedger,
  ledgerToCardanoMainnet,
  ledgerToCardanoMainnet',
) where

import Cardano.Api qualified as CAPI
import Cardano.Wallet.Primitive.Types.Address qualified as Wallet
import Data.Data (Proxy)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Ledger.Tx.CardanoAPI qualified as Ledger
import Plutus.V1.Ledger.Address qualified as Address

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
        (CAPI.deserialiseAddress (CAPI.proxyToAsType px) (decodeUtf8 rawBytes))

walletToCardanoAny :: Wallet.Address -> Either AddressConversionError CAPI.AddressAny
walletToCardanoAny = fmap CAPI.AddressShelley . walletToCardano

walletToLedger :: Wallet.Address -> Either AddressConversionError Address.Address
walletToLedger waddr = Ledger.fromCardanoAddressInEra . CAPI.shelleyAddressInEra @CAPI.BabbageEra <$> walletToCardano waddr

ledgerToCardanoMainnet ::
  Address.Address ->
  Either Ledger.ToCardanoError (CAPI.AddressInEra CAPI.BabbageEra)
ledgerToCardanoMainnet = Ledger.toCardanoAddressInEra CAPI.Mainnet

ledgerToCardanoMainnet' :: Address.Address -> Either Ledger.ToCardanoError Text
ledgerToCardanoMainnet' addr =
  CAPI.serialiseAddress <$> Ledger.toCardanoAddressInEra CAPI.Mainnet addr

-- | Get `String` representation of address on mainnet
-- mkMainnetAddress :: BpiWallet -> String
-- mkMainnetAddress bw =
--   unpack
--     . CAPI.serialiseAddress
--     $ cardanoMainnetAddress bw
