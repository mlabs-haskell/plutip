module Plutip.Keys (
  KeyPair (..),
  PaymentKey,
  SigningKey,
  VerificationKey,
  cardanoMainnetAddress,
  genKeyPair,
  mainnetAddress,
  showAddress,
  saveKeyPair,
  showPkh,
  signKeyCBORHex,
) where

import Cardano.Api (AddressAny, AsType (AsPaymentKey), Key (VerificationKey, getVerificationKey, verificationKeyHash), PaymentKey, SigningKey, TextEnvelopeDescr, generateSigningKey, writeFileTextEnvelope, File(File))
import Cardano.Api qualified as CAPI
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import System.Directory (
  createDirectoryIfMissing,
 )
import System.FilePath ((<.>), (</>))

data KeyPair = KeyPair
  { sKey :: SigningKey PaymentKey
  , vKey :: VerificationKey PaymentKey
  }
  deriving stock (Show)

genKeyPair :: IO KeyPair
genKeyPair = do
  sKey <- generateSigningKey AsPaymentKey
  return $ KeyPair sKey (getVerificationKey sKey)

-- | Make `AnyAddress` for mainnet
cardanoMainnetAddress :: KeyPair -> AddressAny
cardanoMainnetAddress KeyPair {vKey} =
  CAPI.toAddressAny $
    CAPI.makeShelleyAddress
      CAPI.Mainnet
      (CAPI.PaymentCredentialByKey (CAPI.verificationKeyHash vKey))
      CAPI.NoStakeAddress

-- | Get `String` representation of address on mainnet
mainnetAddress :: KeyPair -> String
mainnetAddress =
  showAddress . cardanoMainnetAddress

showAddress :: AddressAny -> String
showAddress = Text.unpack . CAPI.serialiseAddress

showPkh :: KeyPair -> String
showPkh = Text.unpack . TE.decodeUtf8 . CAPI.serialiseToRawBytesHex . verificationKeyHash . vKey

signKeyCBORHex :: KeyPair -> Text
signKeyCBORHex = TE.decodeUtf8 . CAPI.serialiseToRawBytesHex . sKey

saveKeyPair :: FilePath -> KeyPair -> IO (Either (CAPI.FileError ()) ())
saveKeyPair dir kp@KeyPair {sKey, vKey} =
  let skeyDesc, vkeyDesc :: TextEnvelopeDescr
      skeyDesc = "Payment Signing Key"
      vkeyDesc = "Payment Verification Key"

      hash = showPkh kp

      skeyPath = dir </> "signing-key-" <> hash <.> "skey"
      vkeyPath = dir </> "verification-key-" <> hash <.> "vkey"
   in do
        createDirectoryIfMissing True dir
        runExceptT $ do
          ExceptT $ writeFileTextEnvelope (File skeyPath) (Just skeyDesc) sKey
          ExceptT $ writeFileTextEnvelope (File vkeyPath) (Just vkeyDesc) vKey
