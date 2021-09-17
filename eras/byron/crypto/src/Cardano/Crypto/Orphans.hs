{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Crypto.Orphans () where

import Cardano.Binary
  ( FromCBOR (..),
    Size,
    ToCBOR (..),
    encodeBytes,
    withWordSize,
  )
import Cardano.Prelude
import Crypto.Error (CryptoFailable (..))
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.ByteString.Base64.Type (getByteString64, makeByteString64)
import qualified Data.Text as T

fromByteStringToBytes :: ByteString -> BA.Bytes
fromByteStringToBytes = BA.convert

fromByteStringToScrubbedBytes :: ByteString -> BA.ScrubbedBytes
fromByteStringToScrubbedBytes = BA.convert

toByteString :: (BA.ByteArrayAccess bin) => bin -> ByteString
toByteString = BA.convert

fromCryptoFailable :: T.Text -> CryptoFailable a -> Either T.Text a
fromCryptoFailable item (CryptoFailed e) =
  Left $ "Cardano.Crypto.Orphan." <> item <> " failed because " <> show e
fromCryptoFailable _ (CryptoPassed r) = return r

instance FromJSON Ed25519.PublicKey where
  parseJSON v = do
    res <-
      Ed25519.publicKey
        . fromByteStringToBytes
        . getByteString64
        <$> parseJSON v
    toAesonError $ fromCryptoFailable "parseJSON Ed25519.PublicKey" res

instance ToJSON Ed25519.PublicKey where
  toJSON = toJSON . makeByteString64 . toByteString

instance FromJSON Ed25519.Signature where
  parseJSON v = do
    res <-
      Ed25519.signature
        . fromByteStringToBytes
        . getByteString64
        <$> parseJSON v
    toAesonError $ fromCryptoFailable "parseJSON Ed25519.Signature" res

instance ToJSON Ed25519.Signature where
  toJSON = toJSON . makeByteString64 . toByteString

instance ToCBOR Ed25519.PublicKey where
  toCBOR = encodeBytes . toByteString
  encodedSizeExpr _ _ = bsSize 32

instance FromCBOR Ed25519.PublicKey where
  fromCBOR = do
    res <- Ed25519.publicKey . fromByteStringToBytes <$> fromCBOR
    toCborError $ fromCryptoFailable "fromCBOR Ed25519.PublicKey" res

instance ToCBOR Ed25519.SecretKey where
  encodedSizeExpr _ _ = bsSize 64
  toCBOR sk =
    encodeBytes $
      BS.append (toByteString sk) (toByteString $ Ed25519.toPublic sk)

instance FromCBOR Ed25519.SecretKey where
  fromCBOR = do
    res <-
      Ed25519.secretKey
        . fromByteStringToScrubbedBytes
        . BS.take Ed25519.secretKeySize
        <$> fromCBOR
    toCborError $ fromCryptoFailable "fromCBOR Ed25519.SecretKey" res

instance ToCBOR Ed25519.Signature where
  encodedSizeExpr _ _ = bsSize 64
  toCBOR = encodeBytes . toByteString

instance FromCBOR Ed25519.Signature where
  fromCBOR = do
    res <- Ed25519.signature . fromByteStringToBytes <$> fromCBOR
    toCborError $ fromCryptoFailable "fromCBOR Ed25519.Signature" res

-- Helper for encodedSizeExpr in ToCBOR instances
bsSize :: Int -> Size
bsSize x = fromIntegral (x + withWordSize x)
