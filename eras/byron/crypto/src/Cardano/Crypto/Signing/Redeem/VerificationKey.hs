{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Crypto.Signing.Redeem.VerificationKey (
  RedeemVerificationKey (..),
  redeemVKB64F,
  redeemVKB64UrlF,
  redeemVKB64ShortF,
  fromAvvmVK,
  fromVerificationKeyToByteString,
  redeemVKBuild,
) where

import Cardano.Crypto.Orphans ()
import Cardano.Ledger.Binary (DecCBOR, EncCBOR, FromCBOR, ToCBOR)
import Cardano.Prelude
import Crypto.Error (CryptoFailable (..))
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.Aeson (
  FromJSONKey (..),
  FromJSONKeyFunction (..),
  ToJSONKey (..),
  ToJSONKeyFunction (..),
 )
import qualified Data.Aeson.Encoding.Internal as A (key)
import qualified Data.Aeson.Key as A
import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Formatting (
  Format,
  bprint,
  build,
  fitLeft,
  formatToString,
  later,
  sformat,
  stext,
  (%.),
 )
import qualified Formatting.Buildable as B
import NoThunks.Class (InspectHeap (..), NoThunks (..))
import Text.JSON.Canonical (
  FromObjectKey (..),
  JSValue (..),
  ToObjectKey (..),
  toJSString,
 )

-- | Wrapper around 'Ed25519.PublicKey'.
type RedeemVerificationKey :: Type
newtype RedeemVerificationKey
  = RedeemVerificationKey Ed25519.PublicKey
  deriving (Eq, Show, Generic, NFData, DecCBOR, EncCBOR, FromCBOR, ToCBOR)
  deriving (NoThunks) via InspectHeap RedeemVerificationKey

-- Note that normally we would not provide any Ord instances.
-- The crypto libraries encourage using key /hashes/ not keys for
-- things like sets, map etc. However due to a historical mistake the
-- AVVM balances use whole keys, not key hashes. So we compromise here
-- and provide Ord instances so we can use RedeemVerificationKey
-- as the key type in a Data.Map.

instance Ord RedeemVerificationKey where
  RedeemVerificationKey a `compare` RedeemVerificationKey b =
    BA.convert a `compare` (BA.convert b :: ByteString)

fromVerificationKeyToByteString :: Ed25519.PublicKey -> BS.ByteString
fromVerificationKeyToByteString = BA.convert

-- | Base64url Format for 'RedeemVerificationKey'.
redeemVKB64UrlF :: Format r (RedeemVerificationKey -> r)
redeemVKB64UrlF = later $ \(RedeemVerificationKey vk) ->
  B.build . Char8.unpack . B64URL.encode $ fromVerificationKeyToByteString vk

instance Monad m => ToObjectKey m RedeemVerificationKey where
  toObjectKey = pure . toJSString . formatToString redeemVKB64UrlF

type AvvmVKError :: Type
data AvvmVKError
  = ApeAddressFormat Text
  | ApeAddressLength Int
  deriving (Show)

-- | Creates a verification key from 32 byte bytestring, fails with 'error' otherwise
redeemVKBuild :: ByteString -> RedeemVerificationKey
redeemVKBuild bs
  | BS.length bs /= 32 =
      panic
        $ "consRedeemVK: failed to form vk, wrong bs length: "
        <> show (BS.length bs)
        <> ", when should be 32"
  | otherwise =
      case Ed25519.publicKey (BA.convert bs :: BA.Bytes) of
        CryptoPassed r -> RedeemVerificationKey r
        CryptoFailed e ->
          panic
            $ mappend
              "Cardano.Crypto.Signing.Types.Redeem.hs consRedeemVK failed because "
              (T.pack $ show e)

-- | Read the text into a redeeming verification key. The key should be in
--   AVVM format which is base64(url). This function must be inverse of
--   redeemVKB64UrlF formatter.
fromAvvmVK :: Text -> Either AvvmVKError RedeemVerificationKey
fromAvvmVK addrText = do
  let base64rify = T.replace "-" "+" . T.replace "_" "/"
  let parsedM = B64.decode . T.encodeUtf8 $ base64rify addrText
  addrParsed <- case parsedM of
    Left _ -> throwError $ ApeAddressFormat addrText
    Right a -> Right a
  let len = BS.length addrParsed
  (len == 32) `orThrowError` ApeAddressLength len
  pure $ redeemVKBuild addrParsed

instance B.Buildable AvvmVKError where
  build = \case
    ApeAddressFormat addrText ->
      bprint ("Address " . stext . " is not base64(url) format") addrText
    ApeAddressLength len ->
      bprint
        ("Address length is " . build . ", expected 32, can't be redeeming vk")
        len

instance MonadError SchemaError m => FromObjectKey m RedeemVerificationKey where
  fromObjectKey =
    fmap Just . parseJSString (first (sformat build) . fromAvvmVK) . JSString

deriveJSON defaultOptions ''RedeemVerificationKey

instance ToJSONKey RedeemVerificationKey where
  toJSONKey = ToJSONKeyText render (A.key . render)
    where
      render = A.fromText . sformat redeemVKB64UrlF

instance FromJSONKey RedeemVerificationKey where
  fromJSONKey =
    FromJSONKeyTextParser $ toAesonError . first (sformat build) . fromAvvmVK
  fromJSONKeyList =
    FromJSONKeyTextParser
      $ toAesonError
      . bimap (sformat build) pure
      . fromAvvmVK

instance B.Buildable RedeemVerificationKey where
  build = bprint ("redeem_vk:" . redeemVKB64F)

redeemVKB64F :: Format r (RedeemVerificationKey -> r)
redeemVKB64F = later $ \(RedeemVerificationKey vk) ->
  B.build . Char8.unpack . B64.encode $ fromVerificationKeyToByteString vk

redeemVKB64ShortF :: Format r (RedeemVerificationKey -> r)
redeemVKB64ShortF = fitLeft 8 %. redeemVKB64F
