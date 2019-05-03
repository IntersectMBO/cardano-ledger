{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Crypto.Signing.Redeem.VerificationKey
  ( RedeemVerificationKey(..)
  , redeemVKB64F
  , redeemVKB64UrlF
  , redeemVKB64ShortF
  , fromAvvmVK
  , redeemVKBuild
  )
where

import Cardano.Prelude

import Crypto.Error (CryptoFailable(..))
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.Aeson
  ( FromJSONKey(..)
  , FromJSONKeyFunction(..)
  , ToJSONKey(..)
  , ToJSONKeyFunction(..)
  )
import qualified Data.Aeson.Encoding as A
import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import Formatting
  (Format, bprint, build, fitLeft, formatToString, later, sformat, stext, (%.))
import qualified Formatting.Buildable as B
import Text.JSON.Canonical (FromObjectKey(..), JSValue(..), ToObjectKey(..))

import Cardano.Binary (FromCBOR, ToCBOR)
import Cardano.Crypto.Orphans ()


-- | Wrapper around 'Ed25519.PublicKey'.
newtype RedeemVerificationKey =
  RedeemVerificationKey Ed25519.PublicKey
  deriving (Eq, Ord, Show, Generic, NFData, FromCBOR, ToCBOR)

instance Monad m => ToObjectKey m RedeemVerificationKey where
  toObjectKey = pure . formatToString redeemVKB64UrlF

instance MonadError SchemaError m => FromObjectKey m RedeemVerificationKey where
  fromObjectKey =
    fmap Just . parseJSString (first (sformat build) . fromAvvmVK) . JSString

instance ToJSONKey RedeemVerificationKey where
  toJSONKey = ToJSONKeyText render (A.text . render)
    where render = sformat redeemVKB64UrlF

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

fromVerificationKeyToByteString :: Ed25519.PublicKey -> BS.ByteString
fromVerificationKeyToByteString = BA.convert

redeemVKB64F :: Format r (RedeemVerificationKey -> r)
redeemVKB64F = later $ \(RedeemVerificationKey vk) ->
  B.build . Char8.unpack . B64.encode $ fromVerificationKeyToByteString vk

-- | Base64url Format for 'RedeemVerificationKey'.
redeemVKB64UrlF :: Format r (RedeemVerificationKey -> r)
redeemVKB64UrlF = later $ \(RedeemVerificationKey vk) ->
  B.build . Char8.unpack . B64URL.encode $ fromVerificationKeyToByteString vk

redeemVKB64ShortF :: Format r (RedeemVerificationKey -> r)
redeemVKB64ShortF = fitLeft 8 %. redeemVKB64F

-- | Read the text into a redeeming verification key. The key should be in
--   AVVM format which is base64(url). This function must be inverse of
--   redeemVKB64UrlF formatter.
fromAvvmVK :: Text -> Either AvvmVKError RedeemVerificationKey
fromAvvmVK addrText = do
  let base64rify = T.replace "-" "+" . T.replace "_" "/"
  let parsedM    = B64.decode . toS $ base64rify addrText
  addrParsed <- case parsedM of
    Left  _ -> throwError $ ApeAddressFormat addrText
    Right a -> Right a
  let len = BS.length addrParsed
  (len == 32) `orThrowError` ApeAddressLength len
  pure $ redeemVKBuild addrParsed

-- | Creates a verification key from 32 byte bytestring, fails with 'error' otherwise
redeemVKBuild :: ByteString -> RedeemVerificationKey
redeemVKBuild bs
  | BS.length bs /= 32
  = panic
    $  "consRedeemVK: failed to form vk, wrong bs length: "
    <> show (BS.length bs)
    <> ", when should be 32"
  | otherwise
  = case Ed25519.publicKey (BA.convert bs :: BA.Bytes) of
    CryptoPassed r -> RedeemVerificationKey r
    CryptoFailed e -> panic $ mappend
      "Cardano.Crypto.Signing.Types.Redeem.hs consRedeemVK failed because "
      (T.pack $ show e)

data AvvmVKError
  = ApeAddressFormat Text
  | ApeAddressLength Int
  deriving (Show)

instance B.Buildable AvvmVKError where
  build = \case
    ApeAddressFormat addrText ->
      bprint ("Address " . stext . " is not base64(url) format") addrText
    ApeAddressLength len -> bprint
      ("Address length is " . build . ", expected 32, can't be redeeming vk")
      len

deriveJSON defaultOptions ''RedeemVerificationKey
