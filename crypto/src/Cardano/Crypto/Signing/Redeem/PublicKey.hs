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

module Cardano.Crypto.Signing.Redeem.PublicKey
  ( RedeemPublicKey(..)
  , redeemPkB64F
  , redeemPkB64UrlF
  , redeemPkB64ShortF
  , fromAvvmPk
  , redeemPkBuild
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

import Cardano.Binary.Class (Bi)
import Cardano.Crypto.Orphans ()


-- | Wrapper around 'Ed25519.PublicKey'.
newtype RedeemPublicKey =
  RedeemPublicKey Ed25519.PublicKey
  deriving (Eq, Ord, Show, Generic, NFData)

instance Monad m => ToObjectKey m RedeemPublicKey where
  toObjectKey = pure . formatToString redeemPkB64UrlF

instance MonadError SchemaError m => FromObjectKey m RedeemPublicKey where
  fromObjectKey =
    fmap Just . parseJSString (first (sformat build) . fromAvvmPk) . JSString

instance ToJSONKey RedeemPublicKey where
  toJSONKey = ToJSONKeyText render (A.text . render)
    where render = sformat redeemPkB64UrlF

instance FromJSONKey RedeemPublicKey where
  fromJSONKey =
    FromJSONKeyTextParser $ toAesonError . first (sformat build) . fromAvvmPk
  fromJSONKeyList =
    FromJSONKeyTextParser
      $ toAesonError
      . bimap (sformat build) pure
      . fromAvvmPk

deriving instance Bi RedeemPublicKey

instance B.Buildable RedeemPublicKey where
  build = bprint ("redeem_pk:" . redeemPkB64F)

fromPublicKeyToByteString :: Ed25519.PublicKey -> BS.ByteString
fromPublicKeyToByteString = BA.convert

redeemPkB64F :: Format r (RedeemPublicKey -> r)
redeemPkB64F = later $ \(RedeemPublicKey pk) ->
  B.build . Char8.unpack . B64.encode $ fromPublicKeyToByteString pk

-- | Base64url Format for 'RedeemPublicKey'.
redeemPkB64UrlF :: Format r (RedeemPublicKey -> r)
redeemPkB64UrlF = later $ \(RedeemPublicKey pk) ->
  B.build . Char8.unpack . B64URL.encode $ fromPublicKeyToByteString pk

redeemPkB64ShortF :: Format r (RedeemPublicKey -> r)
redeemPkB64ShortF = fitLeft 8 %. redeemPkB64F

-- | Read the text into a redeeming public key. The key should be in
--   AVVM format which is base64(url). This function must be inverse of
--   redeemPkB64UrlF formatter.
fromAvvmPk :: Text -> Either AvvmPkError RedeemPublicKey
fromAvvmPk addrText = do
  let base64rify = T.replace "-" "+" . T.replace "_" "/"
  let parsedM    = B64.decode . toS $ base64rify addrText
  addrParsed <- case parsedM of
    Left  _ -> throwError $ ApeAddressFormat addrText
    Right a -> Right a
  let len = BS.length addrParsed
  (len == 32) `orThrowError` ApeAddressLength len
  pure $ redeemPkBuild addrParsed

-- | Creates a public key from 32 byte bytestring, fails with 'error' otherwise
redeemPkBuild :: ByteString -> RedeemPublicKey
redeemPkBuild bs
  | BS.length bs /= 32
  = panic
    $  "consRedeemPk: failed to form pk, wrong bs length: "
    <> show (BS.length bs)
    <> ", when should be 32"
  | otherwise
  = case Ed25519.publicKey (BA.convert bs :: BA.Bytes) of
    CryptoPassed r -> RedeemPublicKey r
    CryptoFailed e -> panic $ mappend
      "Cardano.Crypto.Signing.Types.Redeem.hs consRedeemPk failed because "
      (T.pack $ show e)

data AvvmPkError
  = ApeAddressFormat Text
  | ApeAddressLength Int
  deriving (Show)

instance B.Buildable AvvmPkError where
  build = \case
    ApeAddressFormat addrText ->
      bprint ("Address " . stext . " is not base64(url) format") addrText
    ApeAddressLength len -> bprint
      ("Address length is " . build . ", expected 32, can't be redeeming pk")
      len

deriveJSON defaultOptions ''RedeemPublicKey
