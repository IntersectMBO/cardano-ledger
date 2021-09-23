{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Crypto.Signing.VerificationKey
  ( VerificationKey (..),
    formatFullVerificationKey,
    fullVerificationKeyF,
    fullVerificationKeyHexF,
    shortVerificationKeyHexF,
    parseFullVerificationKey,
  )
where

import Cardano.Binary
  ( Decoder,
    Encoding,
    FromCBOR (..),
    ToCBOR (..),
    decodeBytesCanonical,
  )
import qualified Cardano.Crypto.Wallet as CC
import Cardano.Prelude
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Formatting
  ( Format,
    bprint,
    fitLeft,
    formatToString,
    later,
    sformat,
    stext,
    (%.),
  )
import Formatting.Buildable (Buildable (..))
import NoThunks.Class (InspectHeap (..), NoThunks (..))
import Text.JSON.Canonical (JSValue (..), toJSString)
import qualified Text.JSON.Canonical as TJC (FromJSON (..), ToJSON (..))

-- | Wrapper around 'CC.XPub'.
newtype VerificationKey = VerificationKey
  { unVerificationKey :: CC.XPub
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (NFData)
  deriving (NoThunks) via InspectHeap CC.XPub

instance ToJSON VerificationKey where
  toJSON = toJSON . sformat fullVerificationKeyF

instance FromJSON VerificationKey where
  parseJSON v = parseJSON v >>= toAesonError . parseFullVerificationKey

instance Monad m => TJC.ToJSON m VerificationKey where
  toJSON = pure . JSString . toJSString . formatToString fullVerificationKeyF

instance MonadError SchemaError m => TJC.FromJSON m VerificationKey where
  fromJSON = parseJSString parseFullVerificationKey

instance ToCBOR VerificationKey where
  toCBOR (VerificationKey a) = toCBORXPub a
  encodedSizeExpr _ _ = 66

instance FromCBOR VerificationKey where
  fromCBOR = fmap VerificationKey fromCBORXPub

toCBORXPub :: CC.XPub -> Encoding
toCBORXPub a = toCBOR $ CC.unXPub a

-- | We enforce canonical CBOR encodings for `VerificationKey`s, because we serialize
--   them before hashing to get `KeyHash`es.
fromCBORXPub :: Decoder s CC.XPub
fromCBORXPub = toCborError . CC.xpub =<< decodeBytesCanonical

instance Buildable VerificationKey where
  build = bprint ("pub:" . shortVerificationKeyHexF)

-- | 'Builder' for 'VerificationKey' to show it in base64 encoded form.
formatFullVerificationKey :: VerificationKey -> Builder
formatFullVerificationKey (VerificationKey vk) =
  Builder.fromString . BS.unpack . B64.encode . CC.unXPub $ vk

-- | Formatter for 'VerificationKey' to show it in base64.
fullVerificationKeyF :: Format r (VerificationKey -> r)
fullVerificationKeyF = later formatFullVerificationKey

-- | Formatter for 'VerificationKey' to show it in hex.
fullVerificationKeyHexF :: Format r (VerificationKey -> r)
fullVerificationKeyHexF = later $ \(VerificationKey x) -> base16Builder . CC.unXPub $ x

-- | Formatter for 'VerificationKey' to show it in hex, but only first 8 chars.
shortVerificationKeyHexF :: Format r (VerificationKey -> r)
shortVerificationKeyHexF = fitLeft 8 %. fullVerificationKeyHexF

data VerificationKeyParseError
  = VerificationKeyParseBase64Error Text
  | VerificationKeyParseXPubError Text
  deriving (Eq, Show)

instance Buildable VerificationKeyParseError where
  build = \case
    VerificationKeyParseBase64Error err ->
      bprint
        ("Failed to decode base 64 while parsing VerificationKey.\n Error: " . stext)
        err
    VerificationKeyParseXPubError err ->
      bprint
        ("Failed to construct XPub while parsing VerificationKey.\n Error: " . stext)
        err

-- | Parse 'VerificationKey' from base64 encoded string
parseFullVerificationKey :: Text -> Either VerificationKeyParseError VerificationKey
parseFullVerificationKey s = do
  b <- first (VerificationKeyParseBase64Error . Text.pack) . B64.decode $ Text.encodeUtf8 s
  VerificationKey <$> first (VerificationKeyParseXPubError . Text.pack) (CC.xpub b)
