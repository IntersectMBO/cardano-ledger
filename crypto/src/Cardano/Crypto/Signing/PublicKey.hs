{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Crypto.Signing.PublicKey
  ( PublicKey(..)
  , formatFullPublicKey
  , fullPublicKeyF
  , fullPublicKeyHexF
  , shortPublicKeyHexF
  , parseFullPublicKey
  )
where

import Cardano.Prelude

import qualified Cardano.Crypto.Wallet as CC
import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Formatting
  (Format, bprint, fitLeft, formatToString, later, sformat, stext, (%.))
import Formatting.Buildable (Buildable(..))
import Text.JSON.Canonical (JSValue(..))
import qualified Text.JSON.Canonical as TJC (FromJSON(..), ToJSON(..))

import Cardano.Binary.Class (Bi(..))


-- | Wrapper around 'CC.XPub'.
newtype PublicKey =
  PublicKey CC.XPub
  deriving (Eq, Ord, Show, Generic, NFData)

instance ToJSON PublicKey where
  toJSON = toJSON . sformat fullPublicKeyF

instance FromJSON PublicKey where
  parseJSON v = parseJSON v >>= toAesonError . parseFullPublicKey

instance Monad m => TJC.ToJSON m PublicKey where
  toJSON = pure . JSString . formatToString fullPublicKeyF

instance MonadError SchemaError m => TJC.FromJSON m PublicKey where
  fromJSON = parseJSString parseFullPublicKey

instance Bi PublicKey where
  encode (PublicKey a) = encodeXPub a
  decode = fmap PublicKey decodeXPub
  encodedSizeExpr _ _ = 66

encodeXPub :: CC.XPub -> E.Encoding
encodeXPub a = encode $ CC.unXPub a

decodeXPub :: D.Decoder s CC.XPub
decodeXPub = toCborError . CC.xpub =<< decode

instance Buildable PublicKey where
  build = bprint ("pub:" . shortPublicKeyHexF)

-- | 'Builder' for 'PublicKey' to show it in base64 encoded form.
formatFullPublicKey :: PublicKey -> Builder
formatFullPublicKey (PublicKey pk) =
  Builder.fromString . BS.unpack . B64.encode . CC.unXPub $ pk

-- | Formatter for 'PublicKey' to show it in base64.
fullPublicKeyF :: Format r (PublicKey -> r)
fullPublicKeyF = later formatFullPublicKey

-- | Formatter for 'PublicKey' to show it in hex.
fullPublicKeyHexF :: Format r (PublicKey -> r)
fullPublicKeyHexF = later $ \(PublicKey x) -> base16Builder . CC.unXPub $ x

-- | Formatter for 'PublicKey' to show it in hex, but only first 8 chars.
shortPublicKeyHexF :: Format r (PublicKey -> r)
shortPublicKeyHexF = fitLeft 8 %. fullPublicKeyHexF

data PublicKeyParseError
  = PublicKeyParseBase64Error Text
  | PublicKeyParseXPubError Text
  deriving (Eq, Show)

instance Buildable PublicKeyParseError where
  build = \case
    PublicKeyParseBase64Error err -> bprint
      ("Failed to decode base 64 while parsing PublicKey.\n Error: " . stext)
      err
    PublicKeyParseXPubError err -> bprint
      ("Failed to construct XPub while parsing PublicKey.\n Error: " . stext)
      err

-- | Parse 'PublicKey' from base64 encoded string
parseFullPublicKey :: Text -> Either PublicKeyParseError PublicKey
parseFullPublicKey s = do
  b <- first (PublicKeyParseBase64Error . toS) . B64.decode $ toS s
  PublicKey <$> first (PublicKeyParseXPubError . toS) (CC.xpub b)
