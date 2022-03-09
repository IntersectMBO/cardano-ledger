{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Crypto.Signing.Redeem.Compact
  ( CompactRedeemVerificationKey (..),
    fromCompactRedeemVerificationKey,
    toCompactRedeemVerificationKey,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
    enforceSize,
  )
import Cardano.Crypto.Signing.Redeem.VerificationKey
  ( RedeemVerificationKey (..),
    fromAvvmVK,
    fromVerificationKeyToByteString,
    redeemVKB64UrlF,
    redeemVKBuild,
  )
import Cardano.Prelude
import Data.Aeson
  ( FromJSON (..),
    FromJSONKey (..),
    ToJSON (..),
    ToJSONKey (..),
    ToJSONKeyFunction (..),
  )
import qualified Data.Aeson.Encoding.Internal as A
import qualified Data.Aeson.Key as A
import Data.Binary.Get (Get, getWord64le, runGet)
import Data.Binary.Put (Put, putWord64le, runPut)
import qualified Data.ByteString.Lazy as BSL (fromStrict, toStrict)
import Formatting (build, formatToString, sformat)
import NoThunks.Class (InspectHeap (..), NoThunks (..))
import Text.JSON.Canonical
  ( FromObjectKey (..),
    JSValue (..),
    ToObjectKey (..),
    toJSString,
  )

data CompactRedeemVerificationKey
  = CompactRedeemVerificationKey
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
  deriving (Eq, Generic, Show)
  deriving (NoThunks) via InspectHeap CompactRedeemVerificationKey
  deriving anyclass (NFData)

instance ToCBOR CompactRedeemVerificationKey where
  toCBOR (CompactRedeemVerificationKey a b c d) =
    mconcat
      [ encodeListLen 4,
        toCBOR @Word64 a,
        toCBOR @Word64 b,
        toCBOR @Word64 c,
        toCBOR @Word64 d
      ]

instance FromCBOR CompactRedeemVerificationKey where
  fromCBOR = do
    enforceSize "CompactRedeemVerificationKey" 4
    CompactRedeemVerificationKey
      <$> fromCBOR @Word64
      <*> fromCBOR @Word64
      <*> fromCBOR @Word64
      <*> fromCBOR @Word64

getCompactRedeemVerificationKey :: Get CompactRedeemVerificationKey
getCompactRedeemVerificationKey =
  CompactRedeemVerificationKey
    <$> getWord64le
    <*> getWord64le
    <*> getWord64le
    <*> getWord64le

putCompactRedeemVerificationKey :: CompactRedeemVerificationKey -> Put
putCompactRedeemVerificationKey (CompactRedeemVerificationKey a b c d) =
  putWord64le a
    >> putWord64le b
    >> putWord64le c
    >> putWord64le d

toCompactRedeemVerificationKey ::
  RedeemVerificationKey ->
  CompactRedeemVerificationKey
toCompactRedeemVerificationKey (RedeemVerificationKey pk) =
  runGet getCompactRedeemVerificationKey (BSL.fromStrict bs)
  where
    bs :: ByteString
    bs = fromVerificationKeyToByteString pk

fromCompactRedeemVerificationKey ::
  CompactRedeemVerificationKey ->
  RedeemVerificationKey
fromCompactRedeemVerificationKey compactRvk =
  redeemVKBuild bs
  where
    bs :: ByteString
    bs =
      BSL.toStrict $
        runPut $
          putCompactRedeemVerificationKey compactRvk

instance Ord CompactRedeemVerificationKey where
  compare = compare `on` fromCompactRedeemVerificationKey

instance ToJSON CompactRedeemVerificationKey where
  toJSON = toJSON . fromCompactRedeemVerificationKey

instance FromJSON CompactRedeemVerificationKey where
  parseJSON = fmap toCompactRedeemVerificationKey . parseJSON

instance Monad m => ToObjectKey m CompactRedeemVerificationKey where
  toObjectKey = pure . toJSString . formatToString redeemVKB64UrlF . fromCompactRedeemVerificationKey

instance MonadError SchemaError m => FromObjectKey m CompactRedeemVerificationKey where
  fromObjectKey =
    fmap (Just . toCompactRedeemVerificationKey)
      . parseJSString (first (sformat build) . fromAvvmVK)
      . JSString

instance ToJSONKey CompactRedeemVerificationKey where
  toJSONKey = ToJSONKeyText render (A.key . render)
    where
      render = A.fromText . sformat redeemVKB64UrlF . fromCompactRedeemVerificationKey

instance FromJSONKey CompactRedeemVerificationKey where
  fromJSONKey = toCompactRedeemVerificationKey <$> fromJSONKey
  fromJSONKeyList = map toCompactRedeemVerificationKey <$> fromJSONKeyList
