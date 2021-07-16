{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Common.AddrAttributes
  ( AddrAttributes (..),
    HDAddressPayload (..),
  )
where

import Cardano.Binary
  ( Decoder,
    FromCBOR (..),
    ToCBOR (..),
    decodeBytesCanonical,
    decodeFull,
    decodeFullDecoder,
    decodeWord32Canonical,
    serialize,
  )
import Cardano.Chain.Common.Attributes
  ( Attributes (..),
    fromCBORAttributes,
    toCBORAttributes,
  )
import Cardano.Chain.Common.NetworkMagic (NetworkMagic (..))
import Cardano.Prelude
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.ByteString.Char8 as Char8
import Data.Text.Lazy.Builder (Builder)
import Formatting (bprint, builder)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))

-- | Additional information stored along with address. It's intended
-- to be put into 'Attributes' data type to make it extensible with
-- softfork.
data AddrAttributes = AddrAttributes
  { aaVKDerivationPath :: !(Maybe HDAddressPayload),
    aaNetworkMagic :: !NetworkMagic
  }
  deriving (Eq, Ord, Show, Generic, NFData, NoThunks)

instance HeapWords AddrAttributes where
  heapWords aa =
    3 + heapWords (aaVKDerivationPath aa)
      + heapWords (aaNetworkMagic aa)

instance B.Buildable AddrAttributes where
  build aa =
    bprint
      ("AddrAttributes { derivation path: " . builder . " }")
      derivationPathBuilder
    where
      derivationPathBuilder :: Builder
      derivationPathBuilder = case aaVKDerivationPath aa of
        Nothing -> "{}"
        Just _ -> "{path is encrypted}"

-- Used for debugging purposes only
instance ToJSON AddrAttributes

{- NOTE: Address attributes serialization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

'Attributes' are conceptually a map, where keys are numbers ('Word8').

For address there are two attributes:
  - 1 - derivation path, defaults to 'Nothing'.

-}

instance ToCBOR (Attributes AddrAttributes) where
  -- FIXME @avieth it was observed that for a 150kb block, this call to
  -- toCBORAttributes allocated 3.685mb
  -- Try using serialize rather than serialize', to avoid the
  -- toStrict call.
  -- Also consider using a custom builder strategy; serialized attributes are
  -- probably small, right?
  toCBOR attrs@Attributes {attrData = AddrAttributes derivationPath networkMagic} =
    toCBORAttributes listWithIndices attrs
    where
      listWithIndices :: [(Word8, AddrAttributes -> LByteString)]
      listWithIndices =
        derivationPathListWithIndices
          <> networkMagicListWithIndices

      derivationPathListWithIndices :: [(Word8, AddrAttributes -> LByteString)]
      derivationPathListWithIndices = case derivationPath of
        Nothing -> []
        -- 'unsafeFromJust' is safe, because 'case' ensures
        -- that derivation path is 'Just'.
        Just _ -> [(1, serialize . unsafeFromJust . aaVKDerivationPath)]
      unsafeFromJust :: Maybe a -> a
      unsafeFromJust =
        fromMaybe (panic "Maybe was Nothing in ToCBOR (Attributes AddrAttributes)")

      networkMagicListWithIndices :: [(Word8, AddrAttributes -> LByteString)]
      networkMagicListWithIndices =
        case networkMagic of
          NetworkMainOrStage -> []
          NetworkTestnet x ->
            [(2, \_ -> serialize x)]

instance FromCBOR (Attributes AddrAttributes) where
  fromCBOR = fromCBORAttributes initValue go
    where
      initValue =
        AddrAttributes
          { aaVKDerivationPath = Nothing,
            aaNetworkMagic = NetworkMainOrStage
          }

      go ::
        Word8 ->
        LByteString ->
        AddrAttributes ->
        Decoder s (Maybe AddrAttributes)
      go n v acc = case n of
        1 ->
          (\deriv -> Just $ acc {aaVKDerivationPath = Just deriv})
            <$> toCborError (decodeFull v)
        2 ->
          (\deriv -> Just $ acc {aaNetworkMagic = NetworkTestnet deriv})
            <$> toCborError
              (decodeFullDecoder "NetworkMagic" decodeWord32Canonical v)
        _ -> pure Nothing

-- | Passphrase is a hash of root verification key.
data HDPassphrase = HDPassphrase !ByteString
  deriving (Eq, Show)

-- | HDAddressPayload is a specific address attribute that was used by the
-- Cardano wallet at mainnet launch, prior to moving to a BIP-44 style scheme.
--
-- It consisted of
--
--   * serialiazed and encrypted using HDPassphrase derivation path from the
--   root key to given descendant key (using ChaChaPoly1305 algorithm)
--
--   * cryptographic tag
--
-- It is still distinguished as an attribute, but not used by the ledger,
-- because the attributes size limits treat this attribute specially.
newtype HDAddressPayload = HDAddressPayload
  { getHDAddressPayload :: ByteString
  }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR, HeapWords)
  deriving anyclass (NFData, NoThunks)

-- Used for debugging purposes only
instance ToJSON HDAddressPayload where
  toJSON (HDAddressPayload bs) = object ["HDAddressPayload" .= Char8.unpack bs]

instance FromCBOR HDAddressPayload where
  fromCBOR = HDAddressPayload <$> decodeBytesCanonical
