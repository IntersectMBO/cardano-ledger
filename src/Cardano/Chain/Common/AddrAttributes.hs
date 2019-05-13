{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Cardano.Chain.Common.AddrAttributes
  ( AddrAttributes(..)
  )
where

import Cardano.Prelude

import Data.Text.Lazy.Builder (Builder)
import Formatting (bprint, builder)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Decoder
  , FromCBOR(..)
  , ToCBOR(..)
  , decodeFull
  , decodeFullDecoder
  , decodeWord32Canonical
  , serialize
  )
import Cardano.Chain.Common.Attributes
  (Attributes(..), fromCBORAttributes, toCBORAttributes)
import Cardano.Chain.Common.NetworkMagic (NetworkMagic(..))
import Cardano.Crypto.HD (HDAddressPayload)


-- | Additional information stored along with address. It's intended
-- to be put into 'Attributes' data type to make it extensible with
-- softfork.
data AddrAttributes = AddrAttributes
    { aaVKDerivationPath  :: !(Maybe HDAddressPayload)
    , aaNetworkMagic      :: !NetworkMagic
    } deriving (Eq, Ord, Show, Generic, NFData)

instance HeapWords AddrAttributes where
  heapWords aa = 3 + heapWords (aaVKDerivationPath aa)
                   + heapWords (aaNetworkMagic aa)

instance B.Buildable AddrAttributes where
  build aa = bprint
    ("AddrAttributes { derivation path: " . builder . " }")
    derivationPathBuilder
   where
    derivationPathBuilder :: Builder
    derivationPathBuilder = case aaVKDerivationPath aa of
      Nothing -> "{}"
      Just _  -> "{path is encrypted}"

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
  toCBOR attrs@Attributes { attrData = AddrAttributes derivationPath networkMagic } =
    toCBORAttributes listWithIndices attrs
   where
    listWithIndices :: [(Word8, AddrAttributes -> LByteString)]
    listWithIndices = derivationPathListWithIndices
                   <> networkMagicListWithIndices

    derivationPathListWithIndices :: [(Word8, AddrAttributes -> LByteString)]
    derivationPathListWithIndices = case derivationPath of
      Nothing -> []
      -- 'unsafeFromJust' is safe, because 'case' ensures
      -- that derivation path is 'Just'.
      Just _  -> [(1, serialize . unsafeFromJust . aaVKDerivationPath)]
    unsafeFromJust :: Maybe a -> a
    unsafeFromJust =
      fromMaybe (panic "Maybe was Nothing in ToCBOR (Attributes AddrAttributes)")

    networkMagicListWithIndices :: [(Word8, AddrAttributes -> LByteString)]
    networkMagicListWithIndices =
      case networkMagic of
        NetworkMainOrStage -> []
        NetworkTestnet x  ->
          [(2, \_ -> serialize x)]

instance FromCBOR (Attributes AddrAttributes) where
  fromCBOR = fromCBORAttributes initValue go
   where
    initValue = AddrAttributes
      { aaVKDerivationPath = Nothing
      , aaNetworkMagic     = NetworkMainOrStage
      }

    go
      :: Word8
      -> LByteString
      -> AddrAttributes
      -> Decoder s (Maybe AddrAttributes)
    go n v acc = case n of
      1 -> (\deriv -> Just $ acc { aaVKDerivationPath = Just deriv })
        <$> toCborError (decodeFull v)
      2 ->
        (\deriv -> Just $ acc { aaNetworkMagic = NetworkTestnet deriv })
          <$> toCborError
                (decodeFullDecoder "NetworkMagic" decodeWord32Canonical v)
      _ -> pure Nothing
