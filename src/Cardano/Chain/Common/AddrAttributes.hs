{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Cardano.Chain.Common.AddrAttributes
  ( AddrAttributes(..)
  )
where

import Cardano.Prelude

import Data.Text.Lazy.Builder (Builder)
import Formatting (bprint, builder)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class (Bi, decode, encode)
import qualified Cardano.Binary.Class as Bi
import Cardano.Chain.Common.Attributes
  (Attributes(..), decodeAttributes, encodeAttributes)
import Cardano.Crypto.HD (HDAddressPayload)

-- | Additional information stored along with address. It's intended
-- to be put into 'Attributes' data type to make it extensible with
-- softfork.
newtype AddrAttributes = AddrAttributes
    { aaPkDerivationPath  :: Maybe HDAddressPayload
    } deriving (Eq, Ord, Show, Generic, NFData, HeapWords)

instance B.Buildable AddrAttributes where
  build aa = bprint
    ("AddrAttributes { derivation path: " . builder . " }")
    derivationPathBuilder
   where
    derivationPathBuilder :: Builder
    derivationPathBuilder = case aaPkDerivationPath aa of
      Nothing -> "{}"
      Just _  -> "{path is encrypted}"

{- NOTE: Address attributes serialization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

'Attributes' are conceptually a map, where keys are numbers ('Word8').

For address there are two attributes:
  - 1 - derivation path, defaults to 'Nothing'.

-}

instance Bi (Attributes AddrAttributes) where
  -- FIXME @avieth it was observed that for a 150kb block, this call to
  -- encodeAttributes allocated 3.685mb
  -- Try using serialize rather than serialize', to avoid the
  -- toStrict call.
  -- Also consider using a custom builder strategy; serialized attributes are
  -- probably small, right?
  encode attrs@Attributes { attrData = AddrAttributes derivationPath } =
    encodeAttributes derivationPathListWithIndices attrs
   where
    derivationPathListWithIndices :: [(Word8, AddrAttributes -> LByteString)]
    derivationPathListWithIndices = case derivationPath of
      Nothing -> []
      -- 'unsafeFromJust' is safe, because 'case' ensures
      -- that derivation path is 'Just'.
      Just _  -> [(1, Bi.serialize . unsafeFromJust . aaPkDerivationPath)]
    unsafeFromJust :: Maybe a -> a
    unsafeFromJust =
      fromMaybe (panic "Maybe was Nothing in Bi (Attributes AddrAttributes)")

  decode = decodeAttributes initValue go
   where
    initValue = AddrAttributes {aaPkDerivationPath = Nothing}
    go
      :: Word8
      -> LByteString
      -> AddrAttributes
      -> Bi.Decoder s (Maybe AddrAttributes)
    go n v acc = case n of
      1 -> (\deriv -> Just $ acc { aaPkDerivationPath = Just deriv })
        <$> Bi.deserialize v
      _ -> pure Nothing
