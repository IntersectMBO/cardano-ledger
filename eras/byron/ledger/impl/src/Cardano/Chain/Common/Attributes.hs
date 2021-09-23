{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Helper data type for block, tx attributes
--
--   Map with integer 1-byte keys, arbitrary-type polymorph values. Needed
--   primarily for partial serialization. Values are either parsed and put to
--   some constructor or left as unparsed.
module Cardano.Chain.Common.Attributes
  ( UnparsedFields (..),
    Attributes (..),
    attributesAreKnown,
    unknownAttributesLength,
    toCBORAttributes,
    fromCBORAttributes,
    mkAttributes,
    dropAttributes,
    dropEmptyAttributes,
  )
where

import Cardano.Binary
  ( Decoder,
    DecoderError (..),
    Dropper,
    Encoding,
    FromCBOR (..),
    ToCBOR (..),
    decodeMapLen,
    dropBytes,
    dropMap,
    dropWord8,
  )
import Cardano.Prelude
import Data.Aeson (ToJSON (..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LChar8
import qualified Data.Map.Strict as M
import Formatting (bprint, build, int)
import Formatting.Buildable (Buildable)
import qualified Formatting.Buildable as Buildable
import NoThunks.Class (NoThunks (..))
import qualified Prelude

-- | Representation of unparsed fields in Attributes. Newtype wrapper is used
--   for clear backward compatibility between previous representation (which was
--   just a single ByteString) during transition from Store to CBOR.
newtype UnparsedFields
  = UnparsedFields (Map Word8 LBS.ByteString)
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (HeapWords)
  deriving anyclass (NFData, NoThunks)

-- Used for debugging purposes only
instance ToJSON UnparsedFields where
  toJSON (UnparsedFields map') = toJSON $ M.map LChar8.unpack map'

fromUnparsedFields :: UnparsedFields -> Map Word8 LBS.ByteString
fromUnparsedFields (UnparsedFields m) = m

----------------------------------------

mkAttributes :: h -> Attributes h
mkAttributes dat = Attributes dat (UnparsedFields M.empty)

-- | Convenient wrapper for the datatype to represent it (in binary format) as
--   k-v map
data Attributes h = Attributes
  { -- | Data, containing known keys (deserialized)
    attrData :: !h,
    -- | Remaining, unparsed fields
    attrRemain :: !UnparsedFields
  }
  deriving (Eq, Ord, Generic, NoThunks)
  deriving anyclass (NFData)

instance Show h => Show (Attributes h) where
  show attr =
    let remain :: Prelude.String
        remain
          | attributesAreKnown attr =
            ""
          | otherwise =
            ", remain: <" <> show (unknownAttributesLength attr) <> " bytes>"
     in mconcat ["Attributes { data: ", show (attrData attr), remain, " }"]

instance {-# OVERLAPPABLE #-} Buildable h => Buildable (Attributes h) where
  build attr =
    if attributesAreKnown attr
      then Buildable.build (attrData attr)
      else
        bprint
          ("Attributes { data: " . build . ", remain: <" . int . " bytes> }")
          (attrData attr)
          (unknownAttributesLength attr)

instance Buildable (Attributes ()) where
  build attr
    | attributesAreKnown attr = "<no attributes>"
    | otherwise =
      bprint
        ("Attributes { data: (), remain: <" . int . " bytes> }")
        (unknownAttributesLength attr)

-- Used for debugging purposes only
instance ToJSON a => ToJSON (Attributes a)

instance ToCBOR (Attributes ()) where
  toCBOR = toCBORAttributes []

instance FromCBOR (Attributes ()) where
  fromCBOR = fromCBORAttributes () $ \_ _ _ -> pure Nothing

instance HeapWords h => HeapWords (Attributes h) where
  heapWords (Attributes dat unparsed) = heapWords2 dat unparsed

-- | Check whether all data from 'Attributes' is known, i. e. was successfully
--   parsed into some structured data
attributesAreKnown :: Attributes a -> Bool
attributesAreKnown = M.null . fromUnparsedFields . attrRemain

unknownAttributesLength :: Attributes a -> Int
unknownAttributesLength =
  fromIntegral . sum . map LBS.length . fromUnparsedFields . attrRemain

{- NOTE: Attributes serialization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Attributes are a way to add fields to datatypes while maintaining backwards
compatibility. Suppose that you have this datatype:

    data Foo = Foo {
        x :: Int,
        y :: Int,
        attrs :: Attributes FooAttrs }

@Attributes FooAttrs@ is a key-value map that deserializes into @FooAttrs@.
Each key is a single byte, and each value is an arbitary bytestring. It's
serialized like this:

    <length of following data>
    <k1><first attribute>
    <k2><second attribute>
    <attrRemain>

The attributes are read as long as their keys are “known” (i.e. as long as we
know how to interpret those keys), and the rest is stored separately. For
instance, let's say that in first version of CSL, @FooAttrs@ looks like this:

    data FooAttrs = FooAttrs {
        foo :: Text,
        bar :: [Int] }

It would be serialized as follows:

    <length> <0x00><foo> <0x01><bar>

In the next version of CSL we add a new field @quux@. The new version would
serialize it like this:

    <length> <0x00><foo> <0x01><bar> <0x02><quux>

And the old version would treat it like this:

    <length> <0x00><foo> <0x01><bar> <attrRemain>

This way the old version can serialize and deserialize data received from the
new version in a lossless way (i.e. when the old version does serialization
it would just put @attrRemain@ back after other attributes and the new
version would be able to parse it).

-}

toCBORAttributes ::
  forall t. [(Word8, t -> LBS.ByteString)] -> Attributes t -> Encoding
toCBORAttributes encs attr =
  toCBOR $
    foldr go (fromUnparsedFields $ attrRemain attr) encs
  where
    go ::
      (Word8, t -> LBS.ByteString) ->
      Map Word8 LBS.ByteString ->
      Map Word8 LBS.ByteString
    go (k, f) = M.alter (insertCheck $ f (attrData attr)) k
      where
        insertCheck :: a -> Maybe LByteString -> Maybe a
        insertCheck v Nothing = Just v
        insertCheck _ (Just v') =
          panic $
            "toCBORAttributes: impossible: field no. "
              <> show k
              <> " is already encoded as unparsed field: "
              <> show v'

fromCBORAttributes ::
  forall t s.
  t ->
  (Word8 -> LBS.ByteString -> t -> Decoder s (Maybe t)) ->
  Decoder s (Attributes t)
fromCBORAttributes initval updater = do
  raw <- fromCBOR @(Map Word8 LBS.ByteString)
  foldrM go (Attributes initval $ UnparsedFields raw) $ M.toList raw
  where
    go :: (Word8, LBS.ByteString) -> Attributes t -> Decoder s (Attributes t)
    go (k, v) attr = do
      updaterData <- updater k v $ attrData attr
      pure $ case updaterData of
        Nothing -> attr
        Just newData ->
          Attributes
            { attrData = newData,
              attrRemain =
                UnparsedFields
                  . M.delete k
                  . fromUnparsedFields
                  $ attrRemain attr
            }

dropAttributes :: Dropper s
dropAttributes = dropMap dropWord8 dropBytes

-- | Drop `Attributes ()` making sure that the `UnparsedFields` are empty
dropEmptyAttributes :: Dropper s
dropEmptyAttributes = do
  len <- decodeMapLen
  unless (len == 0) $ cborError $ DecoderErrorSizeMismatch "Attributes" 0 len
