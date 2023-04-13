{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Binary.Decoding.Annotated (
  Annotated (..),
  ByteSpan (..),
  Decoded (..),
  annotationBytes,
  annotatedDecoder,
  slice,
  decCBORAnnotated,
  reAnnotate,
  Annotator (..),
  annotatorSlice,
  withSlice,
  FullByteString (..),
  decodeAnnSet,
)
where

import Cardano.Ledger.Binary.Decoding.DecCBOR (DecCBOR (..))
import Cardano.Ledger.Binary.Decoding.Decoder (
  Decoder,
  decodeList,
  decodeWithByteSpan,
  fromPlainDecoder,
 )
import Cardano.Ledger.Binary.Encoding (EncCBOR, Version, serialize')
import Codec.CBOR.Read (ByteOffset)
import qualified Codec.Serialise as Serialise (decode)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Bifunctor (Bifunctor (first, second))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Kind (Type)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import qualified PlutusLedgerApi.V1 as PV1

-------------------------------------------------------------------------
-- ByteSpan
-------------------------------------------------------------------------

-- | Extract a substring of a given ByteString corresponding to the offsets.
slice :: BSL.ByteString -> ByteSpan -> BSL.ByteString
slice bytes (ByteSpan start end) =
  BSL.take (end - start) $ BSL.drop start bytes
{-# INLINE slice #-}

-- | A pair of offsets delimiting the beginning and end of a substring of a ByteString
data ByteSpan = ByteSpan !ByteOffset !ByteOffset
  deriving (Generic, Show)

-- Used for debugging purposes only.
instance ToJSON ByteSpan

-------------------------------------------------------------------------
-- Annotator
-------------------------------------------------------------------------

data Annotated b a = Annotated {unAnnotated :: !b, annotation :: !a}
  deriving (Eq, Show, Functor, Generic)
  deriving anyclass (NFData, NoThunks)

instance Bifunctor Annotated where
  first f (Annotated b a) = Annotated (f b) a
  second = fmap

instance (Eq a, Ord b) => Ord (Annotated b a) where
  compare = compare `on` unAnnotated

instance ToJSON b => ToJSON (Annotated b a) where
  toJSON = toJSON . unAnnotated
  toEncoding = toEncoding . unAnnotated

instance FromJSON b => FromJSON (Annotated b ()) where
  parseJSON j = flip Annotated () <$> parseJSON j

-- | A decoder for a value paired with an annotation specifying the start and end
-- of the consumed bytes.
annotatedDecoder :: Decoder s a -> Decoder s (Annotated a ByteSpan)
annotatedDecoder vd =
  decodeWithByteSpan vd <&> \(x, start, end) -> Annotated x (ByteSpan start end)
{-# INLINE annotatedDecoder #-}

-- | A decoder for a value paired with an annotation specifying the start and end
-- of the consumed bytes.
decCBORAnnotated :: DecCBOR a => Decoder s (Annotated a ByteSpan)
decCBORAnnotated = annotatedDecoder decCBOR
{-# INLINE decCBORAnnotated #-}

annotationBytes :: Functor f => BSL.ByteString -> f ByteSpan -> f BS.ByteString
annotationBytes bytes = fmap (BSL.toStrict . slice bytes)

-- | Reconstruct an annotation by re-serialising the payload to a ByteString.
reAnnotate :: EncCBOR a => Version -> Annotated a b -> Annotated a BS.ByteString
reAnnotate version (Annotated x _) = Annotated x (serialize' version x)

class Decoded t where
  type BaseType t :: Type
  recoverBytes :: t -> BS.ByteString

instance Decoded (Annotated b BS.ByteString) where
  type BaseType (Annotated b BS.ByteString) = b
  recoverBytes = annotation

-------------------------------------------------------------------------
-- Annotator
-------------------------------------------------------------------------

-- | This marks the entire bytestring used during decoding, rather than the
--   piece we need to finish constructing our value.
newtype FullByteString = Full BSL.ByteString

-- | A value of type @(Annotator a)@ is one that needs access to the entire bytestring
--   used during decoding to finish construction of a vaue of type @a@. A typical use is
--   some type that stores the bytes that were used to deserialize it.  For example the
--   type @Inner@ below is constructed using the helper function @makeInner@ which
--   serializes and stores its bytes (using 'serialize').  Note how we build the
--   'Annotator' by abstracting over the full bytes, and using those original bytes to
--   fill the bytes field of the constructor @Inner@.  The 'EncCBOR' instance just reuses
--   the stored bytes to produce an encoding (using 'encodePreEncoded').
--
-- @
-- data Inner = Inner Int Bool LByteString
--
-- makeInner :: Int -> Bool -> Inner
-- makeInner i b = Inner i b (serialize (encCBOR i <> encCBOR b))
--
-- instance EncCBOR Inner where
--   encCBOR (Inner _ _ bytes) = encodePreEncoded bytes
--
-- instance DecCBOR (Annotator Inner) where
--   decCBOR = do
--      int <- decCBOR
--      trueOrFalse <- decCBOR
--      pure (Annotator (\(Full bytes) -> Inner int trueOrFalse bytes))
-- @
--
-- if an @Outer@ type has a field of type @Inner@, with a @(EncCBOR (Annotator Inner))@
-- instance, the @Outer@ type must also have a @(EncCBOR (Annotator Outer))@ instance.  The
-- key to writing that instance is to use the operation @withSlice@ which returns a pair.
-- The first component is an @Annotator@ that can build @Inner@, the second is an
-- @Annotator@ that given the full bytes, extracts just the bytes needed to decode
-- @Inner@.
--
-- @
-- data Outer = Outer Text Inner
--
-- instance EncCBOR Outer where
--   encCBOR (Outer t i) = encCBOR t <> encCBOR i
--
-- instance DecCBOR (Annotator Outer) where
--   decCBOR = do
--     t <- decCBOR
--     (Annotator mkInner, Annotator extractInnerBytes) <- withSlice decCBOR
--     pure (Annotator (\ full -> Outer t (mkInner (Full (extractInnerBytes full)))))
-- @
newtype Annotator a = Annotator {runAnnotator :: FullByteString -> a}
  deriving newtype (Monad, Applicative, Functor)

-- | The argument is a decoder for a annotator that needs access to the bytes that
-- | were decoded. This function constructs and supplies the relevant piece.
annotatorSlice ::
  Decoder s (Annotator (BSL.ByteString -> a)) ->
  Decoder s (Annotator a)
annotatorSlice dec = do
  (k, bytes) <- withSlice dec
  pure $ k <*> bytes
{-# INLINE annotatorSlice #-}

-- | Pairs the decoder result with an annotator that can be used to construct the exact
-- bytes used to decode the result.
withSlice :: Decoder s a -> Decoder s (a, Annotator BSL.ByteString)
withSlice dec = do
  Annotated r byteSpan <- annotatedDecoder dec
  return (r, Annotator (\(Full bsl) -> slice bsl byteSpan))
{-# INLINE withSlice #-}

-- TODO: Implement version that matches the length of a list with the size of the Set in
-- order to enforce no duplicates.
decodeAnnSet :: Ord t => Decoder s (Annotator t) -> Decoder s (Annotator (Set.Set t))
decodeAnnSet dec = do
  xs <- decodeList dec
  pure (Set.fromList <$> sequence xs)
{-# INLINE decodeAnnSet #-}

--------------------------------------------------------------------------------
-- Plutus
--------------------------------------------------------------------------------

instance DecCBOR (Annotator PV1.Data) where
  decCBOR = pure <$> fromPlainDecoder Serialise.decode
  {-# INLINE decCBOR #-}
