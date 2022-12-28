{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Ledger.Binary.Decoding.Sized (
  Sized (..),
  mkSized,
  decodeSized,
  sizedDecoder,
  toSizedL,
)
where

import Cardano.Ledger.Binary.Decoding.Annotated (Annotated (..), ByteSpan (..), annotatedDecoder)
import Cardano.Ledger.Binary.Decoding.Decoder (Decoder)
import Cardano.Ledger.Binary.Decoding.FromCBOR (FromCBOR (fromCBOR))
import Cardano.Ledger.Binary.Encoding (serialize)
import Cardano.Ledger.Binary.Encoding.ToCBOR (ToCBOR (toCBOR))
import Cardano.Ledger.Binary.Version (Version)
import Control.DeepSeq (NFData (..), deepseq)
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (&), (.~), (^.))
import NoThunks.Class (NoThunks)

-- | A CBOR deserialized value together with its size. When deserializing use
-- either `decodeSized` or its `FromCBOR` instance.
--
-- Use `mkSized` to construct such value.
data Sized a = Sized
  { sizedValue :: !a
  , sizedSize :: Int64
  -- ^ Overhead in bytes. The field is lazy on purpose, because it might not
  -- be needed, but it can be expensive to compute.
  }
  deriving (Eq, Show, Generic)

instance NoThunks a => NoThunks (Sized a)

instance NFData a => NFData (Sized a) where
  rnf (Sized val sz) = val `deepseq` sz `seq` ()

-- | Construct a `Sized` value by serializing it first and recording the amount
-- of bytes it requires. Note, however, CBOR serialization is not canonical,
-- therefore it is *NOT* a requirement that this property holds:
--
-- > sizedSize (mkSized a) === sizedSize (unsafeDeserialize (serialize a) :: a)
mkSized :: ToCBOR a => Version -> a -> Sized a
mkSized version a =
  Sized
    { sizedValue = a
    , sizedSize = BSL.length (serialize version a)
    }

decodeSized :: Decoder s a -> Decoder s (Sized a)
decodeSized decoder = do
  Annotated v (ByteSpan start end) <- annotatedDecoder decoder
  pure $! Sized v $! end - start

sizedDecoder :: Decoder s a -> Decoder s (Sized a)
sizedDecoder = decodeSized
{-# DEPRECATED sizedDecoder "In favor of more consistently named `decodeSized`" #-}

instance FromCBOR a => FromCBOR (Sized a) where
  fromCBOR = decodeSized fromCBOR

-- | Discards the size.
instance ToCBOR a => ToCBOR (Sized a) where
  -- Size is an auxiliary value and should not be transmitted over the wire,
  -- therefore it is ignored.
  toCBOR (Sized v _) = toCBOR v

-- | Take a lens that operates on a particular type and convert it into a lens
-- that operates on the `Sized` version of the type.
toSizedL :: ToCBOR s => Version -> Lens' s a -> Lens' (Sized s) a
toSizedL version l =
  lens (\sv -> sizedValue sv ^. l) (\sv a -> mkSized version (sizedValue sv & l .~ a))
{-# INLINEABLE toSizedL #-}
