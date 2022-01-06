{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines reusable abstractions for testing Roundtrip properties of CBOR instances
module Data.Roundtrip
  ( roundTrip,
    roundTrip',
    embedTrip,
    embedTrip',
    roundTripAnn,
    embedTripAnn,
    RoundTripResult,
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    FullByteString (Full),
    ToCBOR (toCBOR),
  )
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Lazy as Lazy

-- =====================================================================

type RoundTripResult t = Either Codec.CBOR.Read.DeserialiseFailure (Lazy.ByteString, t)

roundTrip :: (ToCBOR t, FromCBOR t) => t -> RoundTripResult t
roundTrip s = deserialiseFromBytes fromCBOR (toLazyByteString (toCBOR s))

roundTrip' :: (t -> Encoding) -> (forall s. Decoder s t) -> t -> RoundTripResult t
roundTrip' enc dec t = deserialiseFromBytes dec (toLazyByteString (enc t))

roundTripAnn :: (ToCBOR t, FromCBOR (Annotator t)) => t -> RoundTripResult t
roundTripAnn s =
  let bytes = toLazyByteString (toCBOR s)
   in case deserialiseFromBytes fromCBOR bytes of
        Left err -> Left err
        Right (leftover, Annotator f) -> Right (leftover, f (Full bytes))

-- | Can we serialise a type, and then deserialise it as something else?
embedTrip :: (ToCBOR t, FromCBOR s) => t -> RoundTripResult s
embedTrip s = deserialiseFromBytes fromCBOR (toLazyByteString (toCBOR s))

embedTrip' :: (s -> Encoding) -> (forall x. Decoder x t) -> s -> RoundTripResult t
embedTrip' enc dec s = deserialiseFromBytes dec (toLazyByteString (enc s))

embedTripAnn :: forall s t. (ToCBOR t, FromCBOR (Annotator s)) => t -> RoundTripResult s
embedTripAnn s =
  let bytes = toLazyByteString (toCBOR s)
   in case deserialiseFromBytes fromCBOR bytes of
        Left err -> Left err
        Right (leftover, Annotator f) -> Right (leftover, f (Full bytes))
