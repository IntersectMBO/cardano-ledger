module Cardano.Binary.ToCBOR
  ( ToCBOR(..)
  )
where

import Codec.CBOR.Encoding (Encoding)

class ToCBOR a where
  toCBOR :: a -> Encoding
