{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Binary.Class.ToCBOR
  ( ToCBOR(..)
  )
where

import Codec.CBOR.Encoding (Encoding)

import qualified Cardano.Binary.Class.Core as B

class ToCBOR a where
  toCBOR :: a -> Encoding

instance (B.Bi a) => ToCBOR a where
  toCBOR = B.encode
