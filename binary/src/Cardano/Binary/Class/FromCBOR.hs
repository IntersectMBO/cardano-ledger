
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Binary.Class.FromCBOR
  ( FromCBOR(..)
  )
where

import Codec.CBOR.Decoding (Decoder)

import qualified Cardano.Binary.Class.Core as B


class FromCBOR a where
  fromCBOR :: Decoder s a

instance (B.Bi a) => FromCBOR a where
  fromCBOR = B.decode
