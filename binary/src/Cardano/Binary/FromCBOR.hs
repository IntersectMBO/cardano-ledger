{-# LANGUAGE DefaultSignatures #-}

module Cardano.Binary.FromCBOR
  ( FromCBOR(..)
  )
where

import Cardano.Prelude

import Codec.CBOR.Decoding (Decoder)

class FromCBOR a where
  fromCBOR :: Decoder s a

  label :: proxy a -> Text

  default label :: Typeable a => proxy a -> Text
  label = show . typeRep
