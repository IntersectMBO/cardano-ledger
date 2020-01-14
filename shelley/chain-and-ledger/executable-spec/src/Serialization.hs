{-# LANGUAGE OverloadedStrings #-}

module Serialization where

import           Cardano.Binary (Decoder, Encoding, FromCBOR (..), ToCBOR (..), decodeListLen,
                     encodeListLen, matchSize)
import           Data.Typeable

class Typeable a => ToCBORGroup a where
  toCBORGroup :: a -> Encoding
  listLen     :: a -> Word

newtype CBORGroup a = CBORGroup a

instance ToCBORGroup a => ToCBOR (CBORGroup a) where
  toCBOR (CBORGroup x) = encodeListLen (listLen x) <> toCBORGroup x

class Typeable a => FromCBORGroup a where
  fromCBORGroup :: Decoder s a

instance (FromCBORGroup a, ToCBORGroup a) => FromCBOR (CBORGroup a) where
  fromCBOR = do
    n <- decodeListLen
    x <- fromCBORGroup
    matchSize "CBORGroup" ((fromIntegral . toInteger . listLen) x) n
    pure $ CBORGroup x
