{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Ledger.Binary.Group (
  CBORGroup (..),
  groupRecord,
  EncCBORGroup (..),
  listLenInt,
  DecCBORGroup (..),
) where

import Cardano.Ledger.Binary.Decoding
import Cardano.Ledger.Binary.Encoding
import Data.Proxy
import Data.Typeable

--------------------------------------------------------------------------------
-- CBORGroup
--------------------------------------------------------------------------------

newtype CBORGroup a = CBORGroup {unCBORGroup :: a}
  deriving (Eq, Show)

instance (DecCBORGroup a, EncCBORGroup a) => DecCBOR (CBORGroup a) where
  decCBOR = CBORGroup <$> groupRecord

instance EncCBORGroup a => EncCBOR (CBORGroup a) where
  encCBOR (CBORGroup x) = encodeListLen (listLen x) <> encCBORGroup x

groupRecord :: forall a s. (EncCBORGroup a, DecCBORGroup a) => Decoder s a
groupRecord = decodeRecordNamed "CBORGroup" (fromIntegral . toInteger . listLen) decCBORGroup

--------------------------------------------------------------------------------
-- EncCBORGroup
--------------------------------------------------------------------------------

class EncCBORGroup a where
  encCBORGroup :: a -> Encoding

  listLen :: a -> Word

  -- | an upper bound for 'listLen', used in 'Size' expressions.
  listLenBound :: Proxy a -> Word

listLenInt :: EncCBORGroup a => a -> Int
listLenInt x = fromIntegral (listLen x)

--------------------------------------------------------------------------------
-- DecCBORGroup
--------------------------------------------------------------------------------

class Typeable a => DecCBORGroup a where
  decCBORGroup :: Decoder s a

instance EncCBOR a => EncCBORGroup (a, a) where
  encCBORGroup (x, y) =
    encCBOR x <> encCBOR y
  listLen _ = 2
  listLenBound _ = 2

instance DecCBOR a => DecCBORGroup (a, a) where
  decCBORGroup = do
    x <- decCBOR
    y <- decCBOR
    pure (x, y)
