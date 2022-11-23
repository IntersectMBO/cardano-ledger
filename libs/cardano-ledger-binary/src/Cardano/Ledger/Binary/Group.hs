{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Ledger.Binary.Group
  ( CBORGroup (..),
    groupRecord,
    ToCBORGroup (..),
    listLenInt,
    FromCBORGroup (..),
  )
where

import Cardano.Ledger.Binary.Decoding
import Cardano.Ledger.Binary.Encoding
import Data.Proxy
import Data.Typeable

--------------------------------------------------------------------------------
-- CBORGroup
--------------------------------------------------------------------------------

newtype CBORGroup a = CBORGroup {unCBORGroup :: a}

instance (FromCBORGroup a, ToCBORGroup a) => FromCBOR (CBORGroup a) where
  fromCBOR = CBORGroup <$> groupRecord

instance ToCBORGroup a => ToCBOR (CBORGroup a) where
  toCBOR (CBORGroup x) = encodeListLen (listLen x) <> toCBORGroup x
  encodedSizeExpr size proxy =
    fromInteger (withWordSize (listLenBound proxy'))
      + encodedGroupSizeExpr size proxy'
    where
      proxy' = unCBORGroup <$> proxy

groupRecord :: forall a s. (ToCBORGroup a, FromCBORGroup a) => Decoder s a
groupRecord = decodeRecordNamed "CBORGroup" (fromIntegral . toInteger . listLen) fromCBORGroup

--------------------------------------------------------------------------------
-- ToCBORGroup
--------------------------------------------------------------------------------

class Typeable a => ToCBORGroup a where
  toCBORGroup :: a -> Encoding
  encodedGroupSizeExpr ::
    (forall x. ToCBOR x => Proxy x -> Size) ->
    Proxy a ->
    Size

  listLen :: a -> Word

  -- | an upper bound for 'listLen', used in 'Size' expressions.
  listLenBound :: Proxy a -> Word

listLenInt :: ToCBORGroup a => a -> Int
listLenInt x = fromIntegral (listLen x)

--------------------------------------------------------------------------------
-- FromCBORGroup
--------------------------------------------------------------------------------

class Typeable a => FromCBORGroup a where
  fromCBORGroup :: Decoder s a
