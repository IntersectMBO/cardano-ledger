{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Ledger.Binary.Group (
  CBORGroup (..),
  groupRecord,
  EncCBORGroup (..),
  listLenInt,
  DecCBORGroup (..),
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
  deriving (Eq, Show)

instance (DecCBORGroup a, EncCBORGroup a) => DecCBOR (CBORGroup a) where
  decCBOR = CBORGroup <$> groupRecord

instance EncCBORGroup a => EncCBOR (CBORGroup a) where
  encCBOR (CBORGroup x) = encodeListLen (listLen x) <> encCBORGroup x
  encodedSizeExpr size proxy =
    fromInteger (withWordSize (listLenBound proxy'))
      + encodedGroupSizeExpr size proxy'
    where
      proxy' = unCBORGroup <$> proxy

groupRecord :: forall a s. (EncCBORGroup a, DecCBORGroup a) => Decoder s a
groupRecord = decodeRecordNamed "CBORGroup" (fromIntegral . toInteger . listLen) decCBORGroup

--------------------------------------------------------------------------------
-- EncCBORGroup
--------------------------------------------------------------------------------

class Typeable a => EncCBORGroup a where
  encCBORGroup :: a -> Encoding
  encodedGroupSizeExpr ::
    (forall x. EncCBOR x => Proxy x -> Size) ->
    Proxy a ->
    Size

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
