{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Binary.Group (
  CBORGroup (..),
  groupRecord,
  EncCBORGroup (..),
  listLenInt,
  DecCBORGroup (..),
) where

import Cardano.Ledger.Binary.Decoding
import Cardano.Ledger.Binary.Encoding
import Data.Typeable (Proxy (..), Typeable)

--------------------------------------------------------------------------------
-- CBORGroup
--------------------------------------------------------------------------------

newtype CBORGroup a = CBORGroup {unCBORGroup :: a}
  deriving (Eq, Show)

instance (DecCBORGroup a, EncCBORGroup a) => DecCBOR (CBORGroup a) where
  decCBOR = CBORGroup <$> groupRecord

instance EncCBORGroup a => EncCBOR (CBORGroup a) where
  encCBOR (CBORGroup x) = encodeListLen (listLen $ Proxy @a) <> encCBORGroup x

groupRecord :: forall a s. (EncCBORGroup a, DecCBORGroup a) => Decoder s a
groupRecord =
  decodeRecordNamed "CBORGroup" (fromIntegral . toInteger . const (listLen $ Proxy @a)) decCBORGroup

--------------------------------------------------------------------------------
-- EncCBORGroup
--------------------------------------------------------------------------------

class EncCBORGroup a where
  encCBORGroup :: a -> Encoding

  listLen :: Proxy a -> Word

listLenInt :: forall a. EncCBORGroup a => a -> Int
listLenInt _ = fromIntegral $ listLen $ Proxy @a

--------------------------------------------------------------------------------
-- DecCBORGroup
--------------------------------------------------------------------------------

class Typeable a => DecCBORGroup a where
  decCBORGroup :: Decoder s a

instance EncCBOR a => EncCBORGroup (a, a) where
  encCBORGroup (x, y) =
    encCBOR x <> encCBOR y
  listLen _ = 2

instance DecCBOR a => DecCBORGroup (a, a) where
  decCBORGroup = do
    x <- decCBOR
    y <- decCBOR
    pure (x, y)
