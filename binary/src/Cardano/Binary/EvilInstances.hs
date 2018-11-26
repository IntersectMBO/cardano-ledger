{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Binary.EvilInstances
  ()
where

import Cardano.Prelude

import qualified Cardano.Binary.Class.Core as B
import Cardano.Binary.FromCBOR (FromCBOR(..))
import Cardano.Binary.ToCBOR (ToCBOR(..))

-- This temporary module exists to ease the transition from Bi to ToCBOR and FromCBOR

instance (B.Bi a) => ToCBOR a where
  toCBOR = B.encode

instance (B.Bi a) => FromCBOR a where
  fromCBOR = B.decode
  label = B.label . proxy

proxy :: proxy a -> Proxy a
proxy _ = Proxy :: Proxy a
