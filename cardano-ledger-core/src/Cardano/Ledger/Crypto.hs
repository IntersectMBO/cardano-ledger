{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Package all the crypto constraints into one place.
module Cardano.Ledger.Crypto where

import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash
import Cardano.Crypto.KES
import Cardano.Crypto.VRF
import Data.Kind (Type)
import Data.Typeable (Typeable)

class
  ( HashAlgorithm (HASH c),
    HashAlgorithm (ADDRHASH c),
    DSIGNAlgorithm (DSIGN c),
    KESAlgorithm (KES c),
    VRFAlgorithm (VRF c),
    ContextDSIGN (DSIGN c) ~ (),
    ContextKES (KES c) ~ (),
    ContextVRF (VRF c) ~ (),
    Typeable c
  ) =>
  Crypto c
  where
  type HASH c :: Type
  type ADDRHASH c :: Type
  type DSIGN c :: Type
  type KES c :: Type
  type VRF c :: Type
