{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Package all the crypto constraints into one place.
module Cardano.Ledger.Crypto where

import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash
import Cardano.Crypto.KES
import Cardano.Crypto.VRF
import Cardano.Crypto.VRF.Praos
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

-- ================================

-- | The same crypto used on the net
data StandardCrypto

instance Crypto StandardCrypto where
  type DSIGN StandardCrypto = Ed25519DSIGN
  type KES StandardCrypto = Sum6KES Ed25519DSIGN Blake2b_256
  type VRF StandardCrypto = PraosVRF
  type HASH StandardCrypto = Blake2b_256
  type ADDRHASH StandardCrypto = Blake2b_224
