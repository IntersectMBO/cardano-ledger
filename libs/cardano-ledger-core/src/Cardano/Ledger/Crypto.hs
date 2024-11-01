{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
  ( UnsoundPureKESAlgorithm (KES c)
  , VRFAlgorithm (VRF c)
  , ContextKES (KES c) ~ ()
  , ContextVRF (VRF c) ~ ()
  , Typeable c
  ) =>
  Crypto c
  where
  type KES c :: Type
  type VRF c :: Type

-- ================================

-- | The same crypto used on the net
data StandardCrypto

instance Crypto StandardCrypto where
  type KES StandardCrypto = Sum6KES Ed25519DSIGN Blake2b_256
  type VRF StandardCrypto = PraosVRF


type HASH = Blake2b_256
type ADDRHASH = Blake2b_224
type DSIGN = Ed25519DSIGN
