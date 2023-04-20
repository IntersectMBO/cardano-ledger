{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Protocol.HeaderCrypto where

import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash
import Cardano.Crypto.KES
import Cardano.Crypto.VRF
import Cardano.Crypto.VRF.Praos
import Cardano.Ledger.Crypto
import Data.Kind (Type)
import Data.Typeable (Typeable)

-- Crypto is split into two parts, Crypto or StableCrypto which contains hash,
-- address hash and digital sign types. The header crypto contains KES and VRF
-- that is mainly used by consensus

-- Moving HeaderCrypto to TPraos. This eventually should move into consensus
-- anyways.

class
  ( KESAlgorithm (KES c)
  , VRFAlgorithm (VRF c)
  , ContextKES (KES c) ~ ()
  , ContextVRF (VRF c) ~ ()
  , Typeable c
  ) =>
  HeaderCrypto c
  where
  type KES c :: Type
  type VRF c :: Type

-- ================================

-- | The same crypto used on the net
instance HeaderCrypto StandardCrypto where
  type KES StandardCrypto = Sum6KES Ed25519DSIGN Blake2b_256 -- TODO: KES/VRF
  type VRF StandardCrypto = PraosVRF
