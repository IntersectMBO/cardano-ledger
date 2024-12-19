{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- Contents of this module will be moved to `Cardano.Protocol.Crypto` at some point later
-- whenever we are ready to remove derpecations
module Cardano.Ledger.Crypto.Internal (
  Crypto (..),
  StandardCrypto,
  HASH,
  ADDRHASH,
  DSIGN,
) where

import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import qualified Cardano.Crypto.VRF.Praos as VRF
import Cardano.Ledger.Hashes (ADDRHASH, HASH)
import Cardano.Ledger.Keys.Internal (DSIGN)
import Data.Kind (Type)
import Data.Typeable (Typeable)

class
  ( KES.UnsoundPureKESAlgorithm (KES c)
  , VRF.VRFAlgorithm (VRF c)
  , KES.ContextKES (KES c) ~ ()
  , VRF.ContextVRF (VRF c) ~ ()
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
  type KES StandardCrypto = KES.Sum6KES Ed25519DSIGN Blake2b_256
  type VRF StandardCrypto = VRF.PraosVRF
