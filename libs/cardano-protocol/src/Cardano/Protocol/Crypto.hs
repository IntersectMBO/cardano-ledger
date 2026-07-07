{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Package all the crypto constraints into one place.
module Cardano.Protocol.Crypto (
  Crypto (..),
  StandardCrypto,
  VRFVerKeyHash (..),
  KeyRoleVRF (..),
  hashVerKeyVRF,
) where

import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import qualified Cardano.Crypto.VRF.Praos as VRF
import Cardano.Ledger.Hashes
import Data.Kind (Type)
import Data.Typeable (Typeable)

hashVerKeyVRF :: Crypto c => VRF.VerKeyVRF (VRF c) -> VRFVerKeyHash (r :: KeyRoleVRF)
hashVerKeyVRF = VRFVerKeyHash . Hash.castHash . VRF.hashVerKeyVRF

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
