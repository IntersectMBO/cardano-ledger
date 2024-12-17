{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Package all the crypto constraints into one place.
module Cardano.Protocol.Crypto where

-- -- * KES
-- KESignable,

-- -- * VRF
-- VRFSignable,
-- CertifiedVRF,
-- SignedKES,
-- SignKeyKES,
-- SignKeyVRF,
-- VerKeyKES,
-- VerKeyVRF,
-- KES.signedKES,
-- KES.updateKES,
-- KES.verifyKES,
-- KES.verifySignedKES,
-- VRF.verifyVRF,

import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import qualified Cardano.Crypto.VRF.Praos as VRF
import Cardano.Ledger.Keys (KeyRoleVRF (..), VRFVerKeyHash (..))
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

hashVerKeyVRF :: Crypto c => VerKeyVRF c -> VRFVerKeyHash (r :: KeyRoleVRF)
hashVerKeyVRF = VRFVerKeyHash . Hash.castHash . VRF.hashVerKeyVRF

--------------------------------------------------------------------------------
-- KES
--------------------------------------------------------------------------------

type KESignable c = KES.Signable (KES c)

--------------------------------------------------------------------------------
-- VRF
--------------------------------------------------------------------------------

type VRFSignable c = VRF.Signable (VRF c)

type SignedKES c = KES.SignedKES (KES c)

type SignKeyKES c = KES.SignKeyKES (KES c)

type VerKeyKES c = KES.VerKeyKES (KES c)

type CertifiedVRF c = VRF.CertifiedVRF (VRF c)

type SignKeyVRF c = VRF.SignKeyVRF (VRF c)

type VerKeyVRF c = VRF.VerKeyVRF (VRF c)
