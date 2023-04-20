{-# LANGUAGE ConstraintKinds #-}

module Cardano.Protocol.HeaderKeys (
  -- * KES
  KESignable,

  -- * VRF
  VRFSignable,
  CertifiedVRF,
  SignedKES,
  SignKeyKES,
  SignKeyVRF,
  VerKeyKES,
  VerKeyVRF,
)
where

import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Protocol.HeaderCrypto (KES, VRF)

-- TODO: Moved from Cardano.Ledger.Keys

--------------------------------------------------------------------------------
-- KES
--------------------------------------------------------------------------------

type KESignable c = KES.Signable (KES c)

--------------------------------------------------------------------------------
-- VRF
--------------------------------------------------------------------------------

type VRFSignable c = VRF.Signable (VRF c)

--------------------------------------------------------------------------------
-- header-crypto-parametrised types
--
-- Within `cardano-ledger`, we parametrise everything on our `crypto` type
-- "package". However, in `cardano-crypto-class`, things are parametrised on the
-- original algorithm. In order to make using types from that module easier, we
-- provide some type aliases which unwrap the crypto parameters.
--------------------------------------------------------------------------------

type SignedKES c = KES.SignedKES (KES c)

type SignKeyKES c = KES.SignKeyKES (KES c)

type VerKeyKES c = KES.VerKeyKES (KES c)

type CertifiedVRF c = VRF.CertifiedVRF (VRF c)

type SignKeyVRF c = VRF.SignKeyVRF (VRF c)

type VerKeyVRF c = VRF.VerKeyVRF (VRF c)
