{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.Ledger.Keys (
  -- * VKey
  VKey (..),

  -- * Key Roles
  KeyRole (..),
  HasKeyRole (..),
  asWitness,

  -- * Key Hashes
  KeyHash (..),
  hashKey,

  -- * Signature
  DSIGN,
  DSignable,
  signedDSIGN,
  verifySignedDSIGN,
  hashSignature,

  -- * VRF Key Hashes
  KeyRoleVRF (..),
  VRFVerKeyHash (..),
  toVRFVerKeyHash,
  fromVRFVerKeyHash,

  -- * Genesis delegations
  GenDelegPair (..),
  GenDelegs (..),
  module Cardano.Ledger.Keys.WitVKey,
  module Cardano.Ledger.Keys.Bootstrap,

  -- * To be removed

  -- ** Re-exports from cardano-crypto-class
  decodeSignedDSIGN,
  encodeSignedDSIGN,
  Hash.hashWithSerialiser,

  -- * Deprecated
  Hash,

  -- ** DSIGN
  SignedDSIGN,
  SignKeyDSIGN,

  -- ** KES
  KESignable,
  SignedKES,
  SignKeyKES,
  VerKeyKES,

  -- ** VRF
  VRFSignable,
  CertifiedVRF,
  SignKeyVRF,
  VerKeyVRF,
  hashVerKeyVRF,
) where

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Crypto
import Cardano.Ledger.Hashes (
  GenDelegPair (..),
  GenDelegs (..),
  KeyHash (..),
  KeyRoleVRF (..),
  VRFVerKeyHash (..),
  fromVRFVerKeyHash,
  hashKey,
  toVRFVerKeyHash,
 )
import Cardano.Ledger.Keys.Bootstrap
import Cardano.Ledger.Keys.Internal
import Cardano.Ledger.Keys.WitVKey

hashVerKeyVRF :: Crypto c => VRF.VerKeyVRF (VRF c) -> VRFVerKeyHash (r :: KeyRoleVRF)
hashVerKeyVRF = VRFVerKeyHash . Hash.castHash . VRF.hashVerKeyVRF
{-# DEPRECATED hashVerKeyVRF "Use `Cardano.Protocol.Crypto.hashVerKeyVRF` instead" #-}

type Hash = Hash.Hash HASH

{-# DEPRECATED Hash "In favor of `Cardano.Crypto.Hash.Hash` `HASH`" #-}

type SignedDSIGN = DSIGN.SignedDSIGN DSIGN

{-# DEPRECATED SignedDSIGN "In favor of @`Cardano.Crypto.DSIGN.SignedDSIGN` `DSIGN`@" #-}

type SignKeyDSIGN = DSIGN.SignKeyDSIGN DSIGN

{-# DEPRECATED SignKeyDSIGN "In favor of @`Cardano.Crypto.DSIGN.SignKeyDSIGN` `DSIGN`@" #-}

-- | Hash a given signature
hashSignature ::
  SignedDSIGN (Hash h) ->
  Hash (SignedDSIGN (Hash h))
hashSignature (DSIGN.SignedDSIGN sigDSIGN) = Hash.castHash $ Hash.hashWith DSIGN.rawSerialiseSigDSIGN sigDSIGN
{-# DEPRECATED
  hashSignature
  "In favor of `Cardano.Ledger.Hashes.hashTxBodySignature`. \
  \Fallback on `Cardano.Crypto.Hash.hashWith` if you need more general hashing functionality."
  #-}

--------------------------------------------------------------------------------
-- KES
--------------------------------------------------------------------------------

type KESignable c = KES.Signable (KES c)

{-# DEPRECATED KESignable "In favor of @`Cardano.Crypto.KES.Signable` (`KES` c)@" #-}

type SignedKES c = KES.SignedKES (KES c)

{-# DEPRECATED SignedKES "In favor of @`Cardano.Crypto.KES.SignedKES` (`KES` c)@" #-}

type SignKeyKES c = KES.SignKeyKES (KES c)

{-# DEPRECATED SignKeyKES "In favor of @`Cardano.Crypto.KES.SignKeyKES` (`KES` c)@" #-}

type VerKeyKES c = KES.VerKeyKES (KES c)

{-# DEPRECATED VerKeyKES "In favor of @`Cardano.Crypto.KES.VerKeyKES` (`KES` c)@" #-}

--------------------------------------------------------------------------------
-- VRF
--------------------------------------------------------------------------------

type VRFSignable c = VRF.Signable (VRF c)

{-# DEPRECATED VRFSignable "In favor of @`Cardano.Crypto.VRF.Signable` (`VRF` c)@" #-}

type CertifiedVRF c = VRF.CertifiedVRF (VRF c)

{-# DEPRECATED CertifiedVRF "In favor of @`Cardano.Crypto.VRF.CertifiedVRF` (`VRF` c)@" #-}

type SignKeyVRF c = VRF.SignKeyVRF (VRF c)

{-# DEPRECATED SignKeyVRF "In favor of @`Cardano.Crypto.VRF.SignKeyVRF` (`VRF` c)@" #-}

type VerKeyVRF c = VRF.VerKeyVRF (VRF c)

{-# DEPRECATED VerKeyVRF "In favor of @`Cardano.Crypto.VRF.VerKeyVRF` (`VRF` c)@" #-}
