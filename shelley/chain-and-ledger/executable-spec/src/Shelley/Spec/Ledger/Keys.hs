{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Shelley.Spec.Ledger.Keys
  ( KeyRole (..),
    HasKeyRole (..),

    -- * DSIGN
    DSignable,
    VKey (..),
    KeyPair (..),
    signedDSIGN,
    verifySignedDSIGN,

    -- * Key hashes
    KeyHash (..),
    hashKey,

    -- * Genesis delegations
    GenDelegs (..),
    GKeys (..),

    -- * KES
    KESignable,

    -- * VRF
    VRFSignable,
    VRFValue (..),

    -- * Re-exports from cardano-crypto-class
    DSIGN.decodeSignedDSIGN,
    DSIGN.encodeSignedDSIGN,
    Hash.hash,
    KES.decodeSignedKES,
    KES.decodeVerKeyKES,
    KES.encodeSignedKES,
    KES.encodeVerKeyKES,
    KES.signedKES,
    KES.updateKES,
    KES.verifyKES,
    KES.verifySignedKES,
    VRF.decodeVerKeyVRF,
    VRF.encodeVerKeyVRF,
    VRF.hashVerKeyVRF,
    VRF.verifyVRF,

    -- * Re-parametrised types over `crypto`
    CertifiedVRF,
    Hash,
    SignedDSIGN,
    SignKeyDSIGN,
    SignedKES,
    SignKeyKES,
    SignKeyVRF,
    VerKeyKES,
    VerKeyVRF,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Prelude (NFData, NoUnexpectedThunks (..))
import Data.Coerce (Coercible, coerce)
import Data.Map.Strict (Map)
import Data.Ratio ((%))
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes (Nonce, UnitInterval, mkNonce, truncateUnitInterval)
import Shelley.Spec.Ledger.Crypto

-- | The role of a key.
--
--   Note that a role is not _fixed_, nor is it unique. In particular, keys may
--   variously be used as witnesses, and so in many case we will change the role
--   of a key to the 'Witness' role.
--
--   It is also perfectly allowable for a key to be used in many roles; there is
--   nothing prohibiting somebody using the same underlying key as their payment
--   and staking key, as well as the key for their stake pool. So these roles
--   are more intended for two purposes:
--
--   - To make explicit how we are using a key in the specifications
--   - To provide a guide to downstream implementors, for whom the profusion of
--     keys may be confusing.
data KeyRole
  = Genesis
  | GenesisDelegate
  | Payment
  | Staking
  | StakePool
  | -- | A block issuer might be either a genesis delegate or a stake pool
    BlockIssuer
  | Witness
  deriving (Show)

class HasKeyRole a where
  -- | General coercion of key roles.
  --
  --   The presence of this function is mostly to help the user realise where they
  --   are converting key roles.
  coerceKeyRole :: a r crypto -> a r' crypto
  default coerceKeyRole ::
    Coercible (a r crypto) (a r' crypto) =>
    a r crypto ->
    a r' crypto
  coerceKeyRole = coerce

  -- | Use a key as a witness.
  --
  --   This is the most common coercion between key roles, because most keys can
  --   be used as witnesses to some types of transaction. As such, we provide an
  --   explicit coercion for it.
  asWitness :: a r crypto -> a 'Witness crypto
  asWitness = coerceKeyRole

--------------------------------------------------------------------------------
-- Verification keys
--------------------------------------------------------------------------------

type DSignable c = DSIGN.Signable (DSIGN c)

-- | Discriminated verification key
--
--   We wrap the basic `VerKeyDSIGN` in order to add the key role.
newtype VKey (kd :: KeyRole) crypto = VKey (DSIGN.VerKeyDSIGN (DSIGN crypto))

deriving instance Crypto crypto => Show (VKey kd crypto)

deriving instance Crypto crypto => Eq (VKey kd crypto)

deriving instance Num (DSIGN.VerKeyDSIGN (DSIGN crypto)) => Num (VKey kd crypto)

deriving instance (Crypto crypto, NFData (DSIGN.VerKeyDSIGN (DSIGN crypto))) => NFData (VKey kd crypto)

deriving instance Crypto crypto => NoUnexpectedThunks (VKey kd crypto)

instance HasKeyRole VKey

instance (Crypto crypto, Typeable kd) => FromCBOR (VKey kd crypto) where
  fromCBOR = VKey <$> DSIGN.decodeVerKeyDSIGN

instance (Crypto crypto, Typeable kd) => ToCBOR (VKey kd crypto) where
  toCBOR (VKey vk) = DSIGN.encodeVerKeyDSIGN vk
  encodedSizeExpr _size proxy = DSIGN.encodedVerKeyDSIGNSizeExpr ((\(VKey k) -> k) <$> proxy)

-- | Pair of signing key and verification key, with a usage role.
data KeyPair (kd :: KeyRole) crypto = KeyPair
  { vKey :: !(VKey kd crypto),
    sKey :: !(DSIGN.SignKeyDSIGN (DSIGN crypto))
  }
  deriving (Generic, Show)

instance
  ( Crypto crypto,
    NFData (DSIGN.VerKeyDSIGN (DSIGN crypto)),
    NFData (DSIGN.SignKeyDSIGN (DSIGN crypto))
  ) =>
  NFData (KeyPair kd crypto)

instance Crypto crypto => NoUnexpectedThunks (KeyPair kd crypto)

instance HasKeyRole KeyPair

-- | Produce a digital signature
signedDSIGN ::
  (Crypto crypto, DSIGN.Signable (DSIGN crypto) a) =>
  DSIGN.SignKeyDSIGN (DSIGN crypto) ->
  a ->
  SignedDSIGN crypto a
signedDSIGN key a = DSIGN.signedDSIGN () a key

-- | Verify a digital signature
verifySignedDSIGN ::
  (Crypto crypto, DSIGN.Signable (DSIGN crypto) a) =>
  VKey kd crypto ->
  a ->
  SignedDSIGN crypto a ->
  Bool
verifySignedDSIGN (VKey vk) vd sigDSIGN =
  either (const False) (const True) $ DSIGN.verifySignedDSIGN () vk vd sigDSIGN

--------------------------------------------------------------------------------
-- Key Hashes
--------------------------------------------------------------------------------

-- | Discriminated hash of public Key
newtype KeyHash (discriminator :: KeyRole) crypto
  = KeyHash (Hash crypto (DSIGN.VerKeyDSIGN (DSIGN crypto)))
  deriving (Show, Eq, Ord)
  deriving newtype (NFData, NoUnexpectedThunks)

deriving instance (Crypto crypto, Typeable disc) => ToCBOR (KeyHash disc crypto)

deriving instance (Crypto crypto, Typeable disc) => FromCBOR (KeyHash disc crypto)

instance HasKeyRole KeyHash

-- | Hash a given public key
hashKey ::
  Crypto crypto =>
  VKey kd crypto ->
  KeyHash kd crypto
hashKey (VKey vk) = KeyHash $ DSIGN.hashVerKeyDSIGN vk

--------------------------------------------------------------------------------
-- KES
--------------------------------------------------------------------------------

type KESignable c = KES.Signable (KES c)

--------------------------------------------------------------------------------
-- VRF
--------------------------------------------------------------------------------

type VRFSignable c = VRF.Signable (VRF c)

-- | Our VRFAlgorithm provides a 'Natural', so we must exhibit an extractor to
--   use the bits as some other type
class VRFValue a where
  -- | Extract a value from a natural number derived from the VRF computation.
  fromNatural :: Natural -> a

instance VRFValue Nonce where
  fromNatural = mkNonce

instance VRFValue UnitInterval where
  -- TODO Consider whether this is a reasonable thing to do
  fromNatural k = truncateUnitInterval $ toInteger k % 10000

--------------------------------------------------------------------------------
-- Genesis delegation
--
-- TODO should this really live in here?
--------------------------------------------------------------------------------

newtype GenDelegs crypto
  = GenDelegs (Map (KeyHash 'Genesis crypto) (KeyHash 'GenesisDelegate crypto))
  deriving (Show, Eq, ToCBOR, FromCBOR, NoUnexpectedThunks)

newtype GKeys crypto = GKeys (Set (VKey 'Genesis crypto))
  deriving (Show, Eq, NoUnexpectedThunks)

--------------------------------------------------------------------------------
-- crypto-parametrised types
--
-- Within `cardano-ledger-specs`, we parametrise everything on our `crypto` type
-- "package". However, in `cardano-crypto-class`, things are parametrised on the
-- original algorithm. In order to make using types from that module easier, we
-- provide some type aliases which unwrap the crypto parameters.
--------------------------------------------------------------------------------

type Hash c = Hash.Hash (HASH c)

type SignedDSIGN c = DSIGN.SignedDSIGN (DSIGN c)

type SignKeyDSIGN c = DSIGN.SignKeyDSIGN (DSIGN c)

type SignedKES c = KES.SignedKES (KES c)

type SignKeyKES c = KES.SignKeyKES (KES c)

type VerKeyKES c = KES.VerKeyKES (KES c)

type CertifiedVRF c = VRF.CertifiedVRF (VRF c)

type SignKeyVRF c = VRF.SignKeyVRF (VRF c)

type VerKeyVRF c = VRF.VerKeyVRF (VRF c)
