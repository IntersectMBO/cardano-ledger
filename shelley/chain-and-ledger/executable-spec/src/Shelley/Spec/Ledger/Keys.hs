{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Shelley.Spec.Ledger.Keys
  ( HashType (..),
    KeyRole (..),
    HasKeyRole (..),
    IsKeyRole,
    WitnessFor,
    asWitness,

    -- * Exported for testing purposes
    AlgorithmForHashType,
    KeyRoleHashType,

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
    GenDelegPair (..),
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

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen, enforceSize)
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Prelude (NFData, NoUnexpectedThunks (..))
import Data.Aeson ((.:), (.=), FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import qualified Data.Aeson as Aeson
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Ratio ((%))
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes (Nonce, UnitInterval, mkNonce, truncateUnitInterval)
import Shelley.Spec.Ledger.Crypto

-- | Shelley has two types of hashes.
--
--   - Address hashes, used for payment and staking credentials, and
--   - Regular hashes, used for everything else.
data HashType
  = AddrHash
  | RegularHash
  deriving (Show)

-- | Map a hash type to the algorithm used to compute that hash.
type family AlgorithmForHashType c (h :: HashType) :: Type where
  AlgorithmForHashType c AddrHash = ADDRHASH c
  AlgorithmForHashType c RegularHash = HASH c

-- | The role of a key.
--
--   Note that a role is not _fixed_, nor is it unique. In particular, keys may
--   variously be used as witnesses, and so in many case we will change the role
--   of a key to the 'Witness' role.
--
--   The role is itself parametrised by the type of hash used when a key is used
--   in that role. This is fixed for most roles, but we may have witnesses of
--   either type.
--
--   It is also perfectly allowable for a key to be used in many roles; there is
--   nothing prohibiting somebody using the same underlying key as their payment
--   and staking key, as well as the key for their stake pool. So these roles
--   are more intended for two purposes:
--
--   - To make explicit how we are using a key in the specifications
--   - To provide a guide to downstream implementors, for whom the profusion of
--     keys may be confusing.
data KeyRole (h :: HashType) where
  Genesis :: KeyRole RegularHash
  GenesisDelegate :: KeyRole RegularHash
  Payment :: KeyRole AddrHash
  Staking :: KeyRole AddrHash
  StakePool :: KeyRole RegularHash
  -- A block issuer might be either a genesis delegate or a stake pool
  BlockIssuer :: KeyRole RegularHash
  -- Witness keys may have either hash type.
  AWitness :: KeyRole AddrHash
  RWitness :: KeyRole RegularHash

deriving instance Show (KeyRole h)

-- | Map a key role to its required hash type.
type family KeyRoleHashType (kd :: KeyRole h) :: HashType where
  KeyRoleHashType (kd :: KeyRole h) = h

-- | Map a key role to the correct witness role.
type family WitnessFor (kd :: KeyRole h) :: KeyRole h where
  WitnessFor (kh :: KeyRole AddrHash) = AWitness
  WitnessFor (kh :: KeyRole RegularHash) = RWitness

-- | Wrap up various constraints needed to work with key roles.
class
  ( Crypto crypto,
    Typeable kr,
    Typeable (KeyRoleHashType kr),
    Hash.HashAlgorithm (AlgorithmForHashType crypto (KeyRoleHashType kr))
  ) =>
  IsKeyRole kr crypto

instance Crypto crypto => IsKeyRole Genesis crypto

instance Crypto crypto => IsKeyRole GenesisDelegate crypto

instance Crypto crypto => IsKeyRole Payment crypto

instance Crypto crypto => IsKeyRole Staking crypto

instance Crypto crypto => IsKeyRole StakePool crypto

instance Crypto crypto => IsKeyRole BlockIssuer crypto

instance Crypto crypto => IsKeyRole AWitness crypto

instance Crypto crypto => IsKeyRole RWitness crypto

class HasKeyRole a where
  -- | General coercion of key roles.
  --
  --   The presence of this function is mostly to help the user realise where they
  --   are converting key roles.
  coerceKeyRole ::
    (KeyRoleHashType r ~ KeyRoleHashType r') =>
    a r crypto ->
    a r' crypto
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
asWitness ::
  (HasKeyRole a) =>
  a r crypto ->
  a (WitnessFor r) crypto
asWitness = coerceKeyRole

--------------------------------------------------------------------------------
-- Verification keys
--------------------------------------------------------------------------------

type DSignable c = DSIGN.Signable (DSIGN c)

-- | Discriminated verification key
--
--   We wrap the basic `VerKeyDSIGN` in order to add the key role.
newtype VKey (kd :: KeyRole h) crypto = VKey (DSIGN.VerKeyDSIGN (DSIGN crypto))

deriving instance Crypto crypto => Show (VKey kd crypto)

deriving instance Crypto crypto => Eq (VKey kd crypto)

deriving instance (Crypto crypto, NFData (DSIGN.VerKeyDSIGN (DSIGN crypto))) => NFData (VKey kd crypto)

deriving instance Crypto crypto => NoUnexpectedThunks (VKey kd crypto)

instance HasKeyRole VKey

instance
  (Crypto crypto, Typeable kd, Typeable (KeyRoleHashType kd)) =>
  FromCBOR (VKey kd crypto)
  where
  fromCBOR = VKey <$> DSIGN.decodeVerKeyDSIGN

instance
  (Crypto crypto, Typeable kd, Typeable (KeyRoleHashType kd)) =>
  ToCBOR (VKey kd crypto)
  where
  toCBOR (VKey vk) = DSIGN.encodeVerKeyDSIGN vk
  encodedSizeExpr _size proxy = DSIGN.encodedVerKeyDSIGNSizeExpr ((\(VKey k) -> k) <$> proxy)

-- | Pair of signing key and verification key, with a usage role.
data KeyPair (kd :: KeyRole h) crypto = KeyPair
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
newtype KeyHash (discriminator :: KeyRole h) crypto
  = KeyHash (Hash.Hash (AlgorithmForHashType crypto h) (DSIGN.VerKeyDSIGN (DSIGN crypto)))
  deriving (Show, Eq, Ord)
  deriving newtype (NFData, NoUnexpectedThunks, Generic)

deriving instance
  (IsKeyRole disc crypto) =>
  ToCBOR (KeyHash disc crypto)

deriving instance
  (IsKeyRole disc crypto) =>
  FromCBOR (KeyHash disc crypto)

deriving newtype instance ToJSONKey (KeyHash disc crypto)

deriving newtype instance
  (IsKeyRole disc crypto) =>
  FromJSONKey (KeyHash disc crypto)

deriving newtype instance ToJSON (KeyHash disc crypto)

deriving newtype instance
  (IsKeyRole disc crypto) =>
  FromJSON (KeyHash disc crypto)

instance HasKeyRole KeyHash

-- | Hash a given public key
hashKey ::
  ( Crypto crypto,
    Hash.HashAlgorithm (AlgorithmForHashType crypto (KeyRoleHashType kd))
  ) =>
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
  fromNatural k = truncateUnitInterval $ fromIntegral k % 10000

--------------------------------------------------------------------------------
-- Genesis delegation
--
-- TODO should this really live in here?
--------------------------------------------------------------------------------

data GenDelegPair crypto = GenDelegPair
  { genDelegKeyHash :: !(KeyHash 'GenesisDelegate crypto),
    genDelegVrfHash :: !(Hash crypto (VerKeyVRF crypto))
  }
  deriving (Show, Eq, Ord, Generic)

instance NoUnexpectedThunks (GenDelegPair crypto)

instance NFData (GenDelegPair crypto)

instance Crypto crypto => ToCBOR (GenDelegPair crypto) where
  toCBOR (GenDelegPair hk vrf) =
    encodeListLen 2 <> toCBOR hk <> toCBOR vrf

instance Crypto crypto => FromCBOR (GenDelegPair crypto) where
  fromCBOR = do
    enforceSize "GenDelegPair" 2
    GenDelegPair <$> fromCBOR <*> fromCBOR

instance Crypto crypto => ToJSON (GenDelegPair crypto) where
  toJSON (GenDelegPair d v) =
    Aeson.object
      [ "delegate" .= d,
        "vrf" .= v
      ]

instance Crypto crypto => FromJSON (GenDelegPair crypto) where
  parseJSON =
    Aeson.withObject "GenDelegPair" $ \obj ->
      GenDelegPair
        <$> obj .: "delegate"
        <*> obj .: "vrf"

newtype GenDelegs crypto
  = GenDelegs (Map (KeyHash 'Genesis crypto) (GenDelegPair crypto))
  deriving (Show, Eq, FromCBOR, NoUnexpectedThunks, NFData, Generic)

deriving instance
  (Crypto crypto) =>
  ToCBOR (GenDelegs crypto)

newtype GKeys crypto = GKeys (Set (VKey 'Genesis crypto))
  deriving (Show, Eq, NoUnexpectedThunks, Generic)

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
