{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Ledger.Keys
  ( KeyRole (..),
    HasKeyRole (..),
    asWitness,

    -- * DSIGN
    DSignable,
    VKey (..),
    KeyPair (..),
    signedDSIGN,
    verifySignedDSIGN,
    hashSignature,

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

    -- * Re-exports from cardano-crypto-class
    DSIGN.decodeSignedDSIGN,
    DSIGN.encodeSignedDSIGN,
    Hash.hashWithSerialiser,
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

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen)
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Crypto (ADDRHASH, Crypto, DSIGN, HASH, KES, VRF)
import Cardano.Ledger.Serialization (decodeRecordNamed)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Quiet

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
  | BlockIssuer
  | Witness
  deriving (Show)

class HasKeyRole (a :: KeyRole -> Type -> Type) where
  -- | General coercion of key roles.
  --
  --   The presence of this function is mostly to help the user realise where they
  --   are converting key roles.
  coerceKeyRole ::
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
  a 'Witness crypto
asWitness = coerceKeyRole

--------------------------------------------------------------------------------
-- Verification keys
--------------------------------------------------------------------------------

type DSignable c = DSIGN.Signable (DSIGN c)

-- | Discriminated verification key
--
--   We wrap the basic `VerKeyDSIGN` in order to add the key role.
newtype VKey (kd :: KeyRole) crypto = VKey {unVKey :: DSIGN.VerKeyDSIGN (DSIGN crypto)}
  deriving (Generic)

deriving via Quiet (VKey kd crypto) instance Crypto crypto => Show (VKey kd crypto)

deriving instance Crypto crypto => Eq (VKey kd crypto)

deriving instance
  (Crypto crypto, NFData (DSIGN.VerKeyDSIGN (DSIGN crypto))) =>
  NFData (VKey kd crypto)

deriving instance Crypto crypto => NoThunks (VKey kd crypto)

instance HasKeyRole VKey

instance
  (Crypto crypto, Typeable kd) =>
  FromCBOR (VKey kd crypto)
  where
  fromCBOR = VKey <$> DSIGN.decodeVerKeyDSIGN

instance
  (Crypto crypto, Typeable kd) =>
  ToCBOR (VKey kd crypto)
  where
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

instance Crypto crypto => NoThunks (KeyPair kd crypto)

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

-- | Hash a given signature
hashSignature ::
  (Crypto crypto) =>
  SignedDSIGN crypto (Hash crypto h) ->
  Hash crypto (SignedDSIGN crypto (Hash crypto h))
hashSignature = Hash.hashWith (DSIGN.rawSerialiseSigDSIGN . coerce)

--------------------------------------------------------------------------------
-- Key Hashes
--------------------------------------------------------------------------------

-- | Discriminated hash of public Key
newtype KeyHash (discriminator :: KeyRole) crypto
  = KeyHash (Hash.Hash (ADDRHASH crypto) (DSIGN.VerKeyDSIGN (DSIGN crypto)))
  deriving (Show, Eq, Ord)
  deriving newtype (NFData, NoThunks, Generic)

deriving instance
  (Crypto crypto, Typeable disc) =>
  ToCBOR (KeyHash disc crypto)

deriving instance
  (Crypto crypto, Typeable disc) =>
  FromCBOR (KeyHash disc crypto)

deriving newtype instance
  Crypto crypto =>
  ToJSONKey (KeyHash disc crypto)

deriving newtype instance
  Crypto crypto =>
  FromJSONKey (KeyHash disc crypto)

deriving newtype instance
  Crypto crypto =>
  ToJSON (KeyHash disc crypto)

deriving newtype instance
  Crypto crypto =>
  FromJSON (KeyHash disc crypto)

instance HasKeyRole KeyHash

-- | Hash a given public key
hashKey ::
  ( Crypto crypto
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

instance NoThunks (GenDelegPair crypto)

instance NFData (GenDelegPair crypto)

instance Crypto crypto => ToCBOR (GenDelegPair crypto) where
  toCBOR (GenDelegPair hk vrf) =
    encodeListLen 2 <> toCBOR hk <> toCBOR vrf

instance Crypto crypto => FromCBOR (GenDelegPair crypto) where
  fromCBOR = do
    decodeRecordNamed
      "GenDelegPair"
      (const 2)
      (GenDelegPair <$> fromCBOR <*> fromCBOR)

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

newtype GenDelegs crypto = GenDelegs
  { unGenDelegs :: Map (KeyHash 'Genesis crypto) (GenDelegPair crypto)
  }
  deriving (Eq, FromCBOR, NoThunks, NFData, Generic)
  deriving (Show) via Quiet (GenDelegs crypto)

deriving instance
  (Crypto crypto) =>
  ToCBOR (GenDelegs crypto)

newtype GKeys crypto = GKeys {unGKeys :: Set (VKey 'Genesis crypto)}
  deriving (Eq, NoThunks, Generic)
  deriving (Show) via Quiet (GKeys crypto)

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
