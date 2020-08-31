{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Shelley.Spec.Ledger.Keys
  ( KeyRole (..),
    HasKeyRole (..),
    asWitness,

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
import Cardano.Ledger.Crypto (ADDRHASH, DSIGN, HASH, KES, VRF)
import Cardano.Ledger.Era
import Cardano.Prelude (NFData, NoUnexpectedThunks (..))
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Quiet
import Shelley.Spec.Ledger.Serialization (decodeRecordNamed)

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
    a r era ->
    a r' era
  default coerceKeyRole ::
    Coercible (a r era) (a r' era) =>
    a r era ->
    a r' era
  coerceKeyRole = coerce

-- | Use a key as a witness.
--
--   This is the most common coercion between key roles, because most keys can
--   be used as witnesses to some types of transaction. As such, we provide an
--   explicit coercion for it.
asWitness ::
  (HasKeyRole a) =>
  a r era ->
  a 'Witness era
asWitness = coerceKeyRole

--------------------------------------------------------------------------------
-- Verification keys
--------------------------------------------------------------------------------

type DSignable e = DSIGN.Signable (DSIGN (Crypto e))

-- | Discriminated verification key
--
--   We wrap the basic `VerKeyDSIGN` in order to add the key role.
newtype VKey (kd :: KeyRole) era = VKey {unVKey :: DSIGN.VerKeyDSIGN (DSIGN (Crypto era))}
  deriving (Generic)

deriving via Quiet (VKey kd era) instance Era era => Show (VKey kd era)

deriving instance Era era => Eq (VKey kd era)

deriving instance
  (Era era, NFData (DSIGN.VerKeyDSIGN (DSIGN (Crypto era)))) =>
  NFData (VKey kd era)

deriving instance Era era => NoUnexpectedThunks (VKey kd era)

instance HasKeyRole VKey

instance
  (Era era, Typeable kd) =>
  FromCBOR (VKey kd era)
  where
  fromCBOR = VKey <$> DSIGN.decodeVerKeyDSIGN

instance
  (Era era, Typeable kd) =>
  ToCBOR (VKey kd era)
  where
  toCBOR (VKey vk) = DSIGN.encodeVerKeyDSIGN vk
  encodedSizeExpr _size proxy =
    DSIGN.encodedVerKeyDSIGNSizeExpr
      ((\(VKey k) -> k) <$> proxy)

-- | Pair of signing key and verification key, with a usage role.
data KeyPair (kd :: KeyRole) era = KeyPair
  { vKey :: !(VKey kd era),
    sKey :: !(DSIGN.SignKeyDSIGN (DSIGN (Crypto era)))
  }
  deriving (Generic, Show)

instance
  ( Era era,
    NFData (DSIGN.VerKeyDSIGN (DSIGN (Crypto era))),
    NFData (DSIGN.SignKeyDSIGN (DSIGN (Crypto era)))
  ) =>
  NFData (KeyPair kd era)

instance Era era => NoUnexpectedThunks (KeyPair kd era)

instance HasKeyRole KeyPair

-- | Produce a digital signature
signedDSIGN ::
  (Era era, DSIGN.Signable (DSIGN (Crypto era)) a) =>
  DSIGN.SignKeyDSIGN (DSIGN (Crypto era)) ->
  a ->
  SignedDSIGN era a
signedDSIGN key a = DSIGN.signedDSIGN () a key

-- | Verify a digital signature
verifySignedDSIGN ::
  (Era era, DSIGN.Signable (DSIGN (Crypto era)) a) =>
  VKey kd era ->
  a ->
  SignedDSIGN era a ->
  Bool
verifySignedDSIGN (VKey vk) vd sigDSIGN =
  either (const False) (const True) $ DSIGN.verifySignedDSIGN () vk vd sigDSIGN

--------------------------------------------------------------------------------
-- Key Hashes
--------------------------------------------------------------------------------

-- | Discriminated hash of public Key
newtype KeyHash (discriminator :: KeyRole) era
  = KeyHash
      ( Hash.Hash
          (ADDRHASH (Crypto era))
          (DSIGN.VerKeyDSIGN (DSIGN (Crypto era)))
      )
  deriving (Show, Eq, Ord)
  deriving newtype (NFData, NoUnexpectedThunks, Generic)

deriving instance
  (Era era, Typeable disc) =>
  ToCBOR (KeyHash disc era)

deriving instance
  (Era era, Typeable disc) =>
  FromCBOR (KeyHash disc era)

deriving newtype instance ToJSONKey (KeyHash disc era)

deriving newtype instance
  (Era era) =>
  FromJSONKey (KeyHash disc era)

deriving newtype instance ToJSON (KeyHash disc era)

deriving newtype instance
  (Era era) =>
  FromJSON (KeyHash disc era)

instance HasKeyRole KeyHash

-- | Hash a given public key
hashKey ::
  ( Era era
  ) =>
  VKey kd era ->
  KeyHash kd era
hashKey (VKey vk) = KeyHash $ DSIGN.hashVerKeyDSIGN vk

--------------------------------------------------------------------------------
-- KES
--------------------------------------------------------------------------------

type KESignable e = KES.Signable (KES (Crypto e))

--------------------------------------------------------------------------------
-- VRF
--------------------------------------------------------------------------------

type VRFSignable e = VRF.Signable (VRF (Crypto e))

--------------------------------------------------------------------------------
-- Genesis delegation
--
-- TODO should this really live in here?
--------------------------------------------------------------------------------

data GenDelegPair era = GenDelegPair
  { genDelegKeyHash :: !(KeyHash 'GenesisDelegate era),
    genDelegVrfHash :: !(Hash era (VerKeyVRF era))
  }
  deriving (Show, Eq, Ord, Generic)

instance NoUnexpectedThunks (GenDelegPair era)

instance NFData (GenDelegPair era)

instance Era era => ToCBOR (GenDelegPair era) where
  toCBOR (GenDelegPair hk vrf) =
    encodeListLen 2 <> toCBOR hk <> toCBOR vrf

instance Era era => FromCBOR (GenDelegPair era) where
  fromCBOR = do
    decodeRecordNamed
      "GenDelegPair"
      (const 2)
      (GenDelegPair <$> fromCBOR <*> fromCBOR)

instance Era era => ToJSON (GenDelegPair era) where
  toJSON (GenDelegPair d v) =
    Aeson.object
      [ "delegate" .= d,
        "vrf" .= v
      ]

instance Era era => FromJSON (GenDelegPair era) where
  parseJSON =
    Aeson.withObject "GenDelegPair" $ \obj ->
      GenDelegPair
        <$> obj .: "delegate"
        <*> obj .: "vrf"

newtype GenDelegs era = GenDelegs
  { unGenDelegs :: Map (KeyHash 'Genesis era) (GenDelegPair era)
  }
  deriving (Eq, FromCBOR, NoUnexpectedThunks, NFData, Generic)
  deriving (Show) via Quiet (GenDelegs era)

deriving instance
  (Era era) =>
  ToCBOR (GenDelegs era)

newtype GKeys era = GKeys {unGKeys :: Set (VKey 'Genesis era)}
  deriving (Eq, NoUnexpectedThunks, Generic)
  deriving (Show) via Quiet (GKeys era)

--------------------------------------------------------------------------------
-- crypto-parametrised types
--
-- Within `cardano-ledger-specs`, we parametrise everything on our `era` type
-- "package". However, in `cardano-era-class`, things are parametrised on the
-- original algorithm. In order to make using types from that module easier, we
-- provide some type aliases which unwrap the era parameters.
--------------------------------------------------------------------------------

type Hash e = Hash.Hash (HASH (Crypto e))

type SignedDSIGN e = DSIGN.SignedDSIGN (DSIGN (Crypto e))

type SignKeyDSIGN e = DSIGN.SignKeyDSIGN (DSIGN (Crypto e))

type SignedKES e = KES.SignedKES (KES (Crypto e))

type SignKeyKES e = KES.SignKeyKES (KES (Crypto e))

type VerKeyKES e = KES.VerKeyKES (KES (Crypto e))

type CertifiedVRF e = VRF.CertifiedVRF (VRF (Crypto e))

type SignKeyVRF e = VRF.SignKeyVRF (VRF (Crypto e))

type VerKeyVRF e = VRF.VerKeyVRF (VRF (Crypto e))
