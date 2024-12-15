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

module Cardano.Ledger.Keys.Internal (
  KeyRole (..),
  HasKeyRole (..),
  asWitness,

  -- * DSIGN
  DSignable,
  VKey (..),
  signedDSIGN,
  verifySignedDSIGN,
  hashSignature,

  -- * Key hashes
  KeyHash (..),
  hashKey,

  -- * VRF Key Hashes
  KeyRoleVRF (..),
  VRFVerKeyHash (..),
  hashVerKeyVRF,
  toVRFVerKeyHash,
  fromVRFVerKeyHash,

  -- * Genesis delegations
  GenDelegPair (..),
  GenDelegs (..),

  -- * KES
  KESignable,

  -- * VRF
  VRFSignable,

  -- * Re-exports from cardano-crypto-class
  decodeSignedDSIGN,
  encodeSignedDSIGN,
  Hash.hashWithSerialiser,
  decodeSignedKES,
  decodeVerKeyKES,
  encodeSignedKES,
  encodeVerKeyKES,
  KES.signedKES,
  KES.updateKES,
  KES.verifyKES,
  KES.verifySignedKES,
  decodeVerKeyVRF,
  encodeVerKeyVRF,
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

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  decodeRecordNamed,
  encodeListLen,
 )
import Cardano.Ledger.Binary.Crypto
import Cardano.Ledger.Crypto (ADDRHASH, Crypto, DSIGN, HASH, KES, VRF)
import Cardano.Ledger.Orphans ()
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Coerce (Coercible, coerce)
import Data.Default (Default (..))
import Data.Kind (Type)
import Data.Map.Strict (Map)
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
  | DRepRole
  | HotCommitteeRole
  | ColdCommitteeRole
  deriving (Show)

class HasKeyRole (a :: KeyRole -> Type) where
  -- | General coercion of key roles.
  --
  --   The presence of this function is mostly to help the user realise where they
  --   are converting key roles.
  coerceKeyRole ::
    a r ->
    a r'
  default coerceKeyRole ::
    Coercible (a r) (a r') =>
    a r ->
    a r'
  coerceKeyRole = coerce

-- | Use a key as a witness.
--
--   This is the most common coercion between key roles, because most keys can
--   be used as witnesses to some types of transaction. As such, we provide an
--   explicit coercion for it.
asWitness ::
  HasKeyRole a =>
  a r ->
  a 'Witness
asWitness = coerceKeyRole

--------------------------------------------------------------------------------
-- Verification keys
--------------------------------------------------------------------------------

type DSignable = DSIGN.Signable DSIGN

-- | Discriminated verification key
--
--   We wrap the basic `VerKeyDSIGN` in order to add the key role.
newtype VKey (kd :: KeyRole) = VKey {unVKey :: DSIGN.VerKeyDSIGN DSIGN}
  deriving (Generic, Eq, NFData, NoThunks, DecCBOR, EncCBOR)

deriving via Quiet (VKey kd) instance Show (VKey kd)

instance HasKeyRole VKey

instance Typeable kd => FromCBOR (VKey kd) where
  fromCBOR = VKey <$> DSIGN.decodeVerKeyDSIGN
  {-# INLINE fromCBOR #-}

instance Typeable kd => ToCBOR (VKey kd) where
  toCBOR = DSIGN.encodeVerKeyDSIGN . unVKey

-- | Produce a digital signature
signedDSIGN ::
  DSIGN.Signable DSIGN a =>
  DSIGN.SignKeyDSIGN DSIGN ->
  a ->
  SignedDSIGN a
signedDSIGN key a = DSIGN.signedDSIGN () a key

-- | Verify a digital signature
verifySignedDSIGN ::
  DSIGN.Signable DSIGN a =>
  VKey kd ->
  a ->
  SignedDSIGN a ->
  Bool
verifySignedDSIGN (VKey vk) vd sigDSIGN =
  either (const False) (const True) $ DSIGN.verifySignedDSIGN () vk vd sigDSIGN
{-# INLINE verifySignedDSIGN #-}

-- | Hash a given signature
hashSignature ::
  SignedDSIGN (Hash h) ->
  Hash (SignedDSIGN (Hash h))
hashSignature (DSIGN.SignedDSIGN sigDSIGN) = Hash.castHash $ Hash.hashWith DSIGN.rawSerialiseSigDSIGN sigDSIGN
{-# INLINE hashSignature #-}

--------------------------------------------------------------------------------
-- Key Hashes
--------------------------------------------------------------------------------

-- | Discriminated hash of public Key
newtype KeyHash (r :: KeyRole) = KeyHash
  {unKeyHash :: Hash.Hash ADDRHASH (DSIGN.VerKeyDSIGN DSIGN)}
  deriving (Show, Eq, Ord)
  deriving newtype
    ( NFData
    , NoThunks
    , Generic
    , ToCBOR
    , FromCBOR
    , EncCBOR
    , DecCBOR
    , ToJSONKey
    , FromJSONKey
    , ToJSON
    , FromJSON
    , Default
    )

instance HasKeyRole KeyHash

-- | Hash a given public key
hashKey :: VKey kd -> KeyHash kd
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

data GenDelegPair = GenDelegPair
  { genDelegKeyHash :: !(KeyHash 'GenesisDelegate)
  , genDelegVrfHash :: !(VRFVerKeyHash 'GenDelegVRF)
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks GenDelegPair

instance NFData GenDelegPair

instance EncCBOR GenDelegPair where
  encCBOR (GenDelegPair hk vrf) =
    encodeListLen 2 <> encCBOR hk <> encCBOR vrf

instance DecCBOR GenDelegPair where
  decCBOR = do
    decodeRecordNamed
      "GenDelegPair"
      (const 2)
      (GenDelegPair <$> decCBOR <*> decCBOR)
  {-# INLINE decCBOR #-}

instance ToJSON GenDelegPair where
  toJSON (GenDelegPair d v) =
    Aeson.object
      [ "delegate" .= d
      , "vrf" .= v
      ]

instance FromJSON GenDelegPair where
  parseJSON =
    Aeson.withObject "GenDelegPair" $ \obj ->
      GenDelegPair
        <$> obj .: "delegate"
        <*> obj .: "vrf"

newtype GenDelegs = GenDelegs
  { unGenDelegs :: Map (KeyHash 'Genesis) GenDelegPair
  }
  deriving (Eq, EncCBOR, DecCBOR, NoThunks, NFData, Generic, FromJSON, ToJSON)
  deriving (Show) via Quiet GenDelegs

--------------------------------------------------------------------------------
-- crypto-parametrised types
--
-- Within `cardano-ledger`, we parametrise everything on our `crypto` type
-- "package". However, in `cardano-crypto-class`, things are parametrised on the
-- original algorithm. In order to make using types from that module easier, we
-- provide some type aliases which unwrap the crypto parameters.
--------------------------------------------------------------------------------

type Hash = Hash.Hash HASH

type SignedDSIGN = DSIGN.SignedDSIGN DSIGN

type SignKeyDSIGN = DSIGN.SignKeyDSIGN DSIGN

type SignedKES c = KES.SignedKES (KES c)

type SignKeyKES c = KES.SignKeyKES (KES c)

type VerKeyKES c = KES.VerKeyKES (KES c)

type CertifiedVRF c = VRF.CertifiedVRF (VRF c)

type SignKeyVRF c = VRF.SignKeyVRF (VRF c)

type VerKeyVRF c = VRF.VerKeyVRF (VRF c)

data KeyRoleVRF
  = StakePoolVRF
  | GenDelegVRF
  | BlockIssuerVRF

-- | Discriminated hash of VRF Verification Key
newtype VRFVerKeyHash (r :: KeyRoleVRF) = VRFVerKeyHash
  {unVRFVerKeyHash :: Hash.Hash HASH KeyRoleVRF}
  deriving (Show, Eq, Ord)
  deriving newtype
    ( NFData
    , NoThunks
    , Generic
    , ToCBOR
    , FromCBOR
    , EncCBOR
    , DecCBOR
    , ToJSONKey
    , FromJSONKey
    , ToJSON
    , FromJSON
    , Default
    )

hashVerKeyVRF :: Crypto c => VerKeyVRF c -> VRFVerKeyHash (r :: KeyRoleVRF)
hashVerKeyVRF = VRFVerKeyHash . Hash.castHash . VRF.hashVerKeyVRF

toVRFVerKeyHash :: Hash.Hash HASH (VRF.VerKeyVRF v) -> VRFVerKeyHash (r :: KeyRoleVRF)
toVRFVerKeyHash = VRFVerKeyHash . Hash.castHash

fromVRFVerKeyHash :: VRFVerKeyHash (r :: KeyRoleVRF) -> Hash.Hash HASH (VRF.VerKeyVRF v)
fromVRFVerKeyHash = Hash.castHash . unVRFVerKeyHash
