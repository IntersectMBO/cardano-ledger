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

module Cardano.Ledger.Keys (
  KeyRole (..),
  HasKeyRole (..),
  asWitness,

  -- * DSIGN
  DSignable,
  VKey (..),
  KeyPair (..), -- deprecated
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

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Binary (
  FromCBOR (..),
  ToCBOR (..),
  decodeRecordNamed,
  encodeListLen,
  encodedVerKeyDSIGNSizeExpr,
 )
import Cardano.Ledger.Binary.Crypto
import Cardano.Ledger.Binary.Plain (DecCBOR, EncCBOR)
import Cardano.Ledger.Crypto (ADDRHASH, Crypto, DSIGN, HASH, KES, VRF)
import Cardano.Ledger.TreeDiff (Expr (App), ToExpr (toExpr))
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
  | Voting
  deriving (Show)

class HasKeyRole (a :: KeyRole -> Type -> Type) where
  -- | General coercion of key roles.
  --
  --   The presence of this function is mostly to help the user realise where they
  --   are converting key roles.
  coerceKeyRole ::
    a r c ->
    a r' c
  default coerceKeyRole ::
    Coercible (a r c) (a r' c) =>
    a r c ->
    a r' c
  coerceKeyRole = coerce

-- | Use a key as a witness.
--
--   This is the most common coercion between key roles, because most keys can
--   be used as witnesses to some types of transaction. As such, we provide an
--   explicit coercion for it.
asWitness ::
  (HasKeyRole a) =>
  a r c ->
  a 'Witness c
asWitness = coerceKeyRole

--------------------------------------------------------------------------------
-- Verification keys
--------------------------------------------------------------------------------

type DSignable c = DSIGN.Signable (DSIGN c)

-- | Discriminated verification key
--
--   We wrap the basic `VerKeyDSIGN` in order to add the key role.
newtype VKey (kd :: KeyRole) c = VKey {unVKey :: DSIGN.VerKeyDSIGN (DSIGN c)}
  deriving (Generic)

deriving via Quiet (VKey kd c) instance Crypto c => Show (VKey kd c)

deriving instance Crypto c => Eq (VKey kd c)

deriving instance
  (Crypto c, NFData (DSIGN.VerKeyDSIGN (DSIGN c))) =>
  NFData (VKey kd c)

deriving instance Crypto c => NoThunks (VKey kd c)

instance HasKeyRole VKey

instance
  (Crypto c, Typeable kd) =>
  FromCBOR (VKey kd c)
  where
  fromCBOR = VKey <$> decodeVerKeyDSIGN

instance
  (Crypto c, Typeable kd) =>
  ToCBOR (VKey kd c)
  where
  toCBOR (VKey vk) = encodeVerKeyDSIGN vk
  encodedSizeExpr _size proxy = encodedVerKeyDSIGNSizeExpr ((\(VKey k) -> k) <$> proxy)

data KeyPair (kd :: KeyRole) c = KeyPair
  { vKey :: !(VKey kd c)
  , sKey :: !(DSIGN.SignKeyDSIGN (DSIGN c))
  }
{-# DEPRECATED KeyPair "Use `Test.Cardano.Ledger.Core.KeyPair (KeyPair)` instead" #-}

-- | Produce a digital signature
signedDSIGN ::
  (Crypto c, DSIGN.Signable (DSIGN c) a) =>
  DSIGN.SignKeyDSIGN (DSIGN c) ->
  a ->
  SignedDSIGN c a
signedDSIGN key a = DSIGN.signedDSIGN () a key

-- | Verify a digital signature
verifySignedDSIGN ::
  (Crypto c, DSIGN.Signable (DSIGN c) a) =>
  VKey kd c ->
  a ->
  SignedDSIGN c a ->
  Bool
verifySignedDSIGN (VKey vk) vd sigDSIGN =
  either (const False) (const True) $ DSIGN.verifySignedDSIGN () vk vd sigDSIGN

-- | Hash a given signature
hashSignature ::
  (Crypto c) =>
  SignedDSIGN c (Hash c h) ->
  Hash c (SignedDSIGN c (Hash c h))
hashSignature = Hash.hashWith (DSIGN.rawSerialiseSigDSIGN . coerce)

--------------------------------------------------------------------------------
-- Key Hashes
--------------------------------------------------------------------------------

-- | Discriminated hash of public Key
newtype KeyHash (discriminator :: KeyRole) c
  = KeyHash (Hash.Hash (ADDRHASH c) (DSIGN.VerKeyDSIGN (DSIGN c)))
  deriving (Show, Eq, Ord)
  deriving newtype (NFData, NoThunks, Generic)

deriving instance
  (Crypto c, Typeable disc) =>
  EncCBOR (KeyHash disc c)

deriving instance
  (Crypto c, Typeable disc) =>
  DecCBOR (KeyHash disc c)

deriving instance
  (Crypto c, Typeable disc) =>
  ToCBOR (KeyHash disc c)

deriving instance
  (Crypto c, Typeable disc) =>
  FromCBOR (KeyHash disc c)

deriving newtype instance
  Crypto c =>
  ToJSONKey (KeyHash disc c)

deriving newtype instance
  Crypto c =>
  FromJSONKey (KeyHash disc c)

deriving newtype instance
  Crypto c =>
  ToJSON (KeyHash disc c)

deriving newtype instance
  Crypto c =>
  FromJSON (KeyHash disc c)

instance HasKeyRole KeyHash

-- | Hash a given public key
hashKey ::
  ( Crypto c
  ) =>
  VKey kd c ->
  KeyHash kd c
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

data GenDelegPair c = GenDelegPair
  { genDelegKeyHash :: !(KeyHash 'GenesisDelegate c)
  , genDelegVrfHash :: !(Hash c (VerKeyVRF c))
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks (GenDelegPair c)

instance NFData (GenDelegPair c)

instance Crypto c => ToCBOR (GenDelegPair c) where
  toCBOR (GenDelegPair hk vrf) =
    encodeListLen 2 <> toCBOR hk <> toCBOR vrf

instance Crypto c => FromCBOR (GenDelegPair c) where
  fromCBOR = do
    decodeRecordNamed
      "GenDelegPair"
      (const 2)
      (GenDelegPair <$> fromCBOR <*> fromCBOR)

instance Crypto c => ToJSON (GenDelegPair c) where
  toJSON (GenDelegPair d v) =
    Aeson.object
      [ "delegate" .= d
      , "vrf" .= v
      ]

instance Crypto c => FromJSON (GenDelegPair c) where
  parseJSON =
    Aeson.withObject "GenDelegPair" $ \obj ->
      GenDelegPair
        <$> obj .: "delegate"
        <*> obj .: "vrf"

newtype GenDelegs c = GenDelegs
  { unGenDelegs :: Map (KeyHash 'Genesis c) (GenDelegPair c)
  }
  deriving (Eq, FromCBOR, NoThunks, NFData, Generic, FromJSON)
  deriving (Show) via Quiet (GenDelegs c)

deriving instance
  (Crypto c) =>
  ToCBOR (GenDelegs c)

deriving instance
  Crypto c =>
  ToJSON (GenDelegs c)

newtype GKeys c = GKeys {unGKeys :: Set (VKey 'Genesis c)}
  deriving (Eq, NoThunks, Generic)
  deriving (Show) via Quiet (GKeys c)

--------------------------------------------------------------------------------
-- crypto-parametrised types
--
-- Within `cardano-ledger`, we parametrise everything on our `crypto` type
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

-- ===============================================

instance ToExpr (GenDelegs c)

instance ToExpr (GenDelegPair c)

instance ToExpr (KeyHash keyrole c) where
  toExpr (KeyHash x) = App "KeyHash" [toExpr x]
