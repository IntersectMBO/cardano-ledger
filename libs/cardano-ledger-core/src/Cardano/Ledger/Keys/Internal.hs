{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Keys.Internal (
  -- * DSIGN
  DSIGN,
  DSignable,
  VKey (..),
  ADDRHASH,
  KeyHash (..),
  hashKey,
  signedDSIGN,
  verifySignedDSIGN,

  -- * Key roles
  KeyRole (..),
  HasKeyRole (..),
  asWitness,
) where

import Cardano.Crypto.DSIGN hiding (
  decodeSignedDSIGN,
  encodeSignedDSIGN,
  signedDSIGN,
  verifySignedDSIGN,
 )
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR,
  ToCBOR,
 )
import Cardano.Ledger.Orphans ()
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Coerce (Coercible, coerce)
import Data.Default (Default)
import Data.Kind (Type)
import Data.MemPack (FailT (..), MemPack (..), StateT (..), Unpack (..))
import Data.Ord (comparing)
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Quiet

-- | Cryptographic signing algorithm used on Cardano blockchain.
type DSIGN = DSIGN.Ed25519DSIGN

-- | The role of a key.
--
-- All key roles are __fixed__ and unique, except for the `Witness` role. In particular,
-- keys can be cast to a `Witness` role with the help of `asWitness`, because same witness
-- can be valid for many roles.
--
-- In fact, it is perfectly allowable for a key to be used in many roles by the end user;
-- there is nothing prohibiting somebody using the same underlying key or a script as
-- their payment and staking credential, as well as the key for their stake pool. However,
-- in the ledger code mixing up keys with different roles could be catastrophic, that is
-- why we have this separation.
type data KeyRole
  = GenesisRole
  | GenesisDelegate
  | Payment
  | Staking
  | StakePool
  | BlockIssuer
  | Witness
  | DRepRole
  | HotCommitteeRole
  | ColdCommitteeRole
  | Guard

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
  a Witness
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

instance Ord (VKey kd) where
  -- `VerKeyDSIGN` disallows an `Ord` instance and specifies that hashes should be compared instead.
  -- We need an `Ord` instance for `VKey`, so it can be stored inside other types that need an `Ord`
  -- instance, so we define it using the `VerKeyDSIGN` hashes as requested.
  compare = comparing hashKey

deriving via Quiet (VKey kd) instance Show (VKey kd)

instance HasKeyRole VKey

-- | Hashing algorithm used for hashing cryptographic keys and scripts (aka addresses).
type ADDRHASH = Hash.Blake2b_224

-- | Discriminated hash of public Key
newtype KeyHash (r :: KeyRole) = KeyHash
  {unKeyHash :: Hash.Hash ADDRHASH (VerKeyDSIGN DSIGN)}
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
    , MemPack
    , Storable
    )

instance HasKeyRole KeyHash

-- | Hash a given public key
hashKey :: VKey kd -> KeyHash kd
hashKey (VKey vk) = KeyHash $ DSIGN.hashVerKeyDSIGN vk

-- | Produce a digital signature
signedDSIGN ::
  Signable DSIGN a =>
  SignKeyDSIGN DSIGN ->
  a ->
  SignedDSIGN DSIGN a
signedDSIGN key a = DSIGN.signedDSIGN () a key

-- | Verify a digital signature
verifySignedDSIGN ::
  Signable DSIGN a =>
  VKey kd ->
  a ->
  SignedDSIGN DSIGN a ->
  Bool
verifySignedDSIGN (VKey vk) vd sigDSIGN =
  either (const False) (const True) $ DSIGN.verifySignedDSIGN () vk vd sigDSIGN
{-# INLINE verifySignedDSIGN #-}
