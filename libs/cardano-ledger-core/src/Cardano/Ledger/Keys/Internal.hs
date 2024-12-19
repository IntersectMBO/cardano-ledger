{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Ledger.Keys.Internal (
  -- * DSIGN
  DSIGN,
  DSignable,
  VKey (..),
  signedDSIGN,
  verifySignedDSIGN,

  -- * Key roles
  KeyRole (..),
  HasKeyRole (..),
  asWitness,

  -- * Re-exports from cardano-crypto-class
  decodeSignedDSIGN,
  encodeSignedDSIGN,
)
where

import Cardano.Crypto.DSIGN hiding (
  decodeSignedDSIGN,
  encodeSignedDSIGN,
  signedDSIGN,
  verifySignedDSIGN,
 )
import qualified Cardano.Crypto.DSIGN as DSIGN
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
 )
import Cardano.Ledger.Binary.Crypto
import Cardano.Ledger.Orphans ()
import Control.DeepSeq (NFData)
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Quiet

-- | Cryptographic saigning algorithm used on Cardano blockchain.
type DSIGN = DSIGN.Ed25519DSIGN

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
