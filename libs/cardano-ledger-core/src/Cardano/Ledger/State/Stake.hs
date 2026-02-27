{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- TODO: submit a ghc bug report
-- some GHC bug wrongfully complains about CanGetInstantStake constraint being redundant.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.State.Stake (
  Stake (..),
  sumAllStake,
  sumAllStakeCompact,
  sumCredentialsCompactStake,
  StakeWithDelegation (..),
  ActiveStake (..),
  sumAllActiveStake,
  sumCredentialsCompactActiveStake,
  EraStake (..),
  CanGetInstantStake (..),
  CanSetInstantStake (..),
  resolveActiveInstantStakeCredentials,
) where

import Cardano.Ledger.BaseTypes (
  NonZero (..),
  nonZero,
  nonZeroOr,
  unsafeNonZero,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  Interns,
  decodeListLen,
  encodeListLen,
 )
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.State.Account
import Cardano.Ledger.State.UTxO
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Default (Default)
import Data.Foldable (foldMap')
import Data.Functor.Identity
import Data.Kind (Type)
import qualified Data.Map.Merge.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.VMap (VB, VMap, VP)
import qualified Data.VMap as VMap
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

-- | Type of stake as map from staking credential to coins associated. Any staking credential that
-- has no stake will not appear in this Map, even if it is registered. For this reason, this data
-- type should not be used for infering whether credential is registered or not.
newtype Stake = Stake
  { unStake :: VMap VB VP (Credential Staking) (CompactForm Coin)
  }
  deriving (Show, Eq, NFData, Generic, ToJSON, NoThunks, EncCBOR)

instance Monoid Stake where
  mempty = Stake VMap.empty

instance Semigroup Stake where
  Stake s1 <> Stake s2 = Stake $ VMap.unionWith (<>) s1 s2

instance DecShareCBOR Stake where
  type Share Stake = Share (VMap VB VP (Credential Staking) (CompactForm Coin))
  getShare = getShare . unStake
  decShareCBOR = fmap Stake . decShareCBOR

sumAllStake :: Stake -> Coin
sumAllStake = fromCompact . sumAllStakeCompact
{-# INLINE sumAllStake #-}

sumAllStakeCompact :: Stake -> CompactForm Coin
sumAllStakeCompact = VMap.foldl (<>) mempty . unStake
{-# INLINE sumAllStakeCompact #-}

sumCredentialsCompactStake :: Foldable f => Stake -> f (Credential Staking) -> CompactForm Coin
sumCredentialsCompactStake (Stake stake) = foldMap' (fromMaybe mempty . (`VMap.lookup` stake))
{-# INLINE sumCredentialsCompactStake #-}

-- | Combination of non-zero stake with the pool delegation for a single credential.
data StakeWithDelegation = StakeWithDelegation
  { swdStake :: {-# UNPACK #-} !(NonZero (CompactForm Coin))
  , swdDelegation :: !(KeyHash StakePool)
  }
  deriving (Eq, Show, Generic)

instance NoThunks StakeWithDelegation

instance NFData StakeWithDelegation

instance ToJSON StakeWithDelegation

instance EncCBOR StakeWithDelegation where
  encCBOR (StakeWithDelegation s d) = encodeListLen 2 <> encCBOR s <> encCBOR d

instance DecCBOR StakeWithDelegation where
  decCBOR =
    decodeRecordNamed "SnapShot" (const 2) $
      StakeWithDelegation <$> decCBOR <*> decCBOR

instance DecShareCBOR StakeWithDelegation where
  type Share StakeWithDelegation = Interns (Credential Staking)
  decShareCBOR _si = decCBOR

-- | Active stake: maps staking credentials to their non-zero stake paired with delegation.
-- Only credentials that are registered, delegated, and have non-zero stake appear here.
newtype ActiveStake = ActiveStake
  { unActiveStake :: VMap VB VB (Credential Staking) StakeWithDelegation
  }
  deriving (Show, Eq, NFData, Generic, ToJSON, NoThunks, EncCBOR)

instance DecShareCBOR ActiveStake where
  type Share ActiveStake = Interns (Credential Staking)
  getShare (ActiveStake m) = fst (getShare m)
  decShareCBOR si = ActiveStake <$> decShareCBOR (si, mempty)

-- | Sum all active stake. Returns @NonZero Coin@, defaulting to 1 lovelace if empty.
sumAllActiveStake :: ActiveStake -> NonZero Coin
sumAllActiveStake (ActiveStake m) =
  VMap.foldMap (fromCompact . unNonZero . swdStake) m `nonZeroOr` knownNonZeroCoin @1
{-# INLINE sumAllActiveStake #-}

-- | Sum the compact stake for a set of credentials from an @ActiveStake@.
sumCredentialsCompactActiveStake ::
  Foldable f => ActiveStake -> f (Credential Staking) -> CompactForm Coin
sumCredentialsCompactActiveStake (ActiveStake m) =
  foldMap' (\cred -> maybe mempty (unNonZero . swdStake) (VMap.lookup cred m))
{-# INLINE sumCredentialsCompactActiveStake #-}

class
  ( EraAccounts era
  , Eq (InstantStake era)
  , Show (InstantStake era)
  , Monoid (InstantStake era)
  , Default (InstantStake era)
  , NFData (InstantStake era)
  , NoThunks (InstantStake era)
  , ToJSON (InstantStake era)
  , EncCBOR (InstantStake era)
  , DecShareCBOR (InstantStake era)
  , Share (InstantStake era) ~ Interns (Credential Staking)
  ) =>
  EraStake era
  where
  -- | This is the current stake in the system. The important part of this stake is that not all of
  -- it is active. Any stake credential that is not registred will not contribute to the active
  -- stake, however it will be part of the instant stake. Throughout an epoch it is not relevant
  -- which part of the stake is active, because it is only when we take the snaphot that we resolve
  -- all the active stake.
  type InstantStake era = (r :: Type) | r -> era

  instantStakeCredentialsL :: Lens' (InstantStake era) (Map (Credential Staking) (CompactForm Coin))

  -- | Add new UTxO to the `InstantStake`. This is invoked for every new TxOut that is added to the
  -- ledger state
  addInstantStake :: UTxO era -> InstantStake era -> InstantStake era

  -- | Delete spent UTxO from the `InstantStake`. This is invoked for every TxOut that is removed
  -- from the ledger state
  deleteInstantStake :: UTxO era -> InstantStake era -> InstantStake era

  -- TODO: This functionality will be removed and switched to use a pulser

  -- | Using known stake credential registrations and delegations resolve the instant stake into
  -- `ActiveStake` that will be used for `SnapShot` creation by
  -- `Cardano.Ledger.State.snapShotFromInstantStake`.
  resolveInstantStake :: InstantStake era -> Accounts era -> ActiveStake

class CanGetInstantStake t where
  instantStakeG :: SimpleGetter (t era) (InstantStake era)
  default instantStakeG :: CanSetInstantStake t => SimpleGetter (t era) (InstantStake era)
  instantStakeG = instantStakeL
  {-# INLINE instantStakeG #-}

class CanGetInstantStake t => CanSetInstantStake t where
  instantStakeL :: Lens' (t era) (InstantStake era)

-- | This is the total active stake including the rewards, but ignoring all the stake coming from
-- the pointers. Where "active" stake means any stake credential that is registered and delegated to
-- a stake pool.
resolveActiveInstantStakeCredentials ::
  EraStake era =>
  InstantStake era ->
  Accounts era ->
  Map (Credential Staking) StakeWithDelegation
resolveActiveInstantStakeCredentials instantStake accounts =
  Map.merge
    Map.dropMissing -- ignore non-registered stake credentials
    (Map.mapMaybeMissing (const getNonZeroActiveStakeWithDelegation)) -- use the account balance, unless it is zero
    (Map.zipWithMaybeAMatched addInstantActiveStakeWithDelegation) -- combine the stake with the account balance
    (instantStake ^. instantStakeCredentialsL)
    (accounts ^. accountsMapL)
  where
    -- Only return non-zero balance bundled with delegation for active accounts.
    getNonZeroActiveStakeWithDelegation accountState = do
      poolId <- accountState ^. stakePoolDelegationAccountStateL
      nzBalance <- nonZero $ accountState ^. balanceAccountStateL
      pure $! StakeWithDelegation nzBalance poolId
    {-# INLINE getNonZeroActiveStakeWithDelegation #-}
    -- Adds instant stake to any active staking credential, bundling with delegation
    addInstantActiveStakeWithDelegation _ stake accountState = Identity $ do
      poolId <- accountState ^. stakePoolDelegationAccountStateL
      let balance = accountState ^. balanceAccountStateL
      -- instant stake is guaranteed to be non-zero due to minUTxO, so no need to guard against mempty
      pure $! StakeWithDelegation (unsafeNonZero $ stake <> balance) poolId
    {-# INLINE addInstantActiveStakeWithDelegation #-}
{-# INLINEABLE resolveActiveInstantStakeCredentials #-}
