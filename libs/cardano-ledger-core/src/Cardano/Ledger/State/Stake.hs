{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- TODO: submit a ghc bug report
-- some GHC bug wrongfully complains about CanGetInstantStake constraint being redundant.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.State.Stake (
  EraStake (..),
  CanGetInstantStake (..),
  CanSetInstantStake (..),
  snapShotFromInstantStake,
  resolveActiveInstantStakeCredentials,
)
where

import Cardano.Ledger.Binary (
  DecShareCBOR (..),
  EncCBOR (..),
  Interns,
 )
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.State.CertState (DState (..), PState (..))
import Cardano.Ledger.State.SnapShots
import Cardano.Ledger.State.UTxO
import qualified Cardano.Ledger.UMap as UM
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Aeson (ToJSON)
import Data.Default (Default)
import Data.Functor.Identity
import Data.Kind (Type)
import qualified Data.Map.Merge.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.VMap as VMap
import Lens.Micro
import NoThunks.Class (NoThunks)

class
  ( Era era
  , Eq (InstantStake era)
  , Show (InstantStake era)
  , Monoid (InstantStake era)
  , Default (InstantStake era)
  , NFData (InstantStake era)
  , NoThunks (InstantStake era)
  , ToJSON (InstantStake era)
  , EncCBOR (InstantStake era)
  , DecShareCBOR (InstantStake era)
  , Share (InstantStake era) ~ Interns (Credential 'Staking)
  ) =>
  EraStake era
  where
  -- | This is the current stake in the system. The important part of this stake is that not all of
  -- it is active. Any stake credential that is not registred will not contribute to the active
  -- stake, however it will be part of the instant stake. Throughout an epoch it is not relevant
  -- which part of the stake is active, because it is only when we take the snaphot that we resolve
  -- all the active stake.
  type InstantStake era = (r :: Type) | r -> era

  instantStakeCredentialsL :: Lens' (InstantStake era) (Map (Credential 'Staking) (CompactForm Coin))

  -- | Add new UTxO to the `InstantStake`. This is invoked for every new TxOut that is added to the
  -- ledger state
  addInstantStake :: UTxO era -> InstantStake era -> InstantStake era

  -- | Delete spent UTxO from the `InstantStake`. This is invoked for every TxOut that is removed
  -- from the ledger state
  deleteInstantStake :: UTxO era -> InstantStake era -> InstantStake era

  -- TODO: This functionality will be removed and switched to use a pulser

  -- | Using known stake credential registrations and delegations resolve the instant stake into a
  -- `Stake` that will be used for `SnapShot` creation by `snapShotFromInstantStake`.
  resolveInstantStake :: InstantStake era -> UM.UMap -> Stake

snapShotFromInstantStake :: EraStake era => InstantStake era -> DState era -> PState era -> SnapShot
snapShotFromInstantStake iStake dState PState {psStakePoolParams} =
  SnapShot stake delegs (VMap.fromMap psStakePoolParams)
  where
    !stake = resolveInstantStake iStake um
    !delegs = UM.unUnifyToVMap (UM.SPoolUView um)
    !um = dsUnified dState
{-# INLINEABLE snapShotFromInstantStake #-}

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
  InstantStake era -> UM.UMap -> Map (Credential 'Staking) (CompactForm Coin)
resolveActiveInstantStakeCredentials instantStake (UM.UMap triplesMap _) =
  Map.merge
    Map.dropMissing -- ignore non-registered stake credentials
    (Map.mapMaybeMissing (const getNonZeroActiveReward)) -- use the reward amount, unless it is zero
    (Map.zipWithMaybeAMatched addInstantActiveStake) -- combine the stake with the reward amount
    (instantStake ^. instantStakeCredentialsL)
    triplesMap
  where
    -- Retain any non-zero reward
    getActiveReward umElem = do
      rd <- UM.umElemRDActive umElem
      pure $! UM.rdReward rd
    {-# INLINE getActiveReward #-}
    getNonZeroActiveReward umElem = do
      reward <- getActiveReward umElem
      reward <$ guard (reward > mempty)
    {-# INLINE getNonZeroActiveReward #-}
    -- Adds instant stake to any active staking credential
    addInstantActiveStake _ stake umElem = Identity $ do
      reward <- getActiveReward umElem
      -- instant stake is guaranteed to be non-zero due to minUTxO, so no need to guard against mempty
      pure $! stake <> reward
    {-# INLINE addInstantActiveStake #-}
{-# INLINEABLE resolveActiveInstantStakeCredentials #-}
