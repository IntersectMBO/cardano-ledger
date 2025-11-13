{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
) where

import Cardano.Ledger.Binary (
  DecShareCBOR (..),
  EncCBOR (..),
  Interns,
 )
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.State.Account
import Cardano.Ledger.State.CertState (DState (..), PState (..))
import Cardano.Ledger.State.SnapShots
import Cardano.Ledger.State.StakePool (stakePoolStateToStakePoolParams)
import Cardano.Ledger.State.UTxO
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Aeson (ToJSON)
import Data.Default (Default)
import Data.Functor.Identity
import Data.Kind (Type)
import qualified Data.Map.Merge.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.VMap as VMap
import Lens.Micro
import NoThunks.Class (NoThunks)

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

  -- | Using known stake credential registrations and delegations resolve the instant stake into a
  -- `Stake` that will be used for `SnapShot` creation by `snapShotFromInstantStake`.
  resolveInstantStake :: InstantStake era -> Accounts era -> Stake

snapShotFromInstantStake ::
  forall era. EraStake era => InstantStake era -> DState era -> PState era -> SnapShot
snapShotFromInstantStake iStake dState PState {psStakePools} =
  SnapShot
    { ssStake = resolveInstantStake iStake accounts
    , ssDelegations = VMap.fromDistinctAscListN delegsCount delegsAscList
    , ssPoolParams =
        VMap.fromDistinctAscListN
          (Map.size psStakePools)
          [(poolId, stakePoolStateToStakePoolParams poolId ps) | (poolId, ps) <- Map.toAscList psStakePools]
    }
  where
    accounts = dsAccounts dState
    keepAndCountDelegations ::
      Credential Staking ->
      AccountState era ->
      ([(Credential Staking, KeyHash StakePool)], Int) ->
      ([(Credential Staking, KeyHash StakePool)], Int)
    keepAndCountDelegations cred accountState acc@(!delegs, !count) =
      case accountState ^. stakePoolDelegationAccountStateL of
        Nothing -> acc
        Just deleg -> ((cred, deleg) : delegs, count + 1)
    (delegsAscList, delegsCount) =
      Map.foldrWithKey keepAndCountDelegations ([], 0) $ accounts ^. accountsMapL
{-# INLINE snapShotFromInstantStake #-}

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
  Map (Credential Staking) (CompactForm Coin)
resolveActiveInstantStakeCredentials instantStake accounts =
  Map.merge
    Map.dropMissing -- ignore non-registered stake credentials
    (Map.mapMaybeMissing (const getNonZeroActiveBalance)) -- use the account balance, unless it is zero
    (Map.zipWithMaybeAMatched addInstantActiveStake) -- combine the stake with the account balance
    (instantStake ^. instantStakeCredentialsL)
    (accounts ^. accountsMapL)
  where
    -- Only return balance for accounts that have an active delegation to a stake pool.
    getActiveBalance accountState = do
      _ <- accountState ^. stakePoolDelegationAccountStateL
      pure $! accountState ^. balanceAccountStateL
    {-# INLINE getActiveBalance #-}
    -- Retain any non-zero balance
    getNonZeroActiveBalance accountState = do
      balance <- getActiveBalance accountState
      balance <$ guard (balance > mempty)
    {-# INLINE getNonZeroActiveBalance #-}
    -- Adds instant stake to any active staking credential
    addInstantActiveStake _ stake accountState = Identity $ do
      balance <- getActiveBalance accountState
      -- instant stake is guaranteed to be non-zero due to minUTxO, so no need to guard against mempty
      pure $! stake <> balance
    {-# INLINE addInstantActiveStake #-}
{-# INLINEABLE resolveActiveInstantStakeCredentials #-}
