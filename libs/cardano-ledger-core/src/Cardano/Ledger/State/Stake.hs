{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
  ActiveStake (..),
  sumAllActiveStake,
  StakeWithDelegation (..),
  sumCredentialsCompactStake,
  EraStake (..),
  CanGetInstantStake (..),
  CanSetInstantStake (..),
  resolveActiveInstantStakeCredentials,
) where

import Cardano.Ledger.BaseTypes (NonZero, nonZeroOr)
import Cardano.Ledger.Binary (
  DecShareCBOR (..),
  EncCBOR (..),
  Interns,
 )
import Cardano.Ledger.Binary.Coders (Encode (..), encode, (!>))
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.State.Account
import Cardano.Ledger.State.UTxO
import Control.DeepSeq (NFData (..), rwhnf)
import Control.Monad (guard)
import Data.Aeson (ToJSON (..), object, (.=))
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

data StakeWithDelegation = StakeWithDelegation
  { swdStake :: {-# UNPACK #-} !(CompactForm Coin)
  , swdDelegation :: !(KeyHash StakePool)
  }
  deriving (Eq, Show, Generic)

instance ToJSON StakeWithDelegation where
  toJSON swd@(StakeWithDelegation _ _) =
    let StakeWithDelegation {swdStake, swdDelegation} = swd
     in object
          [ "stake" .= swdStake
          , "delegation" .= swdDelegation
          ]

instance NoThunks StakeWithDelegation

instance NFData StakeWithDelegation where
  rnf = rwhnf

instance EncCBOR StakeWithDelegation where
  encCBOR
    StakeWithDelegation {swdStake, swdDelegation} =
      encode $
        Rec StakeWithDelegation
          !> To swdStake
          !> To swdDelegation

instance DecShareCBOR StakeWithDelegation where
  decShareCBOR = undefined

-- | Type of stake as map from staking credential to coins associated. Any staking credential that
-- has no stake will not appear in this Map, even if it is registered. For this reason, this data
-- type should not be used for infering whether credential is registered or not.
newtype ActiveStake = ActiveStake
  { unActiveStake :: VMap VB VB (Credential Staking) StakeWithDelegation
  }
  deriving (Show, Eq, NFData, Generic, ToJSON, NoThunks, EncCBOR)

instance DecShareCBOR ActiveStake where
  type Share ActiveStake = Share (VMap VB VP (Credential Staking) StakeWithDelegation)

  -- getShare = getShare . unActiveStake
  decShareCBOR = undefined -- fmap ActiveStake . decShareCBOR

sumAllActiveStake :: ActiveStake -> NonZero Coin
sumAllActiveStake as =
  fromCompact (VMap.foldMap swdStake (unActiveStake as)) `nonZeroOr` knownNonZeroCoin @1
{-# INLINE sumAllActiveStake #-}

sumAllStake :: Stake -> Coin
sumAllStake = fromCompact . sumAllStakeCompact
{-# INLINE sumAllStake #-}

sumAllStakeCompact :: Stake -> CompactForm Coin
sumAllStakeCompact = VMap.foldl (<>) mempty . unStake
{-# INLINE sumAllStakeCompact #-}

sumCredentialsCompactStake :: Foldable f => Stake -> f (Credential Staking) -> CompactForm Coin
sumCredentialsCompactStake (Stake stake) = foldMap' (fromMaybe mempty . (`VMap.lookup` stake))
{-# INLINE sumCredentialsCompactStake #-}

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
  -- `Stake` that will be used for `SnapShot` creation by
  -- `Cardano.Ledger.State.snapShotFromInstantStake`.
  resolveInstantStake :: InstantStake era -> Accounts era -> StakeWithDelegation

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
    (Map.mapMaybeMissing (const getNonZeroActiveBalance)) -- use the account balance, unless it is zero
    (Map.zipWithMaybeAMatched addInstantActiveStake) -- combine the stake with the account balance
    (instantStake ^. instantStakeCredentialsL)
    (accounts ^. accountsMapL)
  where
    -- Only return balance for accounts that have an active delegation to a stake pool.
    getActiveBalanceWithDelegation accountState = do
      delegation <- accountState ^. stakePoolDelegationAccountStateL
      pure $!
        StakeWithDelegation
          { swdStake = accountState ^. balanceAccountStateL
          , swdDelegation = delegation
          }
    {-# INLINE getActiveBalanceWithDelegation #-}
    -- Retain any non-zero balance
    getNonZeroActiveBalance accountState = do
      balanceWithDelegation <- getActiveBalanceWithDelegation accountState
      balanceWithDelegation <$ guard (swdStake balanceWithDelegation > mempty)
    {-# INLINE getNonZeroActiveBalance #-}
    -- Adds instant stake to any active staking credential
    addInstantActiveStake _ stake accountState = Identity $ do
      balanceWithDelegation <- getActiveBalanceWithDelegation accountState
      -- instant stake is guaranteed to be non-zero due to minUTxO, so no need to guard against mempty
      pure $!
        balanceWithDelegation
          { swdStake = swdStake balanceWithDelegation <> stake
          }
    {-# INLINE addInstantActiveStake #-}
{-# INLINEABLE resolveActiveInstantStakeCredentials #-}
