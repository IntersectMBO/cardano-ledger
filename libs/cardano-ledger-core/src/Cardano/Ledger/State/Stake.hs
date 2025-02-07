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
)
where

import Cardano.Ledger.Binary (
  DecShareCBOR (..),
  EncCBOR (..),
  Interns,
 )
import Cardano.Ledger.CertState (DState (..), PState (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.State.SnapShots
import Cardano.Ledger.State.UTxO
import qualified Cardano.Ledger.UMap as UM
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Default (Default)
import Data.Kind (Type)
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

  -- | Add new UTxO to the `InstantStake`. This is invoked for every new TxOut that is added to the
  -- ledger state
  addInstantStake :: UTxO era -> InstantStake era -> InstantStake era

  -- | Delete spent UTxO from the `InstantStake`. This is invoked for every TxOut that is removed
  -- from the ledger state
  deleteInstantStake :: UTxO era -> InstantStake era -> InstantStake era

  -- TODO: This functionality will be removed and switched to use a pulser

  -- | Using known stake credential registrations and delegations resolve the instant stake into a
  -- `SnapShot`.
  resolveInstantStake :: InstantStake era -> UM.UMap -> Stake

snapShotFromInstantStake :: EraStake era => InstantStake era -> DState era -> PState era -> SnapShot
snapShotFromInstantStake iStake dState PState {psStakePoolParams} =
  SnapShot stake delegs (VMap.fromMap psStakePoolParams)
  where
    stake = resolveInstantStake iStake um
    delegs = UM.unUnifyToVMap (UM.SPoolUView um)
    um = dsUnified dState

class CanGetInstantStake t where
  instantStakeG :: SimpleGetter (t era) (InstantStake era)
  default instantStakeG :: CanSetInstantStake t => SimpleGetter (t era) (InstantStake era)
  instantStakeG = instantStakeL
  {-# INLINE instantStakeG #-}

class CanGetInstantStake t => CanSetInstantStake t where
  instantStakeL :: Lens' (t era) (InstantStake era)
