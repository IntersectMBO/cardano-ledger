{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Snap (
  ShelleySNAP,
  PredicateFailure,
  SnapEvent (..),
  SnapEnv (..),
) where

import Cardano.Ledger.BaseTypes (ShelleyBase, unNonZero)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Shelley.Era (ShelleySNAP)
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState (..),
  UTxOState (..),
 )
import Cardano.Ledger.State
import Control.DeepSeq (NFData)
import Control.State.Transition (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  tellEvent,
 )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.VMap as VMap
import Data.Void (Void)
import GHC.Generics (Generic)
import Lens.Micro

-- ======================================================

newtype SnapEvent era
  = StakeDistEvent
      (Map (Credential Staking) (Coin, KeyHash StakePool))
  deriving (Generic)

deriving instance Eq (SnapEvent era)

instance NFData (SnapEvent era)

data SnapEnv era = SnapEnv (LedgerState era) (PParams era)

instance (EraTxOut era, EraStake era, EraCertState era) => STS (ShelleySNAP era) where
  type State (ShelleySNAP era) = SnapShots
  type Signal (ShelleySNAP era) = ()
  type Environment (ShelleySNAP era) = SnapEnv era
  type BaseM (ShelleySNAP era) = ShelleyBase
  type PredicateFailure (ShelleySNAP era) = Void
  type Event (ShelleySNAP era) = SnapEvent era
  initialRules = [pure emptySnapShots]
  transitionRules = [snapTransition]

-- | The stake distribution was previously computed as in the spec:
--
-- @
--  stakeDistr @era utxo dstate pstate
-- @
--
-- but is now computed incrementally. We leave the comment as a historical note about
-- where important changes were made to the source code.
snapTransition ::
  (EraStake era, EraCertState era) => TransitionRule (ShelleySNAP era)
snapTransition = do
  TRC (snapEnv, s, _) <- judgmentContext

  let SnapEnv ls@(LedgerState (UTxOState _utxo _ fees _ _ _) certState) _pp = snapEnv
      instantStake = ls ^. instantStakeG
      -- per the spec: stakeSnap = stakeDistr @era utxo dstate pstate
      istakeSnap =
        snapShotFromInstantStake instantStake (certState ^. certDStateL) (certState ^. certPStateL)

  tellEvent $
    let stakeMap :: Map (Credential Staking) (Coin, KeyHash StakePool)
        stakeMap =
          Map.map
            (\swd -> (fromCompact $ unNonZero $ swdStake swd, swdDelegation swd))
            (VMap.toMap $ unActiveStake $ ssActiveStake istakeSnap)
     in StakeDistEvent stakeMap

  pure $
    SnapShots
      { ssStakeMark = istakeSnap
      , ssStakeMarkPoolDistr = calculatePoolDistr istakeSnap
      , -- ssStakeMarkPoolDistr exists for performance reasons, see ADR-7
        ssStakeSet = ssStakeMark s
      , ssStakeGo = ssStakeSet s
      , ssFee = fees
      }
