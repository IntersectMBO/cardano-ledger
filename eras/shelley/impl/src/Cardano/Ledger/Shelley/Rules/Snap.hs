{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Snap
  ( ShelleySNAP,
    PredicateFailure,
    ShelleySnapPredFailure,
    SnapEvent (..),
  )
where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin, CompactForm)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core (EraTxOut)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Era (EraCrypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.Shelley.EpochBoundary
  ( SnapShot (_delegations, _stake),
    SnapShots (_feeSS, _pstakeGo, _pstakeMark, _pstakeSet),
    Stake (unStake),
    emptySnapShots,
  )
import Cardano.Ledger.Shelley.Era (ShelleySNAP)
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    LedgerState (..),
    UTxOState (..),
    incrementalStakeDistr,
  )
import Control.State.Transition
  ( STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    tellEvent,
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.VMap as VMap
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Control.DeepSeq (NFData (..))

-- ======================================================

data ShelleySnapPredFailure era -- No predicate failures
  deriving (Show, Generic, Eq)

instance NoThunks (ShelleySnapPredFailure era)

deriving instance NFData (ShelleySnapPredFailure era)

newtype SnapEvent era
  = StakeDistEvent (Map (Credential 'Staking (EraCrypto era)) (Coin, KeyHash 'StakePool (EraCrypto era)))

instance EraTxOut era => STS (ShelleySNAP era) where
  type State (ShelleySNAP era) = SnapShots (EraCrypto era)
  type Signal (ShelleySNAP era) = ()
  type Environment (ShelleySNAP era) = LedgerState era
  type BaseM (ShelleySNAP era) = ShelleyBase
  type PredicateFailure (ShelleySNAP era) = ShelleySnapPredFailure era
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
  forall era.
  TransitionRule (ShelleySNAP era)
snapTransition = do
  TRC (lstate, s, _) <- judgmentContext

  let LedgerState (UTxOState _utxo _ fees _ incStake) (DPState dstate pstate) = lstate
      -- stakeSnap = stakeDistr @era utxo dstate pstate  -- HISTORICAL NOTE
      istakeSnap = incrementalStakeDistr @(EraCrypto era) incStake dstate pstate

  tellEvent $
    let stMap :: Map (Credential 'Staking (EraCrypto era)) (CompactForm Coin)
        stMap = VMap.toMap . unStake $ _stake istakeSnap

        stakeCoinMap :: Map (Credential 'Staking (EraCrypto era)) Coin
        stakeCoinMap = fmap fromCompact stMap

        stakePoolMap :: Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))
        stakePoolMap = VMap.toMap $ _delegations istakeSnap

        stakeMap :: Map (Credential 'Staking (EraCrypto era)) (Coin, KeyHash 'StakePool (EraCrypto era))
        stakeMap = Map.intersectionWith (,) stakeCoinMap stakePoolMap
     in StakeDistEvent stakeMap

  pure $
    s
      { _pstakeMark = istakeSnap,
        _pstakeSet = _pstakeMark s,
        _pstakeGo = _pstakeSet s,
        _feeSS = fees
      }
