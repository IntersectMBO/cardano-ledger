{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Pool (
  ConwayPOOL,
  ConwayPoolPredFailure (..),
)
where

import Cardano.Crypto.Hash (sizeHash)
import Cardano.Ledger.BaseTypes (EpochNo, Network, ShelleyBase, epochInfoPure)
import Cardano.Ledger.CertState (PState (..), payPoolDeposit)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Era (ConwayPOOL)
import Cardano.Ledger.Core (
  Era (EraCrypto),
  EraPParams,
  EraRule,
  PoolCert (..),
  ppEMaxL,
  ppMinPoolCostL,
  ppProtocolVersionL,
 )
import Cardano.Ledger.Crypto (Crypto (HASH))
import Cardano.Ledger.Shelley.API (
  KeyHash,
  KeyRole (StakePool),
  PoolEnv (PoolEnv),
  PoolParams (PoolParams),
  getRwdNetwork,
  networkId,
  pmHash,
  ppCost,
  ppId,
  ppMetadata,
  ppRewardAcnt,
 )
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.Rules (PoolEvent (RegisterPool, ReregisterPool))
import qualified Cardano.Ledger.Shelley.SoftForks as SoftForks
import Cardano.Ledger.Slot (epochInfoEpoch)
import Control.DeepSeq (NFData)
import Control.Monad (forM_, when)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, (∈), (∉), (∪), (⋪), (⨃))
import Control.State.Transition (
  BaseM,
  Environment,
  Event,
  PredicateFailure,
  STS,
  Signal,
  State,
  transitionRules,
 )
import Control.State.Transition.Extended (TRC (TRC), TransitionRule, judgmentContext, liftSTS, tellEvent, (?!))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

data ConwayPoolPredFailure era
  = ConwayPoolPredFailure
  | WrongNetworkPOOL !Network !Network !(KeyHash 'StakePool (EraCrypto era))
  | PoolMedataHashTooBigPOOL !(KeyHash 'StakePool (EraCrypto era)) !Int
  | StakePoolCostTooLowPOOL !Coin !Coin
  | StakePoolNotRegisteredOnKeyPOOL !(KeyHash 'StakePool (EraCrypto era))
  | StakePoolRetirementWrongEpochPOOL EpochNo EpochNo EpochNo
  deriving (Show, Eq, Generic, NoThunks, NFData)

instance
  ( EraPParams era
  , State (EraRule "POOL" era) ~ PState era
  , Signal (EraRule "POOL" era) ~ PoolCert (EraCrypto era)
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , EraRule "POOL" era ~ ConwayPOOL era
  ) =>
  STS (ConwayPOOL era)
  where
  type State (ConwayPOOL era) = PState era
  type Signal (ConwayPOOL era) = PoolCert (EraCrypto era)
  type Environment (ConwayPOOL era) = PoolEnv era
  type BaseM (ConwayPOOL era) = ShelleyBase
  type PredicateFailure (ConwayPOOL era) = ConwayPoolPredFailure era
  type Event (ConwayPOOL era) = PoolEvent era

  transitionRules = [conwayPoolTransition @era]

conwayPoolTransition ::
  forall era.
  ( EraPParams era
  , EraRule "POOL" era ~ ConwayPOOL era
  ) =>
  TransitionRule (ConwayPOOL era)
conwayPoolTransition = do
  TRC
    ( PoolEnv slot pp
      , pState@PState {psStakePoolParams, psFutureStakePoolParams, psRetiring}
      , c
      ) <-
    judgmentContext
  let pv = pp ^. ppProtocolVersionL
  case c of
    RegPool poolParams@PoolParams {ppId, ppCost, ppRewardAcnt, ppMetadata} -> do
      when (HardForks.validatePoolRewardAccountNetID pv) $ do
        actualNetID <- liftSTS $ asks networkId
        let suppliedNetID = getRwdNetwork ppRewardAcnt
        actualNetID == suppliedNetID ?! WrongNetworkPOOL actualNetID suppliedNetID ppId
      when (SoftForks.restrictPoolMetadataHash pv) $ do
        forM_ ppMetadata $ \pMetadata ->
          let s = BS.length (pmHash pMetadata)
           in s <= fromIntegral (sizeHash ([] @(HASH (EraCrypto era)))) ?! PoolMedataHashTooBigPOOL ppId s
      let minPoolCost = pp ^. ppMinPoolCostL
      ppCost >= minPoolCost ?! StakePoolCostTooLowPOOL ppCost minPoolCost
      if eval (ppId ∉ dom psStakePoolParams)
        then do
          -- NOTE: @Soupstraw only this is part is in the spec right now.
          -- register new, Pool-Reg
          tellEvent $ RegisterPool ppId
          pure $
            payPoolDeposit ppId pp $
              pState
                { psStakePoolParams = eval $ psStakePoolParams ∪ Map.singleton ppId poolParams
                }
        else do
          tellEvent $ ReregisterPool ppId
          -- hk is already registered, so we want to reregister it. That means adding it to the
          -- Future pool params (if it is not there already), and overriding the range with the new 'pp',
          -- if it is (using ⨃ ). We must also unretire it, if it has been scheduled for retirement.
          -- The deposit does not change. One pays the deposit just once. Only if it is fully retired
          -- (i.e. it's deposit has been refunded, and it has been removed from the registered pools).
          -- does it need to pay a new deposit (at the current deposit amount). But of course,
          -- if that has happened, we cannot be in this branch of the if statement.
          pure $
            pState
              { psFutureStakePoolParams = eval $ psFutureStakePoolParams ⨃ Map.singleton ppId poolParams
              , psRetiring = eval $ Set.singleton ppId ⋪ psRetiring
              }
    RetirePool sPool epochN -> do
      eval (sPool ∈ dom psStakePoolParams) ?! StakePoolNotRegisteredOnKeyPOOL sPool
      cepoch <- liftSTS $ do
        ei <- asks epochInfoPure
        epochInfoEpoch ei slot
      let maxEpoch = pp ^. ppEMaxL
      (cepoch < epochN && epochN <= cepoch + maxEpoch) ?! StakePoolRetirementWrongEpochPOOL cepoch epochN (cepoch + maxEpoch)
      -- We just schedule it for retirement. When it is retired we refund the deposit (see POOLREAP)
      -- pure $ pState {psRetiring = eval $ psRetiring ⨃ Map.singleton sPool epochN}

      -- NOTE: @Soupstraw: only this line is in the spec now.
      pure $ pState {psRetiring = Map.singleton sPool epochN `Map.union` psRetiring}
