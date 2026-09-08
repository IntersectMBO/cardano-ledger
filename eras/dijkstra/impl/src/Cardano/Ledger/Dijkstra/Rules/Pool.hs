{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Pool (
  POOL,
  poolTransition,
) where

import Cardano.Crypto.Hash.Class (hashSize)
import Cardano.Ledger.BaseTypes (
  Globals (..),
  Mismatch (..),
  NonZero,
  ProtVer,
  ShelleyBase,
  addEpochInterval,
  knownNonZeroBounded,
  mapNonZero,
  natVersion,
  networkId,
  pvMajor,
  unNonZero,
 )
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, POOL)
import Cardano.Ledger.Shelley.Rules (
  PoolEnv (..),
  PoolEvent (..),
  ShelleyPoolPredFailure (..),
 )
import qualified Cardano.Ledger.Shelley.SoftForks as SoftForks
import Cardano.Ledger.State
import Control.Monad (forM_, when)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  tellEvent,
  (?!),
 )
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Primitive.ByteArray (sizeofByteArray)
import Data.Word (Word64)
import Lens.Micro

-- Private copies of the protocol-version gates from the hidden
-- `Cardano.Ledger.Shelley.Era` module, kept so that this rule remains a verbatim
-- copy of the Shelley POOL rule. Both are always `True` in this era.
hardforkAlonzoValidatePoolAccountAddressNetID :: ProtVer -> Bool
hardforkAlonzoValidatePoolAccountAddressNetID pv = pvMajor pv > natVersion @4

hardforkConwayDisallowDuplicatedVRFKeys :: ProtVer -> Bool
hardforkConwayDisallowDuplicatedVRFKeys pv = pvMajor pv > natVersion @10

type instance EraRuleFailure "POOL" DijkstraEra = ShelleyPoolPredFailure DijkstraEra

type instance EraRuleEvent "POOL" DijkstraEra = PoolEvent DijkstraEra

instance InjectRuleFailure "POOL" ShelleyPoolPredFailure DijkstraEra

instance InjectRuleEvent "POOL" PoolEvent DijkstraEra

instance
  ( EraPParams era
  , EraRule "POOL" era ~ POOL era
  , InjectRuleFailure "POOL" ShelleyPoolPredFailure era
  , InjectRuleEvent "POOL" PoolEvent era
  ) =>
  STS (POOL era)
  where
  type State (POOL era) = PState era

  type Signal (POOL era) = PoolCert era

  type Environment (POOL era) = PoolEnv era

  type BaseM (POOL era) = ShelleyBase
  type PredicateFailure (POOL era) = ShelleyPoolPredFailure era
  type Event (POOL era) = PoolEvent era

  transitionRules = [poolTransition]

-- Invariant of `psVRFKeyHashes`: a VRF key hash maps to the number of
-- references held by registered stake pools, where a pool holds one reference
-- through its active parameters (`psStakePools`) and one more through its
-- future parameters (`psFutureStakePoolParams`) whenever the future VRF key
-- hash differs from the active one. A future VRF key hash that coincides with
-- the pool's active one is not counted separately, which is exactly the
-- accounting that POOLREAP maintains at the epoch boundary when future
-- parameters are adopted.

addVRFKeyHashOccurrence ::
  VRFVerKeyHash StakePoolVRF ->
  Map.Map (VRFVerKeyHash StakePoolVRF) (NonZero Word64) ->
  Map.Map (VRFVerKeyHash StakePoolVRF) (NonZero Word64)
addVRFKeyHashOccurrence vrfKeyHash =
  Map.insertWith combine vrfKeyHash (knownNonZeroBounded @1)
  where
    -- Saturates at maxBound: if (+1) would overflow to 0, keep the existing value
    combine _ oldVal = fromMaybe oldVal $ mapNonZero (+ 1) oldVal

-- | Removes the key from the map if the count drops to 0
removeVRFKeyHashOccurrence ::
  VRFVerKeyHash StakePoolVRF ->
  Map.Map (VRFVerKeyHash StakePoolVRF) (NonZero Word64) ->
  Map.Map (VRFVerKeyHash StakePoolVRF) (NonZero Word64)
removeVRFKeyHashOccurrence = Map.update (mapNonZero (subtract 1))

poolTransition ::
  forall rule era.
  ( EraPParams era
  , Signal (EraRule rule era) ~ PoolCert era
  , Environment (EraRule rule era) ~ PoolEnv era
  , State (EraRule rule era) ~ PState era
  , STS (EraRule rule era)
  , BaseM (EraRule rule era) ~ ShelleyBase
  , InjectRuleFailure rule ShelleyPoolPredFailure era
  , InjectRuleEvent rule PoolEvent era
  ) =>
  TransitionRule (EraRule rule era)
poolTransition = do
  TRC
    ( PoolEnv cEpoch pp
      , ps@PState {psStakePools, psFutureStakePoolParams, psVRFKeyHashes}
      , poolCert
      ) <-
    judgmentContext
  case poolCert of
    RegPool stakePoolParams@StakePoolParams {sppId, sppVrf, sppAccountAddress, sppMetadata, sppCost} -> do
      let pv = pp ^. ppProtocolVersionL
      when (hardforkAlonzoValidatePoolAccountAddressNetID pv) $ do
        actualNetID <- liftSTS $ asks networkId
        let suppliedNetID = aaNetworkId sppAccountAddress
        actualNetID
          == suppliedNetID
            ?! injectFailure
              ( WrongNetworkPOOL
                  Mismatch
                    { mismatchSupplied = suppliedNetID
                    , mismatchExpected = actualNetID
                    }
                  sppId
              )

      when (SoftForks.restrictPoolMetadataHash pv) $
        forM_ sppMetadata $ \pmd ->
          let s = sizeofByteArray $ pmHash pmd
           in s
                <= fromIntegral (hashSize ([] @HASH))
                  ?! injectFailure (PoolMedataHashTooBig sppId s)

      let minPoolCost = pp ^. ppMinPoolCostL
      sppCost
        >= minPoolCost
          ?! injectFailure
            ( StakePoolCostTooLowPOOL
                Mismatch
                  { mismatchSupplied = sppCost
                  , mismatchExpected = minPoolCost
                  }
            )
      case Map.lookup sppId psStakePools of
        -- register new, Pool-Reg
        Nothing -> do
          when (hardforkConwayDisallowDuplicatedVRFKeys pv) $ do
            Map.notMember sppVrf psVRFKeyHashes
              ?! injectFailure (VRFKeyHashAlreadyRegistered sppId sppVrf)
          let updateVRFKeyHash
                | hardforkConwayDisallowDuplicatedVRFKeys pv = addVRFKeyHashOccurrence sppVrf
                | otherwise = id
          tellEvent $ injectEvent $ RegisterPool sppId
          pure $
            ps
              & psStakePoolsL
                %~ Map.insert sppId (mkStakePoolState (pp ^. ppPoolDepositCompactL) mempty stakePoolParams)
              & psVRFKeyHashesL %~ updateVRFKeyHash
        -- re-register Pool
        Just stakePoolState -> do
          let activeVrf = stakePoolState ^. spsVrfL
              mbFutureVrf = (^. sppVrfL) <$> Map.lookup sppId psFutureStakePoolParams
          when (hardforkConwayDisallowDuplicatedVRFKeys pv) $ do
            -- A pool may only re-register with a VRF key hash to which every
            -- recorded reference is its own, held through its active and/or
            -- its future parameters.
            let ownOccurrences :: Word64
                ownOccurrences =
                  (if sppVrf == activeVrf then 1 else 0)
                    + (if mbFutureVrf == Just sppVrf && sppVrf /= activeVrf then 1 else 0)
            maybe 0 unNonZero (Map.lookup sppVrf psVRFKeyHashes)
              == ownOccurrences
                ?! injectFailure (VRFKeyHashAlreadyRegistered sppId sppVrf)
          let updateFutureVRFKeyHash
                | hardforkConwayDisallowDuplicatedVRFKeys pv
                , mbFutureVrf /= Just sppVrf =
                    -- The reference held by the future parameters moves from
                    -- `mbFutureVrf` to `sppVrf`. References that coincide with
                    -- the active VRF key hash are not counted separately, per
                    -- the invariant on `psVRFKeyHashes`.
                    let removeOldOccurrence = case mbFutureVrf of
                          Just oldVrf
                            | oldVrf /= activeVrf -> removeVRFKeyHashOccurrence oldVrf
                          _ -> id
                        addNewOccurrence
                          | sppVrf /= activeVrf = addVRFKeyHashOccurrence sppVrf
                          | otherwise = id
                     in addNewOccurrence . removeOldOccurrence
                | otherwise = id
          tellEvent $ injectEvent $ ReregisterPool sppId
          -- This `sppId` is already registered, so we want to reregister it.
          -- That means adding it to the futureStakePoolParams or overriding it  with the new 'poolParams'.
          -- We must also unretire it, if it has been scheduled for retirement.
          -- The deposit does not change.
          pure $
            ps
              & psFutureStakePoolParamsL
                %~ Map.insert sppId stakePoolParams
              & psRetiringL %~ Map.delete sppId
              & psVRFKeyHashesL %~ updateFutureVRFKeyHash
    RetirePool sppId e -> do
      Map.member sppId psStakePools ?! injectFailure (StakePoolNotRegisteredOnKeyPOOL sppId)
      let maxEpoch = pp ^. ppEMaxL
          limitEpoch = addEpochInterval cEpoch maxEpoch
      (cEpoch < e && e <= limitEpoch)
        ?! injectFailure
          ( StakePoolRetirementWrongEpochPOOL
              Mismatch -- RelGT - The supplied value should be greater than the current epoch
                { mismatchSupplied = e
                , mismatchExpected = cEpoch
                }
              Mismatch -- RelLTEQ - The supplied value should be less then or equal to ppEMax after the current epoch
                { mismatchSupplied = e
                , mismatchExpected = limitEpoch
                }
          )
      -- We just schedule it for retirement. When it is retired we refund the deposit (see POOLREAP)
      pure $ ps & psRetiringL %~ Map.insert sppId e
