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
  ProtVer,
  ShelleyBase,
  addEpochInterval,
  knownNonZeroBounded,
  natVersion,
  networkId,
  pvMajor,
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
import Data.Primitive.ByteArray (sizeofByteArray)
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
                | hardforkConwayDisallowDuplicatedVRFKeys pv = Map.insert sppVrf (knownNonZeroBounded @1)
                | otherwise = id
          tellEvent $ injectEvent $ RegisterPool sppId
          pure $
            ps
              & psStakePoolsL
                %~ Map.insert sppId (mkStakePoolState (pp ^. ppPoolDepositCompactL) mempty stakePoolParams)
              & psVRFKeyHashesL %~ updateVRFKeyHash
        -- re-register Pool
        Just stakePoolState -> do
          when (hardforkConwayDisallowDuplicatedVRFKeys pv) $ do
            sppVrf == stakePoolState ^. spsVrfL
              || Map.notMember sppVrf psVRFKeyHashes
                ?! injectFailure (VRFKeyHashAlreadyRegistered sppId sppVrf)
          let updateFutureVRFKeyHash
                | hardforkConwayDisallowDuplicatedVRFKeys pv =
                    -- If a pool re-registers with a fresh VRF, we have to record it in the map,
                    -- but also remove the previous VRFHashKey potentially stored in previous re-registration within the same epoch,
                    -- which we retrieve from futureStakePools.
                    case Map.lookup sppId psFutureStakePoolParams of
                      Nothing -> Map.insert sppVrf (knownNonZeroBounded @1)
                      Just futureStakePoolParams
                        | futureStakePoolParams ^. sppVrfL /= sppVrf ->
                            Map.insert sppVrf (knownNonZeroBounded @1)
                              . Map.delete (futureStakePoolParams ^. sppVrfL)
                        | otherwise -> id
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
