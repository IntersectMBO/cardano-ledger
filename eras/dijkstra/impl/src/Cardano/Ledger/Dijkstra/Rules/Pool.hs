{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Pool (
  POOL,
  DijkstraPoolPredFailure (..),
  poolTransition,
) where

import Cardano.Crypto.DSIGN (verifyPossessionProofDSIGN)
import Cardano.Crypto.DSIGN.BLS12381.Internal (minSigPoPDST)
import Cardano.Crypto.Hash.Class (hashSize)
import Cardano.Ledger.BaseTypes (
  EpochNo,
  Globals (..),
  Mismatch (..),
  Network,
  Relation (RelEQ, RelGT, RelGTEQ, RelLTEQ),
  ShelleyBase,
  StrictMaybe (SJust),
  addEpochInterval,
  invalidKey,
  knownNonZeroBounded,
  networkId,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, POOL)
import Cardano.Ledger.Rules.ValidationMode (checkFailOnJustStatic)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State
import Control.DeepSeq (NFData)
import Control.Monad (forM_)
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
import Data.Word (Word8)
import GHC.Generics (Generic)
import Lens.Micro

data DijkstraPoolPredFailure era
  = StakePoolNotRegisteredOnKeyPOOL
      -- | KeyHash which cannot be retired since it is not registered
      (KeyHash StakePool)
  | StakePoolRetirementWrongEpochPOOL
      (Mismatch RelGT EpochNo)
      (Mismatch RelLTEQ EpochNo)
  | StakePoolCostTooLowPOOL
      (Mismatch RelGTEQ Coin)
  | WrongNetworkPOOL
      (Mismatch RelEQ Network)
      -- | Stake Pool ID
      (KeyHash StakePool)
  | PoolMedataHashTooBig
      -- | Stake Pool ID
      (KeyHash StakePool)
      -- | Size of the metadata hash
      Int
  | VRFKeyHashAlreadyRegistered
      -- | Stake Pool ID
      (KeyHash StakePool)
      -- | VRF key attempted to use, that has already been registered
      (VRFVerKeyHash StakePoolVRF)
  | BlsKeyInvalidProofOfPossession
      -- | Stake Pool ID
      (KeyHash StakePool)
      -- | BLS key set
      BlsKey
  deriving (Eq, Ord, Show, Generic, NFData)

instance Era era => EncCBOR (DijkstraPoolPredFailure era) where
  encCBOR = \case
    StakePoolNotRegisteredOnKeyPOOL kh ->
      encodeListLen 2 <> encCBOR (0 :: Word8) <> encCBOR kh
    StakePoolRetirementWrongEpochPOOL gt lt ->
      encodeListLen 3
        <> encCBOR (1 :: Word8)
        <> encCBOR gt
        <> encCBOR lt
    StakePoolCostTooLowPOOL m ->
      encodeListLen 2 <> encCBOR (3 :: Word8) <> encCBOR m
    WrongNetworkPOOL m c ->
      encodeListLen 3 <> encCBOR (4 :: Word8) <> encCBOR m <> encCBOR c
    PoolMedataHashTooBig a b ->
      encodeListLen 3 <> encCBOR (5 :: Word8) <> encCBOR a <> encCBOR b
    VRFKeyHashAlreadyRegistered a b ->
      encodeListLen 3 <> encCBOR (6 :: Word8) <> encCBOR a <> encCBOR b
    BlsKeyInvalidProofOfPossession a b ->
      encodeListLen 3 <> encCBOR (7 :: Word8) <> encCBOR a <> encCBOR b

instance Era era => DecCBOR (DijkstraPoolPredFailure era) where
  decCBOR = decodeRecordSum "PredicateFailure (POOL era)" $
    \case
      0 -> do
        kh <- decCBOR
        pure (2, StakePoolNotRegisteredOnKeyPOOL kh)
      1 -> do
        gt <- decCBOR
        lt <- decCBOR
        pure (3, StakePoolRetirementWrongEpochPOOL gt lt)
      3 -> do
        costMismatch <- decCBOR
        pure (2, StakePoolCostTooLowPOOL costMismatch)
      4 -> do
        networkIdMismatch <- decCBOR
        poolId <- decCBOR
        pure (3, WrongNetworkPOOL networkIdMismatch poolId)
      5 -> do
        poolID <- decCBOR
        s <- decCBOR
        pure (3, PoolMedataHashTooBig poolID s)
      6 -> do
        poolID <- decCBOR
        vrfKeyHash <- decCBOR
        pure (3, VRFKeyHashAlreadyRegistered poolID vrfKeyHash)
      7 -> do
        poolID <- decCBOR
        blsKey <- decCBOR
        pure (3, BlsKeyInvalidProofOfPossession poolID blsKey)
      k -> invalidKey k

type instance EraRuleFailure "POOL" DijkstraEra = DijkstraPoolPredFailure DijkstraEra

type instance EraRuleEvent "POOL" DijkstraEra = Shelley.PoolEvent DijkstraEra

instance InjectRuleFailure "POOL" DijkstraPoolPredFailure DijkstraEra

instance InjectRuleEvent "POOL" Shelley.PoolEvent DijkstraEra

instance InjectRuleFailure "POOL" Shelley.ShelleyPoolPredFailure DijkstraEra where
  injectFailure = \case
    Shelley.StakePoolNotRegisteredOnKeyPOOL poolId -> StakePoolNotRegisteredOnKeyPOOL poolId
    Shelley.StakePoolRetirementWrongEpochPOOL mismatchGTEpochNo mismatchLTEQEpochNo -> StakePoolRetirementWrongEpochPOOL mismatchGTEpochNo mismatchLTEQEpochNo
    Shelley.StakePoolCostTooLowPOOL mismatchCost -> StakePoolCostTooLowPOOL mismatchCost
    Shelley.WrongNetworkPOOL mismatchNetwork poolId -> WrongNetworkPOOL mismatchNetwork poolId
    Shelley.PoolMedataHashTooBig poolId metadataHashSize -> PoolMedataHashTooBig poolId metadataHashSize
    Shelley.VRFKeyHashAlreadyRegistered poolId poolVrfKeyHash -> VRFKeyHashAlreadyRegistered poolId poolVrfKeyHash

instance
  ( EraPParams era
  , EraRule "POOL" era ~ POOL era
  , InjectRuleFailure "POOL" Shelley.ShelleyPoolPredFailure era
  , InjectRuleFailure "POOL" DijkstraPoolPredFailure era
  , InjectRuleEvent "POOL" Shelley.PoolEvent era
  ) =>
  STS (POOL era)
  where
  type State (POOL era) = PState era

  type Signal (POOL era) = PoolCert era

  type Environment (POOL era) = Shelley.PoolEnv era

  type BaseM (POOL era) = ShelleyBase
  type PredicateFailure (POOL era) = DijkstraPoolPredFailure era
  type Event (POOL era) = Shelley.PoolEvent era

  transitionRules = [poolTransition]

-- Invariant of `psVRFKeyHashes`: a VRF key hash maps to the number of
-- references held by registered stake pools, where a pool holds one reference
-- through its active parameters (`psStakePools`) and one more through its
-- future parameters (`psFutureStakePoolParams`) whenever the future VRF key
-- hash differs from the active one. A future VRF key hash that coincides with
-- the pool's active one is not counted separately.
--
-- POOLREAP follows the same accounting at the epoch boundary when it adopts
-- future parameters and retires pools, except that it drops a superseded
-- active VRF key hash entirely instead of decrementing its count. The two only
-- differ for VRF key hashes that several pools have shared since before their
-- uniqueness was enforced.
poolTransition ::
  forall rule era.
  ( EraPParams era
  , Signal (EraRule rule era) ~ PoolCert era
  , Environment (EraRule rule era) ~ Shelley.PoolEnv era
  , State (EraRule rule era) ~ PState era
  , STS (EraRule rule era)
  , BaseM (EraRule rule era) ~ ShelleyBase
  , InjectRuleFailure rule DijkstraPoolPredFailure era
  , InjectRuleEvent rule Shelley.PoolEvent era
  ) =>
  TransitionRule (EraRule rule era)
poolTransition = do
  TRC
    ( Shelley.PoolEnv cEpoch pp
      , ps@PState {psStakePools, psFutureStakePoolParams, psVRFKeyHashes}
      , poolCert
      ) <-
    judgmentContext
  case poolCert of
    RegPool stakePoolParams -> do
      let StakePoolParams {sppId, sppBlsKey, sppVrf, sppAccountAddress, sppMetadata, sppCost} = stakePoolParams
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

      -- Returns the BlsKey wrapped in 'SJust' if it is invalid. A valid 'BlsKey' returns 'SNothing'.
      let invalidBlsKey = do
            SJust blsKey@BlsKey {blsPubKey, blsPossessionProof} <- pure sppBlsKey
            Left _ <-
              pure $
                verifyPossessionProofDSIGN
                  minSigPoPDST
                  blsPubKey
                  blsPossessionProof
            pure (sppId, blsKey)
      checkFailOnJustStatic invalidBlsKey $ uncurry BlsKeyInvalidProofOfPossession

      case Map.lookup sppId psStakePools of
        -- register new, Pool-Reg
        Nothing -> do
          Map.notMember sppVrf psVRFKeyHashes
            ?! injectFailure (VRFKeyHashAlreadyRegistered sppId sppVrf)
          tellEvent $ injectEvent $ Shelley.RegisterPool sppId
          pure $
            ps
              & psStakePoolsL
                %~ Map.insert sppId (mkStakePoolState cEpoch (pp ^. ppPoolDepositCompactL) mempty stakePoolParams)
              & psVRFKeyHashesL %~ addVRFKeyHashOccurrence sppVrf
        -- re-register Pool
        Just stakePoolState -> do
          let activeVrf = stakePoolState ^. spsVrfL
              mbFutureVrf = (^. sppVrfL) <$> Map.lookup sppId psFutureStakePoolParams
              -- The only reference to this VRF key hash, if any, must be the
              -- pool's own, held through its active or its future parameters.
              expectedOccurrences
                | sppVrf == activeVrf || mbFutureVrf == Just sppVrf = Just (knownNonZeroBounded @1)
                | otherwise = Nothing
          Map.lookup sppVrf psVRFKeyHashes
            == expectedOccurrences
              ?! injectFailure (VRFKeyHashAlreadyRegistered sppId sppVrf)
          let updateFutureVRFKeyHash
                | mbFutureVrf /= Just sppVrf =
                    -- The reference held by the future parameters moves from
                    -- `mbFutureVrf` to `sppVrf`. References that coincide with
                    -- the active VRF key hash are not counted separately, per
                    -- the invariant on `psVRFKeyHashes`.
                    let removeOldOccurrence = case mbFutureVrf of
                          Just oldFutureVrf
                            | oldFutureVrf /= activeVrf -> removeVRFKeyHashOccurrence oldFutureVrf
                          _ -> id
                        addNewOccurrence
                          | sppVrf /= activeVrf = addVRFKeyHashOccurrence sppVrf
                          | otherwise = id
                     in addNewOccurrence . removeOldOccurrence
                | otherwise = id
          tellEvent $ injectEvent $ Shelley.ReregisterPool sppId
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
