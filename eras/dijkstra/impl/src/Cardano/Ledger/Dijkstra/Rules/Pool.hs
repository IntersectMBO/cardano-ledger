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
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, POOL)
import Cardano.Ledger.Rules.ValidationMode (checkFailOnJust, checkFailOnJustStatic)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State
import Control.DeepSeq (NFData)
import Control.Monad (forM_, guard)
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
import qualified Data.Set as Set
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
  | BlsKeyAlreadyRegistered
      -- | Stake Pool ID
      (KeyHash StakePool)
      -- | BLS key attempted to use, whose public key has already been registered
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
    BlsKeyAlreadyRegistered a b ->
      encodeListLen 3 <> encCBOR (8 :: Word8) <> encCBOR a <> encCBOR b

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
      8 -> do
        poolID <- decCBOR
        blsKey <- decCBOR
        pure (3, BlsKeyAlreadyRegistered poolID blsKey)
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
-- Invariant of `psBlsKeyHashes`: it holds the hash of every BLS public key that
-- a registered stake pool uses through its active or its future parameters. No
-- two pools may use the same BLS public key, so it only needs to tell whether a
-- key is in use. Only the public key is hashed, so the proof of possession does
-- not matter for the uniqueness, and a BLS key that has aged out stays in the
-- state and still counts, as in `IsBLSUnique` of the Agda specification.
--
-- The Dijkstra POOLREAP rule keeps these invariants at the epoch boundary: a
-- pool whose future VRF or BLS key hash differs from its active one releases
-- the active one once the future parameters are adopted, and a retired pool
-- releases the key hashes it uses.
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
      , ps@PState {psStakePools, psFutureStakePoolParams, psVRFKeyHashes, psBlsKeyHashes}
      , poolCert
      ) <-
    judgmentContext
  case poolCert of
    RegPool stakePoolParams -> do
      let StakePoolParams {sppId, sppBlsKey = newBlsKey, sppVrf, sppAccountAddress, sppMetadata} =
            stakePoolParams
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

      -- Returns the BlsKey wrapped in 'SJust' if it is invalid. A valid 'BlsKey' returns 'SNothing'.
      let invalidBlsKey = do
            SJust blsKey@BlsKey {blsPubKey, blsPossessionProof} <- pure newBlsKey
            Left _ <-
              pure $
                verifyPossessionProofDSIGN
                  minSigPoPDST
                  blsPubKey
                  blsPossessionProof
            pure (sppId, blsKey)
      checkFailOnJustStatic invalidBlsKey $ uncurry BlsKeyInvalidProofOfPossession

      let newBlsKeyHash = hashBlsKey <$> newBlsKey
          -- Returns the BLS key if another pool uses its public key, which is the case when
          -- its hash is in use, but is none of the given hashes of the pool's own BLS keys. A
          -- registration without a BLS key never clashes.
          blsKeyUsedByAnotherPool ownBlsKeyHashes = strictMaybeToMaybe $ do
            blsKey <- newBlsKey
            blsKeyHash <- newBlsKeyHash
            guard $
              Set.member blsKeyHash psBlsKeyHashes && newBlsKeyHash `notElem` ownBlsKeyHashes
            pure blsKey
          trackNewBlsKeyHash = strictMaybe id Set.insert newBlsKeyHash

      case Map.lookup sppId psStakePools of
        -- register new, Pool-Reg
        Nothing -> do
          Map.notMember sppVrf psVRFKeyHashes
            ?! injectFailure (VRFKeyHashAlreadyRegistered sppId sppVrf)
          checkFailOnJust (blsKeyUsedByAnotherPool []) $ BlsKeyAlreadyRegistered sppId
          tellEvent $ injectEvent $ Shelley.RegisterPool sppId
          pure $
            ps
              & psStakePoolsL
                %~ Map.insert
                  sppId
                  (mkStakePoolState cEpoch SNothing (pp ^. ppPoolDepositCompactL) mempty stakePoolParams)
              & psVRFKeyHashesL %~ addVRFKeyHashOccurrence sppVrf
              & psBlsKeyHashesL %~ trackNewBlsKeyHash
        -- re-register Pool
        Just stakePoolState -> do
          let activeVrf = stakePoolState ^. spsVrfL
              mbFutureParams = Map.lookup sppId psFutureStakePoolParams
              mbFutureVrf = (^. sppVrfL) <$> mbFutureParams
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
              activeBlsKeyHash = hashBlsKey . bksKey <$> stakePoolState ^. spsBlsKeyL
              newFutureBlsKeyHash =
                hashBlsKey <$> (sppBlsKey =<< maybeToStrictMaybe mbFutureParams)
              -- The re-registration replaces the future parameters, which releases their BLS key,
              -- unless the active parameters use it as well. If the re-registration uses that
              -- same key, `trackNewBlsKeyHash` records it again.
              releaseFutureBlsKeyHash = case newFutureBlsKeyHash of
                SJust curFutureBlsKeyHash
                  | newFutureBlsKeyHash /= activeBlsKeyHash -> Set.delete curFutureBlsKeyHash
                _ -> id
          checkFailOnJust (blsKeyUsedByAnotherPool [activeBlsKeyHash, newFutureBlsKeyHash]) $
            BlsKeyAlreadyRegistered sppId
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
              & psBlsKeyHashesL %~ trackNewBlsKeyHash . releaseFutureBlsKeyHash
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
