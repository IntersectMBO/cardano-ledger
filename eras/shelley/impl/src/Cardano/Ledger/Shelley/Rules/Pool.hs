{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Pool (
  ShelleyPOOL,
  PoolEvent (..),
  PoolEnv (..),
  PredicateFailure,
  ShelleyPoolPredFailure (..),
) where

import Cardano.Crypto.Hash.Class (sizeHash)
import Cardano.Ledger.Address (raNetwork)
import Cardano.Ledger.BaseTypes (
  EpochNo,
  Globals (..),
  Mismatch (..),
  Network,
  Relation (..),
  ShelleyBase,
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
import Cardano.Ledger.Binary.Coders (Encode (..), encode, (!>))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (
  ShelleyEra,
  ShelleyPOOL,
  hardforkAlonzoValidatePoolRewardAccountNetID,
  hardforkConwayDisallowDuplicatedVRFKeys,
 )
import qualified Cardano.Ledger.Shelley.SoftForks as SoftForks
import Cardano.Ledger.State
import Control.DeepSeq
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
import Data.Word (Word8)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

data PoolEnv era
  = PoolEnv EpochNo (PParams era)
  deriving (Generic)

instance EraPParams era => EncCBOR (PoolEnv era) where
  encCBOR (PoolEnv e pp) =
    encode $
      Rec PoolEnv
        !> To e
        !> To pp

deriving instance Show (PParams era) => Show (PoolEnv era)

deriving instance Eq (PParams era) => Eq (PoolEnv era)

instance NFData (PParams era) => NFData (PoolEnv era)

data ShelleyPoolPredFailure era
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
  deriving (Eq, Show, Generic)

type instance EraRuleFailure "POOL" ShelleyEra = ShelleyPoolPredFailure ShelleyEra

instance InjectRuleFailure "POOL" ShelleyPoolPredFailure ShelleyEra

type instance EraRuleEvent "POOL" ShelleyEra = PoolEvent ShelleyEra

instance InjectRuleEvent "POOL" PoolEvent ShelleyEra

instance NoThunks (ShelleyPoolPredFailure era)

instance NFData (ShelleyPoolPredFailure era)

instance
  ( EraPParams era
  , EraRule "POOL" era ~ ShelleyPOOL era
  , InjectRuleFailure "POOL" ShelleyPoolPredFailure era
  , InjectRuleEvent "POOL" PoolEvent era
  ) =>
  STS (ShelleyPOOL era)
  where
  type State (ShelleyPOOL era) = PState era

  type Signal (ShelleyPOOL era) = PoolCert

  type Environment (ShelleyPOOL era) = PoolEnv era

  type BaseM (ShelleyPOOL era) = ShelleyBase
  type PredicateFailure (ShelleyPOOL era) = ShelleyPoolPredFailure era
  type Event (ShelleyPOOL era) = PoolEvent era

  transitionRules = [poolTransition]

data PoolEvent era
  = RegisterPool (KeyHash StakePool)
  | ReregisterPool (KeyHash StakePool)
  deriving (Generic, Eq)

instance NFData (PoolEvent era)

instance Era era => EncCBOR (ShelleyPoolPredFailure era) where
  encCBOR = \case
    StakePoolNotRegisteredOnKeyPOOL kh ->
      encodeListLen 2 <> encCBOR (0 :: Word8) <> encCBOR kh
    StakePoolRetirementWrongEpochPOOL (Mismatch _ gtExpected) (Mismatch ltSupplied ltExpected) ->
      encodeListLen 4
        <> encCBOR (1 :: Word8)
        <> encCBOR gtExpected
        <> encCBOR ltSupplied
        <> encCBOR ltExpected
    StakePoolCostTooLowPOOL (Mismatch supplied expected) ->
      encodeListLen 3 <> encCBOR (3 :: Word8) <> encCBOR supplied <> encCBOR expected
    WrongNetworkPOOL (Mismatch supplied expected) c ->
      encodeListLen 4 <> encCBOR (4 :: Word8) <> encCBOR expected <> encCBOR supplied <> encCBOR c
    PoolMedataHashTooBig a b ->
      encodeListLen 3 <> encCBOR (5 :: Word8) <> encCBOR a <> encCBOR b
    VRFKeyHashAlreadyRegistered a b ->
      encodeListLen 3 <> encCBOR (6 :: Word8) <> encCBOR a <> encCBOR b

-- `ShelleyPoolPredFailure` is used in Conway POOL rule, so we need to keep the serialization unchanged
instance Era era => DecCBOR (ShelleyPoolPredFailure era) where
  decCBOR = decodeRecordSum "PredicateFailure (POOL era)" $
    \case
      0 -> do
        kh <- decCBOR
        pure (2, StakePoolNotRegisteredOnKeyPOOL kh)
      1 -> do
        gtExpected <- decCBOR
        ltSupplied <- decCBOR
        ltExpected <- decCBOR
        pure
          ( 4
          , StakePoolRetirementWrongEpochPOOL
              (Mismatch ltSupplied gtExpected)
              (Mismatch ltSupplied ltExpected)
          )
      2 -> fail "WrongCertificateTypePOOL has been removed as impossible case"
      3 -> do
        supplied <- decCBOR
        expected <- decCBOR
        pure (3, StakePoolCostTooLowPOOL (Mismatch supplied expected))
      4 -> do
        expectedNetId <- decCBOR
        suppliedNetId <- decCBOR
        poolId <- decCBOR
        pure (4, WrongNetworkPOOL (Mismatch suppliedNetId expectedNetId) poolId)
      5 -> do
        poolID <- decCBOR
        s <- decCBOR
        pure (3, PoolMedataHashTooBig poolID s)
      6 -> do
        poolID <- decCBOR
        vrfKeyHash <- decCBOR
        pure (3, VRFKeyHashAlreadyRegistered poolID vrfKeyHash)
      k -> invalidKey k

poolTransition ::
  forall rule era.
  ( EraPParams era
  , Signal (EraRule rule era) ~ PoolCert
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
    RegPool stakePoolParams@StakePoolParams {sppId, sppVrf, sppRewardAccount, sppMetadata, sppCost} -> do
      let pv = pp ^. ppProtocolVersionL
      when (hardforkAlonzoValidatePoolRewardAccountNetID pv) $ do
        actualNetID <- liftSTS $ asks networkId
        let suppliedNetID = raNetwork sppRewardAccount
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
                <= fromIntegral (sizeHash ([] @HASH))
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
