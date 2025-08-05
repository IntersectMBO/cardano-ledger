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
 )
import Cardano.Ledger.Shelley.LedgerState (PState (..), payPoolDeposit)
import qualified Cardano.Ledger.Shelley.SoftForks as SoftForks
import Cardano.Ledger.State (PoolMetadata (..), PoolParams (..), mkStakePoolState)
import Control.DeepSeq
import Control.Monad (forM_, when)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, setSingleton, singleton, (∈), (∉), (⋪), (⨃))
import Control.State.Transition (
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  tellEvent,
  (?!),
 )
import qualified Data.ByteString as BS
import Data.Kind (Type)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
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
      (KeyHash 'StakePool) -- KeyHash which cannot be retired since it is not registered
  | StakePoolRetirementWrongEpochPOOL
      (Mismatch 'RelGT EpochNo)
      (Mismatch 'RelLTEQ EpochNo)
  | StakePoolCostTooLowPOOL
      (Mismatch 'RelGTEQ Coin)
  | WrongNetworkPOOL
      (Mismatch 'RelEQ Network)
      (KeyHash 'StakePool) -- Stake Pool ID
  | PoolMedataHashTooBig
      (KeyHash 'StakePool) -- Stake Pool ID
      Int -- Size of the metadata hash
  deriving (Eq, Show, Generic)

type instance EraRuleFailure "POOL" ShelleyEra = ShelleyPoolPredFailure ShelleyEra

instance InjectRuleFailure "POOL" ShelleyPoolPredFailure ShelleyEra

instance NoThunks (ShelleyPoolPredFailure era)

instance NFData (ShelleyPoolPredFailure era)

instance (ShelleyEraTxCert era, EraPParams era) => STS (ShelleyPOOL era) where
  type State (ShelleyPOOL era) = PState era

  type Signal (ShelleyPOOL era) = PoolCert

  type Environment (ShelleyPOOL era) = PoolEnv era

  type BaseM (ShelleyPOOL era) = ShelleyBase
  type PredicateFailure (ShelleyPOOL era) = ShelleyPoolPredFailure era
  type Event (ShelleyPOOL era) = PoolEvent era

  transitionRules = [poolDelegationTransition]

data PoolEvent era
  = RegisterPool (KeyHash 'StakePool)
  | ReregisterPool (KeyHash 'StakePool)
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
      k -> invalidKey k

poolDelegationTransition ::
  forall (ledger :: Type -> Type) era.
  ( EraPParams era
  , Signal (ledger era) ~ PoolCert
  , Environment (ledger era) ~ PoolEnv era
  , State (ledger era) ~ PState era
  , STS (ledger era)
  , Event (ledger era) ~ PoolEvent era
  , BaseM (ledger era) ~ ShelleyBase
  , PredicateFailure (ledger era) ~ ShelleyPoolPredFailure era
  ) =>
  TransitionRule (ledger era)
poolDelegationTransition = do
  TRC
    ( PoolEnv cEpoch pp
      , ps@PState {psStakePoolState, psFutureStakePoolState, psRetiring}
      , poolCert
      ) <-
    judgmentContext
  case poolCert of
    RegPool poolParams@PoolParams {ppId, ppRewardAccount, ppMetadata, ppCost} -> do
      let pv = pp ^. ppProtocolVersionL
      when (hardforkAlonzoValidatePoolRewardAccountNetID pv) $ do
        actualNetID <- liftSTS $ asks networkId
        let suppliedNetID = raNetwork ppRewardAccount
        actualNetID
          == suppliedNetID
            ?! WrongNetworkPOOL
              Mismatch
                { mismatchSupplied = suppliedNetID
                , mismatchExpected = actualNetID
                }
              ppId

      when (SoftForks.restrictPoolMetadataHash pv) $
        forM_ ppMetadata $ \pmd ->
          let s = BS.length (pmHash pmd)
           in s
                <= fromIntegral (sizeHash ([] @HASH))
                  ?! PoolMedataHashTooBig ppId s

      let minPoolCost = pp ^. ppMinPoolCostL
      ppCost
        >= minPoolCost
          ?! StakePoolCostTooLowPOOL
            Mismatch
              { mismatchSupplied = ppCost
              , mismatchExpected = minPoolCost
              }

      if eval (ppId ∉ dom psStakePoolState)
        then do
          -- register new, Pool-Reg
          tellEvent $ RegisterPool ppId
          pure $
            payPoolDeposit ppId pp $
              ps {psStakePoolState = eval (psStakePoolState ⨃ singleton ppId (mkStakePoolState poolParams))}
        else do
          tellEvent $ ReregisterPool ppId
          -- hk is already registered, so we want to reregister it. That means adding it
          -- to the Future pool params (if it is not there already), and overriding the
          -- range with the new 'poolParam', if it is (using ⨃ ). We must also unretire
          -- it, if it has been scheduled for retirement.  The deposit does not
          -- change. One pays the deposit just once. Only if it is fully retired
          -- (i.e. it's deposit has been refunded, and it has been removed from the
          -- registered pools).  does it need to pay a new deposit (at the current deposit
          -- amount). But of course, if that has happened, we cannot be in this branch of
          -- the if statement.
          pure $
            ps
              { psFutureStakePoolState =
                  eval (psFutureStakePoolState ⨃ singleton ppId (mkStakePoolState poolParams))
              , psRetiring = eval (setSingleton ppId ⋪ psRetiring)
              }
    RetirePool hk e -> do
      eval (hk ∈ dom psStakePoolState) ?! StakePoolNotRegisteredOnKeyPOOL hk
      let maxEpoch = pp ^. ppEMaxL
          limitEpoch = addEpochInterval cEpoch maxEpoch
      (cEpoch < e && e <= limitEpoch)
        ?! StakePoolRetirementWrongEpochPOOL
          Mismatch -- 'RelGT - The supplied value should be greater than the current epoch
            { mismatchSupplied = e
            , mismatchExpected = cEpoch
            }
          Mismatch -- 'RelLTEQ - The supplied value should be less then or equal to ppEMax after the current epoch
            { mismatchSupplied = e
            , mismatchExpected = limitEpoch
            }
      -- We just schedule it for retirement. When it is retired we refund the deposit (see POOLREAP)
      pure $ ps {psRetiring = eval (psRetiring ⨃ singleton hk e)}
