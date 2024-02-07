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
)
where

import Cardano.Crypto.Hash.Class (sizeHash)
import Cardano.Ledger.Address (raNetwork)
import Cardano.Ledger.BaseTypes (
  Globals (..),
  Network,
  ShelleyBase,
  addEpochInterval,
  epochInfoPure,
  invalidKey,
  networkId,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
 )
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Crypto as CC (Crypto (HASH))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.PoolParams (PoolMetadata (..), PoolParams (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (ShelleyEra, ShelleyPOOL)
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.LedgerState (PState (..), payPoolDeposit)
import qualified Cardano.Ledger.Shelley.SoftForks as SoftForks
import Cardano.Ledger.Slot (EpochNo (..), SlotNo, epochInfoEpoch)
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
  = PoolEnv !SlotNo !(PParams era)
  deriving (Generic)

deriving instance Show (PParams era) => Show (PoolEnv era)

deriving instance Eq (PParams era) => Eq (PoolEnv era)

data ShelleyPoolPredFailure era
  = StakePoolNotRegisteredOnKeyPOOL
      !(KeyHash 'StakePool (EraCrypto era)) -- KeyHash which cannot be retired since it is not registered
  | StakePoolRetirementWrongEpochPOOL
      !EpochNo -- Current Epoch
      !EpochNo -- The epoch listed in the Pool Retirement Certificate
      !EpochNo -- The first epoch that is too far out for retirement
  | StakePoolCostTooLowPOOL
      !Coin -- The stake pool cost listed in the Pool Registration Certificate
      !Coin -- The minimum stake pool cost listed in the protocol parameters
  | WrongNetworkPOOL
      !Network -- Actual Network ID
      !Network -- Network ID listed in Pool Registration Certificate
      !(KeyHash 'StakePool (EraCrypto era)) -- Stake Pool ID
  | PoolMedataHashTooBig
      !(KeyHash 'StakePool (EraCrypto era)) -- Stake Pool ID
      !Int -- Size of the metadata hash
  deriving (Eq, Show, Generic)

type instance EraRuleFailure "POOL" (ShelleyEra c) = ShelleyPoolPredFailure (ShelleyEra c)

instance InjectRuleFailure "POOL" ShelleyPoolPredFailure (ShelleyEra c) where
  injectFailure = id

instance NoThunks (ShelleyPoolPredFailure era)

instance NFData (ShelleyPoolPredFailure era)

instance (ShelleyEraTxCert era, EraPParams era) => STS (ShelleyPOOL era) where
  type State (ShelleyPOOL era) = PState era

  type Signal (ShelleyPOOL era) = PoolCert (EraCrypto era)

  type Environment (ShelleyPOOL era) = PoolEnv era

  type BaseM (ShelleyPOOL era) = ShelleyBase
  type PredicateFailure (ShelleyPOOL era) = ShelleyPoolPredFailure era
  type Event (ShelleyPOOL era) = PoolEvent era

  transitionRules = [poolDelegationTransition]

data PoolEvent era
  = RegisterPool (KeyHash 'StakePool (EraCrypto era))
  | ReregisterPool (KeyHash 'StakePool (EraCrypto era))

instance Era era => EncCBOR (ShelleyPoolPredFailure era) where
  encCBOR = \case
    StakePoolNotRegisteredOnKeyPOOL kh ->
      encodeListLen 2 <> encCBOR (0 :: Word8) <> encCBOR kh
    StakePoolRetirementWrongEpochPOOL ce e em ->
      encodeListLen 4 <> encCBOR (1 :: Word8) <> encCBOR ce <> encCBOR e <> encCBOR em
    StakePoolCostTooLowPOOL pc mc ->
      encodeListLen 3 <> encCBOR (3 :: Word8) <> encCBOR pc <> encCBOR mc
    WrongNetworkPOOL a b c ->
      encodeListLen 4 <> encCBOR (4 :: Word8) <> encCBOR a <> encCBOR b <> encCBOR c
    PoolMedataHashTooBig a b ->
      encodeListLen 3 <> encCBOR (5 :: Word8) <> encCBOR a <> encCBOR b

instance Era era => DecCBOR (ShelleyPoolPredFailure era) where
  decCBOR = decodeRecordSum "PredicateFailure (POOL era)" $
    \case
      0 -> do
        kh <- decCBOR
        pure (2, StakePoolNotRegisteredOnKeyPOOL kh)
      1 -> do
        ce <- decCBOR
        e <- decCBOR
        em <- decCBOR
        pure (4, StakePoolRetirementWrongEpochPOOL ce e em)
      2 -> fail "WrongCertificateTypePOOL has been removed as impossible case"
      3 -> do
        pc <- decCBOR
        mc <- decCBOR
        pure (3, StakePoolCostTooLowPOOL pc mc)
      4 -> do
        actualNetID <- decCBOR
        suppliedNetID <- decCBOR
        poolID <- decCBOR
        pure (4, WrongNetworkPOOL actualNetID suppliedNetID poolID)
      5 -> do
        poolID <- decCBOR
        s <- decCBOR
        pure (3, PoolMedataHashTooBig poolID s)
      k -> invalidKey k

poolDelegationTransition ::
  forall (ledger :: Type -> Type) era.
  ( EraPParams era
  , Signal (ledger era) ~ PoolCert (EraCrypto era)
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
    ( PoolEnv slot pp
      , ps@PState {psStakePoolParams, psFutureStakePoolParams, psRetiring}
      , poolCert
      ) <-
    judgmentContext
  case poolCert of
    RegPool poolParams@PoolParams {ppId, ppRewardAccount, ppMetadata, ppCost} -> do
      let pv = pp ^. ppProtocolVersionL
      when (HardForks.validatePoolRewardAccountNetID pv) $ do
        actualNetID <- liftSTS $ asks networkId
        let suppliedNetID = raNetwork ppRewardAccount
        actualNetID == suppliedNetID ?! WrongNetworkPOOL actualNetID suppliedNetID ppId

      when (SoftForks.restrictPoolMetadataHash pv) $
        forM_ ppMetadata $ \pmd ->
          let s = BS.length (pmHash pmd)
           in s
                <= fromIntegral (sizeHash ([] @(CC.HASH (EraCrypto era))))
                  ?! PoolMedataHashTooBig ppId s

      let minPoolCost = pp ^. ppMinPoolCostL
      ppCost >= minPoolCost ?! StakePoolCostTooLowPOOL ppCost minPoolCost

      if eval (ppId ∉ dom psStakePoolParams)
        then do
          -- register new, Pool-Reg
          tellEvent $ RegisterPool ppId
          pure $
            payPoolDeposit ppId pp $
              ps {psStakePoolParams = eval (psStakePoolParams ⨃ singleton ppId poolParams)}
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
              { psFutureStakePoolParams = eval (psFutureStakePoolParams ⨃ singleton ppId poolParams)
              , psRetiring = eval (setSingleton ppId ⋪ psRetiring)
              }
    RetirePool hk e -> do
      eval (hk ∈ dom psStakePoolParams) ?! StakePoolNotRegisteredOnKeyPOOL hk
      cepoch <- liftSTS $ do
        ei <- asks epochInfoPure
        epochInfoEpoch ei slot
      let maxEpoch = pp ^. ppEMaxL
      (cepoch < e && e <= addEpochInterval cepoch maxEpoch)
        ?! StakePoolRetirementWrongEpochPOOL cepoch e (addEpochInterval cepoch maxEpoch)
      -- We just schedule it for retirement. When it is retired we refund the deposit (see POOLREAP)
      pure $ ps {psRetiring = eval (psRetiring ⨃ singleton hk e)}
