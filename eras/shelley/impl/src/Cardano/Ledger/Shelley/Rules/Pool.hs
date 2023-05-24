{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
import Cardano.Ledger.BaseTypes (
  Globals (..),
  Network,
  ShelleyBase,
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
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (ShelleyPOOL)
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.LedgerState (PState (..), payPoolDeposit)
import qualified Cardano.Ledger.Shelley.SoftForks as SoftForks
import Cardano.Ledger.Shelley.TxBody (
  PoolMetadata (..),
  PoolParams (..),
  getRwdNetwork,
 )
import Cardano.Ledger.Shelley.TxCert (
  isInstantaneousRewards,
  pattern DelegStakeTxCert,
  pattern GenesisDelegTxCert,
  pattern RegTxCert,
  pattern UnRegTxCert,
 )
import Cardano.Ledger.Slot (EpochNo (..), SlotNo, epochInfoEpoch)
import Control.DeepSeq
import Control.Monad (forM_, when)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, setSingleton, singleton, (∈), (∉), (∪), (⋪), (⨃))
import Control.State.Transition (
  STS (..),
  TRC (..),
  TransitionRule,
  failBecause,
  judgmentContext,
  liftSTS,
  tellEvent,
  (?!),
 )
import qualified Data.ByteString as BS
import Data.Word (Word8)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

data PoolEnv era
  = PoolEnv !SlotNo !(PParams era)

deriving instance Show (PParams era) => Show (PoolEnv era)

deriving instance Eq (PParams era) => Eq (PoolEnv era)

data ShelleyPoolPredFailure era
  = StakePoolNotRegisteredOnKeyPOOL
      !(KeyHash 'StakePool (EraCrypto era)) -- KeyHash which cannot be retired since it is not registered
  | StakePoolRetirementWrongEpochPOOL
      !EpochNo -- Current Epoch
      !EpochNo -- The epoch listed in the Pool Retirement Certificate
      !EpochNo -- The first epoch that is too far out for retirement
  | WrongCertificateTypePOOL
      !Word8 -- The disallowed certificate (this case should never happen)
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
  deriving (Show, Eq, Generic)

instance NoThunks (ShelleyPoolPredFailure era)

instance NFData (ShelleyPoolPredFailure era)

instance (ShelleyEraTxCert era, EraPParams era) => STS (ShelleyPOOL era) where
  type State (ShelleyPOOL era) = PState era

  type Signal (ShelleyPOOL era) = TxCert era

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
    WrongCertificateTypePOOL ct ->
      encodeListLen 2 <> encCBOR (2 :: Word8) <> encCBOR ct
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
      2 -> do
        ct <- decCBOR
        pure (2, WrongCertificateTypePOOL ct)
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
  forall era.
  (ShelleyEraTxCert era, EraPParams era) =>
  TransitionRule (ShelleyPOOL era)
poolDelegationTransition = do
  TRC (PoolEnv slot pp, ps, c) <- judgmentContext
  let stpools = psStakePoolParams ps
  let pv = pp ^. ppProtocolVersionL
  case c of
    RegPoolTxCert poolParam -> do
      -- note that pattern match is used instead of cwitness, as in the spec

      when (HardForks.validatePoolRewardAccountNetID pv) $ do
        actualNetID <- liftSTS $ asks networkId
        let suppliedNetID = getRwdNetwork (ppRewardAcnt poolParam)
        actualNetID
          == suppliedNetID
          ?! WrongNetworkPOOL actualNetID suppliedNetID (ppId poolParam)

      when (SoftForks.restrictPoolMetadataHash pv) $
        forM_ (ppMetadata poolParam) $ \pmd ->
          let s = BS.length (pmHash pmd)
           in s
                <= fromIntegral (sizeHash ([] @(CC.HASH (EraCrypto era))))
                ?! PoolMedataHashTooBig (ppId poolParam) s

      let poolCost = ppCost poolParam
          minPoolCost = pp ^. ppMinPoolCostL
      poolCost >= minPoolCost ?! StakePoolCostTooLowPOOL poolCost minPoolCost

      let hk = ppId poolParam
      if eval (hk ∉ dom stpools)
        then do
          -- register new, Pool-Reg
          tellEvent $ RegisterPool hk
          pure $
            payPoolDeposit hk pp $
              ps
                { psStakePoolParams = eval (psStakePoolParams ps ∪ singleton hk poolParam)
                }
        else do
          tellEvent $ ReregisterPool hk
          -- hk is already registered, so we want to reregister it. That means adding it to the
          -- Future pool params (if it is not there already), and overriding the range with the new 'poolParam',
          -- if it is (using ⨃ ). We must also unretire it, if it has been scheduled for retirement.
          -- The deposit does not change. One pays the deposit just once. Only if it is fully retired
          -- (i.e. it's deposit has been refunded, and it has been removed from the registered pools).
          -- does it need to pay a new deposit (at the current deposit amount). But of course,
          -- if that has happened, we cannot be in this branch of the if statement.
          pure $
            ps
              { psFutureStakePoolParams = eval (psFutureStakePoolParams ps ⨃ singleton hk poolParam)
              , psRetiring = eval (setSingleton hk ⋪ psRetiring ps)
              }
    RetirePoolTxCert hk e -> do
      -- note that pattern match is used instead of cwitness, as in the spec
      eval (hk ∈ dom stpools) ?! StakePoolNotRegisteredOnKeyPOOL hk
      cepoch <- liftSTS $ do
        ei <- asks epochInfoPure
        epochInfoEpoch ei slot
      let maxEpoch = pp ^. ppEMaxL
      (cepoch < e && e <= cepoch + maxEpoch)
        ?! StakePoolRetirementWrongEpochPOOL cepoch e (cepoch + maxEpoch)
      -- We just schedule it for retirement. When it is retired we refund the deposit (see POOLREAP)
      pure $ ps {psRetiring = eval (psRetiring ps ⨃ singleton hk e)}
    RegTxCert _ -> do
      failBecause $ WrongCertificateTypePOOL 0
      pure ps
    UnRegTxCert _ -> do
      failBecause $ WrongCertificateTypePOOL 0
      pure ps
    DelegStakeTxCert _ _ -> do
      failBecause $ WrongCertificateTypePOOL 0
      pure ps
    GenesisDelegTxCert _ _ _ -> do
      failBecause $ WrongCertificateTypePOOL 2
      pure ps
    _ | isInstantaneousRewards c -> do
      failBecause $ WrongCertificateTypePOOL 1
      pure ps
    _ -> do
      -- Impossible case
      failBecause $ WrongCertificateTypePOOL 3
      pure ps
