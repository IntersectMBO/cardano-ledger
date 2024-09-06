{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Deleg (
  ConwayDELEG,
  ConwayDelegPredFailure (..),
  ConwayDelegEnv (..),
) where

import Cardano.Ledger.BaseTypes (ShelleyBase, StrictMaybe (..))
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (From, Invalid, SumD, Summands),
  Encode (Sum, To),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayDELEG, ConwayEra)
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert (ConwayDelegCert, ConwayRegCert, ConwayRegDelegCert, ConwayUnRegCert),
  Delegatee (DelegStake, DelegStakeVote, DelegVote),
 )
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolParams (PoolParams)
import Cardano.Ledger.Shelley.LedgerState (DState (..))
import qualified Cardano.Ledger.UMap as UM
import Control.DeepSeq (NFData)
import Control.Monad (forM_, guard)
import Control.State.Transition (
  BaseM,
  Environment,
  Event,
  PredicateFailure,
  STS (..),
  Signal,
  State,
  TRC (TRC),
  TransitionRule,
  failOnJust,
  judgmentContext,
  transitionRules,
  (?!),
 )
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Void (Void)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)

data ConwayDelegEnv era = ConwayDelegEnv
  { cdePParams :: PParams era
  , cdePools ::
      !( Map
          (KeyHash 'StakePool (EraCrypto era))
          (PoolParams (EraCrypto era))
       )
  }
  deriving (Generic)

instance NFData (PParams era) => NFData (ConwayDelegEnv era)

deriving instance Eq (PParams era) => Eq (ConwayDelegEnv era)

deriving instance Show (PParams era) => Show (ConwayDelegEnv era)

data ConwayDelegPredFailure era
  = IncorrectDepositDELEG !Coin
  | StakeKeyRegisteredDELEG !(Credential 'Staking (EraCrypto era))
  | StakeKeyNotRegisteredDELEG !(Credential 'Staking (EraCrypto era))
  | StakeKeyHasNonZeroRewardAccountBalanceDELEG !Coin
  | DRepAlreadyRegisteredForStakeKeyDELEG !(Credential 'Staking (EraCrypto era))
  | DelegateeNotRegisteredDELEG !(KeyHash 'StakePool (EraCrypto era))
  deriving (Show, Eq, Generic)

type instance EraRuleFailure "DELEG" (ConwayEra c) = ConwayDelegPredFailure (ConwayEra c)

type instance EraRuleEvent "DELEG" (ConwayEra c) = VoidEraRule "DELEG" (ConwayEra c)

instance InjectRuleFailure "DELEG" ConwayDelegPredFailure (ConwayEra c)

instance NoThunks (ConwayDelegPredFailure era)

instance NFData (ConwayDelegPredFailure era)

instance Era era => EncCBOR (ConwayDelegPredFailure era) where
  encCBOR =
    encode . \case
      IncorrectDepositDELEG mCoin ->
        Sum (IncorrectDepositDELEG @era) 1 !> To mCoin
      StakeKeyRegisteredDELEG stakeCred ->
        Sum (StakeKeyRegisteredDELEG @era) 2 !> To stakeCred
      StakeKeyNotRegisteredDELEG stakeCred ->
        Sum (StakeKeyNotRegisteredDELEG @era) 3 !> To stakeCred
      StakeKeyHasNonZeroRewardAccountBalanceDELEG mCoin ->
        Sum (StakeKeyHasNonZeroRewardAccountBalanceDELEG @era) 4 !> To mCoin
      DRepAlreadyRegisteredForStakeKeyDELEG stakeCred ->
        Sum (DRepAlreadyRegisteredForStakeKeyDELEG @era) 5 !> To stakeCred
      DelegateeNotRegisteredDELEG delegatee ->
        Sum (DelegateeNotRegisteredDELEG @era) 6 !> To delegatee

instance Era era => DecCBOR (ConwayDelegPredFailure era) where
  decCBOR = decode $ Summands "ConwayDelegPredFailure" $ \case
    1 -> SumD IncorrectDepositDELEG <! From
    2 -> SumD StakeKeyRegisteredDELEG <! From
    3 -> SumD StakeKeyNotRegisteredDELEG <! From
    4 -> SumD StakeKeyHasNonZeroRewardAccountBalanceDELEG <! From
    5 -> SumD DRepAlreadyRegisteredForStakeKeyDELEG <! From
    6 -> SumD DelegateeNotRegisteredDELEG <! From
    n -> Invalid n

instance
  ( EraPParams era
  , State (EraRule "DELEG" era) ~ DState era
  , Signal (EraRule "DELEG" era) ~ ConwayDelegCert (EraCrypto era)
  , Environment (EraRule "DELEG" era) ~ ConwayDelegEnv era
  , EraRule "DELEG" era ~ ConwayDELEG era
  ) =>
  STS (ConwayDELEG era)
  where
  type State (ConwayDELEG era) = DState era
  type Signal (ConwayDELEG era) = ConwayDelegCert (EraCrypto era)
  type Environment (ConwayDELEG era) = ConwayDelegEnv era
  type BaseM (ConwayDELEG era) = ShelleyBase
  type PredicateFailure (ConwayDELEG era) = ConwayDelegPredFailure era
  type Event (ConwayDELEG era) = Void

  transitionRules = [conwayDelegTransition @era]

conwayDelegTransition :: forall era. EraPParams era => TransitionRule (ConwayDELEG era)
conwayDelegTransition = do
  TRC
    ( ConwayDelegEnv pp pools
      , dState@DState {dsUnified}
      , c
      ) <-
    judgmentContext
  let
    ppKeyDeposit = pp ^. ppKeyDepositL
    checkDepositAgainstPParams deposit =
      deposit == ppKeyDeposit ?! IncorrectDepositDELEG deposit
    registerStakeCredential stakeCred =
      let rdPair = UM.RDPair (UM.CompactCoin 0) (UM.compactCoinOrError ppKeyDeposit)
       in UM.insert stakeCred rdPair $ UM.RewDepUView dsUnified
  case c of
    ConwayRegCert stakeCred sMayDeposit -> do
      forM_ sMayDeposit checkDepositAgainstPParams
      checkStakeKeyNotRegistered stakeCred dsUnified
      pure $ dState {dsUnified = registerStakeCredential stakeCred}
    ConwayUnRegCert stakeCred sMayRefund -> do
      let mRDPair = UM.lookup stakeCred $ UM.RewDepUView dsUnified
          checkInvalidRefund = do
            SJust suppliedRefund <- Just sMayRefund
            -- we don't want to report invalid refund when stake credential is not registered:
            UM.RDPair _ actualRefund <- mRDPair
            -- we return offending refund only when it doesn't match the expected one:
            guard (suppliedRefund /= UM.fromCompact actualRefund)
            Just suppliedRefund
          checkStakeKeyHasZeroRewardBalance = do
            UM.RDPair compactReward _ <- mRDPair
            guard (compactReward /= mempty)
            Just $ UM.fromCompact compactReward
      failOnJust checkInvalidRefund IncorrectDepositDELEG
      isJust mRDPair ?! StakeKeyNotRegisteredDELEG stakeCred
      failOnJust checkStakeKeyHasZeroRewardBalance StakeKeyHasNonZeroRewardAccountBalanceDELEG
      pure $ dState {dsUnified = UM.domDeleteAll (Set.singleton stakeCred) dsUnified}
    ConwayDelegCert stakeCred delegatee -> do
      checkStakeKeyIsRegistered stakeCred dsUnified
      checkStakeDelegateeRegistered pools delegatee
      pure $ dState {dsUnified = processDelegation stakeCred delegatee dsUnified}
    ConwayRegDelegCert stakeCred delegatee deposit -> do
      checkDepositAgainstPParams deposit
      checkStakeKeyNotRegistered stakeCred dsUnified
      checkStakeDelegateeRegistered pools delegatee
      pure $
        dState
          { dsUnified = processDelegation stakeCred delegatee $ registerStakeCredential stakeCred
          }
  where
    checkStakeDelegateeRegistered pools =
      let checkPoolRegistered targetPool =
            targetPool `Map.member` pools ?! DelegateeNotRegisteredDELEG targetPool
       in \case
            DelegStake targetPool -> checkPoolRegistered targetPool
            DelegStakeVote targetPool _ -> checkPoolRegistered targetPool
            DelegVote _ -> pure ()
    delegStake stakeCred sPool dsUnified =
      UM.SPoolUView dsUnified UM.⨃ Map.singleton stakeCred sPool
    delegVote stakeCred dRep dsUnified =
      UM.DRepUView dsUnified UM.⨃ Map.singleton stakeCred dRep
    processDelegation stakeCred delegatee dsUnified =
      case delegatee of
        DelegStake sPool -> delegStake stakeCred sPool dsUnified
        DelegVote dRep -> delegVote stakeCred dRep dsUnified
        DelegStakeVote sPool dRep -> delegVote stakeCred dRep $ delegStake stakeCred sPool dsUnified
    checkStakeKeyNotRegistered stakeCred dsUnified =
      UM.notMember stakeCred (UM.RewDepUView dsUnified) ?! StakeKeyRegisteredDELEG stakeCred
    checkStakeKeyIsRegistered stakeCred dsUnified =
      UM.member stakeCred (UM.RewDepUView dsUnified) ?! StakeKeyNotRegisteredDELEG stakeCred
