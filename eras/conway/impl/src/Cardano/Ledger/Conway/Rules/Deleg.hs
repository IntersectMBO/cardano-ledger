{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Deleg (
  ConwayDELEG,
  ConwayDelegEvent (..),
  ConwayDelegPredFailure (..),
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
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
import Cardano.Ledger.Conway.Core (ppKeyDepositL)
import Cardano.Ledger.Conway.Era (ConwayDELEG)
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert (ConwayDelegCert, ConwayRegCert, ConwayRegDelegCert, ConwayUnRegCert),
  Delegatee (DelegStake, DelegStakeVote, DelegVote),
 )
import Cardano.Ledger.Core (Era (EraCrypto), EraPParams, EraRule, PParams)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (Staking))
import Cardano.Ledger.Shelley.LedgerState (DState (..))
import qualified Cardano.Ledger.UMap as UM
import Control.DeepSeq (NFData)
import Control.Monad (forM_)
import Control.State.Transition (
  BaseM,
  Environment,
  Event,
  PredicateFailure,
  STS,
  Signal,
  State,
  TRC (TRC),
  TransitionRule,
  judgmentContext,
  transitionRules,
  (?!),
 )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)

data ConwayDelegPredFailure era
  = IncorrectDepositDELEG !Coin
  | StakeKeyRegisteredDELEG !(Credential 'Staking (EraCrypto era))
  | StakeKeyNotRegisteredDELEG !(Credential 'Staking (EraCrypto era))
  | StakeKeyHasNonZeroRewardAccountBalanceDELEG !Coin
  | DRepAlreadyRegisteredForStakeKeyDELEG !(Credential 'Staking (EraCrypto era))
  deriving (Show, Eq, Generic)

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

instance Era era => DecCBOR (ConwayDelegPredFailure era) where
  decCBOR = decode $ Summands "ConwayDelegPredFailure" $ \case
    1 -> SumD IncorrectDepositDELEG <! From
    2 -> SumD StakeKeyRegisteredDELEG <! From
    3 -> SumD StakeKeyNotRegisteredDELEG <! From
    4 -> SumD StakeKeyHasNonZeroRewardAccountBalanceDELEG <! From
    5 -> SumD DRepAlreadyRegisteredForStakeKeyDELEG <! From
    n -> Invalid n

newtype ConwayDelegEvent era = DelegEvent (Event (EraRule "DELEG" era))

instance
  ( EraPParams era
  , State (EraRule "DELEG" era) ~ DState era
  , Signal (EraRule "DELEG" era) ~ ConwayDelegCert (EraCrypto era)
  , Environment (EraRule "DELEG" era) ~ PParams era
  , EraRule "DELEG" era ~ ConwayDELEG era
  ) =>
  STS (ConwayDELEG era)
  where
  type State (ConwayDELEG era) = DState era
  type Signal (ConwayDELEG era) = ConwayDelegCert (EraCrypto era)
  type Environment (ConwayDELEG era) = PParams era
  type BaseM (ConwayDELEG era) = ShelleyBase
  type PredicateFailure (ConwayDELEG era) = ConwayDelegPredFailure era
  type Event (ConwayDELEG era) = ConwayDelegEvent era

  transitionRules = [conwayDelegTransition @era]

conwayDelegTransition :: forall era. EraPParams era => TransitionRule (ConwayDELEG era)
conwayDelegTransition = do
  TRC
    ( pp
      , dState@DState {dsUnified}
      , c
      ) <-
    judgmentContext
  let ppKeyDeposit = pp ^. ppKeyDepositL
  case c of
    ConwayRegCert stakeCred sMayDeposit -> do
      forM_ sMayDeposit $ checkDepositAgainstPParams ppKeyDeposit
      dsUnified' <- checkAndAcceptDepositForStakeCred stakeCred ppKeyDeposit dsUnified
      pure $ dState {dsUnified = dsUnified'}
    ConwayUnRegCert stakeCred sMayDeposit -> do
      checkStakeKeyIsRegistered stakeCred dsUnified
      checkStakeKeyHasZeroRewardBalance stakeCred dsUnified
      forM_ sMayDeposit $ checkDepositAgainstPaidDeposit stakeCred dsUnified
      pure $ dState {dsUnified = UM.domDeleteAll (Set.singleton stakeCred) dsUnified}
    ConwayDelegCert stakeCred delegatee -> do
      checkStakeKeyIsRegistered stakeCred dsUnified
      pure $ dState {dsUnified = processDelegation stakeCred delegatee dsUnified}
    ConwayRegDelegCert stakeCred delegatee deposit -> do
      checkDepositAgainstPParams ppKeyDeposit deposit
      dsUnified' <- checkAndAcceptDepositForStakeCred stakeCred deposit dsUnified
      pure $ dState {dsUnified = processDelegation stakeCred delegatee dsUnified'}
  where
    -- Whenever we want to accept new deposit, we must always check if the stake credential isn't already registered.
    checkAndAcceptDepositForStakeCred stakeCred deposit dsUnified = do
      checkStakeKeyNotRegistered stakeCred dsUnified
      -- This looks like it should have been a right-biased union, so that the (reward, deposit) pair would be inserted
      -- (or overwritten) in the UMap. But since we are sure that the stake credential isn't a member yet
      -- it will still work. The reason we cannot use a right-biased union here is because UMap treats deposits specially
      -- in right-biased unions, and is unable to accept new deposits.
      pure $ UM.RewDepUView dsUnified UM.∪ (stakeCred, UM.RDPair (UM.CompactCoin 0) (UM.compactCoinOrError deposit))
    delegStake stakeCred sPool dsUnified =
      UM.SPoolUView dsUnified UM.⨃ Map.singleton stakeCred sPool
    delegVote stakeCred dRep dsUnified =
      UM.DRepUView dsUnified UM.⨃ Map.singleton stakeCred dRep
    processDelegation stakeCred delegatee dsUnified =
      case delegatee of
        DelegStake sPool -> delegStake stakeCred sPool dsUnified
        DelegVote dRep -> delegVote stakeCred dRep dsUnified
        DelegStakeVote sPool dRep -> delegVote stakeCred dRep $ delegStake stakeCred sPool dsUnified
    checkDepositAgainstPParams ppKeyDeposit deposit =
      deposit == ppKeyDeposit ?! IncorrectDepositDELEG deposit
    checkDepositAgainstPaidDeposit stakeCred dsUnified deposit =
      Just deposit == fmap (UM.fromCompact . UM.rdDeposit) (UM.lookup stakeCred $ UM.RewDepUView dsUnified) ?! IncorrectDepositDELEG deposit
    checkStakeKeyNotRegistered stakeCred dsUnified =
      UM.notMember stakeCred (UM.RewDepUView dsUnified) ?! StakeKeyRegisteredDELEG stakeCred
    checkStakeKeyIsRegistered stakeCred dsUnified =
      UM.member stakeCred (UM.RewDepUView dsUnified) ?! StakeKeyNotRegisteredDELEG stakeCred
    checkStakeKeyHasZeroRewardBalance stakeCred dsUnified =
      let mReward = UM.rdReward <$> UM.lookup stakeCred (UM.RewDepUView dsUnified)
       in forM_ mReward $ \r -> r == mempty ?! StakeKeyHasNonZeroRewardAccountBalanceDELEG (UM.fromCompact r)
