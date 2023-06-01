{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
import Cardano.Ledger.Core (Era (EraCrypto), EraPParams, EraRule)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (Staking))
import Cardano.Ledger.Shelley.LedgerState (DState (..))
import Cardano.Ledger.Shelley.Rules (DelegEnv (DelegEnv))
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
  | StakeKeyAlreadyRegisteredDELEG !(Credential 'Staking (EraCrypto era))
  | StakeKeyNotRegisteredDELEG !(Credential 'Staking (EraCrypto era))
  | StakeKeyHasNonZeroAccountBalanceDELEG !Coin
  | DRepAlreadyRegisteredForStakeKeyDELEG !(Credential 'Staking (EraCrypto era))
  | WrongCertificateTypeDELEG
  deriving (Show, Eq, Generic)

instance NoThunks (ConwayDelegPredFailure era)

instance NFData (ConwayDelegPredFailure era)

instance Era era => EncCBOR (ConwayDelegPredFailure era) where
  encCBOR =
    encode . \case
      IncorrectDepositDELEG mCoin ->
        Sum (IncorrectDepositDELEG @era) 1 !> To mCoin
      StakeKeyAlreadyRegisteredDELEG stakeCred ->
        Sum (StakeKeyAlreadyRegisteredDELEG @era) 2 !> To stakeCred
      StakeKeyNotRegisteredDELEG stakeCred ->
        Sum (StakeKeyNotRegisteredDELEG @era) 3 !> To stakeCred
      StakeKeyHasNonZeroAccountBalanceDELEG mCoin ->
        Sum (StakeKeyHasNonZeroAccountBalanceDELEG @era) 4 !> To mCoin
      DRepAlreadyRegisteredForStakeKeyDELEG stakeCred ->
        Sum (DRepAlreadyRegisteredForStakeKeyDELEG @era) 5 !> To stakeCred
      WrongCertificateTypeDELEG ->
        Sum (WrongCertificateTypeDELEG @era) 6

instance Era era => DecCBOR (ConwayDelegPredFailure era) where
  decCBOR = decode $ Summands "ConwayDelegPredFailure" $ \case
    1 -> SumD IncorrectDepositDELEG <! From
    2 -> SumD StakeKeyAlreadyRegisteredDELEG <! From
    3 -> SumD StakeKeyNotRegisteredDELEG <! From
    4 -> SumD StakeKeyHasNonZeroAccountBalanceDELEG <! From
    5 -> SumD DRepAlreadyRegisteredForStakeKeyDELEG <! From
    6 -> SumD WrongCertificateTypeDELEG
    n -> Invalid n

newtype ConwayDelegEvent era = DelegEvent (Event (EraRule "DELEG" era))

instance
  ( EraPParams era
  , State (EraRule "DELEG" era) ~ DState era
  , Signal (EraRule "DELEG" era) ~ ConwayDelegCert (EraCrypto era)
  , Environment (EraRule "DELEG" era) ~ DelegEnv era
  , EraRule "DELEG" era ~ ConwayDELEG era
  ) =>
  STS (ConwayDELEG era)
  where
  type State (ConwayDELEG era) = DState era
  type Signal (ConwayDELEG era) = ConwayDelegCert (EraCrypto era)
  type Environment (ConwayDELEG era) = DelegEnv era
  type BaseM (ConwayDELEG era) = ShelleyBase
  type PredicateFailure (ConwayDELEG era) = ConwayDelegPredFailure era
  type Event (ConwayDELEG era) = ConwayDelegEvent era

  transitionRules = [conwayDelegTransition @era]

conwayDelegTransition :: forall era. EraPParams era => TransitionRule (ConwayDELEG era)
conwayDelegTransition = do
  TRC
    ( DelegEnv _slot _ptr _acnt pp
      , dState@DState {dsUnified}
      , c
      ) <-
    judgmentContext
  let ppKeyDeposit = pp ^. ppKeyDepositL
  case c of
    ConwayRegCert stakeCred sMayDeposit -> do
      forM_ sMayDeposit $ checkDepositAgainstPParams ppKeyDeposit
      checkStakeKeyNotAlreadyRegistered stakeCred dsUnified
      pure $ dState {dsUnified = acceptDepositForStakeKey stakeCred dsUnified ppKeyDeposit}
    ConwayUnRegCert stakeCred sMayDeposit -> do
      checkStakeKeyIsAlreadyRegistered stakeCred dsUnified
      checkStakeKeyHasZeroBalance stakeCred dsUnified
      forM_ sMayDeposit $ checkDepositAgainstPaidDeposit stakeCred dsUnified
      pure $
        dState
          { dsUnified = UM.domDeleteAll (Set.singleton stakeCred) dsUnified
          }
    ConwayDelegCert stakeCred delegatee -> do
      checkStakeKeyIsAlreadyRegistered stakeCred dsUnified
      pure $
        dState
          { dsUnified = processDelegation stakeCred delegatee dsUnified
          }
    ConwayRegDelegCert stakeCred delegatee deposit -> do
      deposit == ppKeyDeposit ?! IncorrectDepositDELEG deposit
      checkStakeKeyNotAlreadyRegistered stakeCred dsUnified
      -- checkDRepNotAlreadyRegistered stakeCred dsUnified -- TODO: @aniketd to confirm
      pure $
        dState
          { dsUnified =
              processDelegation stakeCred delegatee $
                acceptDepositForStakeKey stakeCred dsUnified ppKeyDeposit
          }
  where
    acceptDepositForStakeKey stakeCred dsUnified ppKeyDeposit =
      UM.RewDepUView dsUnified UM.∪ (stakeCred, UM.RDPair (UM.CompactCoin 0) (UM.compactCoinOrError ppKeyDeposit))
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
    checkStakeKeyNotAlreadyRegistered stakeCred dsUnified =
      UM.notMember stakeCred (UM.RewDepUView dsUnified) ?! StakeKeyAlreadyRegisteredDELEG stakeCred
    checkStakeKeyIsAlreadyRegistered stakeCred dsUnified =
      UM.member stakeCred (UM.RewDepUView dsUnified) ?! StakeKeyNotRegisteredDELEG stakeCred
    -- checkDRepNotAlreadyRegistered stakeCred dsUnified = -- TODO: @aniketd to confirm
    --   UM.notMember stakeCred (DRepUView dsUnified)
    --     ?! DRepAlreadyRegisteredForStakeKeyDELEG stakeCred
    checkStakeKeyHasZeroBalance stakeCred dsUnified =
      let mReward = UM.rdReward <$> UM.lookup stakeCred (UM.RewDepUView dsUnified)
       in forM_ mReward $ \r -> r == mempty ?! StakeKeyHasNonZeroAccountBalanceDELEG (UM.fromCompact r)
