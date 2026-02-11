{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
  conwayDelegTransition,
  processDelegation,
) where

import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  ProtVer (..),
  Relation (RelEQ),
  ShelleyBase,
  StrictMaybe (..),
  natVersion,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (From, Invalid, SumD, Summands),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin, compactCoinOrError)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (
  ConwayDELEG,
  ConwayEra,
  hardforkConwayBootstrapPhase,
  hardforkConwayDELEGIncorrectDepositsAndRefunds,
 )
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert (ConwayDelegCert, ConwayRegCert, ConwayRegDelegCert, ConwayUnRegCert),
  Delegatee (DelegStake, DelegStakeVote, DelegVote),
 )
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.DRep
import Control.DeepSeq (NFData)
import Control.Monad (forM_, guard, unless)
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
  failBecause,
  failOnJust,
  judgmentContext,
  transitionRules,
  (?!),
 )
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Set as Set
import Data.Void (Void)
import GHC.Generics (Generic)
import Lens.Micro ((%~), (&), (.~), (?~), (^.))
import NoThunks.Class (NoThunks)

data ConwayDelegEnv era = ConwayDelegEnv
  { cdePParams :: PParams era
  , cdePools :: Map (KeyHash StakePool) StakePoolState
  }
  deriving (Generic)

instance EraPParams era => EncCBOR (ConwayDelegEnv era) where
  encCBOR x@(ConwayDelegEnv _ _) =
    let ConwayDelegEnv {..} = x
     in encode $
          Rec ConwayDelegEnv
            !> To cdePParams
            !> To cdePools

instance (Era era, NFData (PParams era)) => NFData (ConwayDelegEnv era)

deriving instance Eq (PParams era) => Eq (ConwayDelegEnv era)

deriving instance Show (PParams era) => Show (ConwayDelegEnv era)

data ConwayDelegPredFailure era
  = IncorrectDepositDELEG Coin
  | StakeKeyRegisteredDELEG (Credential Staking)
  | StakeKeyNotRegisteredDELEG (Credential Staking)
  | StakeKeyHasNonZeroAccountBalanceDELEG Coin
  | DelegateeDRepNotRegisteredDELEG (Credential DRepRole)
  | DelegateeStakePoolNotRegisteredDELEG (KeyHash StakePool)
  | DepositIncorrectDELEG (Mismatch RelEQ Coin)
  | RefundIncorrectDELEG (Mismatch RelEQ Coin)
  deriving (Show, Eq, Generic)

type instance EraRuleFailure "DELEG" ConwayEra = ConwayDelegPredFailure ConwayEra

type instance EraRuleEvent "DELEG" ConwayEra = VoidEraRule "DELEG" ConwayEra

instance InjectRuleFailure "DELEG" ConwayDelegPredFailure ConwayEra

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
      StakeKeyHasNonZeroAccountBalanceDELEG mCoin ->
        Sum (StakeKeyHasNonZeroAccountBalanceDELEG @era) 4 !> To mCoin
      DelegateeDRepNotRegisteredDELEG delegatee ->
        Sum (DelegateeDRepNotRegisteredDELEG @era) 5 !> To delegatee
      DelegateeStakePoolNotRegisteredDELEG delegatee ->
        Sum (DelegateeStakePoolNotRegisteredDELEG @era) 6 !> To delegatee
      DepositIncorrectDELEG mm ->
        Sum (DepositIncorrectDELEG @era) 7 !> To mm
      RefundIncorrectDELEG mm ->
        Sum (RefundIncorrectDELEG @era) 8 !> To mm

instance Era era => DecCBOR (ConwayDelegPredFailure era) where
  decCBOR = decode $ Summands "ConwayDelegPredFailure" $ \case
    1 -> SumD IncorrectDepositDELEG <! From
    2 -> SumD StakeKeyRegisteredDELEG <! From
    3 -> SumD StakeKeyNotRegisteredDELEG <! From
    4 -> SumD StakeKeyHasNonZeroAccountBalanceDELEG <! From
    5 -> SumD DelegateeDRepNotRegisteredDELEG <! From
    6 -> SumD DelegateeStakePoolNotRegisteredDELEG <! From
    7 -> SumD DepositIncorrectDELEG <! From
    8 -> SumD RefundIncorrectDELEG <! From
    n -> Invalid n

instance
  ( EraPParams era
  , EraCertState era
  , ConwayEraCertState era
  , State (EraRule "DELEG" era) ~ CertState era
  , Signal (EraRule "DELEG" era) ~ ConwayDelegCert
  , Environment (EraRule "DELEG" era) ~ ConwayDelegEnv era
  , EraRule "DELEG" era ~ ConwayDELEG era
  , InjectRuleFailure "DELEG" ConwayDelegPredFailure era
  ) =>
  STS (ConwayDELEG era)
  where
  type State (ConwayDELEG era) = CertState era
  type Signal (ConwayDELEG era) = ConwayDelegCert
  type Environment (ConwayDELEG era) = ConwayDelegEnv era
  type BaseM (ConwayDELEG era) = ShelleyBase
  type PredicateFailure (ConwayDELEG era) = ConwayDelegPredFailure era
  type Event (ConwayDELEG era) = Void

  transitionRules = [conwayDelegTransition]

conwayDelegTransition ::
  forall rule era.
  ( EraPParams era
  , ConwayEraCertState era
  , Signal (EraRule rule era) ~ ConwayDelegCert
  , Environment (EraRule rule era) ~ ConwayDelegEnv era
  , State (EraRule rule era) ~ CertState era
  , InjectRuleFailure rule ConwayDelegPredFailure era
  ) =>
  TransitionRule (EraRule rule era)
conwayDelegTransition = do
  TRC
    ( ConwayDelegEnv pp pools
      , certState
      , cert
      ) <-
    judgmentContext
  let
    accounts = certState ^. certDStateL . accountsL
    ppKeyDeposit = pp ^. ppKeyDepositL
    ppKeyDepositCompact = compactCoinOrError ppKeyDeposit
    pv = pp ^. ppProtocolVersionL
    checkDepositAgainstPParams deposit =
      deposit
        == ppKeyDeposit
          ?! if hardforkConwayDELEGIncorrectDepositsAndRefunds pv
            then
              injectFailure
                ( DepositIncorrectDELEG
                    Mismatch
                      { mismatchSupplied = deposit
                      , mismatchExpected = ppKeyDeposit
                      }
                )
            else injectFailure $ IncorrectDepositDELEG deposit
    checkStakeKeyNotRegistered stakeCred =
      not (isAccountRegistered stakeCred accounts)
        ?! injectFailure (StakeKeyRegisteredDELEG stakeCred)
    checkStakeDelegateeRegistered =
      let checkPoolRegistered targetPool =
            targetPool
              `Map.member` pools
              ?! injectFailure (DelegateeStakePoolNotRegisteredDELEG targetPool)
          checkDRepRegistered = \case
            DRepAlwaysAbstain -> pure ()
            DRepAlwaysNoConfidence -> pure ()
            DRepCredential targetDRep -> do
              let dReps = certState ^. certVStateL . vsDRepsL
              unless (hardforkConwayBootstrapPhase (pp ^. ppProtocolVersionL)) $
                targetDRep `Map.member` dReps ?! injectFailure (DelegateeDRepNotRegisteredDELEG targetDRep)
       in \case
            DelegStake targetPool -> checkPoolRegistered targetPool
            DelegStakeVote targetPool targetDRep ->
              checkPoolRegistered targetPool >> checkDRepRegistered targetDRep
            DelegVote targetDRep -> checkDRepRegistered targetDRep
  case cert of
    ConwayRegCert stakeCred sMayDeposit -> do
      forM_ sMayDeposit checkDepositAgainstPParams
      checkStakeKeyNotRegistered stakeCred
      pure $
        certState
          & certDStateL . accountsL
            %~ registerConwayAccount stakeCred ppKeyDepositCompact Nothing
    ConwayUnRegCert stakeCred sMayRefund -> do
      let (mAccountState, newAccounts) = unregisterConwayAccount stakeCred accounts
          checkInvalidRefund = do
            SJust suppliedRefund <- Just sMayRefund
            -- we don't want to report invalid refund when stake credential is not registered:
            accountState <- mAccountState
            -- we return offending refund only when it doesn't match the expected one:
            let expectedRefund = fromCompact $ accountState ^. depositAccountStateL
            guard (suppliedRefund /= expectedRefund)
            Just $
              if hardforkConwayDELEGIncorrectDepositsAndRefunds pv
                then
                  injectFailure
                    ( RefundIncorrectDELEG
                        Mismatch
                          { mismatchSupplied = suppliedRefund
                          , mismatchExpected = expectedRefund
                          }
                    )
                else injectFailure $ IncorrectDepositDELEG suppliedRefund
          checkStakeKeyHasZeroRewardBalance = do
            accountState <- mAccountState
            let balanceCompact = accountState ^. balanceAccountStateL
            guard (balanceCompact /= mempty)
            Just $ fromCompact balanceCompact
      failOnJust checkInvalidRefund id
      failOnJust
        checkStakeKeyHasZeroRewardBalance
        (injectFailure . StakeKeyHasNonZeroAccountBalanceDELEG)
      case mAccountState of
        Nothing -> do
          failBecause $ injectFailure (StakeKeyNotRegisteredDELEG stakeCred)
          pure certState
        Just accountState ->
          pure $
            certState
              & certDStateL . accountsL .~ newAccounts
              & certVStateL %~ unDelegReDelegDRep stakeCred accountState Nothing
              & certPStateL %~ unDelegReDelegStakePool stakeCred accountState Nothing
    ConwayDelegCert stakeCred delegatee -> do
      checkStakeDelegateeRegistered delegatee
      case lookupAccountStateIntern stakeCred accounts of
        Nothing -> do
          failBecause $ injectFailure (StakeKeyNotRegisteredDELEG stakeCred)
          pure certState
        Just (internedCred, accountState) -> do
          pure $
            processDelegationInternal
              (pvMajor pv < natVersion @10)
              internedCred
              (Just accountState)
              delegatee
              certState
    ConwayRegDelegCert stakeCred delegatee deposit -> do
      checkDepositAgainstPParams deposit
      checkStakeKeyNotRegistered stakeCred
      checkStakeDelegateeRegistered delegatee
      pure $
        processDelegationInternal (pvMajor pv < natVersion @10) stakeCred Nothing delegatee $
          certState
            & certDStateL . accountsL
              %~ registerConwayAccount stakeCred ppKeyDepositCompact (Just delegatee)

-- | Apply new delegation, while properly cleaning up older delegations. This function
-- does not enforce that delegatee is registered, that has to be handled by the caller.
processDelegation ::
  ConwayEraCertState era =>
  -- | Delegator
  Credential Staking ->
  -- | New delegatee
  Delegatee ->
  CertState era ->
  CertState era
processDelegation stakeCred newDelegatee !certState = certState'
  where
    !certState' = processDelegationInternal False stakeCred mAccountState newDelegatee certState
    mAccountState = Map.lookup stakeCred (certState ^. certDStateL . accountsL . accountsMapL)

-- | Same as `processDelegation`, except it expects the current delegation supplied as an
-- argument, because in ledger rules we already have it readily available.
processDelegationInternal ::
  ConwayEraCertState era =>
  -- | Preserve the buggy behavior where DRep delegations are not updated correctly (See #4772)
  Bool ->
  -- | Delegator
  Credential Staking ->
  -- | Account state for the above stake credential
  Maybe (AccountState era) ->
  -- | New delegatee
  Delegatee ->
  CertState era ->
  CertState era
processDelegationInternal preserveIncorrectDelegation stakeCred mAccountState newDelegatee =
  case newDelegatee of
    DelegStake sPool -> delegStake sPool
    DelegVote dRep -> delegVote dRep
    DelegStakeVote sPool dRep -> delegVote dRep . delegStake sPool
  where
    delegStake stakePool cState =
      cState
        & certDStateL . accountsL
          %~ adjustAccountState (stakePoolDelegationAccountStateL ?~ stakePool) stakeCred
        & maybe
          (certPStateL . psStakePoolsL %~ Map.adjust (spsDelegatorsL %~ Set.insert stakeCred) stakePool)
          (\accountState -> certPStateL %~ unDelegReDelegStakePool stakeCred accountState (Just stakePool))
          mAccountState

    delegVote dRep cState =
      let handleReverseDelegation =
            case dRepToCred dRep of
              Just dRepCred
                -- This is the case where we only add the new reverse delegation and do not remove
                -- the old one, which is the behavior that we want:
                --
                -- - for new accounts, since there is no old reverse delegeation to remove
                --
                -- - in the bootstrap phase to preserve the incorrect behavior, where old reverse
                --   delegation for the prior DRep was wrongfully retained. It is important to note
                --   that in case when the new delegation was to a predefined DRep, the reverse
                --   delegations where handled correctly even in the boostrap phase
                --
                -- For reference here is the original bug report:
                --   https://github.com/IntersectMBO/cardano-ledger/issues/4772
                | isNothing mAccountState || preserveIncorrectDelegation ->
                    certVStateL . vsDRepsL
                      %~ Map.adjust (drepDelegsL %~ Set.insert stakeCred) dRepCred
              _
                -- AccountState existed before this delegation, therefore we need to properly handle
                -- potential undelegation of the old DRep
                | Just accountState <- mAccountState ->
                    certVStateL %~ unDelegReDelegDRep stakeCred accountState (Just dRep)
                -- If this is a fresh registration with delegation to a predefined DRep, there are
                -- no extra steps that need to be done
                | otherwise -> id
       in cState
            & certDStateL . accountsL
              %~ adjustAccountState (dRepDelegationAccountStateL ?~ dRep) stakeCred
            & handleReverseDelegation
