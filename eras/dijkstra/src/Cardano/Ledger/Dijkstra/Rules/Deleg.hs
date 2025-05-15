{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Deleg (
  DijkstraDELEG,
  DijkstraDelegPredFailure (..),
  DijkstraDelegEnv (..),
  processDelegation,
) where

import Cardano.Ledger.BaseTypes (ProtVer (..), ShelleyBase, StrictMaybe (..), natVersion)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (From, Invalid, SumD, Summands),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraDELEG, DijkstraEra)
import Cardano.Ledger.Dijkstra.State (
  DijkstraEraCertState (..),
  vsDRepsL,
 )
import Cardano.Ledger.Dijkstra.TxCert (
  DijkstraDelegCert (DijkstraDelegCert, DijkstraRegCert, DijkstraRegDelegCert, DijkstraUnRegCert),
  Delegatee (DelegStake, DelegStakeVote, DelegVote),
 )
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.PoolParams (PoolParams)
import qualified Cardano.Ledger.Shelley.HardForks as HF
import Cardano.Ledger.State (
  EraCertState (..),
  dsUnifiedL,
 )
import qualified Cardano.Ledger.UMap as UM
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
  failOnJust,
  judgmentContext,
  transitionRules,
  (?!),
 )
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set as Set
import Data.Void (Void)
import GHC.Generics (Generic)
import Lens.Micro ((%~), (&), (.~), (^.))
import NoThunks.Class (NoThunks)

data DijkstraDelegEnv era = DijkstraDelegEnv
  { cdePParams :: PParams era
  , cdePools :: Map (KeyHash 'StakePool) PoolParams
  }
  deriving (Generic)

instance EraPParams era => EncCBOR (DijkstraDelegEnv era) where
  encCBOR x@(DijkstraDelegEnv _ _) =
    let DijkstraDelegEnv {..} = x
     in encode $
          Rec DijkstraDelegEnv
            !> To cdePParams
            !> To cdePools

instance (Era era, NFData (PParams era)) => NFData (DijkstraDelegEnv era)

deriving instance Eq (PParams era) => Eq (DijkstraDelegEnv era)

deriving instance Show (PParams era) => Show (DijkstraDelegEnv era)

data DijkstraDelegPredFailure era
  = IncorrectDepositDELEG Coin
  | StakeKeyRegisteredDELEG (Credential 'Staking)
  | StakeKeyNotRegisteredDELEG (Credential 'Staking)
  | StakeKeyHasNonZeroRewardAccountBalanceDELEG Coin
  | DelegateeDRepNotRegisteredDELEG (Credential 'DRepRole)
  | DelegateeStakePoolNotRegisteredDELEG (KeyHash 'StakePool)
  deriving (Show, Eq, Generic)

type instance EraRuleFailure "DELEG" DijkstraEra = DijkstraDelegPredFailure DijkstraEra

type instance EraRuleEvent "DELEG" DijkstraEra = VoidEraRule "DELEG" DijkstraEra

instance InjectRuleFailure "DELEG" DijkstraDelegPredFailure DijkstraEra

instance NoThunks (DijkstraDelegPredFailure era)

instance NFData (DijkstraDelegPredFailure era)

instance Era era => EncCBOR (DijkstraDelegPredFailure era) where
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
      DelegateeDRepNotRegisteredDELEG delegatee ->
        Sum (DelegateeDRepNotRegisteredDELEG @era) 5 !> To delegatee
      DelegateeStakePoolNotRegisteredDELEG delegatee ->
        Sum (DelegateeStakePoolNotRegisteredDELEG @era) 6 !> To delegatee

instance Era era => DecCBOR (DijkstraDelegPredFailure era) where
  decCBOR = decode $ Summands "DijkstraDelegPredFailure" $ \case
    1 -> SumD IncorrectDepositDELEG <! From
    2 -> SumD StakeKeyRegisteredDELEG <! From
    3 -> SumD StakeKeyNotRegisteredDELEG <! From
    4 -> SumD StakeKeyHasNonZeroRewardAccountBalanceDELEG <! From
    5 -> SumD DelegateeDRepNotRegisteredDELEG <! From
    6 -> SumD DelegateeStakePoolNotRegisteredDELEG <! From
    n -> Invalid n

instance
  ( EraPParams era
  , State (EraRule "DELEG" era) ~ CertState era
  , Signal (EraRule "DELEG" era) ~ DijkstraDelegCert
  , Environment (EraRule "DELEG" era) ~ DijkstraDelegEnv era
  , EraRule "DELEG" era ~ DijkstraDELEG era
  , EraCertState era
  , DijkstraEraCertState era
  ) =>
  STS (DijkstraDELEG era)
  where
  type State (DijkstraDELEG era) = CertState era
  type Signal (DijkstraDELEG era) = DijkstraDelegCert
  type Environment (DijkstraDELEG era) = DijkstraDelegEnv era
  type BaseM (DijkstraDELEG era) = ShelleyBase
  type PredicateFailure (DijkstraDELEG era) = DijkstraDelegPredFailure era
  type Event (DijkstraDELEG era) = Void

  transitionRules = [conwayDelegTransition @era]

conwayDelegTransition ::
  (EraPParams era, DijkstraEraCertState era) => TransitionRule (DijkstraDELEG era)
conwayDelegTransition = do
  TRC
    ( DijkstraDelegEnv pp pools
      , certState
      , cert
      ) <-
    judgmentContext
  let
    dsUnified = certState ^. certDStateL . dsUnifiedL
    ppKeyDeposit = pp ^. ppKeyDepositL
    pv = pp ^. ppProtocolVersionL
    checkDepositAgainstPParams deposit =
      deposit == ppKeyDeposit ?! IncorrectDepositDELEG deposit
    registerStakeCredential stakeCred =
      let rdPair = UM.RDPair (UM.CompactCoin 0) (UM.compactCoinOrError ppKeyDeposit)
       in UM.insert stakeCred rdPair $ UM.RewDepUView dsUnified
    checkStakeKeyNotRegistered stakeCred =
      UM.notMember stakeCred (UM.RewDepUView dsUnified) ?! StakeKeyRegisteredDELEG stakeCred
    checkStakeKeyIsRegistered stakeCred = do
      let mUMElem = Map.lookup stakeCred (UM.umElems dsUnified)
      isJust mUMElem ?! StakeKeyNotRegisteredDELEG stakeCred
      pure $ mUMElem >>= umElemToDelegatee
    checkStakeDelegateeRegistered =
      let checkPoolRegistered targetPool =
            targetPool `Map.member` pools ?! DelegateeStakePoolNotRegisteredDELEG targetPool
          checkDRepRegistered = \case
            DRepAlwaysAbstain -> pure ()
            DRepAlwaysNoConfidence -> pure ()
            DRepCredential targetDRep -> do
              let dReps = certState ^. certVStateL . vsDRepsL
              unless (HF.bootstrapPhase (pp ^. ppProtocolVersionL)) $
                targetDRep `Map.member` dReps ?! DelegateeDRepNotRegisteredDELEG targetDRep
       in \case
            DelegStake targetPool -> checkPoolRegistered targetPool
            DelegStakeVote targetPool targetDRep ->
              checkPoolRegistered targetPool >> checkDRepRegistered targetDRep
            DelegVote targetDRep -> checkDRepRegistered targetDRep
  case cert of
    DijkstraRegCert stakeCred sMayDeposit -> do
      forM_ sMayDeposit checkDepositAgainstPParams
      checkStakeKeyNotRegistered stakeCred
      pure $ certState & certDStateL . dsUnifiedL .~ registerStakeCredential stakeCred
    DijkstraUnRegCert stakeCred sMayRefund -> do
      let (mUMElem, umap) = UM.extractStakingCredential stakeCred dsUnified
          mCurDelegatee = mUMElem >>= umElemToDelegatee
          checkInvalidRefund = do
            SJust suppliedRefund <- Just sMayRefund
            -- we don't want to report invalid refund when stake credential is not registered:
            UM.UMElem (SJust rd) _ _ _ <- mUMElem
            -- we return offending refund only when it doesn't match the expected one:
            guard (suppliedRefund /= UM.fromCompact (UM.rdDeposit rd))
            Just suppliedRefund
          checkStakeKeyHasZeroRewardBalance = do
            UM.UMElem (SJust rd) _ _ _ <- mUMElem
            guard (UM.rdReward rd /= mempty)
            Just $ UM.fromCompact (UM.rdReward rd)
      failOnJust checkInvalidRefund IncorrectDepositDELEG
      isJust mUMElem ?! StakeKeyNotRegisteredDELEG stakeCred
      failOnJust checkStakeKeyHasZeroRewardBalance StakeKeyHasNonZeroRewardAccountBalanceDELEG
      pure $
        processDRepUnDelegation stakeCred mCurDelegatee $
          certState & certDStateL . dsUnifiedL .~ umap
    DijkstraDelegCert stakeCred delegatee -> do
      mCurDelegatee <- checkStakeKeyIsRegistered stakeCred
      checkStakeDelegateeRegistered delegatee
      pure $
        processDelegationInternal (pvMajor pv < natVersion @10) stakeCred mCurDelegatee delegatee certState
    DijkstraRegDelegCert stakeCred delegatee deposit -> do
      checkDepositAgainstPParams deposit
      checkStakeKeyNotRegistered stakeCred
      checkStakeDelegateeRegistered delegatee
      pure $
        processDelegationInternal (pvMajor pv < natVersion @10) stakeCred Nothing delegatee $
          certState & certDStateL . dsUnifiedL .~ registerStakeCredential stakeCred

-- | Apply new delegation, while properly cleaning up older delegations. This function
-- does not enforce that delegatee is registered, that has to be handled by the caller.
processDelegation ::
  DijkstraEraCertState era =>
  -- | Delegator
  Credential 'Staking ->
  -- | New delegatee
  Delegatee ->
  CertState era ->
  CertState era
processDelegation stakeCred newDelegatee !certState = certState'
  where
    !certState' = processDelegationInternal False stakeCred mCurDelegatee newDelegatee certState
    mUMElem = Map.lookup stakeCred (UM.umElems (certState ^. certDStateL . dsUnifiedL))
    mCurDelegatee = mUMElem >>= umElemToDelegatee

-- | Same as `processDelegation`, except it expects the current delegation supplied as an
-- argument, because in ledger rules we already have it readily available.
processDelegationInternal ::
  DijkstraEraCertState era =>
  -- | Preserve the buggy behavior where DRep delegations are not updated correctly (See #4772)
  Bool ->
  -- | Delegator
  Credential 'Staking ->
  -- | Current delegatee for the above stake credential that needs to be cleaned up.
  Maybe Delegatee ->
  -- | New delegatee
  Delegatee ->
  CertState era ->
  CertState era
processDelegationInternal preserveIncorrectDelegation stakeCred mCurDelegatee newDelegatee =
  case newDelegatee of
    DelegStake sPool -> delegStake sPool
    DelegVote dRep -> delegVote dRep
    DelegStakeVote sPool dRep -> delegVote dRep . delegStake sPool
  where
    delegStake sPool cState =
      cState
        & certDStateL . dsUnifiedL %~ \umap ->
          UM.SPoolUView umap UM.⨃ Map.singleton stakeCred sPool
    delegVote dRep cState =
      let cState' =
            processDRepUnDelegation stakeCred mCurDelegatee cState
              & certDStateL . dsUnifiedL %~ \umap ->
                UM.DRepUView umap UM.⨃ Map.singleton stakeCred dRep
          dReps
            | preserveIncorrectDelegation = cState ^. certVStateL . vsDRepsL
            | otherwise = cState' ^. certVStateL . vsDRepsL
       in case dRep of
            DRepCredential targetDRep
              | Just dRepState <- Map.lookup targetDRep dReps ->
                  let dRepState' = dRepState {drepDelegs = Set.insert stakeCred (drepDelegs dRepState)}
                   in cState' & certVStateL . vsDRepsL .~ Map.insert targetDRep dRepState' dReps
            _ -> cState'

umElemToDelegatee :: UM.UMElem -> Maybe Delegatee
umElemToDelegatee (UM.UMElem _ _ mPool mDRep) =
  case (mPool, mDRep) of
    (SNothing, SNothing) -> Nothing
    (SJust pool, SNothing) -> Just $ DelegStake pool
    (SNothing, SJust dRep) -> Just $ DelegVote dRep
    (SJust pool, SJust dRep) -> Just $ DelegStakeVote pool dRep

processDRepUnDelegation ::
  DijkstraEraCertState era =>
  Credential 'Staking ->
  Maybe Delegatee ->
  CertState era ->
  CertState era
processDRepUnDelegation _ Nothing cState = cState
processDRepUnDelegation stakeCred (Just delegatee) cState =
  case delegatee of
    DelegStake _ -> cState
    DelegVote dRep -> cState & certVStateL .~ unDelegVote (cState ^. certVStateL) dRep
    DelegStakeVote _sPool dRep -> cState & certVStateL .~ unDelegVote (cState ^. certVStateL) dRep
  where
    unDelegVote vState = \case
      DRepCredential dRepCred ->
        let removeDelegation dRepState =
              dRepState {drepDelegs = Set.delete stakeCred (drepDelegs dRepState)}
         in vState & vsDRepsL %~ Map.adjust removeDelegation dRepCred
      _ -> vState
