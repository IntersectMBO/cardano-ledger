{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Transition (
  ConwayEraTransition (..),
  TransitionConfig (..),
  registerDRepsThenDelegs,
  conwayRegisterInitialAccounts,
  conwayRegisterInitialFundsThenStaking,
  injectStakeCredentials,
  injectDRepsThenDelegs,
) where

import Cardano.Ledger.Babbage
import Cardano.Ledger.Babbage.Transition (
  TransitionConfig (BabbageTransitionConfig),
  alonzoInjectCostModels,
 )
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin (compactCoinOrError)
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Conway.Genesis (ConwayExtraConfig (..), ConwayGenesis (..))
import Cardano.Ledger.Conway.Rules.Deleg (processDelegation)
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.Translation ()
import Cardano.Ledger.Conway.TxCert (Delegatee (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.Genesis (
  InjectionData,
  InjectionError (..),
  ShelleyGenesisStaking (..),
  foldInjectionData,
 )
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  curPParamsEpochStateL,
  esLStateL,
  lsCertStateL,
  nesEsL,
 )
import Cardano.Ledger.Shelley.Transition hiding (injectStakeCredentials)
import Control.Monad (when)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow (MonadThrow (throwIO))
import Data.ListMap (ListMap)
import qualified Data.ListMap as ListMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics
import GHC.Stack
import Lens.Micro
import NoThunks.Class (NoThunks (..))
import System.FS.API (HasFS)

class (EraTransition era, ConwayEraCertState era) => ConwayEraTransition era where
  tcConwayGenesisL :: Lens' (TransitionConfig era) ConwayGenesis
  default tcConwayGenesisL ::
    ConwayEraTransition (PreviousEra era) =>
    Lens' (TransitionConfig era) ConwayGenesis
  tcConwayGenesisL = tcPreviousEraConfigL . tcConwayGenesisL

registerDRepsThenDelegs ::
  forall era.
  ConwayEraTransition era =>
  TransitionConfig era ->
  NewEpochState era ->
  NewEpochState era
registerDRepsThenDelegs cfg =
  -- NOTE: The order of registration does not matter.
  registerDelegs . registerInitialDReps
  where
    registerInitialDReps :: NewEpochState era -> NewEpochState era
    registerInitialDReps =
      nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL .~ drepsMap
      where
        drepsMap = ListMap.toMap $ cfg ^. tcInitialDRepsL
    registerDelegs :: NewEpochState era -> NewEpochState era
    registerDelegs =
      nesEsL . esLStateL . lsCertStateL
        %~ \certState -> ListMap.foldrWithKey (uncurry processDelegation) certState (cfg ^. tcDelegsL)
{-# DEPRECATED registerDRepsThenDelegs "Use `injectDRepsThenDelegs` instead" #-}

injectDRepsThenDelegs ::
  (ConwayEraCertState era, MonadST m, MonadThrow m) =>
  Network ->
  HasFS m h ->
  InjectionData (Credential DRepRole) DRepState ->
  InjectionData (Credential Staking) Delegatee ->
  NewEpochState era ->
  m (NewEpochState era)
injectDRepsThenDelegs network fs drepsSource delegsSource newEpochState = do
  when (network == Mainnet) $ throwIO InjectionNotAllowedOnMainnet
  -- NOTE: The order of registration does not matter.
  drepsMap <-
    foldInjectionData
      fs
      drepsSource
      (\ !acc (cred, drepState) -> Map.insert cred drepState acc)
      Map.empty
  let newEpochState' = newEpochState & nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL .~ drepsMap
  updatedCertState <-
    foldInjectionData
      fs
      delegsSource
      (\ !certState (cred, delegatee) -> processDelegation cred delegatee certState)
      (newEpochState' ^. nesEsL . esLStateL . lsCertStateL)
  pure $ newEpochState' & nesEsL . esLStateL . lsCertStateL .~ updatedCertState

instance EraTransition ConwayEra where
  data TransitionConfig ConwayEra = ConwayTransitionConfig
    { ctcConwayGenesis :: !ConwayGenesis
    , ctcBabbageTransitionConfig :: !(TransitionConfig BabbageEra)
    }
    deriving (Show, Eq, Generic)

  mkTransitionConfig = ConwayTransitionConfig

  injectIntoTestState hasFS cfg newEpochState =
    conwayRegisterInitialFundsThenStaking hasFS cfg $
      alonzoInjectCostModels (cfg ^. tcPreviousEraConfigL . tcPreviousEraConfigL) newEpochState

  tcPreviousEraConfigL =
    lens ctcBabbageTransitionConfig (\ctc pc -> ctc {ctcBabbageTransitionConfig = pc})

  tcTranslationContextL =
    lens ctcConwayGenesis (\ctc ag -> ctc {ctcConwayGenesis = ag})

instance ConwayEraTransition ConwayEra where
  tcConwayGenesisL = lens ctcConwayGenesis (\g x -> g {ctcConwayGenesis = x})

tcDelegsL ::
  ConwayEraTransition era => Lens' (TransitionConfig era) (ListMap (Credential Staking) Delegatee)
tcDelegsL =
  protectMainnetLens "ConwayDelegs" null $
    tcConwayGenesisL . lens cgDelegs (\g x -> g {cgDelegs = x})

tcInitialDRepsL ::
  ConwayEraTransition era => Lens' (TransitionConfig era) (ListMap (Credential DRepRole) DRepState)
tcInitialDRepsL =
  protectMainnetLens "InitialDReps" null $
    tcConwayGenesisL . lens cgInitialDReps (\g x -> g {cgInitialDReps = x})

instance NoThunks (TransitionConfig ConwayEra)

conwayRegisterInitialFundsThenStaking ::
  (ConwayEraTransition era, HasCallStack, MonadST m, MonadThrow m) =>
  HasFS m h ->
  TransitionConfig era ->
  NewEpochState era ->
  m (NewEpochState era)
conwayRegisterInitialFundsThenStaking hasFS cfg newEpochState = do
  let cg = cfg ^. tcConwayGenesisL
      network = cfg ^. tcNetworkIDG
  drepsSource <-
    resolveInjectionSource "initialDReps" (cgExtraConfig cg) cecInitialDReps (cgInitialDReps cg)
  delegsSource <- resolveInjectionSource "delegs" (cgExtraConfig cg) cecDelegs (cgDelegs cg)
  fmap resetStakeDistribution $
    injectInitialFundsAndStaking hasFS injectStakeCredentials cfg newEpochState
      >>= injectDRepsThenDelegs network hasFS drepsSource delegsSource

-- | Register all staking credentials and apply delegations. Make sure StakePools that are being
-- delegated to are already registered, which can be done with `registerInitialStakePools`.
conwayRegisterInitialAccounts ::
  forall era.
  (HasCallStack, EraTransition era, ConwayEraAccounts era) =>
  ShelleyGenesisStaking ->
  NewEpochState era ->
  NewEpochState era
conwayRegisterInitialAccounts ShelleyGenesisStaking {sgsStake} nes =
  nes
    & nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL .~ updatedAccounts
    & nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL .~ updatedStakePoolStates
  where
    stakePools = nes ^. nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL
    initialAccounts = nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
    deposit = compactCoinOrError $ nes ^. nesEsL . curPParamsEpochStateL . ppKeyDepositL

    !(!updatedAccounts, !updatedStakePoolStates) =
      foldr registerAndDelegate (initialAccounts, stakePools) (ListMap.toList sgsStake)
    registerAndDelegate (stakeKeyHash, stakePool) (!accounts, !stakePoolMap)
      | stakePool `Map.member` stakePools =
          ( (registerConwayAccount (KeyHashObj stakeKeyHash) deposit (Just (DelegStake stakePool)) accounts)
          , Map.adjust (spsDelegatorsL %~ Set.insert (KeyHashObj stakeKeyHash)) stakePool stakePoolMap
          )
      | otherwise = error $ delegationInvariantMsg stakeKeyHash stakePool
{-# DEPRECATED conwayRegisterInitialAccounts "Use `injectStakeCredentials` instead" #-}

injectStakeCredentials ::
  (ConwayEraAccounts era, EraCertState era, EraGov era, MonadST m, MonadThrow m) =>
  Network ->
  HasFS m h ->
  InjectionData (KeyHash Staking) (KeyHash StakePool) ->
  NewEpochState era ->
  m (NewEpochState era)
injectStakeCredentials network fs source newEpochState = do
  when (network == Mainnet) $ throwIO InjectionNotAllowedOnMainnet
  let stakePools = newEpochState ^. nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL
      initialAccounts = newEpochState ^. nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
      deposit = compactCoinOrError $ newEpochState ^. nesEsL . curPParamsEpochStateL . ppKeyDepositL
      registerAndDelegate (!accounts, !stakePoolMap) (stakeKeyHash, stakePool)
        | stakePool `Map.member` stakePools =
            ( registerConwayAccount (KeyHashObj stakeKeyHash) deposit (Just (DelegStake stakePool)) accounts
            , Map.adjust (spsDelegatorsL %~ Set.insert (KeyHashObj stakeKeyHash)) stakePool stakePoolMap
            )
        | otherwise = error $ delegationInvariantMsg stakeKeyHash stakePool
  (!updatedAccounts, !updatedPools) <-
    foldInjectionData fs source registerAndDelegate (initialAccounts, stakePools)
  pure $
    newEpochState
      & nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL .~ updatedAccounts
      & nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL .~ updatedPools

delegationInvariantMsg :: (Show a, Show b) => a -> b -> String
delegationInvariantMsg stakeKeyHash stakePool =
  "Delegation of "
    ++ show stakeKeyHash
    ++ " to an unregistered stake pool "
    ++ show stakePool
