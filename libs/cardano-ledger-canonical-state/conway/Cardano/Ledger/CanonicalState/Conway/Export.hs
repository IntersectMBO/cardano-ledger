{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Conway.Export () where

import Cardano.Ledger.BaseTypes (StrictMaybe (..), strictMaybeToMaybe)
import Cardano.Ledger.CanonicalState.Conway (
  fromGovActionState,
  mkCanonicalAccountState,
  mkCanonicalConstitution,
  mkCanonicalGovActionId,
 )
import Cardano.Ledger.CanonicalState.Export (
  ExportCanonicalNamespace (..),
  ExportCanonicalState (..),
  addNamespaceToPlan,
 )
import Cardano.Ledger.CanonicalState.Namespace.Blocks.V0 (BlockIn (BlockIn), BlockOut (BlockOut))
import Cardano.Ledger.CanonicalState.Namespace.EntitiesAccounts.V0 (
  EntitiesAccountsIn (EntitiesAccountsIn),
  EntitiesAccountsOut (EntitiesAccountsOut),
 )
import Cardano.Ledger.CanonicalState.Namespace.EntitiesCommittee.V0 (
  CanonicalCommitteeState (..),
  EntitiesCommitteeIn (..),
  EntitiesCommitteeOut (..),
  mkCanonicalCommitteeAuthorization,
 )
import Cardano.Ledger.CanonicalState.Namespace.EntitiesDReps.V0 (
  EntitiesDRepsIn (EntitiesDRepsIn),
  EntitiesDRepsOut (EntitiesDRepsOut),
  mkCanonicalDRepState,
 )
import Cardano.Ledger.CanonicalState.Namespace.EntitiesStakePools.V0 (
  CanonicalStakePool (..),
  EntitiesStakePoolsIn (..),
  EntitiesStakePoolsOut (..),
  mkCanonicalStakePoolParams,
  mkCanonicalStakePoolState,
 )
import Cardano.Ledger.CanonicalState.Namespace.EntitiesStakePools.VRFKeyHashes.V0 (
  EntitiesStakePoolsVRFKeyHashesIn (EntitiesStakePoolsVRFKeyHashesIn),
  EntitiesStakePoolsVRFKeyHashesOut (EntitiesStakePoolsVRFKeyHashesOut),
 )
import Cardano.Ledger.CanonicalState.Namespace.GovCommittee.V0 (
  CanonicalCommittee (..),
  GovCommitteeIn (..),
  GovCommitteeOut (..),
 )
import Cardano.Ledger.CanonicalState.Namespace.GovConstitution.V0 (
  GovConstitutionIn (..),
  GovConstitutionOut (..),
 )
import Cardano.Ledger.CanonicalState.Namespace.GovPParams.V0 (
  GovPParamsIn (..),
  GovPParamsOut (..),
 )
import Cardano.Ledger.CanonicalState.Namespace.GovProposals.Roots.V0 (
  GovProposalsRootsIn (..),
  GovProposalsRootsOut (GovProposalsRootsOut),
 )
import Cardano.Ledger.CanonicalState.Namespace.UTxO.V0 (UtxoIn (UtxoKeyIn), mkUtxo)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  ConwayEraGov (constitutionGovStateL),
  GovPurposeId (unGovPurposeId),
  cgsCommitteeL,
  cgsProposalsL,
  grCommitteeL,
  grConstitutionL,
  grHardForkL,
  grPParamUpdateL,
  pRootsL,
  proposalsActions,
  toPrevGovActionIds,
 )
import Cardano.Ledger.Conway.Rules (updateDormantDRepExpiry)
import Cardano.Ledger.Conway.State (
  CanGetUTxO (utxoG),
  CanSetAccounts (accountsL),
  ConwayEraCertState (certVStateL),
  EraAccounts (accountsMapL),
  EraCertState (certDStateL, certPStateL),
  FuturePParams (..),
  UTxO (unUTxO),
  csCommitteeCredsL,
  psFutureStakePoolParamsL,
  psRetiringL,
  psStakePoolsL,
  psVRFKeyHashesL,
  vsCommitteeStateL,
  vsDRepsL,
 )
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  curPParamsEpochStateL,
  esLStateL,
  futurePParamsEpochStateL,
  lsCertStateL,
  nesBcurL,
  nesELL,
  nesEpochStateL,
  nesEsL,
  newEpochStateGovStateL,
  prevPParamsEpochStateL,
 )
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry (ChunkEntry))
import Cardano.SCLS.Internal.Serializer.Dump.Plan (
  defaultSerializationPlan,
 )
import Data.Foldable (Foldable (toList))
import Data.Functor ((<&>))
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import Lens.Micro ((%~), (&), (^.))
import qualified Streaming.Prelude as S

instance ExportCanonicalNamespace ConwayEra "utxo/v0" where
  canonicalNamespaceEntries nes =
    S.each (Map.toList $ unUTxO $ nes ^. utxoG)
      & S.map (\(txIn, txOut) -> ChunkEntry (UtxoKeyIn txIn) (mkUtxo txOut))

instance ExportCanonicalNamespace ConwayEra "blocks/v0" where
  canonicalNamespaceEntries nes = blocks
    where
      epochNo = nes ^. nesELL
      blocks =
        S.each (Map.toList $ nes ^. nesBcurL)
          & S.map (\(keyHash, n) -> ChunkEntry (BlockIn keyHash epochNo) (BlockOut n))

instance ExportCanonicalNamespace ConwayEra "entities/committee/v0" where
  canonicalNamespaceEntries nes =
    S.yield (ChunkEntry EntitiesCommitteeIn (EntitiesCommitteeOut committeeState))
    where
      committeeState =
        CanonicalCommitteeState $
          Map.map mkCanonicalCommitteeAuthorization $
            nes
              ^. nesEpochStateL
                . esLStateL
                . lsCertStateL
                . certVStateL
                . vsCommitteeStateL
                . csCommitteeCredsL

instance ExportCanonicalNamespace ConwayEra "gov/committee/v0" where
  canonicalNamespaceEntries nes =
    S.yield (ChunkEntry GovCommitteeIn (GovCommitteeOut committee))
    where
      committee =
        fmap
          (\Committee {..} -> CanonicalCommittee {committeeMembers, committeeThreshold})
          $ nes
            ^. newEpochStateGovStateL
              . cgsCommitteeL

instance ExportCanonicalNamespace ConwayEra "gov/constitution/v0" where
  canonicalNamespaceEntries nes =
    S.yield (ChunkEntry GovConstitutionIn (GovConstitutionOut canonicalConstitution))
    where
      constitution = nes ^. newEpochStateGovStateL . constitutionGovStateL
      canonicalConstitution = mkCanonicalConstitution constitution

instance ExportCanonicalNamespace ConwayEra "gov/pparams/v0" where
  canonicalNamespaceEntries nes =
    S.each pparams
    where
      epochState = nes ^. nesEsL
      currPParams = epochState ^. curPParamsEpochStateL
      prevPParams = epochState ^. prevPParamsEpochStateL
      (futurePossiblePParams, futureDefinitePParams) = case epochState ^. futurePParamsEpochStateL of
        NoPParamsUpdate -> ([], [])
        DefinitePParamsUpdate p -> ([], [ChunkEntry GovPParamsInDefiniteFuture (GovPParamsOut p)])
        PotentialPParamsUpdate (Just p) -> ([ChunkEntry GovPParamsInPossibleFuture (GovPParamsOut p)], [])
        PotentialPParamsUpdate Nothing -> ([], [])
      pparams =
        [ ChunkEntry GovPParamsInPrev (GovPParamsOut prevPParams)
        , ChunkEntry GovPParamsInCurr (GovPParamsOut currPParams)
        ]
          ++ futurePossiblePParams
          ++ futureDefinitePParams

instance ExportCanonicalNamespace ConwayEra "gov/proposals/v0" where
  canonicalNamespaceEntries nes = do
    S.each
      [ uncurry ChunkEntry $ fromGovActionState n govActionState
      | (n, govActionState) <-
          zip
            [0 ..]
            (toList $ proposalsActions (nes ^. newEpochStateGovStateL . cgsProposalsL))
      ]

instance ExportCanonicalNamespace ConwayEra "gov/proposals/roots/v0" where
  canonicalNamespaceEntries nes =
    S.each
      [
        ( GovProposalsRootsInPParamUpdate
        , fmap unGovPurposeId $
            prevGovActionIds ^. grPParamUpdateL
        )
      ,
        ( GovProposalsRootsInHardFork
        , fmap unGovPurposeId $
            prevGovActionIds ^. grHardForkL
        )
      ,
        ( GovProposalsRootsInCommittee
        , fmap unGovPurposeId $
            prevGovActionIds ^. grCommitteeL
        )
      ,
        ( GovProposalsRootsInConstitution
        , fmap unGovPurposeId $
            prevGovActionIds ^. grConstitutionL
        )
      ]
      & S.mapMaybe
        ( \(k, mGovActionId) ->
            strictMaybeToMaybe mGovActionId
              <&> \govActionId ->
                ChunkEntry k (GovProposalsRootsOut (mkCanonicalGovActionId govActionId))
        )
    where
      prevGovActionIds =
        toPrevGovActionIds $ nes ^. newEpochStateGovStateL . cgsProposalsL . pRootsL

instance ExportCanonicalNamespace ConwayEra "entities/accounts/v0" where
  canonicalNamespaceEntries nes =
    S.map
      ( \(cred, accountState) ->
          ChunkEntry (EntitiesAccountsIn cred) (EntitiesAccountsOut $ mkCanonicalAccountState accountState)
      )
      $ S.each
      $ Map.toList
      $ nes
        ^. nesEsL
          . esLStateL
          . lsCertStateL
          . certDStateL
          . accountsL
          . accountsMapL

instance ExportCanonicalNamespace ConwayEra "entities/dreps/v0" where
  canonicalNamespaceEntries nes =
    S.each
      ( Map.toList
          ( nes
              ^. nesEsL
                . esLStateL
                . lsCertStateL
                . certVStateL
                . vsDRepsL
          )
      )
      & S.map
        ( \(cred, drepState) -> ChunkEntry (EntitiesDRepsIn cred) (EntitiesDRepsOut $ mkCanonicalDRepState drepState)
        )

instance ExportCanonicalNamespace ConwayEra "entities/stake_pools/v0" where
  canonicalNamespaceEntries nes =
    stakePoolsEntries
    where
      stakePools =
        S.each
          $ Map.toList
          $ Map.merge
            (Map.mapMissing (\_ v -> (SNothing, SNothing, SJust v)))
            (Map.mapMissing (\_ (x, y) -> (x, y, SNothing)))
            ( Map.zipWithMatched
                ( \_ futureStakePoolParams (stakePoolState, retiringEpochNo) -> (stakePoolState, retiringEpochNo, SJust futureStakePoolParams)
                )
            )
            ( nes
                ^. nesEsL
                  . esLStateL
                  . lsCertStateL
                  . certPStateL
                  . psFutureStakePoolParamsL
            )
          $ Map.merge
            (Map.mapMissing (\_ v -> (SJust v, SNothing)))
            (Map.mapMissing (\_ v -> (SNothing, SJust v)))
            ( Map.zipWithMatched
                (\_ stakePoolState retiringEpochNo -> (SJust stakePoolState, SJust retiringEpochNo))
            )
            ( nes
                ^. nesEsL
                  . esLStateL
                  . lsCertStateL
                  . certPStateL
                  . psStakePoolsL
            )
            ( nes
                ^. nesEsL
                  . esLStateL
                  . lsCertStateL
                  . certPStateL
                  . psRetiringL
            )
      stakePoolsEntries =
        S.map
          ( \(k, (stakePoolState, retiringEpochNo, futureStakePoolParams)) ->
              ChunkEntry
                (EntitiesStakePoolsIn k)
                ( EntitiesStakePoolsOut $
                    CanonicalStakePool
                      { cspStakePoolState = fmap mkCanonicalStakePoolState stakePoolState
                      , cspRetiringEpochNo = retiringEpochNo
                      , cspFutureStakePoolParams = fmap mkCanonicalStakePoolParams futureStakePoolParams
                      }
                )
          )
          stakePools

instance ExportCanonicalNamespace ConwayEra "entities/stake_pools/vrf_key_hashes/v0" where
  canonicalNamespaceEntries nes =
    stakePoolsVRFKeyHashesEntries
    where
      stakePoolsVRFKeyHashes =
        S.each $
          Map.toList
            ( nes
                ^. nesEsL
                  . esLStateL
                  . lsCertStateL
                  . certPStateL
                  . psVRFKeyHashesL
            )
      stakePoolsVRFKeyHashesEntries =
        S.map
          ( \(k, n) ->
              ChunkEntry
                (EntitiesStakePoolsVRFKeyHashesIn k)
                (EntitiesStakePoolsVRFKeyHashesOut n)
          )
          stakePoolsVRFKeyHashes

instance ExportCanonicalState ConwayEra where
  type State ConwayEra = NewEpochState ConwayEra
  stateSerializationPlan nes =
    defaultSerializationPlan
      & addNamespaceToPlan @ConwayEra @"utxo/v0" updatedNes
      & addNamespaceToPlan @ConwayEra @"blocks/v0" updatedNes
      & addNamespaceToPlan @ConwayEra @"gov/committee/v0" updatedNes
      & addNamespaceToPlan @ConwayEra @"gov/constitution/v0" updatedNes
      & addNamespaceToPlan @ConwayEra @"gov/pparams/v0" updatedNes
      & addNamespaceToPlan @ConwayEra @"gov/proposals/v0" updatedNes
      & addNamespaceToPlan @ConwayEra @"gov/proposals/roots/v0" updatedNes
      & addNamespaceToPlan @ConwayEra @"entities/accounts/v0" updatedNes
      & addNamespaceToPlan @ConwayEra @"entities/dreps/v0" updatedNes
      & addNamespaceToPlan @ConwayEra @"entities/committee/v0" updatedNes
      & addNamespaceToPlan @ConwayEra @"entities/stake_pools/v0" updatedNes
      & addNamespaceToPlan @ConwayEra @"entities/stake_pools/vrf_key_hashes/v0" updatedNes
    where
      epochNo = nes ^. nesELL
      -- Update DRep expiry before exporting
      updatedNes =
        nes
          & nesEpochStateL
            . esLStateL
            . lsCertStateL
            . certVStateL
            %~ updateDormantDRepExpiry epochNo

  getEpochNo nes = nes ^. nesELL
