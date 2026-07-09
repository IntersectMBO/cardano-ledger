{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Conway.Import () where

import Cardano.Ledger.BaseTypes (
  SlotNo (SlotNo),
  StrictMaybe (SJust),
  strictMaybe,
 )
import Cardano.Ledger.CanonicalState.BasicTypes (OnChain (getValue))
import Cardano.Ledger.CanonicalState.Conway (
  fromCanonicalAccountState,
  fromCanonicalGovActionId,
  toGovActionState,
 )
import Cardano.Ledger.CanonicalState.Conway.Export ()
import Cardano.Ledger.CanonicalState.Export (ExportCanonicalState (State))
import Cardano.Ledger.CanonicalState.Import (
  ImportCanonicalNamespace (..),
  ImportCanonicalState (importCanonicalState),
  importNamespaceFromHandle,
 )
import Cardano.Ledger.CanonicalState.Namespace.Blocks.V0 (BlockIn (BlockIn), BlockOut (BlockOut))
import Cardano.Ledger.CanonicalState.Namespace.EntitiesAccounts.V0 (
  EntitiesAccountsIn (EntitiesAccountsIn),
  EntitiesAccountsOut (EntitiesAccountsOut),
 )
import Cardano.Ledger.CanonicalState.Namespace.EntitiesCommittee.V0 (
  CanonicalCommitteeState (CanonicalCommitteeState),
  EntitiesCommitteeIn (..),
  EntitiesCommitteeOut (..),
  fromCanonicalCommitteeAuthorization,
 )
import Cardano.Ledger.CanonicalState.Namespace.EntitiesDReps.V0 (
  EntitiesDRepsIn (EntitiesDRepsIn),
  EntitiesDRepsOut (EntitiesDRepsOut),
  fromCanonicalDRepState,
 )
import Cardano.Ledger.CanonicalState.Namespace.EntitiesStakePools.V0 (
  CanonicalStakePool (CanonicalStakePool),
  EntitiesStakePoolsIn (EntitiesStakePoolsIn),
  EntitiesStakePoolsOut (EntitiesStakePoolsOut),
  fromCanonicalStakePoolParams,
  fromCanonicalStakePoolState,
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
  CanonicalConstitution (..),
  GovConstitutionIn (..),
  GovConstitutionOut (..),
 )
import Cardano.Ledger.CanonicalState.Namespace.GovPParams.V0 (
  GovPParamsIn (..),
  GovPParamsOut (..),
 )
import Cardano.Ledger.CanonicalState.Namespace.GovProposals.Roots.V0 (
  GovProposalsRootsIn (..),
  GovProposalsRootsOut (..),
 )
import Cardano.Ledger.CanonicalState.Namespace.UTxO.V0 (
  UtxoIn (UtxoKeyIn),
  UtxoOut (UtxoOut),
 )
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  Constitution (..),
  ConwayEraGov (constitutionGovStateL),
  GovPurposeId (GovPurposeId),
  cgsCommitteeL,
  cgsProposalsL,
  fromPrevGovActionIds,
  grCommitteeL,
  grConstitutionL,
  grHardForkL,
  grPParamUpdateL,
  mkProposals,
  pPropsL,
  pRootsL,
  toPrevGovActionIds,
 )
import Cardano.Ledger.Conway.State (
  CanSetAccounts (accountsL),
  CanSetUTxO (utxoL),
  ConwayEraCertState (certVStateL),
  EraAccounts (accountsMapL),
  EraCertState (certDStateL, certPStateL),
  FuturePParams (..),
  UTxO (UTxO),
  csCommitteeCredsL,
  psFutureStakePoolParamsL,
  psRetiringL,
  psStakePoolsL,
  psVRFKeyHashesL,
  vsCommitteeStateL,
  vsDRepsL,
 )
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState (..),
  curPParamsEpochStateL,
  esLStateL,
  futurePParamsEpochStateL,
  lsCertStateL,
  lsUTxOStateL,
  nesBcurL,
  nesEpochStateL,
  nesEsL,
  newEpochStateGovStateL,
  prevPParamsEpochStateL,
  totalObligation,
  utxosDepositedL,
 )
import Cardano.SCLS.Internal.Entry.ChunkEntry (
  ChunkEntry (ChunkEntry),
 )
import Cardano.SCLS.Internal.Reader (
  withLatestManifestFrame,
 )
import Cardano.SCLS.Internal.Record.Manifest (Manifest (..))
import qualified Cardano.Types.SlotNo as SSlotNo
import Data.Data (Proxy (Proxy))
import Data.Default (def)
import Data.List (sortOn)
import qualified Data.Map as Map
import qualified Data.OMap.Strict as OMap
import GHC.TypeLits (KnownSymbol)
import Lens.Micro ((&), (.~), (^.))
import qualified Streaming.Prelude as S
import System.IO (Handle, IOMode (ReadMode), withBinaryFile)

instance ImportCanonicalNamespace ConwayEra "utxo/v0" where
  importNamespaceEntries nes =
    S.fold_
      (\utxos (ChunkEntry (UtxoKeyIn k) (UtxoOut v)) -> Map.insert k (getValue v) utxos)
      mempty
      (\utxos -> nes & utxoL .~ UTxO utxos)

instance ImportCanonicalNamespace ConwayEra "blocks/v0" where
  importNamespaceEntries nes =
    S.fold_
      ( \blocks (ChunkEntry (BlockIn keyHash _epochNo) (BlockOut n)) ->
          Map.insert keyHash n blocks
      )
      mempty
      (\blocks -> nes & nesBcurL .~ blocks)

instance ImportCanonicalNamespace ConwayEra "entities/committee/v0" where
  importNamespaceEntries nes s =
    maybe
      nes
      ( \( ChunkEntry
             EntitiesCommitteeIn
             (EntitiesCommitteeOut (CanonicalCommitteeState canonicalCreds))
           ) ->
            nes
              & nesEpochStateL
                . esLStateL
                . lsCertStateL
                . certVStateL
                . vsCommitteeStateL
                . csCommitteeCredsL
                .~ Map.map fromCanonicalCommitteeAuthorization canonicalCreds
      )
      <$> S.head_ s

instance ImportCanonicalNamespace ConwayEra "gov/committee/v0" where
  importNamespaceEntries nes s =
    maybe
      nes
      ( \( ChunkEntry
             GovCommitteeIn
             (GovCommitteeOut mCommittee)
           ) ->
            nes
              & newEpochStateGovStateL
                . cgsCommitteeL
                .~ fmap (\CanonicalCommittee {..} -> Committee {..}) mCommittee
      )
      <$> S.head_ s

instance ImportCanonicalNamespace ConwayEra "gov/constitution/v0" where
  importNamespaceEntries nes s =
    maybe
      nes
      ( \(ChunkEntry GovConstitutionIn (GovConstitutionOut CanonicalConstitution {..})) ->
          nes & newEpochStateGovStateL . constitutionGovStateL .~ Constitution {..}
      )
      <$> S.head_ s

instance ImportCanonicalNamespace ConwayEra "gov/pparams/v0" where
  importNamespaceEntries nes =
    S.fold_
      ( \es (ChunkEntry key (GovPParamsOut pp)) -> case key of
          GovPParamsInPrev -> es & prevPParamsEpochStateL .~ pp
          GovPParamsInCurr -> es & curPParamsEpochStateL .~ pp
          GovPParamsInPossibleFuture -> es & futurePParamsEpochStateL .~ PotentialPParamsUpdate (Just pp)
          GovPParamsInDefiniteFuture -> es & futurePParamsEpochStateL .~ DefinitePParamsUpdate pp
      )
      (nes ^. nesEsL)
      (\es -> nes & nesEsL .~ es)

instance ImportCanonicalNamespace ConwayEra "gov/proposals/v0" where
  importNamespaceEntries nes s = do
    proposals <-
      S.map
        (\(ChunkEntry govIn govOut) -> toGovActionState (govIn, govOut))
        s
        & S.toList_
    let proposalsMap = OMap.fromFoldable . map snd . sortOn fst $ proposals
    pure $ nes & newEpochStateGovStateL . cgsProposalsL . pPropsL .~ proposalsMap

instance ImportCanonicalNamespace ConwayEra "gov/proposals/roots/v0" where
  importNamespaceEntries nes = do
    S.fold_
      ( \govRelation
         (ChunkEntry govProposalsRootsIn (GovProposalsRootsOut canonicalGovActionId)) ->
            case govProposalsRootsIn of
              GovProposalsRootsInPParamUpdate ->
                govRelation
                  & grPParamUpdateL
                    .~ SJust (GovPurposeId (fromCanonicalGovActionId canonicalGovActionId))
              GovProposalsRootsInHardFork ->
                govRelation
                  & grHardForkL
                    .~ SJust (GovPurposeId (fromCanonicalGovActionId canonicalGovActionId))
              GovProposalsRootsInCommittee ->
                govRelation
                  & grCommitteeL
                    .~ SJust (GovPurposeId (fromCanonicalGovActionId canonicalGovActionId))
              GovProposalsRootsInConstitution ->
                govRelation
                  & grConstitutionL
                    .~ SJust (GovPurposeId (fromCanonicalGovActionId canonicalGovActionId))
      )
      def
      ( \govRelation ->
          nes
            & newEpochStateGovStateL
              . cgsProposalsL
              . pRootsL
              .~ fromPrevGovActionIds govRelation
      )

instance ImportCanonicalNamespace ConwayEra "entities/accounts/v0" where
  importNamespaceEntries nes =
    S.fold_
      ( \accountsMap
         (ChunkEntry (EntitiesAccountsIn credential) (EntitiesAccountsOut accountState)) ->
            Map.insert credential (fromCanonicalAccountState accountState) accountsMap
      )
      mempty
      ( \accountsMap ->
          nes
            & nesEsL
              . esLStateL
              . lsCertStateL
              . certDStateL
              . accountsL
              . accountsMapL
              .~ accountsMap
      )

instance ImportCanonicalNamespace ConwayEra "entities/dreps/v0" where
  importNamespaceEntries nes =
    S.fold_
      ( \dreps
         ( ChunkEntry
             (EntitiesDRepsIn cred)
             (EntitiesDRepsOut canonicalDRepState)
           ) ->
            Map.insert cred (fromCanonicalDRepState canonicalDRepState) dreps
      )
      mempty
      ( \dreps ->
          nes
            & nesEsL
              . esLStateL
              . lsCertStateL
              . certVStateL
              . vsDRepsL
              .~ dreps
      )

instance ImportCanonicalNamespace ConwayEra "entities/stake_pools/v0" where
  importNamespaceEntries nes =
    S.fold_
      ( \(stakePoolStates, retiringEpochNos, futureStakePoolParams)
         ( ChunkEntry
             (EntitiesStakePoolsIn k)
             (EntitiesStakePoolsOut (CanonicalStakePool mStakePoolState mFutureStakePoolParams mRetiringEpochNo))
           ) ->
            let stakePoolStates' =
                  strictMaybe
                    stakePoolStates
                    (flip (Map.insert k) stakePoolStates . fromCanonicalStakePoolState)
                    mStakePoolState
                retiringEpochNos' = strictMaybe retiringEpochNos (flip (Map.insert k) retiringEpochNos) mRetiringEpochNo
                futureStakePoolParams' =
                  strictMaybe
                    futureStakePoolParams
                    (flip (Map.insert k) futureStakePoolParams . fromCanonicalStakePoolParams)
                    mFutureStakePoolParams
             in (stakePoolStates', retiringEpochNos', futureStakePoolParams')
      )
      mempty
      ( \(stakePoolStates, retiringEpochNos, futureStakePoolParams) ->
          nes
            & nesEsL
              . esLStateL
              . lsCertStateL
              . certPStateL
              . psStakePoolsL
              .~ stakePoolStates
            & nesEsL
              . esLStateL
              . lsCertStateL
              . certPStateL
              . psRetiringL
              .~ retiringEpochNos
            & nesEsL
              . esLStateL
              . lsCertStateL
              . certPStateL
              . psFutureStakePoolParamsL
              .~ futureStakePoolParams
      )

instance ImportCanonicalNamespace ConwayEra "entities/stake_pools/vrf_key_hashes/v0" where
  importNamespaceEntries nes =
    S.fold_
      ( \stakePoolsVrfKeyHashes
         ( ChunkEntry
             (EntitiesStakePoolsVRFKeyHashesIn k)
             (EntitiesStakePoolsVRFKeyHashesOut n)
           ) ->
            Map.insert k n stakePoolsVrfKeyHashes
      )
      mempty
      ( \stakePoolsVrfKeyHashes ->
          nes
            & nesEsL
              . esLStateL
              . lsCertStateL
              . certPStateL
              . psVRFKeyHashesL
              .~ stakePoolsVrfKeyHashes
      )

instance ImportCanonicalState ConwayEra where
  importCanonicalState filepath epochNo = do
    flip withLatestManifestFrame filepath $ \Manifest {slotNo} ->
      withBinaryFile filepath ReadMode $ \h ->
        importNs @"utxo/v0" h defaultNes
          >>= importNs @"blocks/v0" h
          >>= importNs @"gov/committee/v0" h
          >>= importNs @"gov/constitution/v0" h
          >>= importNs @"gov/pparams/v0" h
          >>= importNs @"gov/proposals/roots/v0" h
          >>= importNs @"gov/proposals/v0" h
          >>= importNs @"entities/accounts/v0" h
          >>= importNs @"entities/dreps/v0" h
          >>= importNs @"entities/committee/v0" h
          >>= importNs @"entities/stake_pools/v0" h
          >>= importNs @"entities/stake_pools/vrf_key_hashes/v0" h
          >>= \nes -> do
            -- Finalize the state with the data that requires computation/cross-namespace data
            nes' <- computeTotalObligation <$> recreateProposals nes
            pure (SlotNo (SSlotNo.unSlotNo slotNo), nes')
    where
      importNs ::
        forall v.
        (ImportCanonicalNamespace ConwayEra v, KnownSymbol v) =>
        Handle -> State ConwayEra -> IO (State ConwayEra)
      importNs h = importNamespaceFromHandle @ConwayEra h (Proxy :: Proxy v)
      defaultNes =
        (def @(NewEpochState ConwayEra))
          { nesEL = epochNo
          }
      recreateProposals nes = do
        let govState = nes ^. newEpochStateGovStateL
            proposalsMap = govState ^. cgsProposalsL . pPropsL
            prevActionsIds = toPrevGovActionIds (govState ^. cgsProposalsL . pRootsL)
        proposals <- mkProposals prevActionsIds proposalsMap
        pure (nes & newEpochStateGovStateL . cgsProposalsL .~ proposals)
      computeTotalObligation nes =
        let certState = nes ^. nesEsL . esLStateL . lsCertStateL
            govState = nes ^. newEpochStateGovStateL
         in nes
              & nesEsL
                . esLStateL
                . lsUTxOStateL
                . utxosDepositedL
                .~ totalObligation certState govState
