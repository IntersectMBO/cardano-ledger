{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.ImpTest (
  module Test.Cardano.Ledger.Conway.ImpTest,
  exampleDijkstraGenesis,
  DijkstraEraImp,
  impDijkstraSatisfyNativeScript,
) where

import Cardano.Ledger.Allegra.Scripts (
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Compactible
import Cardano.Ledger.Conway.Governance (ConwayEraGov (..), committeeMembersL)
import Cardano.Ledger.Conway.Rules (
  ConwayCertPredFailure (..),
  ConwayCertsPredFailure (..),
  ConwayDelegPredFailure (..),
 )
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Dijkstra.PParams (UpgradeDijkstraPParams (..))
import Cardano.Ledger.Dijkstra.Rules (DijkstraLedgerPredFailure (..))
import Cardano.Ledger.Dijkstra.Scripts (
  DijkstraNativeScript,
  evalDijkstraNativeScript,
  pattern RequireGuard,
 )
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody (..))
import Cardano.Ledger.Plutus (SLanguage (..))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (ShelleyDelegPredFailure)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.State
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Dijkstra.Era
import Test.Cardano.Ledger.Imp.Common

instance ShelleyEraImp DijkstraEra where
  initGenesis = pure exampleDijkstraGenesis

  initNewEpochState = defaultInitNewEpochState $ \nes ->
    nes
      & nesEsL . epochStateGovStateL . committeeGovStateL %~ fmap updateCommitteeExpiry
    where
      updateCommitteeExpiry =
        committeeMembersL
          %~ fmap (const $ addEpochInterval (impEraStartEpochNo @DijkstraEra) (EpochInterval 15))

  impSatisfyNativeScript = impDijkstraSatisfyNativeScript

  modifyPParams = conwayModifyPParams

  fixupTx = babbageFixupTx
  expectTxSuccess = impBabbageExpectTxSuccess
  modifyImpInitProtVer = conwayModifyImpInitProtVer
  genRegTxCert = dijkstraGenRegTxCert
  genUnRegTxCert = dijkstraGenUnRegTxCert
  delegStakeTxCert = conwayDelegStakeTxCert

instance MaryEraImp DijkstraEra

instance AlonzoEraImp DijkstraEra where
  scriptTestContexts =
    plutusTestScripts SPlutusV1
      <> plutusTestScripts SPlutusV2
      <> plutusTestScripts SPlutusV3

instance BabbageEraImp DijkstraEra

instance ConwayEraImp DijkstraEra

class
  ( ConwayEraImp era
  , DijkstraEraTest era
  ) =>
  DijkstraEraImp era

instance DijkstraEraImp DijkstraEra

-- Partial implementation used for checking predicate failures
instance InjectRuleFailure "LEDGER" ShelleyDelegPredFailure DijkstraEra where
  injectFailure = DijkstraCertsFailure . injectFailure

instance InjectRuleFailure "CERTS" ShelleyDelegPredFailure DijkstraEra where
  injectFailure = CertFailure . injectFailure

instance InjectRuleFailure "CERT" ShelleyDelegPredFailure DijkstraEra where
  injectFailure = DelegFailure . injectFailure

instance InjectRuleFailure "DELEG" ShelleyDelegPredFailure DijkstraEra where
  injectFailure (Shelley.StakeKeyAlreadyRegisteredDELEG c) = StakeKeyRegisteredDELEG c
  injectFailure (Shelley.StakeKeyNotRegisteredDELEG c) = StakeKeyNotRegisteredDELEG c
  injectFailure (Shelley.StakeKeyNonZeroAccountBalanceDELEG c) = StakeKeyHasNonZeroRewardAccountBalanceDELEG c
  injectFailure _ = error "Cannot inject ShelleyDelegPredFailure into DijkstraEra"

exampleDijkstraGenesis :: DijkstraGenesis
exampleDijkstraGenesis =
  DijkstraGenesis
    { dgUpgradePParams =
        UpgradeDijkstraPParams
          { udppMaxRefScriptSizePerBlock = 1024 * 1024 -- 1MiB
          , udppMaxRefScriptSizePerTx = 200 * 1024 -- 200KiB
          , udppRefScriptCostStride = knownNonZeroBounded @25_600 -- 25 KiB
          , udppRefScriptCostMultiplier = fromJust $ boundRational 1.2
          }
    }

impDijkstraSatisfyNativeScript ::
  ( DijkstraEraImp era
  , NativeScript era ~ DijkstraNativeScript era
  ) =>
  Set.Set (KeyHash Witness) ->
  TxBody l era ->
  NativeScript era ->
  ImpTestM era (Maybe (Map.Map (KeyHash Witness) (KeyPair Witness)))
impDijkstraSatisfyNativeScript providedVKeyHashes txBody script = do
  let vi = txBody ^. vldtTxBodyL
  let guards = txBody ^. guardsTxBodyL
  case script of
    RequireSignature keyHash -> impSatisfySignature keyHash providedVKeyHashes
    RequireAllOf ss -> impSatisfyMNativeScripts providedVKeyHashes txBody (length ss) ss
    RequireAnyOf ss -> do
      m <- frequency [(9, pure 1), (1, choose (1, length ss))]
      impSatisfyMNativeScripts providedVKeyHashes txBody m ss
    RequireMOf m ss -> impSatisfyMNativeScripts providedVKeyHashes txBody m ss
    lock@(RequireTimeStart _)
      | evalDijkstraNativeScript mempty vi guards lock -> pure $ Just mempty
      | otherwise -> pure Nothing
    lock@(RequireTimeExpire _)
      | evalDijkstraNativeScript mempty vi guards lock -> pure $ Just mempty
      | otherwise -> pure Nothing
    -- TODO: actual satisfy the native scripts by updating the transaction's guards
    ns@(RequireGuard _)
      | evalDijkstraNativeScript mempty vi guards ns -> pure $ Just mempty
      | otherwise -> pure Nothing
    _ -> error "Impossible: All NativeScripts should have been accounted for"

dijkstraGenRegTxCert ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxCert era
  ) =>
  Credential Staking ->
  ImpTestM era (TxCert era)
dijkstraGenRegTxCert stakingCredential =
  RegDepositTxCert stakingCredential
    <$> getsNES (nesEsL . curPParamsEpochStateL . ppKeyDepositL)

dijkstraGenUnRegTxCert ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxCert era
  ) =>
  Credential Staking ->
  ImpTestM era (TxCert era)
dijkstraGenUnRegTxCert stakingCredential = do
  accounts <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
  deposit <- case lookupAccountState stakingCredential accounts of
    Nothing -> getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
    Just accountState -> pure (fromCompact (accountState ^. depositAccountStateL))
  pure $ UnRegDepositTxCert stakingCredential deposit
