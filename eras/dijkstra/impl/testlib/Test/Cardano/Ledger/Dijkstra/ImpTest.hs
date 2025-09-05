{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.ImpTest (
  module Test.Cardano.Ledger.Conway.ImpTest,
  exampleDijkstraGenesis,
  DijkstraEraImp,
) where

import Cardano.Ledger.BaseTypes (
  BoundedRational (..),
  EpochInterval (..),
  addEpochInterval,
  knownNonZeroBounded,
 )
import Cardano.Ledger.Conway.Governance (ConwayEraGov (..), committeeMembersL)
import Cardano.Ledger.Conway.Rules (
  ConwayCertPredFailure (..),
  ConwayCertsPredFailure (..),
  ConwayDelegPredFailure (..),
  ConwayLedgerPredFailure (..),
 )
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Dijkstra.PParams (UpgradeDijkstraPParams (..))
import Cardano.Ledger.Plutus (SLanguage (..))
import Cardano.Ledger.Shelley.LedgerState (epochStateGovStateL, nesEsL)
import Cardano.Ledger.Shelley.Rules (ShelleyDelegPredFailure)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Data.Maybe (fromJust)
import Lens.Micro ((%~), (&))
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Dijkstra.Era

instance ShelleyEraImp DijkstraEra where
  initGenesis = pure exampleDijkstraGenesis

  initNewEpochState = defaultInitNewEpochState $ \nes ->
    nes
      & nesEsL . epochStateGovStateL . committeeGovStateL %~ fmap updateCommitteeExpiry
    where
      updateCommitteeExpiry =
        committeeMembersL
          %~ fmap (const $ addEpochInterval (impEraStartEpochNo @DijkstraEra) (EpochInterval 15))

  impSatisfyNativeScript = impAllegraSatisfyNativeScript

  modifyPParams = conwayModifyPParams

  fixupTx = babbageFixupTx
  expectTxSuccess = impBabbageExpectTxSuccess
  modifyImpInitProtVer = conwayModifyImpInitProtVer

instance MaryEraImp DijkstraEra

instance AlonzoEraImp DijkstraEra where
  scriptTestContexts =
    plutusTestScripts SPlutusV1
      <> plutusTestScripts SPlutusV2
      <> plutusTestScripts SPlutusV3

instance ConwayEraImp DijkstraEra

class
  ( ConwayEraImp era
  , DijkstraEraTest era
  ) =>
  DijkstraEraImp era

instance DijkstraEraImp DijkstraEra

-- Partial implementation used for checking predicate failures
instance InjectRuleFailure "LEDGER" ShelleyDelegPredFailure DijkstraEra where
  injectFailure = ConwayCertsFailure . injectFailure

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
