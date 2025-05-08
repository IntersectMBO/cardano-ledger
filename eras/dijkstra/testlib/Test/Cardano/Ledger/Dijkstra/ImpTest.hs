{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.ImpTest () where

import Cardano.Ledger.Conway.Rules (
  ConwayCertPredFailure (..),
  ConwayCertsPredFailure (..),
  ConwayDelegPredFailure (..),
  ConwayLedgerPredFailure (..),
 )
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Plutus (SLanguage (..))
import Cardano.Ledger.Shelley.Rules (ShelleyDelegPredFailure)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Dijkstra.Era ()

instance ShelleyEraImp DijkstraEra where
  initGenesis = pure ()

  impSatisfyNativeScript = impAllegraSatisfyNativeScript

  modifyPParams = conwayModifyPParams

  fixupTx = babbageFixupTx
  expectTxSuccess = impBabbageExpectTxSuccess

instance MaryEraImp DijkstraEra

instance AlonzoEraImp DijkstraEra where
  scriptTestContexts =
    plutusTestScripts SPlutusV1
      <> plutusTestScripts SPlutusV2
      <> plutusTestScripts SPlutusV3

instance ConwayEraImp DijkstraEra

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
