{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.ImpTest (
  module Test.Cardano.Ledger.Conway.ImpTest,
  exampleDijkstraGenesis,
  DijkstraEraImp,
  dijkstraGenRegTxCert,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Governance (ConwayEraGov (..), committeeMembersL)
import Cardano.Ledger.Conway.Rules (
  ConwayCertPredFailure (..),
  ConwayCertsPredFailure (..),
  ConwayDelegPredFailure (..),
  ConwayLedgerPredFailure (..),
 )
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Dijkstra.PParams (UpgradeDijkstraPParams (..))
import Cardano.Ledger.Plutus (SLanguage (..))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (ShelleyDelegPredFailure)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as SSeq
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

  impSatisfyNativeScript = impAllegraSatisfyNativeScript

  modifyPParams = conwayModifyPParams

  fixupTx = babbageFixupTx
  expectTxSuccess = impBabbageExpectTxSuccess
  registerStakeCredential = conwayRegisterStakeCredential
  genRegTxCert = dijkstraGenRegTxCert

instance MaryEraImp DijkstraEra

instance AlonzoEraImp DijkstraEra where
  scriptTestContexts =
    plutusTestScripts SPlutusV1
      <> plutusTestScripts SPlutusV2
      <> plutusTestScripts SPlutusV3

instance ConwayEraImp DijkstraEra where
  delegateToDRep cred stake dRep = do
    deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
    (_, spendingKP) <- freshKeyPair
    let tx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . outputsTxBodyL
              .~ SSeq.singleton (mkBasicTxOut (mkAddr spendingKP cred) (inject stake))
            & bodyTxL . certsTxBodyL
              .~ SSeq.fromList
                [ RegDepositDelegTxCert
                    cred
                    (DelegVote dRep)
                    deposit
                ]
    submitTx_ tx
    ra <- getRewardAccountFor cred
    pure (spendingKP, ra)

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

dijkstraGenRegTxCert ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxCert era
  ) =>
  ImpTestM era (TxCert era, Credential 'Staking)
dijkstraGenRegTxCert = do
  stakingCredential <- KeyHashObj <$> freshKeyHash
  cert <-
    RegDepositTxCert stakingCredential
      <$> getsNES (nesEsL . curPParamsEpochStateL . ppKeyDepositL)
  pure (cert, stakingCredential)
