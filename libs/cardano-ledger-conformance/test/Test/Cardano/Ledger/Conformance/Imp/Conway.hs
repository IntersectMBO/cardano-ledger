{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Imp.Conway (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway (ConwayEra)
import Test.Cardano.Ledger.Conformance.Imp.Conway.Ratify qualified as RatifySpec
import Test.Cardano.Ledger.Conformance.Imp.Core
import Test.Cardano.Ledger.Conway.Imp.BbodySpec qualified as Bbody
import Test.Cardano.Ledger.Conway.Imp.CertsSpec qualified as Certs
import Test.Cardano.Ledger.Conway.Imp.DelegSpec qualified as Deleg
import Test.Cardano.Ledger.Conway.Imp.EnactSpec qualified as Enact
import Test.Cardano.Ledger.Conway.Imp.EpochSpec qualified as Epoch
import Test.Cardano.Ledger.Conway.Imp.GovCertSpec qualified as GovCert
import Test.Cardano.Ledger.Conway.Imp.GovSpec qualified as Gov
import Test.Cardano.Ledger.Conway.Imp.LedgerSpec qualified as Ledger
import Test.Cardano.Ledger.Conway.Imp.RatifySpec qualified as Ratify
import Test.Cardano.Ledger.Conway.Imp.UtxoSpec qualified as Utxo
import Test.Cardano.Ledger.Conway.Imp.UtxosSpec qualified as Utxos
import Test.Cardano.Ledger.Conway.Imp.UtxowSpec qualified as Utxow
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common hiding (Args)
import UnliftIO (evaluateDeep)

spec :: Spec
spec = do
  describe "Imp" $ do
    withImpInit @(LedgerSpec ConwayEra) $
      modifyImpInitProtVer @ConwayEra (natVersion @10) $
        modifyImpInitPostSubmitTxHook submitTxConformanceHook $ do
          modifyImpInitPostEpochBoundaryHook epochBoundaryConformanceHook $ do
            describe "Basic" $ do
              it "Submit constitution" $ do
                _ <- submitConstitution @ConwayEra SNothing
                passNEpochs 2
              it "Can elect a basic committee" $ do
                void $ evaluateDeep =<< electBasicCommittee
            describe "BBODY" Bbody.spec
            describe "CERTS" Certs.spec
            describe "DELEG" Deleg.spec
            describe "ENACT" Enact.spec
            describe "EPOCH" Epoch.spec
            describe "GOV" Gov.spec
            describe "GOVCERT" GovCert.spec
            describe "LEDGER" Ledger.spec
            describe "RATIFY" Ratify.spec
            describe "UTXO" Utxo.spec
            describe "UTXOW" Utxow.spec
            xdescribe "UTXOS" Utxos.spec
  describe "Imp (only spec)" $ do
    RatifySpec.spec
