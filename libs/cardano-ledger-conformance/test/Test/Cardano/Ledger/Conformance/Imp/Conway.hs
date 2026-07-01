{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Imp.Conway (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway (ConwayEra)
import Test.Cardano.Ledger.Conformance.Imp.Conway.Ratify qualified as RatifySpec
import Test.Cardano.Ledger.Conformance.Imp.Core
import Test.Cardano.Ledger.Conway.Imp.BbodySpec qualified as BBODY
import Test.Cardano.Ledger.Conway.Imp.CertsSpec qualified as CERTS
import Test.Cardano.Ledger.Conway.Imp.DelegSpec qualified as DELEG
import Test.Cardano.Ledger.Conway.Imp.EnactSpec qualified as ENACT
import Test.Cardano.Ledger.Conway.Imp.EpochSpec qualified as EPOCH
import Test.Cardano.Ledger.Conway.Imp.GovCertSpec qualified as GOVCERT
import Test.Cardano.Ledger.Conway.Imp.GovSpec qualified as GOV
import Test.Cardano.Ledger.Conway.Imp.LedgerSpec qualified as LEDGER
import Test.Cardano.Ledger.Conway.Imp.RatifySpec qualified as RATIFY
import Test.Cardano.Ledger.Conway.Imp.UtxoSpec qualified as UTXO
import Test.Cardano.Ledger.Conway.Imp.UtxosSpec qualified as UTXOS
import Test.Cardano.Ledger.Conway.Imp.UtxowSpec qualified as UTXOW
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common hiding (Args)

spec :: Spec
spec = do
  describe "Imp" $ do
    withImpInit @(LedgerSpec ConwayEra) $
      modifyImpInitProtVer @ConwayEra (natVersion @11) $
        modifyImpInitPostSubmitTxHook submitTxConformanceHook $ do
          modifyImpInitPostEpochBoundaryHook epochBoundaryConformanceHook $ do
            BBODY.spec
            CERTS.spec
            DELEG.spec
            ENACT.spec
            EPOCH.spec
            GOV.spec
            GOVCERT.spec
            LEDGER.spec
            RATIFY.spec
            UTXO.spec
            UTXOW.spec
            UTXOS.spec
  describe "Imp (only spec)" $ do
    RatifySpec.spec
