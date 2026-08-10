{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Imp.Conway (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway (ConwayEra)
import Test.Cardano.Ledger.Alonzo.Imp.BbodySpec qualified as AlonzoBBODY
import Test.Cardano.Ledger.Alonzo.Imp.UtxoSpec qualified as AlonzoUTXO
import Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec qualified as AlonzoUTXOS
import Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec qualified as AlonzoUTXOW
import Test.Cardano.Ledger.Babbage.Imp.UtxoSpec qualified as BabbageUTXO
import Test.Cardano.Ledger.Babbage.Imp.UtxosSpec qualified as BabbageUTXOS
import Test.Cardano.Ledger.Babbage.Imp.UtxowSpec qualified as BabbageUTXOW
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
import Test.Cardano.Ledger.Mary.Imp.UtxoSpec qualified as MaryUTXO
import Test.Cardano.Ledger.Shelley.Imp.DelegSpec qualified as ShelleyDELEG
import Test.Cardano.Ledger.Shelley.Imp.EpochSpec qualified as ShelleyEPOCH
import Test.Cardano.Ledger.Shelley.Imp.LedgerSpec qualified as ShelleyLEDGER
import Test.Cardano.Ledger.Shelley.Imp.PoolSpec qualified as ShelleyPOOL
import Test.Cardano.Ledger.Shelley.Imp.UtxoSpec qualified as ShelleyUTXO
import Test.Cardano.Ledger.Shelley.Imp.UtxowSpec qualified as ShelleyUTXOW

spec :: Spec
spec = do
  describe "Imp" $ do
    withImpInit @(LedgerSpec ConwayEra) $
      modifyImpInitProtVer @ConwayEra (natVersion @11) $
        modifyImpInitPostSubmitTxHook submitTxConformanceHook $ do
          modifyImpInitPostEpochBoundaryHook epochBoundaryConformanceHook $ do
            AlonzoBBODY.spec
            BBODY.spec

            CERTS.spec

            ShelleyDELEG.spec
            DELEG.spec

            ENACT.spec

            ShelleyEPOCH.spec
            EPOCH.spec

            GOV.spec

            GOVCERT.spec

            ShelleyLEDGER.spec
            LEDGER.spec

            xdescribe "disabled" ShelleyPOOL.spec

            RATIFY.spec

            ShelleyUTXO.spec
            MaryUTXO.spec
            AlonzoUTXO.spec
            BabbageUTXO.spec
            UTXO.spec

            ShelleyUTXOW.spec
            AlonzoUTXOW.spec
            BabbageUTXOW.spec
            UTXOW.spec

            AlonzoUTXOS.spec
            BabbageUTXOS.spec
            UTXOS.spec
  describe "Imp (only spec)" $ do
    RatifySpec.spec
