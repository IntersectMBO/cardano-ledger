module Test.Cardano.Ledger.Conway.Conformance.ExecutableSpecRule () where
import Cardano.Ledger.Conway (ConwayEra)

type instance EraFn (ConwayEra c) = ConwayFn

