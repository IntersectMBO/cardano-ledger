{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Binary.GoldenSpec (spec) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core (eraProtVerLow)
import Paths_cardano_ledger_alonzo (getDataFileName)
import qualified Test.Cardano.Ledger.Alonzo.Binary.Golden as BinaryGolden
import Test.Cardano.Ledger.Alonzo.Examples (ledgerExamples)
import Test.Cardano.Ledger.Binary.Golden (cborAnnGoldenSpec)
import Test.Cardano.Ledger.Common (Spec)
import Test.Cardano.Ledger.Shelley.Examples (LedgerExamples (..))

spec :: Spec
spec = do
  cborAnnGoldenSpec
    getDataFileName
    "golden/tx.cbor"
    (eraProtVerLow @AlonzoEra)
    (leTx ledgerExamples)
  BinaryGolden.spec @AlonzoEra
