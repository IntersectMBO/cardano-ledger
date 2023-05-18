{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Language (Language (..))
import Test.Cardano.Ledger.Alonzo.Translation.Golden (generateGoldenFile)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.Translation.TranslatableGen ()

-- | Generates golden translation file for Conway era
main :: IO ()
main = generateGoldenFile @Conway [PlutusV1, PlutusV2, PlutusV3] "eras/conway/test-suite/golden/translations.cbor"
