{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Conway (ConwayEra)
import Test.Cardano.Ledger.Alonzo.Translation.Golden (generateGoldenFile)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.Translation.TranslatableGen ()

-- | Generates golden translation file for Conway era
main :: IO ()
main = generateGoldenFile @ConwayEra "eras/conway/impl/golden/translations.cbor"
