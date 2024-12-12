{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Test.Cardano.Ledger.Alonzo.Translation.Golden (generateGoldenFile)
import Test.Cardano.Ledger.Alonzo.Translation.TranslatableGen ()

-- | Generates golden translation file for Alonzo era
main :: IO ()
main = generateGoldenFile @AlonzoEra "eras/alonzo/impl/golden/translations.cbor"
