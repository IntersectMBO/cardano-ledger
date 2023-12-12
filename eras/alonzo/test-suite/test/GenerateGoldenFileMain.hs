{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Alonzo (Alonzo)
import Test.Cardano.Ledger.Alonzo.Translation.Golden (generateGoldenFile)
import Test.Cardano.Ledger.Alonzo.Translation.TranslatableGen ()

-- | Generates golden translation file for Alonzo era
main :: IO ()
main = generateGoldenFile @Alonzo "eras/alonzo/test-suite/golden/translations.cbor"
