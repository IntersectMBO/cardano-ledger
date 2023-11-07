{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Babbage (Babbage)
import Test.Cardano.Ledger.Alonzo.Translation.Golden (generateGoldenFile)
import Test.Cardano.Ledger.Babbage.Translation.TranslatableGen ()

-- | Generates golden translation file for Babbage era
main :: IO ()
main = generateGoldenFile @Babbage "eras/babbage/test-suite/golden/translations.cbor"
