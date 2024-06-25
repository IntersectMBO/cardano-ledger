{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Babel (Babel)
import Test.Cardano.Ledger.Alonzo.Translation.Golden (generateGoldenFile)
import Test.Cardano.Ledger.Babel.Arbitrary ()
import Test.Cardano.Ledger.Babel.Translation.TranslatableGen ()

-- | Generates golden translation file for Babel era
main :: IO ()
main = generateGoldenFile @Babel "eras/conway/test-suite/golden/translations.cbor"
