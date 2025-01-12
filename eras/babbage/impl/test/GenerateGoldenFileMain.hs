{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Babbage (BabbageEra)
import Test.Cardano.Ledger.Alonzo.Translation.Golden (generateGoldenFile)
import Test.Cardano.Ledger.Babbage.Translation.TranslatableGen ()

-- | Generates golden translation file for Babbage era
main :: IO ()
main = generateGoldenFile @BabbageEra "eras/babbage/impl/golden/translations.cbor"
