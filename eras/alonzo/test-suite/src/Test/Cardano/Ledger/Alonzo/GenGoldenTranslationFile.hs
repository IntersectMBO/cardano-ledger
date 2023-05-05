{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Core (eraProtVerHigh)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

import Cardano.Ledger.Binary.Encoding (serialize)
import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_alonzo_test ()

import Test.Cardano.Ledger.Alonzo.TranslationInstance (translationInstances)

file :: String
file = "eras/alonzo/test-suite/golden/translations.cbor"

-- | Generates arguments for `alonzoTxInfo`, applies them to `alonzoTxInfo`
-- and serializes both arguments and result to golden/translations.cbor file
main :: IO ()
main = do
  putStrLn "Generating golden files for TxInfo"
  instances <- translationInstances 100
  let cbor = serialize (eraProtVerHigh @Alonzo) instances
  BSL.writeFile file cbor
