{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Test.Cardano.Ledger.Alonzo.Examples.Bbody (bbodyExamples)
import Test.Cardano.Ledger.Alonzo.Examples.Utxow (plutusScriptExamples, utxowExamples)
import Test.Cardano.Ledger.Alonzo.Golden as Golden
import qualified Test.Cardano.Ledger.Alonzo.Serialisation.CDDL as CDDL
import qualified Test.Cardano.Ledger.Alonzo.Serialisation.Tripping as Tripping
import qualified Test.Cardano.Ledger.Alonzo.Translation as Translation
import Test.Cardano.Ledger.EraBuffet (TestCrypto)
import Test.QuickCheck
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Tasty

-- ====================================================================================

tests :: TestTree
tests =
  testGroup
    "Alonzo tests"
    [ Tripping.tests,
      Translation.tests,
      CDDL.tests 5,
      Golden.goldenUTxOEntryMinAda,
      plutusScriptExamples,
      bbodyExamples,
      utxowExamples
    ]

main :: IO ()
main = defaultMain tests
