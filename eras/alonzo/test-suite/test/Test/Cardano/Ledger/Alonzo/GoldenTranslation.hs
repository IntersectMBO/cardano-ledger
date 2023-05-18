{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Deserializes `TranslationInstance`s from golden/translations.cbor file.
--
-- Each instance represents arguments passed to `ExtendedUTxO.txInfo` along with the produced result.
-- This test checks that calling `alonzoTxInfo` with the arguments from this file, produces the same result as in the flie.
--
-- To regenerate the golden file (for example, if the logic in the translation changes),
-- run the following command from the root of the repository:
-- cabal run cardano-ledger-alonzotest:gen-golden"
module Test.Cardano.Ledger.Alonzo.GoldenTranslation (
  tests,
)
where

import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

import Cardano.Ledger.Alonzo (Alonzo)
import Paths_cardano_ledger_alonzo_test (getDataFileName)
import Test.Cardano.Ledger.Alonzo.Translation.Golden (TxInfoResultComparison (..), compareGoldenTxInfoResults)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Golden translation tests"
    [testCase "golden/translations.cbor" $ goldenAssertion "golden/translations.cbor"]

goldenAssertion :: String -> Assertion
goldenAssertion file = do
  comps <- compareGoldenTxInfoResults @Alonzo (getDataFileName file)
  mapM_
    ( \(TxInfoResultComparison expected actual err) ->
        assertEqual err expected actual
    )
    comps
