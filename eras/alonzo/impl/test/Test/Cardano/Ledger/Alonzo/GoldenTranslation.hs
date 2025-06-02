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
-- This test checks that calling `alonzoTxInfo` with the arguments from this file, produces the same result as in the file.
--
-- To regenerate the golden file (for example, if the logic in the translation changes),
-- run the following command from the root of the repository:
-- cabal run cardano-ledger-alonzo:gen-golden"
module Test.Cardano.Ledger.Alonzo.GoldenTranslation (
  tests,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Paths_cardano_ledger_alonzo (getDataFileName)
import Test.Cardano.Ledger.Alonzo.Binary.Annotator ()
import Test.Cardano.Ledger.Alonzo.Translation.Golden (assertTranslationResultsMatchGolden)
import Test.Cardano.Ledger.Common
import Test.HUnit (Assertion)

tests :: Spec
tests =
  describe "Golden translation tests" $ do
    it "golden/translations.cbor" $
      goldenAssertion "golden/translations.cbor"

goldenAssertion :: String -> Assertion
goldenAssertion file = assertTranslationResultsMatchGolden @AlonzoEra (getDataFileName file)
