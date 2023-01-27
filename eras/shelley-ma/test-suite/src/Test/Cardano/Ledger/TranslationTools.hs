{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- TODO: Move to cardano-ledger-core:test-lib
module Test.Cardano.Ledger.TranslationTools (
  translateEraPartial,
  translateEraEncoding,
  translateEraToCBOR,
)
where

import Cardano.Ledger.Binary (ToCBOR (..), toPlainEncoding)
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Core
import Cardano.Ledger.TreeDiff (diffExpr)
import Control.Monad
import Control.Monad.Except (runExcept)
import GHC.Stack
import Test.Cardano.Ledger.Binary.TreeDiff (CBORBytes (..))
import Test.Tasty.HUnit (Assertion, assertFailure)

translateEraPartial ::
  forall era f.
  (TranslateEra era f, Show (TranslationError era f), HasCallStack) =>
  TranslationContext era ->
  f (PreviousEra era) ->
  f era
translateEraPartial tc fe =
  case runExcept $ translateEra @era tc fe of
    Right result -> result
    Left err -> error $ "TranslateEra failure: " <> show err

-- Tests that the serializing before translation or after translating does not change the
-- result
--
-- FIXME: Replace this test with a better one, since there is no requirement for two
-- different eras to encode the type in the same way. There, however a requirement that
-- the encoding from a previous era, must be decodable by the current era.
translateEraEncoding ::
  forall era f.
  ( HasCallStack
  , TranslateEra era f
  , Show (TranslationError era f)
  ) =>
  TranslationContext era ->
  (f era -> Plain.Encoding) ->
  (f (PreviousEra era) -> Plain.Encoding) ->
  f (PreviousEra era) ->
  Assertion
translateEraEncoding tc encodeThisEra encodePreviousEra x =
  let previousEra =
        Plain.serialize' (encodePreviousEra x)
      currentEra =
        Plain.serialize' (encodeThisEra $ translateEraPartial @era tc x)
   in unless (previousEra == currentEra) $
        assertFailure $
          diffExpr (CBORBytes previousEra) (CBORBytes currentEra)

-- Tests that the serializing before translation or after translating
-- does not change the result
translateEraToCBOR ::
  forall proxy era f.
  ( HasCallStack
  , TranslateEra era f
  , ToCBOR (f era)
  , ToCBOR (f (PreviousEra era))
  , Show (TranslationError era f)
  ) =>
  proxy era ->
  TranslationContext era ->
  f (PreviousEra era) ->
  Assertion
translateEraToCBOR _ tc =
  translateEraEncoding @era
    tc
    (toPlainEncoding (eraProtVerLow @era) . toCBOR)
    (toPlainEncoding (eraProtVerHigh @(PreviousEra era)) . toCBOR)
