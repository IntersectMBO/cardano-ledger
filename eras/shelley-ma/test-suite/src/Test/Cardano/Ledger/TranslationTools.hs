{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- TODO: Move to cardano-ledger-core:test-lib
module Test.Cardano.Ledger.TranslationTools (
  translateEraPartial,
  translateEraEncoding,
  translateEraEncCBOR,
)
where

import Cardano.Ledger.Binary (EncCBOR (..), toPlainEncoding)
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Core
import Cardano.Ledger.TreeDiff (diffExprNoColor)
import Control.Monad
import Control.Monad.Except (runExcept)
import GHC.Stack
import Test.Cardano.Ledger.Binary.TreeDiff (CBORBytes (..))
import Test.Tasty.HUnit (Assertion, assertFailure)

translateEraPartial ::
  forall era f.
  (TranslateEra era f, Show (TranslationError era f), HasCallStack) =>
  TranslationContextF era f ->
  f (PreviousEra era) ->
  f era
translateEraPartial tc fe =
  case runExcept $ translateEra @era tc fe of
    Right result -> result
    Left err -> error $ "TranslateEra failure: " <> show err

-- Tests that the serializing before translation or after translating does not change the
-- result
--
-- FIXME: This property only holds for annotated types. Replace this test with a better
-- one, since there is no requirement for two different eras to encode the type in the
-- same way. There is, however a requirement that the encoding from a previous era, must
-- be decodable by the current era.
translateEraEncoding ::
  forall era f.
  ( HasCallStack
  , TranslateEra era f
  , Show (TranslationError era f)
  ) =>
  TranslationContextF era f ->
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
          diffExprNoColor (CBORBytes previousEra) (CBORBytes currentEra)

-- Tests that the serializing before translation or after translating
-- does not change the result
translateEraEncCBOR ::
  forall proxy era f.
  ( HasCallStack
  , TranslateEra era f
  , EncCBOR (f era)
  , EncCBOR (f (PreviousEra era))
  , Show (TranslationError era f)
  ) =>
  proxy era ->
  TranslationContextF era f ->
  f (PreviousEra era) ->
  Assertion
translateEraEncCBOR _ tc =
  translateEraEncoding @era
    tc
    (toEraCBOR @era)
    (toPlainEncoding (eraProtVerHigh @(PreviousEra era)) . encCBOR)
