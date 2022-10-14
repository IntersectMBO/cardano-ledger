{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- TODO: Move to cardano-ledger-core:test-lib
module Test.Cardano.Ledger.TranslationTools
  ( translateEraPartial,
    translateEraEncoding,
    translateEraToCBOR,
  )
where

import Cardano.Ledger.Binary
  ( Encoding,
    ToCBOR (..),
    serializeEncoding',
  )
import Cardano.Ledger.Core
import Control.Monad
import Control.Monad.Except (runExcept)
import GHC.Stack
import Test.Cardano.Ledger.Binary.TreeDiff
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

-- Tests that the serializing before translation or after translating
-- does not change the result
translateEraEncoding ::
  forall era f.
  ( HasCallStack,
    TranslateEra era f,
    Show (TranslationError era f)
  ) =>
  TranslationContext era ->
  (f era -> Encoding) ->
  (f (PreviousEra era) -> Encoding) ->
  f (PreviousEra era) ->
  Assertion
translateEraEncoding tc encodeThisEra encodePreviousEra x =
  let previousEra =
        serializeEncoding' (eraProtVerHigh @(PreviousEra era)) (encodePreviousEra x)
      currentEra =
        serializeEncoding' (eraProtVerLow @era) (encodeThisEra $ translateEraPartial @era tc x)
   in unless (previousEra == currentEra) $
        assertFailure $
          diffExpr (CBORBytes previousEra) (CBORBytes currentEra)

-- Tests that the serializing before translation or after translating
-- does not change the result
translateEraToCBOR ::
  forall proxy era f.
  ( HasCallStack,
    TranslateEra era f,
    ToCBOR (f era),
    ToCBOR (f (PreviousEra era)),
    Show (TranslationError era f)
  ) =>
  proxy era ->
  TranslationContext era ->
  f (PreviousEra era) ->
  Assertion
translateEraToCBOR _ tc = translateEraEncoding @era tc toCBOR toCBOR
