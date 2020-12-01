
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.TranslationTools
  ( translationCompat
  , translationCompatToCBOR
  , decodeTest
  , decodeTestAnn
  ) where


import Cardano.Ledger.Era (PreviousEra, TranslateEra (..), TranslationContext)

import Cardano.Binary
   (Encoding, ToCBOR (..), FromCBOR (..), serializeEncoding
   , decodeFull
   , serialize
   , Annotator
   , decodeAnnotator
   , DecoderError
   )
import Test.Tasty.HUnit (Assertion, assertFailure)
import Control.Monad.Except (runExcept)

translate ::
  forall era f.
  (TranslateEra era f, Show (TranslationError era f)) =>
  TranslationContext era ->
  f (PreviousEra era) ->
  f era
translate tc fe = right . runExcept $ translateEra @era tc fe
  where
    right = either (\x -> (error $ "translation failure: " <> show x)) id

-- Tests that the serializing before translation or after translating
-- does not change the result
translationCompat ::
  forall era f.
  (TranslateEra era f, Show (TranslationError era f)) =>
  TranslationContext era ->
  (f era -> Encoding) ->
  (f (PreviousEra era) -> Encoding) ->
  f (PreviousEra era) ->
  Bool
translationCompat tc encodeThisEra encodePreviousEra x =
  (serializeEncoding $ encodePreviousEra x)
    == (serializeEncoding . encodeThisEra $ translate @era tc x)

-- Tests that the serializing before translation or after translating
-- does not change the result
translationCompatToCBOR ::
  forall proxy era f.
  ( TranslateEra era f,
    ToCBOR (f era),
    ToCBOR (f (PreviousEra era)),
    Show (TranslationError era f)
  ) =>
  proxy era ->
  TranslationContext era ->
  f (PreviousEra era) ->
  Bool
translationCompatToCBOR _ tc = translationCompat @era tc toCBOR toCBOR

-- Tests that the type a can be decoded as b
decodeTest :: forall a b proxy. (ToCBOR a, FromCBOR b)
   => proxy b -> a -> Assertion
decodeTest _ x = case decodeFull (serialize x) :: Either DecoderError b of
  Left e -> assertFailure $ show e
  Right _ -> return ()

-- Tests that the type a can be decoded as b
decodeTestAnn :: forall a b proxy. (ToCBOR a, FromCBOR (Annotator b))
   => proxy b -> a -> Assertion
decodeTestAnn _ x = case decodeAnnotator mempty fromCBOR (serialize x) :: Either DecoderError b of
  Left e -> assertFailure $ show e
  Right _ -> return ()
