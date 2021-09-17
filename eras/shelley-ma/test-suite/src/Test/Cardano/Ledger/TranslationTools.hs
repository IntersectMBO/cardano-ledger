{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.TranslationTools
  ( translationCompat,
    translationCompatToCBOR,
    decodeTest,
    decodeTestAnn,
    expectDecodeFailure,
  )
where

import Cardano.Binary
  ( Annotator,
    DecoderError,
    Encoding,
    FromCBOR (..),
    ToCBOR (..),
    decodeAnnotator,
    decodeFull,
    serialize,
    serializeEncoding,
  )
import Cardano.Ledger.Era (PreviousEra, TranslateEra (..), TranslationContext)
import Control.Monad.Except (runExcept)
import qualified Data.ByteString.Base16.Lazy as B16
import Test.Tasty.HUnit (Assertion, assertFailure)

translate ::
  forall era f.
  (TranslateEra era f, Show (TranslationError era f)) =>
  TranslationContext era ->
  f (PreviousEra era) ->
  f era
translate tc fe = right . runExcept $ translateEra @era tc fe
  where
    right = either (\x -> error $ "translation failure: " <> show x) id

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
  serializeEncoding (encodePreviousEra x)
    == serializeEncoding (encodeThisEra $ translate @era tc x)

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
decodeTest ::
  forall a b proxy.
  (ToCBOR a, FromCBOR b) =>
  proxy b ->
  a ->
  Assertion
decodeTest _ x = case decodeFull (serialize x) :: Either DecoderError b of
  Left e -> assertFailure $ show e
  Right _ -> return ()

-- Tests that the type a can be decoded as b
decodeTestAnn ::
  forall a b proxy.
  (ToCBOR a, FromCBOR (Annotator b)) =>
  proxy b ->
  a ->
  Assertion
decodeTestAnn _ x =
  let bytes = serialize x
      decoded = decodeAnnotator mempty fromCBOR bytes :: Either DecoderError b
   in case decoded of
        Left e ->
          assertFailure $
            "\nerror: " <> show e
              <> "\nbytes: "
              <> show (B16.encode bytes)
              <> "\n"
        Right _ -> return ()

-- Tests that a decoder error happens
expectDecodeFailure :: forall a. (ToCBOR a, FromCBOR a) => a -> Assertion
expectDecodeFailure x = case decodeFull (serialize x) :: Either DecoderError a of
  Left _ -> pure ()
  Right _ -> assertFailure "should not deserialize"
