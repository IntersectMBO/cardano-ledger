{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Core.Binary.Golden (
  decodeEnc,
  expectDecoderSuccessAnn,
  expectDecoderFailureAnn,
  expectDecoderResultOn,
) where

import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  DecoderError,
  ToCBOR (..),
  Version,
  decodeFullAnnotator,
  toLazyByteString,
 )
import qualified Cardano.Ledger.Binary as Binary
import Data.Typeable (Proxy (..))
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc)
import Test.Cardano.Ledger.Binary.RoundTrip (embedTripAnnExpectation)
import Test.Cardano.Ledger.Common (
  Expectation,
  expectationFailure,
  shouldBe,
  shouldBeExpr,
  showExpr,
 )
import Test.Cardano.Ledger.TreeDiff (ToExpr)

decodeEnc :: forall a. DecCBOR (Annotator a) => Version -> Enc -> Either DecoderError a
decodeEnc version enc = decodeFullAnnotator @a version (Binary.label $ Proxy @(Annotator a)) decCBOR bytes
  where
    bytes = toLazyByteString $ toCBOR enc

expectDecoderSuccessAnn ::
  forall a.
  (ToExpr a, DecCBOR (Annotator a), HasCallStack, Eq a) =>
  Version ->
  Enc ->
  a ->
  Expectation
expectDecoderSuccessAnn version enc expectedVal =
  case decodeEnc @a version enc of
    Right x -> x `shouldBeExpr` expectedVal
    Left err -> expectationFailure $ "Expected a success, but got a failure:\n" <> show err

expectDecoderFailureAnn ::
  forall a.
  (ToExpr a, DecCBOR (Annotator a), HasCallStack) =>
  Version ->
  Enc ->
  DecoderError ->
  Expectation
expectDecoderFailureAnn version enc expectedErr =
  case decodeEnc @a version enc of
    Left err -> err `shouldBe` expectedErr
    Right x ->
      expectationFailure $
        "Expected a failure, but decoder succeeded:\n"
          <> showExpr x

expectDecoderResultOn ::
  forall a b.
  (ToExpr b, DecCBOR (Annotator a), Eq b, HasCallStack) =>
  Version -> Enc -> a -> (a -> b) -> Expectation
expectDecoderResultOn version enc expected f =
  embedTripAnnExpectation version version (\x _ -> f x `shouldBeExpr` f expected) enc
