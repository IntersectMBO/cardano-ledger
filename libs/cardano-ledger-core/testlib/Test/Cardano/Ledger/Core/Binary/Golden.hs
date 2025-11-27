{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Core.Binary.Golden (
  decodeEnc,
  expectDecoderSuccessAnn,
  expectDecoderSuccessAnnWith,
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
import Test.Cardano.Ledger.Binary.Golden (expectDecoderFailureAnn)
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc)
import Test.Cardano.Ledger.Binary.RoundTrip (embedTripAnnExpectation)
import Test.Cardano.Ledger.Common (
  Expectation,
  diffExprString,
  expectationFailure,
 )
import Test.Cardano.Ledger.TreeDiff (ToExpr, expectExprEqualWithMessage)

decodeEnc :: forall a. DecCBOR (Annotator a) => Version -> Enc -> Either DecoderError a
decodeEnc version enc = decodeFullAnnotator @a version (Binary.label $ Proxy @(Annotator a)) decCBOR bytes
  where
    bytes = toLazyByteString $ toCBOR enc

expectDecoderSuccessAnnWith ::
  forall a.
  (ToExpr a, DecCBOR (Annotator a), HasCallStack) =>
  (a -> a -> Bool) ->
  Version ->
  Enc ->
  a ->
  Expectation
expectDecoderSuccessAnnWith equals version enc expected =
  case decodeEnc @a version enc of
    Left err -> expectationFailure $ "Unexpected decoder failure: " <> show err
    Right x | x `equals` expected -> pure ()
    Right result -> expectationFailure $ diffExprString expected result

expectDecoderSuccessAnn ::
  (ToExpr a, DecCBOR (Annotator a), Eq a, HasCallStack) => Version -> Enc -> a -> Expectation
expectDecoderSuccessAnn = expectDecoderSuccessAnnWith (==)

expectDecoderResultOn ::
  forall a b.
  (ToExpr b, DecCBOR (Annotator a), Eq b, HasCallStack) =>
  Version -> Enc -> a -> (a -> b) -> Expectation
expectDecoderResultOn version enc expected f =
  embedTripAnnExpectation
    version
    version
    (\x _ -> expectExprEqualWithMessage "" (f x) (f expected))
    enc
