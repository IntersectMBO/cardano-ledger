{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.Golden (
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
import Data.TreeDiff (ToExpr (..))
import Data.Typeable (Proxy (..))
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc)
import Test.Cardano.Ledger.Binary.RoundTrip (embedTripAnnExpectation)
import Test.Hspec (Expectation, expectationFailure, shouldBe)

decodeEnc :: forall a. DecCBOR (Annotator a) => Version -> Enc -> Either DecoderError a
decodeEnc version enc = decodeFullAnnotator @a version (Binary.label $ Proxy @(Annotator a)) decCBOR bytes
  where
    bytes = toLazyByteString $ toCBOR enc

expectDecoderSuccessAnnWith ::
  forall a.
  ( DecCBOR (Annotator a)
  , HasCallStack
  , Show a
  , Eq a
  ) =>
  (a -> a -> Bool) ->
  Version ->
  Enc ->
  a ->
  Expectation
expectDecoderSuccessAnnWith equals version enc expected =
  case decodeEnc @a version enc of
    Left err -> expectationFailure $ "Unexpected decoder failure: " <> show err
    Right x | x `equals` expected -> pure ()
    Right result -> result `shouldBe` expected

expectDecoderSuccessAnn ::
  ( DecCBOR (Annotator a)
  , Eq a
  , HasCallStack
  , Show a
  ) =>
  Version -> Enc -> a -> Expectation
expectDecoderSuccessAnn = expectDecoderSuccessAnnWith (==)

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
        "Expected a failure, but decoder succeeded:\n" <> show (toExpr x)

expectDecoderResultOn ::
  forall a b.
  ( DecCBOR (Annotator a)
  , Eq b
  , HasCallStack
  , Show b
  ) =>
  Version -> Enc -> a -> (a -> b) -> Expectation
expectDecoderResultOn version enc expected f =
  embedTripAnnExpectation
    version
    version
    (\x _ -> f x `shouldBe` f expected)
    enc
