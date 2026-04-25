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
  toPackageGolden,
  goldenForToCBOR,
  goldenForEncCBOR,
  cborGoldenSpec,
  cborAnnGoldenSpec,
) where

import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  DecoderError,
  EncCBOR (..),
  ToCBOR (..),
  Version,
  decodeFullAnnotator,
  serialize,
  toLazyByteString,
 )
import qualified Cardano.Ledger.Binary as Binary
import qualified Cardano.Ledger.Binary.Plain as Plain
import qualified Data.ByteString.Lazy as BSL
import Data.TreeDiff (ToExpr (..))
import Data.Typeable (Proxy (..), typeOf)
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc)
import Test.Cardano.Ledger.Binary.RoundTrip (
  embedTripAnnExpectation,
  roundTripAnnRangeExpectation,
  roundTripCborRangeExpectation,
 )
import Test.Cardano.Ledger.Binary.TreeDiff (CBORBytes (..))
import Test.Hspec
import qualified Test.Hspec.Golden as Golden (Golden (..))

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

toPackageGolden :: (FilePath -> IO FilePath) -> Golden.Golden g -> IO (Golden.Golden g)
toPackageGolden mkFullPath g = do
  fullPathGoldenFile <- mkFullPath $ Golden.goldenFile g
  fullPathActualFile <- mapM mkFullPath $ Golden.actualFile g
  pure $
    g
      { Golden.goldenFile = fullPathGoldenFile
      , Golden.actualFile = fullPathActualFile
      }

-- | `Golden` specification for `ToCBOR`
goldenForToCBOR ::
  ToCBOR a =>
  -- | Path to the golden file relative to the root of the package
  FilePath ->
  -- | Value, which in an encoded form will be expected to produce the same contents as in the
  -- golden file.
  a ->
  Golden.Golden BSL.ByteString
goldenForToCBOR goldenFileName t =
  Golden.Golden
    { Golden.output = Plain.serialize t
    , Golden.encodePretty = show . CBORBytes . BSL.toStrict
    , Golden.writeToFile = BSL.writeFile
    , Golden.readFromFile = BSL.readFile
    , Golden.goldenFile = goldenFileName
    , Golden.actualFile = Nothing
    , Golden.failFirstTime = False
    }

-- | `Golden` specification for `EncCBOR`
goldenForEncCBOR ::
  EncCBOR a =>
  -- | Path to the golden file relative to the package
  FilePath ->
  -- | Protocol version to be used for encoding
  Version ->
  -- | Value, which in an encoded form will be expected to produce the same contents as in the
  -- golden file.
  a ->
  Golden.Golden BSL.ByteString
goldenForEncCBOR goldenFileName version t =
  Golden.Golden
    { Golden.output = serialize version t
    , Golden.encodePretty = show . CBORBytes . BSL.toStrict
    , Golden.writeToFile = BSL.writeFile
    , Golden.readFromFile = BSL.readFile
    , Golden.goldenFile = goldenFileName
    , Golden.actualFile = Nothing
    , Golden.failFirstTime = False
    }

-- | Check `EncCBOR` golden spec as well as roundtripping of the golden example with `DecCBOR`
cborGoldenSpec ::
  forall a.
  ( Eq a
  , Show a
  , EncCBOR a
  , DecCBOR a
  , HasCallStack
  ) =>
  -- | Action to get the full path, usually will be @Paths_<package_name>.getDataFileName@
  (FilePath -> IO FilePath) ->
  -- | File path to the golden file relative to the root of the package
  FilePath ->
  -- | Protocol version to be used for encoding
  Version ->
  -- | Value, which in an encoded form will be expected to produce the same contents as in the
  -- golden file.
  a ->
  Spec
cborGoldenSpec mkFullPath goldenFileName version a = do
  describe (show (typeOf a)) $ do
    it "Golden" $ toPackageGolden mkFullPath $ goldenForEncCBOR goldenFileName version a
    it "RoundTrip Golden Example" $ roundTripCborRangeExpectation version version a

-- | Check `ToCBOR` golden spec as well as roundtripping of the golden example with `DecCBOR` for
-- its `Annotator` version.
cborAnnGoldenSpec ::
  forall a.
  ( Eq a
  , Show a
  , ToCBOR a
  , DecCBOR (Annotator a)
  , HasCallStack
  ) =>
  -- | Action to get the full path, usually will be @Paths_<package_name>.getDataFileName@
  (FilePath -> IO FilePath) ->
  -- | File path to the golden file relative to the root of the package
  FilePath ->
  -- | Protocol version to be used for decoding
  Version ->
  -- | Value, which in an encoded form will be expected to produce the same contents as in the
  -- golden file.
  a ->
  Spec
cborAnnGoldenSpec mkFullPath goldenFileName version a = do
  describe (show (typeOf a)) $ do
    it "Golden" $ toPackageGolden mkFullPath $ goldenForToCBOR goldenFileName a
    it "RoundTrip Golden Example" $ roundTripAnnRangeExpectation version version a
