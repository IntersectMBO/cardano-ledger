{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Common (
  module X,
  ledgerTestMain,
  ledgerTestMainWith,
  ledgerHspecConfig,
  NFData,
  runGen,

  -- * Expr
  ToExpr (..),
  showExpr,
  ansiExpr,
  ansiExprString,
  diffExpr,
  diffExprString,
  diffExprCompact,
  diffExprCompactString,
  ansiDocToString,

  -- * Expectations
  assertBool,
  assertFailure,
  assertColorFailure,

  -- ** Non-standard expectations
  shouldBeExpr,
  shouldBeRight,
  shouldBeLeft,
  shouldBeRightExpr,
  shouldBeLeftExpr,
  expectRight,
  expectRightDeep,
  expectRightDeep_,
  expectRightExpr,
  expectRightDeepExpr,
  expectRightDeepExpr_,
  expectLeft,
  expectLeftExpr,
  expectLeftDeep,
  expectLeftDeep_,
  expectLeftDeepExpr,
  expectLeftDeepExpr_,
  expectJust,
  expectJustDeep,
  expectJustDeep_,
  expectNothing,

  -- ** Golden testing
  Golden.Golden,
  itGolden,
  toPackageGolden,

  -- ** Aeson testing
  itGoldenToJSON,
  aesonGoldenSpec,
  goldenForToJSON,
  roundTripAesonProperty,

  -- * Miscellanous helpers
  tracedDiscard,
  forEachEraVersion,
) where

import Cardano.Ledger.Binary (Version)
import Cardano.Ledger.Core (Era, eraProtVersions)
import Control.DeepSeq (NFData)
import Control.Monad as X (forM_, replicateM, replicateM_, unless, void, when, (>=>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Types as Aeson (parseEither)
import qualified Data.ByteString.Lazy as BSL
import Data.Typeable
import qualified Debug.Trace as Debug
import Test.Cardano.Ledger.Binary.Golden (toPackageGolden)
import Test.Cardano.Ledger.Binary.TreeDiff (
  ToExpr (..),
  ansiExpr,
  ansiExprString,
  diffExpr,
  diffExprCompact,
  diffExprCompactString,
  diffExprString,
  expectExprEqualWithMessage,
  showExpr,
 )
import Test.Hspec as X
import qualified Test.Hspec.Golden as Golden (Golden (..))
import Test.Hspec.QuickCheck as X
import Test.Hspec.Runner
import Test.ImpSpec (ansiDocToString, impSpecConfig, impSpecMainWithConfig)
import Test.ImpSpec.Expectations
import Test.QuickCheck as X hiding (NonZero, Witness)
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Random (mkQCGen)
import UnliftIO.Exception (evaluateDeep)

infix 1 `shouldBeExpr`
        , `shouldBeRightExpr`
        , `shouldBeLeftExpr`

ledgerHspecConfig :: Config
ledgerHspecConfig = impSpecConfig

ledgerTestMainWith :: Config -> Spec -> IO ()
ledgerTestMainWith = impSpecMainWithConfig

ledgerTestMain :: Spec -> IO ()
ledgerTestMain = ledgerTestMainWith ledgerHspecConfig

shouldBeExpr :: (HasCallStack, ToExpr a, Eq a) => a -> a -> IO ()
shouldBeExpr = expectExprEqualWithMessage ""

-- | Same as `expectRight`, but use `ToExpr` instead of `Show`
expectRightExpr :: (HasCallStack, ToExpr a) => Either a b -> IO b
expectRightExpr (Right r) = pure $! r
expectRightExpr (Left l) = assertFailure $ "Expected Right, got Left:\n" <> showExpr l

-- | Same as `expectRightDeep`,  but use `ToExpr` instead of `Show`
expectRightDeepExpr :: (HasCallStack, ToExpr a, NFData b) => Either a b -> IO b
expectRightDeepExpr = expectRightExpr >=> evaluateDeep

-- | Same as `shouldBeExpr`, except it checks that the value is `Right`
shouldBeRightExpr :: (HasCallStack, ToExpr a, Eq b, ToExpr b) => Either a b -> b -> Expectation
shouldBeRightExpr e x = expectRightExpr e >>= (`shouldBeExpr` x)

-- | Same as `expectRightDeepExpr`, but discard the contents of `Right`
expectRightDeepExpr_ :: (HasCallStack, ToExpr a, NFData b) => Either a b -> IO ()
expectRightDeepExpr_ = void . expectRightDeepExpr

-- | Same as `expectLeft`, but use `ToExpr` instead of `Show`
expectLeftExpr :: (HasCallStack, ToExpr b) => Either a b -> IO a
expectLeftExpr (Left l) = pure $! l
expectLeftExpr (Right r) = assertFailure $ "Expected Left, got Right:\n" <> showExpr r

-- | Same as `expectLeftDeep`, but use `ToExpr` instead of `Show`
expectLeftDeepExpr :: (HasCallStack, ToExpr b, NFData a) => Either a b -> IO a
expectLeftDeepExpr = expectLeftExpr >=> evaluateDeep

-- | Same as `expectLeftDeepExpr`, but discard the contents of `Left`
expectLeftDeepExpr_ :: (HasCallStack, ToExpr b, NFData a) => Either a b -> IO ()
expectLeftDeepExpr_ = void . expectLeftDeepExpr

-- | Same as `shouldBeExpr`, except it checks that the value is `Left`
shouldBeLeftExpr :: (HasCallStack, ToExpr a, ToExpr b, Eq a) => Either a b -> a -> Expectation
shouldBeLeftExpr e x = expectLeftExpr e >>= (`shouldBeExpr` x)

-- | Same as `Test.QuickCheck.discard` but outputs a debug trace message
tracedDiscard :: String -> a
tracedDiscard message = (if False then Debug.trace $ "\nDiscarded trace: " ++ message else id) discard

runGen ::
  -- | Seed
  Int ->
  -- | Size
  Int ->
  -- | Generator to run.
  Gen a ->
  a
runGen seed size gen = unGen gen (mkQCGen seed) size

forEachEraVersion :: forall era. (Era era, HasCallStack) => (Version -> Spec) -> Spec
forEachEraVersion sv = forM_ (eraProtVersions @era) $ \version ->
  describe (show version) $ sv version

-- | `Golden` specification for `ToJSON`.
goldenForToJSON ::
  Aeson.ToJSON a =>
  -- | Path to the golden file relative to the package
  FilePath ->
  -- | Value, which in an encoded form will be expected to produce the same contents as in the
  -- golden file.
  a ->
  Golden.Golden BSL.ByteString
goldenForToJSON goldenFileName actualOutput =
  Golden.Golden
    { Golden.output =
        -- Newline appended at the end for two reasons:
        --
        -- - Github in diffs shows an ugly symbol indicating there is no trailing newline
        --
        -- - Most editors will automatically add a trailing newline upon saving a file. Despite that
        --   these files are autogenerated, we do not want to prevent developers from adjusting them
        --   manually when they are experimenting with codecs
        Aeson.encodePretty actualOutput <> "\n"
    , Golden.encodePretty = show
    , Golden.writeToFile = BSL.writeFile
    , Golden.readFromFile = BSL.readFile
    , Golden.goldenFile = goldenFileName
    , Golden.actualFile = Nothing
    , Golden.failFirstTime = False
    }

-- | Create a `Spec` for testing the `Golden.Golden` specification
itGolden ::
  (Eq g, HasCallStack) =>
  -- | Test name
  String ->
  -- | Action to get the full path, usually will be @Paths_<package_name>.getDataFileName@
  (FilePath -> IO FilePath) ->
  -- | Golden specification
  Golden.Golden g ->
  Spec
itGolden name mkFullPath = it name . toPackageGolden mkFullPath

-- | Check `ToJSON` golden spec. in case when type also has `FromJSON` use `aesonGoldenSpec` instead.
itGoldenToJSON ::
  (Aeson.ToJSON a, Typeable a) =>
  -- | Action to get the full path, usually will be @Paths_<package_name>.getDataFileName@
  (FilePath -> IO FilePath) ->
  -- | File path to the golden file relative to the root of the package
  FilePath ->
  a ->
  Spec
itGoldenToJSON mkFullPath goldenFileName a =
  itGolden (show (typeOf a)) mkFullPath $ goldenForToJSON goldenFileName a

-- | Same as `itGoldenToJSON`, but also test `FromJSON` through roundtripping
aesonGoldenSpec ::
  forall a.
  ( Eq a
  , ToExpr a
  , NFData a
  , Typeable a
  , Aeson.ToJSON a
  , Aeson.FromJSON a
  , HasCallStack
  ) =>
  -- | Action to get the full path, usually will be @Paths_<package_name>.getDataFileName@
  (FilePath -> IO FilePath) ->
  -- | File path to the golden file relative to the root of the package
  FilePath ->
  -- | Value, which in an encoded form will be expected to produce the same contents as in the
  -- golden file.
  a ->
  Spec
aesonGoldenSpec mkFullPath goldenFileName a = do
  describe (show (typeOf a)) $ do
    itGolden "Golden" mkFullPath $ goldenForToJSON goldenFileName a
    it "RoundTrip Golden Example" $ roundTripAesonProperty a

-- | Test Aeson roundtripping
roundTripAesonProperty ::
  forall a.
  (Eq a, NFData a, ToExpr a, Aeson.ToJSON a, Aeson.FromJSON a, HasCallStack) =>
  a ->
  Property
roundTripAesonProperty expected = property $ do
  let jsonValue = Aeson.toJSON expected
  -- There is no need to go through `ByteString` if we fully force the `Value`.
  produced <- expectRightDeep $ Aeson.parseEither Aeson.parseJSON jsonValue
  produced `shouldBeExpr` expected
