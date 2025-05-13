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
  expectLeft,
  expectLeftExpr,
  expectLeftDeep,
  expectLeftDeep_,
  expectLeftDeepExpr,

  -- * Miscellanous helpers
  tracedDiscard,
) where

import Control.DeepSeq (NFData)
import Control.Monad as X (forM_, replicateM, replicateM_, unless, void, when, (>=>))
import qualified Debug.Trace as Debug
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
import Test.Hspec.QuickCheck as X
import Test.Hspec.Runner
import Test.ImpSpec (ansiDocToString, impSpecConfig, impSpecMainWithConfig)
import Test.ImpSpec.Expectations
import Test.QuickCheck as X
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

-- | Same as `expectLeft`, but use `ToExpr` instead of `Show`
expectLeftExpr :: (HasCallStack, ToExpr b) => Either a b -> IO a
expectLeftExpr (Left l) = pure $! l
expectLeftExpr (Right r) = assertFailure $ "Expected Left, got Right:\n" <> showExpr r

-- | Same as `expectLeftDeep`,  but use `ToExpr` instead of `Show`
expectLeftDeepExpr :: (HasCallStack, ToExpr b, NFData a) => Either a b -> IO a
expectLeftDeepExpr = expectLeftExpr >=> evaluateDeep

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
