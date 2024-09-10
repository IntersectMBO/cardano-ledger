module Test.Cardano.Ledger.Common (
  module X,
  ledgerTestMain,
  ledgerTestMainWith,
  ledgerHspecConfig,
  NFData,

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
)
where

import Control.DeepSeq (NFData)
import Control.Monad as X (forM_, replicateM, replicateM_, unless, void, when, (>=>))
import qualified Debug.Trace as Debug
import System.IO (
  BufferMode (LineBuffering),
  hSetBuffering,
  hSetEncoding,
  stdout,
  utf8,
 )
import Test.Cardano.Ledger.Binary.TreeDiff (
  ToExpr (..),
  ansiDocToString,
  ansiExpr,
  ansiExprString,
  assertColorFailure,
  diffExpr,
  diffExprCompact,
  diffExprCompactString,
  diffExprString,
  expectExprEqualWithMessage,
  showExpr,
 )
import Test.HUnit.Base (assertBool, assertFailure)
import Test.Hspec as X
import Test.Hspec.QuickCheck as X
import Test.Hspec.Runner
import Test.QuickCheck as X
import UnliftIO.Exception (evaluateDeep)

infix 1 `shouldBeExpr`
        , `shouldBeRight`
        , `shouldBeRightExpr`
        , `shouldBeLeft`
        , `shouldBeLeftExpr`

ledgerHspecConfig :: Config
ledgerHspecConfig =
  defaultConfig
    { configTimes = True
    , configColorMode = ColorAlways
    }

ledgerTestMainWith :: Config -> Spec -> IO ()
ledgerTestMainWith conf spec = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hspecWith conf spec

ledgerTestMain :: Spec -> IO ()
ledgerTestMain = ledgerTestMainWith ledgerHspecConfig

shouldBeExpr :: (HasCallStack, ToExpr a, Eq a) => a -> a -> IO ()
shouldBeExpr = expectExprEqualWithMessage ""

-- | Return value on the `Right` and fail otherwise
expectRight :: (HasCallStack, Show a) => Either a b -> IO b
expectRight (Right r) = pure $! r
expectRight (Left l) = assertFailure $ "Expected Right, got Left:\n" <> show l

-- | Same as `expectRight`, but also evaluate the returned value to NF
expectRightDeep :: (HasCallStack, Show a, NFData b) => Either a b -> IO b
expectRightDeep = expectRight >=> evaluateDeep

-- | Same as `expectRightDeep`, but discards the result
expectRightDeep_ :: (HasCallStack, Show a, NFData b) => Either a b -> IO ()
expectRightDeep_ = void . expectRightDeep

-- | Same as `expectRight`, but use `ToExpr` instead of `Show`
expectRightExpr :: (HasCallStack, ToExpr a) => Either a b -> IO b
expectRightExpr (Right r) = pure $! r
expectRightExpr (Left l) = assertFailure $ "Expected Right, got Left:\n" <> showExpr l

-- | Same as `expectRightDeep`,  but use `ToExpr` instead of `Show`
expectRightDeepExpr :: (HasCallStack, ToExpr a, NFData b) => Either a b -> IO b
expectRightDeepExpr = expectRightExpr >=> evaluateDeep

-- | Same as `shouldBe`, except it checks that the value is `Right`
shouldBeRight :: (HasCallStack, Show a, Show b, Eq b) => Either a b -> b -> Expectation
shouldBeRight e x = expectRight e >>= (`shouldBe` x)

-- | Same as `shouldBeExpr`, except it checks that the value is `Right`
shouldBeRightExpr :: (HasCallStack, ToExpr a, Eq b, ToExpr b) => Either a b -> b -> Expectation
shouldBeRightExpr e x = expectRightExpr e >>= (`shouldBeExpr` x)

-- | Return value on the `Left` an fail otherwise
expectLeft :: (HasCallStack, Show b) => Either a b -> IO a
expectLeft (Left l) = pure $! l
expectLeft (Right r) = assertFailure $ "Expected Left, got Right:\n" <> show r

-- | Same as `expectLeft`, but also evaluate the returned value to NF
expectLeftDeep :: (HasCallStack, NFData a, Show b) => Either a b -> IO a
expectLeftDeep = expectLeft >=> evaluateDeep

-- | Same as `expectLeftDeep`, but discards the result
expectLeftDeep_ :: (HasCallStack, NFData a, Show b) => Either a b -> IO ()
expectLeftDeep_ = void . expectLeftDeep

-- | Same as `expectLeft`, but use `ToExpr` instead of `Show`
expectLeftExpr :: (HasCallStack, ToExpr b) => Either a b -> IO a
expectLeftExpr (Left l) = pure $! l
expectLeftExpr (Right r) = assertFailure $ "Expected Left, got Right:\n" <> showExpr r

-- | Same as `expectLeftDeep`,  but use `ToExpr` instead of `Show`
expectLeftDeepExpr :: (HasCallStack, ToExpr b, NFData a) => Either a b -> IO a
expectLeftDeepExpr = expectLeftExpr >=> evaluateDeep

-- | Same as `shouldBe`, except it checks that the value is `Left`
shouldBeLeft :: (HasCallStack, Show a, Eq a, Show b) => Either a b -> a -> Expectation
shouldBeLeft e x = expectLeft e >>= (`shouldBe` x)

-- | Same as `shouldBeExpr`, except it checks that the value is `Left`
shouldBeLeftExpr :: (HasCallStack, ToExpr a, ToExpr b, Eq a) => Either a b -> a -> Expectation
shouldBeLeftExpr e x = expectLeftExpr e >>= (`shouldBeExpr` x)

-- | Same as `Test.QuickCheck.discard` but outputs a debug trace message
tracedDiscard :: [Char] -> a
tracedDiscard message = (if False then Debug.trace $ "\nDiscarded trace: " ++ message else id) discard
