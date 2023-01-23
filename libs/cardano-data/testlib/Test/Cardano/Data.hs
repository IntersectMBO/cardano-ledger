module Test.Cardano.Data (
  expectValidMap,
  genNonEmptyMap,
) where

import Control.Monad
import qualified Data.Map.Internal.Debug as Map
import qualified Data.Map.Strict as Map hiding (showTree)
import Test.Hspec
import Test.QuickCheck

expectValidMap :: HasCallStack => (Ord k, Show k, Show a) => Map.Map k a -> Expectation
expectValidMap m =
  unless (Map.valid m) $
    expectationFailure $
      unlines
        [ "Interal strucutre of a map is invalid:"
        , "Keys are ordered: " ++ show (Map.ordered m)
        , "Tree is balanced: " ++ show (Map.balanced m)
        , "Sizes are valid: " ++ show (Map.validsize m)
        , Map.showTree m
        ]

genNonEmptyMap :: Ord k => Gen k -> Gen v -> Gen (Map.Map k v)
genNonEmptyMap genKey genVal = Map.fromList <$> listOf1 ((,) <$> genKey <*> genVal)
