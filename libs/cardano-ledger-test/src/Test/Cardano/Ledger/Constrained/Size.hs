{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.Constrained.Size (
  Size (.., SzExact),
  seps,
  sepsP,
  sepn,
  runSize,
  atLeastDelta,
  atMostAny,
  genFromSize,
  genFromIntRange,
  genFromNonNegIntRange,
  negateSize,
  mergeSize,
) where

import qualified Data.List as List
import Test.Cardano.Ledger.Constrained.Combinators (errorMess)
import Test.Cardano.Ledger.Constrained.Monad (LiftT (..), Typed (..), failT)
import Test.QuickCheck (Gen, chooseInt)

-- ==============================================

seps :: [String] -> String
seps xs = List.intercalate " " xs

sepsP :: [String] -> String
sepsP xs = "(" ++ List.intercalate " " xs ++ ")"

sepn :: [String] -> String
sepn xs = List.intercalate "\n   " xs

-- | Used in tests so things don't get too large
atLeastDelta :: Int
atLeastDelta = 5

-- | Used in tests so things don't get too large
--   If we can't find an era using things of size 10
--   using things of size 100, isn't going to help.
atMostAny :: Int
atMostAny = 10

-- =======================================================================================
-- The type Size and AddsSpec are defined in their own file because its type must be known
-- in many other modules, so to avoid recursive cycles this module depends on only Combinators
-- They act like a Spec, so there are Spec like Monoid and Semigroup instances.

data Size
  = SzNever [String]
  | SzAny
  | SzLeast Int
  | SzMost Int
  | -- | Size is in the range from @i@ to @j@ inclusive: @SzRng i j = [i .. j]@. Invariant @i <= j@
    SzRng Int Int
  deriving (Ord, Eq)

instance LiftT Size where
  liftT (SzNever xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = SzNever s
  dropT (Typed (Right x)) = x

sameR :: Size -> Maybe Int
sameR (SzRng x y) = if x == y then Just x else Nothing
sameR _ = Nothing

pattern SzExact :: Int -> Size
pattern SzExact x <- (sameR -> Just x)
  where
    SzExact x = (SzRng x x)

instance Show Size where
  show (SzNever _) = "NeverSize"
  show SzAny = "AnySize"
  show (SzLeast n) = "(AtLeast " ++ show n ++ ")"
  show (SzMost n) = "(AtMost " ++ show n ++ ")"
  show (SzRng i j) = "(Range " ++ show i ++ " " ++ show j ++ ")"

negateSize :: Size -> Size
negateSize (SzLeast n) = SzMost (-n)
negateSize (SzMost n) = SzLeast (-n)
negateSize (SzRng i j) = SzRng (-j) (-i)
negateSize x = x

mergeSize :: Size -> Size -> Size
mergeSize SzAny x = x
mergeSize x SzAny = x
mergeSize (SzNever xs) (SzNever ys) = SzNever (xs ++ ys)
mergeSize _ (SzNever xs) = SzNever xs
mergeSize (SzNever xs) _ = SzNever xs
mergeSize (SzLeast x) (SzLeast y) = SzLeast (max x y)
mergeSize (SzLeast x) (SzMost y) | x <= y = SzRng x y
mergeSize (SzLeast x) (SzRng i j) | x <= i = SzRng i j
mergeSize (SzLeast x) (SzRng i j) | x >= i && x <= j = SzRng x j
mergeSize (SzMost x) (SzMost y) = SzMost (min x y)
mergeSize (SzMost y) (SzLeast x) | x <= y = SzRng x y
mergeSize (SzMost x) (SzRng i j) | x >= j = SzRng i j
mergeSize (SzMost x) (SzRng i j) | x >= i && x <= j = SzRng i x
mergeSize (SzRng i j) (SzLeast x) | x <= i = SzRng i j
mergeSize (SzRng i j) (SzLeast x) | x >= i && x <= j = SzRng x j
mergeSize (SzRng i j) (SzMost x) | x >= j = SzRng i j
mergeSize (SzRng i j) (SzMost x) | x >= i && x <= j = SzRng i x
mergeSize (SzRng i j) (SzRng m n) | x <= y = SzRng x y
  where
    x = max i m
    y = min j n
mergeSize a b = SzNever ["Size specifications " ++ show a ++ " and " ++ show b ++ " are inconsistent."]

instance Monoid Size where mempty = SzAny

instance Semigroup Size where
  (<>) = mergeSize

runSize :: Int -> Size -> Bool
runSize _ (SzNever xs) = errorMess "SzNever in runSizeSpec" xs
runSize _ SzAny = True
runSize n (SzLeast m) = n >= m
runSize n (SzMost m) = n <= m
runSize n (SzRng i j) = n >= i && n <= j

-- | Use to generate real sizes, where there are no negative numbers,
--   and the smallest possible size is 0.
--   Use this only where you know it is NOT SzNever
genFromSize :: Size -> Gen Int
genFromSize (SzNever _) = error "Bad call to (genFromSize(SzNever ..))."
genFromSize SzAny = chooseInt (0, atMostAny)
genFromSize (SzRng i j) = chooseInt (max i 0, max i $ min atMostAny j)
genFromSize (SzLeast i) = chooseInt (max i 0, max i 0 + atLeastDelta)
genFromSize (SzMost i) = chooseInt (0, min atMostAny i)

-- | Similar to genFromSize, but allows negative numbers (unlike size where the smallest Int is 0)
genFromIntRange :: Size -> Gen Int
genFromIntRange (SzNever _) = error "Bad call to (genFromIntRange(SzNever ..))."
genFromIntRange SzAny = chooseInt (-atMostAny, atMostAny)
genFromIntRange (SzRng i j) = chooseInt (i, j)
genFromIntRange (SzLeast i) = chooseInt (i, i + atLeastDelta)
genFromIntRange (SzMost i) = chooseInt (i - atMostAny, i)

genFromNonNegIntRange :: Size -> Gen Int
genFromNonNegIntRange sz = max 0 <$> genFromIntRange sz
