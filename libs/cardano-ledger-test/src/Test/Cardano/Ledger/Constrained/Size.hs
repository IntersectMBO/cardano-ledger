{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.Constrained.Size (
  Size (.., SzExact),
  AddsSpec (..),
  vLeft,
  vRight,
  vLeftSize,
  vRightSize,
  OrdCond (..),
  reverseOrdCond,
  seps,
  sepsP,
  sepn,
  runOrdCond,
  runSize,
  atleastdelta,
  atmostany,
  genFromSize,
  genFromIntRange,
  genFromNonNegIntRange,
  vLeftNeg,
  vRightNeg,
  negateSize,
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
atleastdelta :: Int
atleastdelta = 5

-- | Used in tests so things don't get too large
--   If we can't find an era using things of size 10
--   using things of size 100, isn't going to help.
atmostany :: Int
atmostany = 10

-- =======================================================================================
-- The type Size and AddsSpec are defined in their own file because its type must be known
-- in many other modules, so to avoid recursive cycles this module depends on only Combinators
-- They act like a Spec, so there are Spec like Monoid and Semigroup instances.

data Size
  = SzNever [String]
  | SzAny
  | SzLeast Int
  | SzMost Int
  | SzRng Int Int -- (SzRng i j) = [i .. j] . Invariant i <= j
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
genFromSize SzAny = chooseInt (0, atmostany)
genFromSize (SzRng i j) = chooseInt (max i 0, max i $ min atmostany j)
genFromSize (SzLeast i) = chooseInt (max i 0, max i 0 + atleastdelta)
genFromSize (SzMost i) = chooseInt (0, min atmostany i)

-- | Similar to genFromSize, but allows negative numbers (unlike size where the smallest Int is 0)
genFromIntRange :: Size -> Gen Int
genFromIntRange (SzNever _) = error "Bad call to (genFromIntRange(SzNever ..))."
genFromIntRange SzAny = chooseInt (-atmostany, atmostany)
genFromIntRange (SzRng i j) = chooseInt (i, j)
genFromIntRange (SzLeast i) = chooseInt (i, i + atleastdelta)
genFromIntRange (SzMost i) = chooseInt (i - atmostany, i)

genFromNonNegIntRange :: Size -> Gen Int
genFromNonNegIntRange sz = max 0 <$> genFromIntRange sz

-- =========================================================================
-- AddsSpec
-- =========================================================================

-- | A specification of summation. like: lhs = ∑ rhs
--   The idea is that the 'rhs' can contain multiple terms: rhs = ∑ r1 + r2 + r3
--   Other example conditions:  (lhs < ∑ rhs), and (lhs >= ∑ rhs)
--   The invariant is that only a single variable appears in the summation.
--   It can appear on either side. If it appears in the 'rhs' then there
--   may be other, constant terms, in the rhs:  7 = ∑ 3 + v + 9
--   We always do the sums and solving at type Int, and cast back and forth to
--   accommodate other types with (Adds c) instances, using the methods 'fromI" and 'toI'
--   This allows the instance to deal with special conditions.
--   There are two (non-failure) possibilities 1) Var on the left, 2) Var on the right
--   We supply functions
--      vLeft  :: String -> OrdCond -> Integer -> AddsSpec c
--                SumsTo _ x <= 4 + 6 + 9 ===> (vLeft x LTE 19) == (AddsSpecSize x (AtMost 19))
--      vRight :: Integer -> OrdCond -> Integer -> String -> AddsSpec c
--                SumsTo _ 8 < 2 + x + 3 ===> (vRight 8 LTH 5 x) == (AddsSpecSize x (AtLeast 4))
--   But internally we store the information as a String and a Size (I.e. a range of Int)
data AddsSpec c where
  AddsSpecSize ::
    String ->
    -- | name
    Size ->
    -- | total (range like (4 .. 12))
    AddsSpec c
  AddsSpecAny :: AddsSpec c
  AddsSpecNever :: [String] -> AddsSpec c

instance LiftT (AddsSpec c) where
  liftT (AddsSpecNever xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = AddsSpecNever s
  dropT (Typed (Right x)) = x

instance Show (AddsSpec c) where show = showAddsSpec

instance Semigroup (AddsSpec c) where (<>) = mergeAddsSpec
instance Monoid (AddsSpec c) where mempty = AddsSpecAny

showAddsSpec :: AddsSpec c -> String
showAddsSpec AddsSpecAny = "AddsSpecAny"
showAddsSpec (AddsSpecSize s size) = sepsP ["AddsSpecSize", s, show size]
showAddsSpec (AddsSpecNever _) = "AddsSpecNever"

mergeAddsSpec :: AddsSpec c -> AddsSpec c -> AddsSpec c
mergeAddsSpec (AddsSpecNever xs) (AddsSpecNever ys) = AddsSpecNever (xs ++ ys)
mergeAddsSpec x@(AddsSpecNever _) _ = x
mergeAddsSpec _ x@(AddsSpecNever _) = x
mergeAddsSpec AddsSpecAny x = x
mergeAddsSpec x AddsSpecAny = x
mergeAddsSpec a@(AddsSpecSize nam1 size1) b@(AddsSpecSize nam2 size2) =
  if nam1 /= nam2
    then
      AddsSpecNever
        [ "vars " ++ nam1 ++ " and " ++ nam2 ++ " are not the same."
        , show a ++ " " ++ show b ++ " are inconsistent."
        ]
    else case size1 <> size2 of
      (SzNever xs) -> AddsSpecNever (xs ++ [show a ++ " " ++ show a ++ " are inconsistent."])
      size3 -> AddsSpecSize nam1 size3

-- =======================================
-- Helper function to create AddsSpecSize

-- Translate some thing like [SumsTo _ x <= 4 + 6 + 9] where the variable 'x' is on the left
vLeft :: String -> OrdCond -> Int -> (AddsSpec c)
vLeft x cond n = AddsSpecSize x (vLeftSize x cond n)

vLeftSize :: String -> OrdCond -> Int -> Size
vLeftSize x cond n = ordCondToSize (x, cond, n)

-- Translate some thing like [SumsTo c 8 < 2 + x + 3] where the variable 'x' is on the right
vRight :: Int -> OrdCond -> Int -> String -> AddsSpec c
vRight n cond m s = AddsSpecSize s (vRightSize n cond m s)

vRightSize :: Int -> OrdCond -> Int -> String -> Size
vRightSize n cond m s = ordCondToSize (s, reverseOrdCond cond, n - m)

-- Translate some thing like [SumsTo (Negate x) <= 4 + 6 + 9] where the variable 'x'
-- is on the left, and we want to produce its negation.
vLeftNeg :: String -> OrdCond -> Int -> (AddsSpec c)
vLeftNeg s cond n = AddsSpecSize s (negateSize (ordCondToSize (s, cond, n)))

-- Translate some thing like [SumsTo 8 < 2 + (Negate x) + 3] where the
-- variable 'x' is on the right, and we want to produce its negation.
vRightNeg :: Int -> OrdCond -> Int -> String -> AddsSpec c
vRightNeg n cond m s = AddsSpecSize s (negateSize (ordCondToSize (s, reverseOrdCond cond, n - m)))

-- | This function `reverseOrdCond` has been defined to handle the Pred SumsTo when the
--   variable is on the right-hand-side (rhs) of the OrdCond operator. In order to do that
--   we must multiply both sides of the inequality by (-1). For example consider
--   [SumsTo (DeltaCoin 1) ▵₳ -2 > ∑ ▵₳ -1 + x]
--                 Note variable x on the rhs ^
--    To solve we subtract 'x' from both sides, and add '▵₳ -2' from bothsides
--    getting      (-x) > ∑  (▵₳ -1) + (▵₳ -2)
--    reduced to   (-x) > ∑  (▵₳ -3)
--    to solve we must multiply both sides by (-1)
--                 x ?? ∑  (▵₳ 3)
-- What operator do we replace ?? by to make the original (▵₳ -2 > ∑ ▵₳ -1 + x) True?
-- The change in the operator is called "reversing" the operator. See
-- https://www.mathsisfun.com/algebra/inequality-solving.html for one explantion.
reverseOrdCond :: OrdCond -> OrdCond
reverseOrdCond EQL = EQL
reverseOrdCond LTH = GTH
reverseOrdCond LTE = GTE
reverseOrdCond GTH = LTH
reverseOrdCond GTE = LTE

-- | Translate (s,cond,n), into a Size which
--   specifies the Int range on which the OrdCond is True.
--   The triple (s, EQL, 2) denotes s = 2
--              (s, LTH, 7) denotes s < 7
--              (s, GTH, 5) denotes s > 5 ...
ordCondToSize :: (String, OrdCond, Int) -> Size
ordCondToSize (_, cond, n) = case cond of
  EQL -> SzExact n
  LTH -> SzMost (n - 1)
  LTE -> SzMost n
  GTH -> SzLeast (n + 1)
  GTE -> SzLeast n

-- =========================================================================
-- OrdCond
-- x <= y
--   ^     paramerterize over the condition
--
-- EQL = (==), LTH = (<), LTE = (<=), GTH = (>), GTE = (>=)
-- =========================================================================

-- | First order representation of the Ord comparisons
data OrdCond = EQL | LTH | LTE | GTH | GTE
  deriving (Eq)

instance Show OrdCond where
  show EQL = " = ∑ "
  show LTH = " < ∑ "
  show LTE = " <= ∑ "
  show GTH = " > ∑ "
  show GTE = " >= ∑ "

runOrdCond :: Ord c => OrdCond -> c -> c -> Bool
runOrdCond EQL x y = x == y
runOrdCond LTH x y = x < y
runOrdCond LTE x y = x <= y
runOrdCond GTH x y = x > y
runOrdCond GTE x y = x >= y
