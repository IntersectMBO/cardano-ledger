{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.Constrained.Size (
  Size (.., SzExact),
  SumSpec (..),
  vLeft,
  vRight,
  vLeftSize,
  vRightSize,
  OrdCond (..),
  negOrdCond,
  seps,
  sepsP,
  sepn,
  runOrdCond,
  runSize,
  atleastdelta,
  atmostany,
  genFromSize,
  genFromIntRange,
  vLeftNeg,
  vRightNeg,
  tripToSize,
  negateSize,
) where

import qualified Data.List as List
import Test.Cardano.Ledger.Constrained.Combinators (errorMess)
import Test.Cardano.Ledger.Constrained.Monad (LiftT (..), Typed (..), failT, requireAll)
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
-- The type Size and SumSpec are defined in their own file because its type must be known
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
genFromSize (SzRng i j) = chooseInt (max i 0, j)
genFromSize (SzLeast i) = chooseInt (max i 0, (max i 0) + atleastdelta)
genFromSize (SzMost i) = chooseInt (0, i)

-- | Similar to genFromSize, but allows negative numbers (unlike size where the smallest Int is 0)
genFromIntRange :: Size -> Gen Int
genFromIntRange (SzNever _) = error "Bad call to (genFromIntRange(SzNever ..))."
genFromIntRange SzAny = chooseInt (-atmostany, atmostany)
genFromIntRange (SzRng i j) = chooseInt (i, j)
genFromIntRange (SzLeast i) = chooseInt (i, i + atleastdelta)
genFromIntRange (SzMost i) = chooseInt (i - atmostany, i)

-- =========================================================================
-- SumSpec
-- =========================================================================

-- | A specification of summation. like: lhs = ∑ rhs
--   The idea is that the 'rhs' can contain multiple terms: lhs = ∑ r1 + r2 + r3
--   Other example conditions:  (lhs < ∑ rhs), and (lhs >= ∑ rhs)
--   The invariant is that only a single variable appears in the summation.
--   It can appear on either side. If it appears in the 'rhs' then there
--   may be other, constant terms, in the rhs:  7 = ∑ 3 + v + 9
--   We always do the sums and solving at type Int, and cast back and forth to
--   accommodate other types with (Adds c) instances, using the methods 'fromI" and 'toI'
--   This allows the instance to deal with special conditions.
--   There are two (non-failure) possibilities 1) Var on the left, 2) Var on the right
--   We supply functions
--      vLeft  :: c -> String -> OrdCond -> Integer -> SumSpec c
--                SumsTo c x <= 4 + 6 + 9 ===> (vLeft i x LTE 19) == (SumSpecSize c x (AtMost 19))
--      vRight :: c -> Integer -> OrdCond -> Integer -> String -> SumSpec c
--                SumsTo c 8 < 2 + x + 3 ===> (vRight c 8 LTH 5 x) == (SumSpecSize c x (AtLeast 4))
--   But internally we store the information as a String and a Size (I.e. a range of Int)
data SumSpec c where
  SumSpecSize :: (Ord c, Show c) => c -> String -> Size -> SumSpec c
  SumSpecAny :: SumSpec c
  SumSpecNever :: [String] -> SumSpec c

instance LiftT (SumSpec c) where
  liftT (SumSpecNever xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = SumSpecNever s
  dropT (Typed (Right x)) = x

-- Translate some thing like [SumsTo c x <= 4 + 6 + 9] where the variable 'x' is on the left
vLeft :: (Show c, Ord c) => c -> String -> OrdCond -> Int -> (SumSpec c)
vLeft c x cond n = SumSpecSize c x (vLeftSize x cond n)

vLeftSize :: String -> OrdCond -> Int -> Size
vLeftSize x cond n = tripToSize (x, cond, n)

-- Translate some thing like [SumsTo c 8 < 2 + x + 3] where the variable 'x' is on the right
vRight :: (Ord c, Show c) => c -> Int -> OrdCond -> Int -> String -> SumSpec c
vRight c n cond m s = SumSpecSize c s (vRightSize n cond m s)

vRightSize :: Int -> OrdCond -> Int -> String -> Size
vRightSize n cond m s = tripToSize (s, negOrdCond cond, n - m)

-- Translate some thing like [SumsTo (Negate x) <= 4 + 6 + 9] where the variable 'x'
-- is on the left, and we want to produce its negation.
vLeftNeg :: (Show c, Ord c) => c -> String -> OrdCond -> Int -> (SumSpec c)
vLeftNeg c s cond n = SumSpecSize c s (negateSize (tripToSize (s, cond, n)))

-- Translate some thing like [SumsTo 8 < 2 + (Negate x) + 3] where the
-- variable 'x' is on the right, and we want to produce its negation.
vRightNeg :: (Show c, Ord c) => c -> Int -> OrdCond -> Int -> String -> SumSpec c
vRightNeg c n cond m s = SumSpecSize c s (negateSize (tripToSize (s, negOrdCond cond, n - m)))

-- | Not exactly conditional negation, but what we need to make 'vRight' work out
negOrdCond :: OrdCond -> OrdCond
negOrdCond EQL = EQL
negOrdCond LTH = GTH
negOrdCond LTE = GTH
negOrdCond GTH = LTH
negOrdCond GTE = LTH
negOrdCond x = x

instance Show (SumSpec c) where show = showSumSpec

instance Semigroup (SumSpec c) where (<>) = mergeSumSpec
instance Monoid (SumSpec c) where mempty = SumSpecAny

showSumSpec :: SumSpec c -> String
showSumSpec SumSpecAny = "SumSpecAny"
showSumSpec (SumSpecSize c s size) = sepsP ["SumSpecSize", show c, s, show size]
showSumSpec (SumSpecNever _) = "SumSpecNever"

mergeSumSpec :: SumSpec c -> SumSpec c -> SumSpec c
mergeSumSpec (SumSpecNever xs) (SumSpecNever ys) = SumSpecNever (xs ++ ys)
mergeSumSpec x@(SumSpecNever _) _ = x
mergeSumSpec _ x@(SumSpecNever _) = x
mergeSumSpec SumSpecAny x = x
mergeSumSpec x SumSpecAny = x
mergeSumSpec a@(SumSpecSize c1 s1 size1) b@(SumSpecSize c2 s2 size2) =
  dropT $
    requireAll
      [
        ( s1 == s2
        ,
          [ "vars " ++ s1 ++ " and " ++ s2 ++ " are not the same."
          , show a ++ " " ++ show b ++ " are inconsistent."
          ]
        )
      ,
        ( c1 == c2
        ,
          [ "smallest " ++ show s1 ++ " and " ++ show s2 ++ " are not the same."
          , show a ++ " " ++ show b ++ " are inconsistent."
          ]
        )
      ]
      ( case size1 <> size2 of
          (SzNever xs) -> failT (xs ++ [show a ++ " " ++ show a ++ " are inconsistent."])
          size3 -> pure $ SumSpecSize c1 s1 size3
      )

-- =========================================================================
-- OrdCond
-- x <= y
--   ^     paramerterize over the condition
--
-- EQL = (==), LTH = (<), LTE = (<=), GTH = (>), GTE = (>=)
-- =========================================================================

-- | First order representation of the Ord comparisons
data OrdCond = EQL | LTH | LTE | GTH | GTE | CondNever [String] | CondAny
  deriving (Eq)

instance Show OrdCond where
  show EQL = " = ∑ "
  show LTH = " < ∑ "
  show LTE = " <= ∑ "
  show GTH = " > ∑ "
  show GTE = " >= ∑ "
  show (CondNever xs) = unlines xs
  show CondAny = " `always` ∑ "

always :: c -> c -> Bool
always _ _ = True

runOrdCond :: Ord c => OrdCond -> c -> c -> Bool
runOrdCond EQL x y = x == y
runOrdCond LTH x y = x < y
runOrdCond LTE x y = x <= y
runOrdCond GTH x y = x > y
runOrdCond GTE x y = x >= y
runOrdCond CondAny x y = always x y -- Always True
runOrdCond (CondNever _) _ _ = False

-- | Translate an OrdCond on Int, into a Size which
--   specifies the Int range on which the OrdCond is True.
--   The triple (s, EQL, 2) denotes s = 2
--              (s, LTH, 7) denotes s < 7
--   and each of these corresponds to a range encoded in Size
tripToSize :: (String, OrdCond, Int) -> Size
tripToSize (_s, EQL, n) = SzExact n
tripToSize (_s, LTH, n) = SzMost (n - 1)
tripToSize (_s, LTE, n) = SzMost n
tripToSize (_s, GTH, n) = SzLeast (n + 1)
tripToSize (_s, GTE, n) = SzLeast n
tripToSize (_s, CondAny, _) = SzAny
tripToSize (_s, CondNever xs, _) = SzNever xs
