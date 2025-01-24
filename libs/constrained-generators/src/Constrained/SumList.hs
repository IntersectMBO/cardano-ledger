{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Constrained.SumList where

import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (sconcat)
import System.Random (Random (..))
import Test.QuickCheck (Gen, choose, shuffle, vectorOf)

-- =======================================================
-- Helper functions for genSizedList

data Solution t = Yes (NonEmpty [t]) | No [String]
  deriving (Eq)

instance Show t => Show (Solution t) where
  show (No xs) = "No" ++ "\n" ++ unlines xs
  show (Yes xs) = "Yes " ++ show xs

-- | The basic idea is to concat all the Yes's and skip over the No's.
--   The one wrinkle is if everything is No, then in that case return an arbitrary one of the No's.
--   This can be done in linear time in the length of the list. Call that length n.
--   Check for all No. This takes time proportional to n. If it is true return one of them.
--   If it is not all No, then concat all the Yes, and skip all the No.
--   We find the first No (if it exist), and all the Yes by partitioning the list
--   This is similar in spirit to Constrained.GenT.catGEs, but doesn't require a
--   a Monad to escape on the first No.
concatSolution :: Show t => t -> t -> String -> t -> Int -> [Solution t] -> Solution t
concatSolution smallest largest pName total count sols =
  case partitionEithers (map (\case Yes x -> Left x; No x -> Right x) sols) of
    ([], n : _) -> No n -- All No, arbitrarily return the first.
    (y : ys, _) -> Yes $ sconcat (y :| ys) -- At least one Yes, and all No's skipped ('ys')
    ([], []) ->
      No -- The list is empty
        [ "\nThe sample in pickAll was empty"
        , "  smallest = " ++ show smallest
        , "  largest = " ++ show largest
        , "  pred = " ++ pName
        , "  total = " ++ show total
        , "  count = " ++ show count
        ]

newtype Cost = Cost Int deriving (Eq, Show, Num, Ord)

firstYesG ::
  Monad m => Solution t -> (x -> Cost -> m (Cost, Solution t)) -> [x] -> Cost -> m (Cost, Solution t)
firstYesG nullSolution f xs c = go xs c
  where
    go [] cost = pure (cost, nullSolution)
    go [x] cost = f x (cost + 1)
    go (x : more) cost = do
      ans <- f x (cost + 1)
      case ans of
        (cost1, No _) -> go more cost1
        (_, Yes _) -> pure ans

noChoices :: Show t => Cost -> String -> t -> t -> t -> Int -> [(t, t)] -> Solution t
noChoices cost p smallest largest total count samp =
  No
    [ "\nNo legal choice can be found, where for each sample (x,y)"
    , "x+y = total && predicate x && predicate y"
    , "  predicate = " ++ p
    , "  smallest = " ++ show smallest
    , "  largest = " ++ show largest
    , "  total = " ++ show total
    , "  count = " ++ show count
    , "  cost = " ++ show cost
    , "Small sample of what was explored"
    , show samp
    ]

-- =====================================================

-- | Given 'count', return a list if pairs, that add to 'count'
--   splitsOf 6 --> [(1,5),(2,4),(3,3)].
--   Note we don't return reflections like (5,1) and (4,2),
--   as they have the same information as (1,5) and (2,4).
splitsOf :: Integral b => b -> [(b, b)]
splitsOf count = [(i, j) | i <- [1 .. div count 2], let j = count - i]
{-# SPECIALIZE splitsOf :: Int -> [(Int, Int)] #-}

-- | Given a Path, find a representative solution, 'ans', for that path, such that
--   1) (length ans) == 'count',
--   2) (sum ans) == 'total'
--   3) (all p ans) is True
--   What is a path?
--   Suppose i==5, then we recursively explore every way to split 5 into
--   split pairs that add to 5. I.e. (1,4) (2,3), then we split each of those.
--   Here is a picture of the graph of all paths for i==5. A path goes from the root '5'
--   to one of the leaves. Note all leaves are count == '1 (where the solution is '[total]').
--   To solve for 5, we could solve either of the sub problems rooted at 5: [1,4] or [2,3].
--   In 'pickAll' we will try to solve both, but in pick1, we only attempt 1 of those sub problems.
--   5
--   |
--   [1,4]
--   |  |
--   |  [1,3]
--   |  |  |
--   |  |  [1,2]
--   |  |     |
--   |  |     [1,1]
--   |  |
--   |  [2,2]
--   |   | |
--   |   | [1,1]
--   |   |
--   |   [1,1]
--   |
--   [2,3]
--    | |
--    | [1,2]
--    |    |
--    |    [1,1]
--    [1,1]
--  In 'pickAll' will explore a path for every split of 'count'
--  so if it returns (No _), we can be somewhat confidant that no solution exists.
--  Note that count of 1 and 2, are base cases.
--  When 'count' is greater than 1, we need to sample from [smallest..total],
--  so 'smallest' better be less that or equal to 'total'
pickAll ::
  forall t.
  (Show t, Integral t, Random t) =>
  t -> t -> (String, t -> Bool) -> t -> Int -> Cost -> Gen (Cost, Solution t)
pickAll smallest largest (pName, _) total count cost
  | cost > 1000 =
      pure $
        ( cost
        , No
            [ "\nPickAll exceeds cost limit " ++ show cost
            , "  predicate = " ++ pName
            , "  smallest = " ++ show smallest
            , "  largest = " ++ show largest
            , "  total = " ++ show total
            , "  count = " ++ show count
            ]
        )
pickAll smallest largest (pName, p) total 1 cost =
  if p total
    then pure (cost, Yes $ pure [total])
    else pure (cost, noChoices cost pName smallest largest total 1 [(total, 0)])
pickAll smallest largest (pName, _) total count cost
  | smallest > largest =
      pure $
        ( cost
        , No
            [ "\nThe feasible range to pickAll ["
                ++ show smallest
                ++ " .. "
                ++ show (div total 2)
                ++ "] was empty"
            , "  predicate = " ++ pName
            , "  smallest = " ++ show smallest
            , "  largest = " ++ show largest
            , "  total = " ++ show total
            , "  count = " ++ show count
            , "  cost = " ++ show cost
            ]
        )
pickAll smallest largest (pName, p) total 2 cost = do
  -- for small things, enumerate all possibilities
  -- for large things, use a fair sample.
  choices <- smallSample smallest largest total 1000 100
  case filter (\(x, y) -> p x && p y) choices of
    [] -> pure $ (cost + 1, noChoices cost pName smallest largest total 2 (take 10 choices))
    zs -> pure $ (cost + 1, Yes $ NE.fromList (fmap (\(x, y) -> [x, y]) zs))
pickAll smallest largest (pName, p) total count cost = do
  -- Compute a representative sample of the choices between smallest and total.
  -- E.g. when smallest = -2, and total = 5, the complete set of values is:
  -- [(-2,7),(-1,6),(0,5),(1,4),(2,3),(3,2),(4,1),(5,0)]  Note they all add to 5
  -- We could explore the whole set of values, but that can be millions of choices.
  -- so we choose to explore a representative subset. See the function 'fairSample', for details.
  -- Remember this is just 1 step on one path. So if this step fails, there are many more
  -- paths to explore. In fact there are usually many many solutions. We need to find just 1.
  choices <- smallSample smallest largest total 1000 20
  -- The choice of splits is crucial. If total >> count, we want the larger splits first
  -- if count >> total , we want smaller splits first
  splits <-
    if count >= 20
      then shuffle $ take 10 (splitsOf count)
      else
        if total > fromIntegral count
          then pure (reverse (splitsOf count))
          else pure (splitsOf count)

  firstYesG
    (No ["\nNo split has a solution", "cost = " ++ show cost])
    (doSplit smallest largest (pName, p) total choices)
    splits
    cost

-- TODO run some tests to see if this is a better solution than firstYesG
-- concatSolution smallest pName total count
--  <$> mapM  (doSplit smallest largest total (pName, p) choices (pickAll (depth +1) smallest)) splits

-- {-# SPECIALIZE pickAll::Int -> (String, Int -> Bool) -> Int -> Int -> Cost -> Gen (Cost, Solution Int) #-}

doSplit ::
  (Random t, Show t, Integral t) =>
  t ->
  t ->
  (String, t -> Bool) ->
  t ->
  [(t, t)] ->
  -- (t -> (String, t -> Bool) -> t -> Int -> Cost -> Gen (Cost, Solution t)) ->
  (Int, Int) ->
  Cost ->
  Gen (Cost, Solution t)
doSplit smallest largest (pName, p) total sample (i, j) c = go sample c
  where
    -- The 'sample' is a list of pairs (x,y), where we know (x+y) == total.
    -- We will search for the first good solution in the given sample
    -- to build a representative value for this path, with split (i,j).
    go ((x, y) : more) cost0 = do
      -- Note (i+j) = current length of the ans we are looking for
      --      (x+y) = total
      -- pick 'ans1' such that (sum ans1 == x) and (length ans1 == i)
      (cost1, ans1) <- pickAll smallest largest (pName, p) x i cost0
      -- pick 'ans2' such that (sum ans2 == y) and (length ans2 == j)
      (cost2, ans2) <- pickAll smallest largest (pName, p) y j cost1
      case (ans1, ans2) of
        (Yes ys, Yes zs) -> pure $ (cost2, Yes (NE.fromList [a <> b | a <- NE.toList ys, b <- NE.toList zs]))
        _ -> go more cost2
    go [] cost =
      case sample of
        [] ->
          pure $
            ( cost
            , No
                [ "\nThe sample passed to doSplit [" ++ show smallest ++ " .. " ++ show (div total 2) ++ "] was empty"
                , "  predicate = " ++ pName
                , "  smallest = " ++ show smallest
                , "  largest = " ++ show largest
                , "  total " ++ show total
                , "  count = " ++ show (i + j)
                , "  split of count = " ++ show (i, j)
                ]
            )
        ((left, right) : _) ->
          pure $
            ( cost
            , No
                [ "\nAll choices in (genSizedList " ++ show (i + j) ++ " 'p' " ++ show total ++ ") have failed."
                , "Here is 1 example failure."
                , "  smallest = " ++ show smallest
                , "  largest = " ++ show largest
                , "  total " ++ show total ++ " = " ++ show left ++ " + " ++ show right
                , "  count = " ++ show (i + j) ++ ", split of count = " ++ show (i, j)
                , "We are trying to solve sub-problems like:"
                , "  split " ++ show left ++ " into " ++ show i ++ " parts, where all parts meet 'p'"
                , "  split " ++ show right ++ " into " ++ show j ++ " parts, where all parts meet 'p'"
                , "Predicate 'p' = " ++ pName
                , "A small prefix of the sample, elements (x,y) where x+y = " ++ show total
                , unlines (map (("  " ++) . show) (take 10 sample))
                ]
            )
{-# INLINE doSplit #-}

-- | If the sample is small enough, then enumerate all of it, otherwise take a fair sample.
smallSample :: (Random t, Integral t) => t -> t -> t -> t -> Int -> Gen [(t, t)]
smallSample smallest largest total bound size
  | largest - smallest <= bound = do
      shuffle $ takeWhile (uncurry (<=)) [(x, total - x) | x <- [smallest .. total]]
  | otherwise = do
      choices <- fair smallest largest size 5 True
      shuffle [(x, total - x) | x <- choices]
{-# INLINE smallSample #-}

-- | Generates a fair sample of numbers between 'smallest' and 'largest'.
--   makes sure there are numbers of all sizes. Controls both the size of the sample
--   and the precision (how many powers of 10 are covered)
--   Here is how we generate one sample when we call (fair (-3455) (10234) 12 3 True)
--   raw = [(-9999,-1000),(-999,-100),(-99,-10),(-9,-1),(0,9),(10,99),(100,999),(1000,9999),(10000,99999)]
--   ranges = [(-3455,-1000),(-999,-100),(-99,-10),(-9,-1),(0,9),(10,99),(100,999),(1000,9999),(10000,10234)]
--   count = 4
--   largePrecision = [(10000,10234),(1000,9999),(100,999)]
--   smallPrecision = [(-3455,-1000),(-999,-100),(-99,-10)]
--   answer generated = [10128,10104,10027,10048,4911,7821,5585,2157,448,630,802,889]
--   isLarge==True   means be biased towards the large end of the range,
--   isLArge==False  means be biased towards the small end of the range,
fair :: (Random a, Integral a) => a -> a -> Int -> Int -> Bool -> Gen [a]
fair smallest largest size precision isLarge =
  concat <$> mapM oneRange (if isLarge then largePrecision else smallPrecision)
  where
    raw = map logRange [logish smallest .. logish largest]
    fixEnds (x, y) = (max smallest x, min largest y)
    ranges = map fixEnds raw
    count = div size precision
    largePrecision = take precision (reverse ranges)
    smallPrecision = take precision ranges
    oneRange (x, y) = vectorOf count (choose (x, y))

logRange :: Integral a => a -> (a, a)
logRange 1 = (10, 99)
logRange (-1) = (-9, -1)
logRange n = case compare n 0 of
  EQ -> (0, 9)
  LT -> (negate (div b 10), negate (div a 10))
  GT -> (10 ^ n, 10 ^ (n + 1) - 1)
  where
    (a, b) = logRange (negate n)

-- | like (logBase10 n), except negative answers mean negative numbers, rather than fractions less than 1.
logish :: Integral t => t -> t
logish n
  | 0 <= n && n <= 9 = 0
  | n > 9 = 1 + logish (n `div` 10)
  | (-9) <= n && n <= (-1) = -1
  | True = negate (1 + logish (negate n))

-- =====================================================================
