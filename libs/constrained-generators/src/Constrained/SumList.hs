{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

-- | Operations for generating random elements of Num like types, that sum to a particular total.
--   The class `Foldy` (defined in the TheKnot.hs) gives the operations necessary to do this.
--   In this module we define the helper functions necessary to define the methods of the Foldy class.
--   The helper functions do not need to know about the Foldy class, and are not dependent upon any of
--   the mutually recursive operations defined in TheKnot, except the operations defined in the Complete class.
--   That class is defined in this module, but the instance for that class is made in TheKnot.
module Constrained.SumList where

import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (sconcat)
import System.Random (Random (..))
import Test.QuickCheck (Arbitrary, Gen, choose, shuffle, vectorOf)

import Constrained.AbstractSyntax
import Constrained.Base
import Constrained.Conformance (conformsToSpec)
import Constrained.Core (Value (..))
import Constrained.GenT (
  GE (..),
  GenT,
  MonadGenError (..),
  oneofT,
  pureGen,
  push,
  scaleT,
  sizeT,
  suchThatT,
  tryGenT,
 )
import Constrained.List (List (..), ListCtx (..))
import Constrained.NumSpec (
  IntW (..),
  MaybeBounded (..),
  NumSpec (..),
  Numeric,
  geqSpec,
  gtSpec,
  leqSpec,
  ltSpec,
  nubOrd,
 )
import Constrained.PrettyUtils
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.List ((\\))
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import qualified Data.Set as Set
import GHC.Stack
import Prettyprinter hiding (cat)

-- ====================================================================
-- What we need to know, that can only be defined in TheKnot module, is
-- abstracted into this class, which will be a precondition on the `Foldy` class

class HasSpec a => Complete a where
  -- method standing for `simplifySpec`
  simplifyA :: Specification a -> Specification a

  -- method standing for `genFromSpecT`
  genFromSpecA :: forall m. (HasCallStack, HasSpec a, MonadGenError m) => Specification a -> GenT m a

  -- method standing for method `theAddFn` from the `Foldy` class
  theAddA :: Numeric a => IntW '[a, a] a
  theAddA = AddW

-- ==========================================================
-- helpers

-- ===================================================================

noNegativeValues :: forall a. (Num a, Eq a, MaybeBounded a) => Bool
noNegativeValues = lowerBound @a == Just 0

knownUpperBound ::
  (TypeSpec a ~ NumSpec a, Ord a, Enum a, Num a, MaybeBounded a) =>
  Specification a ->
  Maybe a
knownUpperBound (ExplainSpec _ s) = knownUpperBound s
knownUpperBound TrueSpec = upperBound
knownUpperBound (MemberSpec as) = Just $ maximum as
knownUpperBound ErrorSpec {} = Nothing
knownUpperBound SuspendedSpec {} = upperBound
knownUpperBound (TypeSpec (NumSpecInterval lo hi) cant) = upper (lo <|> lowerBound) (hi <|> upperBound)
  where
    upper _ Nothing = Nothing
    upper Nothing (Just b) = listToMaybe $ [b, b - 1 ..] \\ cant
    upper (Just a) (Just b)
      | a == b = a <$ guard (a `notElem` cant)
      | otherwise = listToMaybe $ [b, b - 1 .. a] \\ cant

knownLowerBound ::
  (TypeSpec a ~ NumSpec a, Ord a, Enum a, Num a, MaybeBounded a) =>
  Specification a ->
  Maybe a
knownLowerBound (ExplainSpec _ s) = knownLowerBound s
knownLowerBound TrueSpec = lowerBound
knownLowerBound (MemberSpec as) = Just $ minimum as
knownLowerBound ErrorSpec {} = Nothing
knownLowerBound SuspendedSpec {} = lowerBound
knownLowerBound (TypeSpec (NumSpecInterval lo hi) cant) =
  lower (lo <|> lowerBound) (hi <|> upperBound)
  where
    lower Nothing _ = Nothing
    lower (Just a) Nothing = listToMaybe $ [a, a + 1 ..] \\ cant
    lower (Just a) (Just b)
      | a == b = a <$ guard (a `notElem` cant)
      | otherwise = listToMaybe $ [a, a + 1 .. b] \\ cant

isEmptyNumSpec ::
  (TypeSpec a ~ NumSpec a, Ord a, Enum a, Num a, MaybeBounded a) => Specification a -> Bool
isEmptyNumSpec = \case
  ExplainSpec _ s -> isEmptyNumSpec s
  ErrorSpec {} -> True
  TrueSpec -> False
  MemberSpec _ -> False -- MemberSpec always has at least one element (NE.NonEmpty)
  SuspendedSpec {} -> False
  TypeSpec i cant -> null $ enumerateInterval i \\ cant

-- | Note: potentially infinite list
enumerateInterval :: (Enum a, Num a, MaybeBounded a) => NumSpec a -> [a]
enumerateInterval (NumSpecInterval lo hi) =
  case (lo <|> lowerBound, hi <|> upperBound) of
    (Nothing, Nothing) -> interleave [0 ..] [-1, -2 ..]
    (Nothing, Just b) -> [b, b - 1 ..]
    (Just a, Nothing) -> [a ..]
    (Just a, Just b) -> [a .. b]
  where
    interleave [] ys = ys
    interleave (x : xs) ys = x : interleave ys xs

-- ========================================================================
-- Operations to complete the Foldy instances genNumList, genListWithSize

genNumList ::
  forall a m.
  ( MonadGenError m
  , Arbitrary a
  , Integral a
  , MaybeBounded a
  , TypeSpec a ~ NumSpec a
  , -- , Foldy a
    Random a
  , Complete a
  ) =>
  Specification a ->
  Specification a ->
  GenT m [a]
genNumList elemSIn foldSIn = do
  let extraElemConstraints
        | Just l <- knownLowerBound elemSIn
        , 0 <= l
        , Just u <- knownUpperBound foldSIn =
            leqSpec u
        | otherwise = TrueSpec
      elemSIn' = elemSIn <> extraElemConstraints
  normElemS <- normalize elemSIn'
  normFoldS <- normalize foldSIn
  let narrowedSpecs = narrowFoldSpecs (normElemS, normFoldS)
  explainNE
    ( NE.fromList
        [ "Can't generate list of ints with fold constraint"
        , "  elemSpec = " ++ show elemSIn
        , "  normElemSpec = " ++ show normElemS
        , "  foldSpec = " ++ show foldSIn
        ]
    )
    $ gen narrowedSpecs 50 [] >>= pureGen . shuffle
  where
    normalize (ExplainSpec es x) = explainSpecOpt es <$> normalize x
    normalize spec@SuspendedSpec {} = do
      sz <- sizeT
      spec' <- buildMemberSpec sz (100 :: Int) mempty spec
      normalize $ spec'
    normalize spec =
      pure $
        maybe mempty geqSpec lowerBound
          <> maybe mempty leqSpec upperBound
          <> spec

    buildMemberSpec _ 0 es _ =
      pure
        ( memberSpecList
            (Set.toList es)
            (pure "In genNumList, in buildMemberSpec 'es' is the empty list, can't make a MemberSpec from that")
        )
    buildMemberSpec sz fuel es spec = do
      me <- scaleT (const sz) $ tryGenT (genFromSpecA @a spec)
      let sz'
            | sz > 100 = sz
            | isNothing me = 2 * sz + 1
            | Just e <- me, Set.member e es = 2 * sz + 1
            | otherwise = sz
      buildMemberSpec
        sz'
        (fuel - 1)
        (maybe es (flip Set.insert es) me)
        spec

    gen ::
      forall m'. MonadGenError m' => (Specification a, Specification a) -> Int -> [a] -> GenT m' [a]
    gen (elemS, foldS) fuel lst
      | fuel <= 0
      , not $ 0 `conformsToSpec` foldS =
          genErrorNE $
            NE.fromList
              [ "Ran out of fuel in genNumList"
              , "  elemSpec =" ++ show elemSIn
              , "  foldSpec = " ++ show foldSIn
              , "  lst = " ++ show (reverse lst)
              ]
      | ErrorSpec err <- foldS = genErrorNE err
      | ErrorSpec {} <- elemS = pure lst -- At this point we know that foldS admits 0 (also this should be redundant)
      | 0 `conformsToSpec` foldS = oneofT [pure lst, nonemptyList @GE] -- TODO: distribution
      | otherwise = nonemptyList
      where
        isUnsat (elemSpec, foldSpec) = isEmptyNumSpec foldSpec || not (0 `conformsToSpec` foldSpec) && isEmptyNumSpec elemSpec
        nonemptyList :: forall m''. MonadGenError m'' => GenT m'' [a]
        nonemptyList = do
          (x, specs') <-
            explainNE
              ( NE.fromList
                  [ "Generating an element:"
                  , "  elemS = " ++ show elemS
                  , "  foldS = " ++ show foldS
                  , "  fuel  = " ++ show fuel
                  , "  lst   = " ++ show (reverse lst)
                  ]
              )
              $ do
                sz <- sizeT
                x <- genFromSpecA @a elemS
                let foldS' = propagate theAddA (HOLE :? Value x :> Nil) foldS
                    specs' = narrowByFuelAndSize (fromIntegral $ fuel - 1) sz (elemS, foldS')
                pure (x, specs')
                `suchThatT` not
                . isUnsat
                . snd
          gen specs' (fuel - 1) (x : lst)

narrowFoldSpecs ::
  forall a.
  ( TypeSpec a ~ NumSpec a
  , Arbitrary a
  , Integral a
  , Random a
  , MaybeBounded a
  , Complete a
  ) =>
  (Specification a, Specification a) ->
  (Specification a, Specification a)
narrowFoldSpecs specs = maybe specs narrowFoldSpecs (go specs)
  where
    -- Note: make sure there is some progress when returning Just or this will loop forever
    go :: (Specification a, Specification a) -> Maybe (Specification a, Specification a)
    go (simplifyA -> elemS, simplifyA -> foldS) = case (elemS, foldS) of
      -- Empty foldSpec
      (_, ErrorSpec {}) -> Nothing
      _ | isEmptyNumSpec foldS -> Just (elemS, ErrorSpec (NE.fromList ["Empty foldSpec:", show foldS]))
      -- Empty elemSpec
      (ErrorSpec {}, MemberSpec ys) | NE.toList ys == [0] -> Nothing
      (ErrorSpec {}, _)
        | 0 `conformsToSpec` foldS -> Just (elemS, MemberSpec (pure 0))
        | otherwise ->
            Just
              ( elemS
              , ErrorSpec $
                  NE.fromList
                    [ "Empty elemSpec and non-zero foldSpec"
                    , show $ indent 2 $ "elemSpec =" /> pretty elemS
                    , show $ indent 2 $ "foldSpec =" /> pretty foldS
                    ]
              )
      -- We can reduce the size of the `elemS` interval when it is
      -- `[l, u]` or `[l, ∞)` given that `0 <= l` and we have
      -- an upper bound on the sum - we can't pick things bigger than the
      -- upper bound.
      _
        | Just lo <- knownLowerBound elemS
        , 0 <= lo
        , Just hi <- knownUpperBound foldS
        , -- Check that we will actually be making the set smaller
          fromMaybe True ((hi <) <$> knownUpperBound elemS) ->
            Just (elemS <> typeSpec (NumSpecInterval (Just lo) (Just hi)), foldS)
      -- We can reduce the size of the foldS set by bumping the lower bound when
      -- there is a positive lower bound on the elemS, we can't generate things smaller
      -- than the lower bound on `elemS`.
      _
        | Just lo <- knownLowerBound elemS
        , 0 <= lo
        , not $ 0 `conformsToSpec` foldS
        , -- Check that we will actually be making the set smaller
          fromMaybe True ((lo >) <$> knownLowerBound foldS) ->
            Just (elemS, foldS <> typeSpec (NumSpecInterval (Just lo) Nothing))
      -- NOTE: this is far from sufficient, but it's good enough of an approximation
      -- to avoid the worst failures.
      _
        | Just lo <- knownLowerBound elemS
        , Just loS <- knownLowerBound foldS
        , Just hi <- knownUpperBound elemS
        , Just hiS <- knownUpperBound foldS
        , hi < loS
        , lo > hiS - lo ->
            Just
              ( ErrorSpec $ NE.fromList ["Can't solve diophantine equation"]
              , ErrorSpec $ NE.fromList ["Can't solve diophantine equation"]
              )
      _ -> Nothing

narrowByFuelAndSize ::
  forall a.
  ( TypeSpec a ~ NumSpec a
  , Arbitrary a
  , Integral a
  , Random a
  , MaybeBounded a
  , Complete a
  ) =>
  -- | Fuel
  a ->
  -- | Integer
  Int ->
  (Specification a, Specification a) ->
  (Specification a, Specification a)
narrowByFuelAndSize fuel size specpair =
  loop (100 :: Int) (onlyOnceTransformations $ (narrowFoldSpecs specpair))
  where
    loop 0 specs =
      error $
        unlines
          [ "narrowByFuelAndSize loops:"
          , "  fuel = " ++ show fuel
          , "  size = " ++ show size
          , "  specs = " ++ show specs
          , "  narrowFoldSpecs spec = " ++ show (narrowFoldSpecs specs)
          , "  go (narrowFoldSpecs specs) = " ++ show (go (narrowFoldSpecs specs))
          ]
    loop n specs = case go specs of
      Nothing -> specs
      Just specs' -> loop (n - 1) (narrowFoldSpecs specs')

    -- Transformations only applied once. It's annoying to check if you're
    -- going to change the spec with these so easier to just make sure you only apply
    -- these once
    onlyOnceTransformations (elemS, foldS)
      | fuel == 1 = (elemS <> foldS, foldS)
      | otherwise = (elemS, foldS)

    canReach _ 0 s = s == 0
    canReach e currentfuel s
      -- You can reach it in one step
      | s <= e = 0 < currentfuel
      | otherwise = canReach e (currentfuel - 1) (s - e)

    -- Precondition:
    --   a is negative
    --   the type has more negative numbers than positive ones
    safeNegate a
      | Just u <- upperBound
      , a < negate u =
          u
      | otherwise = negate a

    divCeil a b
      | b * d < a = d + 1
      | otherwise = d
      where
        d = a `div` b

    go :: (Specification a, Specification a) -> Maybe (Specification a, Specification a)
    go (simplifyA -> elemS, simplifyA -> foldS)
      -- There is nothing we can do
      | fuel == 0 = Nothing
      | ErrorSpec {} <- elemS = Nothing
      | ErrorSpec {} <- foldS = Nothing
      -- Give up as early as possible
      | Just 0 <- knownUpperBound elemS
      , Just 0 <- knownLowerBound elemS
      , not $ 0 `conformsToSpec` foldS =
          Just (ErrorSpec (NE.fromList ["only 0 left"]), foldS)
      -- Make sure we try to generate the smallest possible list
      -- that gives you the right result - don't put a bunch of zeroes in
      -- a _small_ (size 0) list.
      | size == 0
      , 0 `conformsToSpec` elemS =
          Just (elemS <> notEqualSpec 0, foldS)
      -- Member specs with non-zero elements, TODO: explain
      | MemberSpec ys <- elemS
      , let xs = NE.toList ys
      , Just u <- knownUpperBound foldS
      , all (0 <=) xs
      , any (0 <) xs
      , let xMinP = minimum $ filter (0 <) xs
            possible x = x == u || xMinP <= u - x
            xs' = filter possible xs
      , xs' /= xs =
          Just (memberSpecList (nubOrd xs') (pure ("None of " ++ show xs ++ " are possible")), foldS)
      -- The lower bound on the number of elements is too low
      | Just e <- knownLowerBound elemS
      , e > 0
      , Just s <- knownLowerBound foldS
      , s > 0
      , let c = divCeil s fuel
      , e < c =
          Just (elemS <> geqSpec c, foldS)
      -- The upper bound on the number of elements is too high
      | Just e <- knownUpperBound elemS
      , e < 0
      , Just s <- knownUpperBound foldS
      , s < 0
      , let c = divCeil (safeNegate s) fuel
      , negate c < e
      , maybe True (c <) (knownUpperBound elemS) =
          Just (elemS <> leqSpec c, foldS)
      -- It's time to stop generating negative numbers
      | Just s <- knownLowerBound foldS
      , s > 0
      , Just e <- knownUpperBound elemS
      , e > 0
      , not $ canReach e (fuel `div` 2 + 1) s
      , maybe True (<= 0) (knownLowerBound elemS) =
          Just (elemS <> gtSpec 0, foldS)
      -- It's time to stop generating positive numbers
      | Just s <- knownUpperBound foldS
      , s < 0
      , Just e <- knownLowerBound elemS
      , e < 0
      , not $ canReach (safeNegate e) (fuel `div` 2 + 1) (safeNegate s)
      , maybe True (0 <=) (knownUpperBound elemS) =
          Just (elemS <> ltSpec 0, foldS)
      -- There is nothing we need to do
      | otherwise = Nothing

-- =====================================================================================
-- Like genList, but generate a list whose size conforms to s SizeSpec
-- =====================================================================================

-- | Generate a list with 'sizeSpec' elements, that add up to a total that conforms
--   to 'foldSpec'. Every element in the list should conform to 'elemSpec'
genListWithSize ::
  forall a m.
  ( Complete a
  , TypeSpec a ~ NumSpec a
  , MonadGenError m
  , Random a
  , Integral a
  , Arbitrary a
  , MaybeBounded a
  , Complete Integer
  , TypeSpec Integer ~ NumSpec Integer
  ) =>
  Specification Integer ->
  Specification a ->
  Specification a ->
  GenT m [a]
genListWithSize sizeSpec elemSpec foldSpec
  | TrueSpec <- sizeSpec = genNumList elemSpec foldSpec
  | ErrorSpec _ <- sizeSpec <> geqSpec 0 =
      fatalErrorNE
        ( NE.fromList
            [ "genListWithSize called with possible negative size"
            , "  sizeSpec = " ++ specName sizeSpec
            , "  elemSpec = " ++ specName elemSpec
            , "  foldSpec = " ++ specName foldSpec
            ]
        )
  | otherwise = do
      total <- genFromSpecA @a foldSpec
      -- The compatible sizes for the list, for a given choice of total
      let sizeAdjusted =
            if total /= 0
              then sizeSpec <> gtSpec 0 -- if total is not zero, we better not pick a 0 size
              else
                if lowerBound @a == Just 0 -- Type `a` has no negative numbers (Natural, Word8, Word16, Word 32, Word64)
                  then sizeSpec <> equalSpec 0 -- if it is zero, and negative numbers not allowed, then only possible size is 0
                  else sizeSpec <> gtSpec 0
          message =
            [ "\nGenSizedList fails"
            , "sizespec = " ++ specName sizeSpec
            , "elemSpec = " ++ specName elemSpec
            , "foldSpec = " ++ specName foldSpec
            , "total choosen from foldSpec = " ++ show total
            , "size adjusted for total = " ++ show sizeAdjusted
            ]
      push message $ do
        count <- genFromSpecA @Integer sizeAdjusted
        case compare total 0 of
          EQ ->
            if count == 0
              then pure []
              else pickPositive elemSpec total count
          GT -> pickPositive elemSpec total count
          LT -> pickNegative elemSpec total count

pickPositive ::
  forall t m.
  (Integral t, Random t, MonadGenError m, TypeSpec t ~ NumSpec t, Complete t) =>
  Specification t ->
  t ->
  Integer ->
  GenT m [t]
pickPositive elemspec total count = do
  sol <-
    pureGen $
      pickAll
        (minFromSpec 0 elemspec) -- Search from [0..total] unless elemspec says otherwise
        (maxFromSpec total elemspec)
        (predSpecPair elemspec)
        total
        (fromInteger count)
        (Cost 0)
  case snd sol of
    No msgs -> fatalErrorNE (NE.fromList msgs)
    Yes (x :| _) -> pure x

pickNegative ::
  forall t m.
  (Integral t, Complete t, Random t, MonadGenError m, TypeSpec t ~ NumSpec t) =>
  Specification t ->
  t ->
  Integer ->
  GenT m [t]

-- | total can be either negative, or 0. If it is 0, we want `count` numbers that add to `zero`
pickNegative elemspec total count = do
  sol <-
    pureGen $
      pickAll
        -- Recall 'total' is negative here.
        -- Here is a graphic of the range we search in (smallest .. largest)
        -- [(total+n) .. total .. 0 .. (0-n)],  where n = (total `div` 4) which is negative.
        (minFromSpec (total + (total `div` 4)) elemspec)
        (maxFromSpec (0 - (total `div` 4)) elemspec)
        (predSpecPair elemspec)
        total
        (fromInteger count)
        (Cost 0)
  case snd sol of
    No msgs -> fatalErrorNE (NE.fromList msgs)
    Yes (x :| _) -> pure x

specName :: forall a. HasSpec a => Specification a -> String
specName (ExplainSpec [x] _) = x
specName x = show x

predSpecPair :: forall a. HasSpec a => Specification a -> (String, a -> Bool)
predSpecPair spec = (specName spec, (`conformsToSpec` spec))

-- | The smallest number admitted by the spec, if we can find one.
--   if not return the defaultValue 'dv'
minFromSpec ::
  forall n.
  (Ord n, Complete n, TypeSpec n ~ NumSpec n) =>
  n ->
  Specification n ->
  n
minFromSpec dv (ExplainSpec _ spec) = minFromSpec @n dv spec
minFromSpec dv TrueSpec = dv
minFromSpec dv s@(SuspendedSpec _ _) =
  case simplifyA s of
    SuspendedSpec {} -> dv
    x -> minFromSpec @n dv x
minFromSpec dv (ErrorSpec _) = dv
minFromSpec _ (MemberSpec xs) = minimum xs
minFromSpec dv (TypeSpec (NumSpecInterval lo _) _) = maybe dv id lo

-- | The largest number admitted by the spec, if we can find one.
--   if not return the defaultValue 'dv'
maxFromSpec ::
  forall n.
  (Ord n, Complete n, TypeSpec n ~ NumSpec n) =>
  n ->
  Specification n ->
  n
maxFromSpec dv (ExplainSpec _ spec) = maxFromSpec @n dv spec
maxFromSpec dv TrueSpec = dv
maxFromSpec dv s@(SuspendedSpec _ _) =
  case simplifyA s of
    SuspendedSpec {} -> dv
    x -> maxFromSpec @n dv x
maxFromSpec dv (ErrorSpec _) = dv
maxFromSpec _ (MemberSpec xs) = maximum xs
maxFromSpec dv (TypeSpec (NumSpecInterval _ hi) _) = maybe dv id hi

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

-- | Given 'count', return a list of pairs, that add to 'count'
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
  t ->
  t ->
  (String, t -> Bool) ->
  t ->
  Int ->
  Cost ->
  Gen (Cost, Solution t)
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
pickAll smallest largest (pName, p) total 0 cost =
  if total == 0 && p total
    then pure (cost, Yes $ pure [])
    else
      pure
        ( cost
        , No
            [ "We are trying to find list of length 0."
            , "  Whose sum is " ++ show total ++ "."
            , "  That is only possible if the sum == 0."
            , "  All elements have to satisfy " ++ pName
            , "  smallest = " ++ show smallest
            , "  largest = " ++ show largest
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
