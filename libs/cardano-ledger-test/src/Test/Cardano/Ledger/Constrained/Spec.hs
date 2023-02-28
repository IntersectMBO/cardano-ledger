{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A 'Spec' is a first order data structure that denotes a random generator
--   For example
--   (MapSpec era dom rng) denotes Gen(Map dom rng)
--   (RngSpec era t)       denotes Gen[t]  where the [t] has some Summing properties
--   (RelSep era t)        denotes Gen(Set t) where the set meets some relational properties
--   (Size)                denotes Gen Int, the size of some Map, Set, List etc.
--   (SumSpec n t)         denotes Gen([t]), a list of length 'n' that adds up to 't'
module Test.Cardano.Ledger.Constrained.Spec where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (Era (..))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import Debug.Trace (trace)
import Test.Cardano.Ledger.Constrained.Ast (runSize)
import Test.Cardano.Ledger.Constrained.Classes (
  Adds (..),
  SumCond (..),
  Sums (..),
  projAdds,
  runCond,
  sumAdds,
  --  anySumV,
  --  nonNegSumV,
 )
import Test.Cardano.Ledger.Constrained.Combinators (
  errorMess,
  fixSet,
  itemFromSet,
  setSized,
  subsetFromSet,
  suchThatErr,
  superSetFromSet,
  superSetFromSetWithSize,
 )
import Test.Cardano.Ledger.Constrained.Env (Access (No), V (..))
import Test.Cardano.Ledger.Constrained.Monad
import Test.Cardano.Ledger.Constrained.TypeRep (
  Rep (..),
  Size (..),
  genRep,
  synopsis,
  testEql,
  (:~:) (Refl),
 )
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Generic.Proof (C_Crypto, MaryEra)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Tasty
import Test.Tasty.QuickCheck hiding (Fixed, total)
import Prelude hiding (subtract)

-- ===========================================================
{- TODO, possible extensions and improvements, so we don't forget
1) Redo Size with: data Size = SzNever [String] | SzRange Int (Maybe Int) Move to own file
2) Add newtype Never = Never [String], then instead of (XXXNever xs) we use (XX (Never xs))
3) class Specify
4) In RelSpec add (RelOp musthave canhave neverhave) to replace subset, superset, disjoint, can also express membership
5) A better story about fields in constraints. Maybe add FieldSpec type.
-}

-- =========================================================

-- | used when we run tests, and we have to pick some concrete Era
type TT = MaryEra C_Crypto

seps :: [String] -> String
seps xs = List.intercalate " " xs

sepsP :: [String] -> String
sepsP xs = "(" ++ List.intercalate " " xs ++ ")"

sepn :: [String] -> String
sepn xs = List.intercalate "\n   " xs

-- ============================================================
-- Operators for Size (Defined in TypeRep.hs)

-- | Used in tests so things don't get too large
atleastdelta :: Int
atleastdelta = 5

-- | Used in tests so things don't get too large
--   If we can't find an era using things of size 10
--   using things of size 100, isn't going to help.
atmostany :: Int
atmostany = 10

maxSize :: Size -> Int
maxSize SzAny = atmostany
maxSize (SzLeast i) = i + atleastdelta
maxSize (SzMost n) = n
maxSize (SzRng _ j) = j
maxSize (SzNever xs) = errorMess "SzNever in maxSize" xs

minSize :: Size -> Int
minSize SzAny = 0
minSize (SzLeast n) = n
minSize (SzMost _) = 0
minSize (SzRng i _) = i
minSize (SzNever xs) = errorMess "SzNever in minSize" xs

genSize :: Gen Size
genSize =
  frequency
    [ (1, SzLeast <$> pos)
    , (1, SzMost <$> anyAdds)
    , (4, (\x -> SzRng x x) <$> pos)
    , (1, do lo <- anyAdds; hi <- greater lo; pure (SzRng lo hi))
    ]

-- | Use to generate real sizes, where there are no negative numbers,
--   and the smallest possible size is 0.
--   Use this only where you know it is NOT SzNever
genFromSize :: Size -> Gen Int
genFromSize (SzNever _) = error "Bad call to (genFromSize(SzNever ..))."
genFromSize SzAny = chooseInt (0, atmostany)
genFromSize (SzRng i j) = chooseInt (i, j)
genFromSize (SzLeast i) = chooseInt (i, i + atleastdelta)
genFromSize (SzMost i) = chooseInt (0, i)

-- | Similar to genFromSize, but allows negative numbers (unlike size where the smallest Int is 0)
genFromIntRange :: Size -> Gen Int
genFromIntRange (SzNever _) = error "Bad call to (genFromIntRange(SzNever ..))."
genFromIntRange SzAny = chooseInt (-atmostany, atmostany)
genFromIntRange (SzRng i j) = chooseInt (i, j)
genFromIntRange (SzLeast i) = chooseInt (i, i + atleastdelta)
genFromIntRange (SzMost i) = chooseInt (i - atmostany, i)

testSoundSize :: Gen Bool
testSoundSize = do
  spec <- genSize
  ans <- genFromSize spec
  pure $ runSize ans spec

testMergeSize :: Gen Bool
testMergeSize = do
  spec1 <- genSize
  spec2 <- genSize
  case (spec1 <> spec2) of
    SzNever _xs -> pure True
    SzAny -> pure True
    spec -> do
      ans <- genFromSize spec
      pure $ runSize ans spec && runSize ans spec1 && runSize ans spec2

-- =====================================================
-- RelSpec

data RelSpec era dom where
  RelAny ::
    -- | There is no restriction on the set. Denotes the universe.
    RelSpec era dom
  RelEqual ::
    Ord dom =>
    Rep era dom ->
    Set dom ->
    -- | Must be a subset
    RelSpec era dom
  RelNever ::
    -- | Something is inconsistent
    [String] ->
    RelSpec era dom
  RelOper ::
    Ord d =>
    Rep era d ->
    Set d ->
    Maybe (Set d) ->
    -- | Denotes things like: (x == y) equality, (x ⊆ y) subset, ( x ∩ y = ∅) disjointness, (x ⊇ y) superset.
    -- Invariants of r@(RepOper must (Just may) cant)
    -- 1) must is a subset of may
    -- 2) must is disjoint from cant
    -- 3) (sizeFromRel r) is realizable E.g.  (SzRng 10 3) is NOT realizable
    Set d ->
    RelSpec era d

relSubset, relSuperset, relDisjoint :: Ord t => Rep era t -> Set t -> RelSpec era t
relSubset r set = RelOper r Set.empty (Just set) Set.empty
relSuperset r set = RelOper r set Nothing Set.empty
relDisjoint r set = RelOper r Set.empty Nothing set

instance Monoid (RelSpec era dom) where mempty = RelAny

instance Semigroup (RelSpec era dom) where
  (<>) = mergeRelSpec

instance Show (RelSpec era dom) where show = showRelSpec

instance LiftT (RelSpec era a) where
  liftT (RelNever xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = RelNever s
  dropT (Typed (Right x)) = x

showRelSpec :: RelSpec era dom -> String
showRelSpec RelAny = "RelAny"
showRelSpec (RelOper r x (Just s) y) | Set.null y && x == s = sepsP ["RelEqual", synopsis (SetR r) x]
showRelSpec (RelOper r x (Just s) y) | Set.null x && Set.null y = sepsP ["RelSubset", synopsis (SetR r) s]
-- showRelSpec (RelOper _ x Nothing y) | Set.null x && Set.null y = "RelAny"
showRelSpec (RelOper r x Nothing y) | Set.null y = sepsP ["RelSuperset", synopsis (SetR r) x]
showRelSpec (RelOper r x Nothing y) | Set.null x = sepsP ["RelDisjoint", synopsis (SetR r) y]
showRelSpec (RelOper r x Nothing y) = sepsP ["RelOper", synopsis (SetR r) x, "Univ", synopsis (SetR r) y]
showRelSpec (RelOper r x (Just y) z) = sepsP ["RelOper", synopsis (SetR r) x, synopsis (SetR r) y, synopsis (SetR r) z]
showRelSpec (RelEqual r x) = sepsP ["RelEqual", synopsis (SetR r) x]
showRelSpec (RelNever _) = "RelNever"

mergeRelSpec :: RelSpec era d -> RelSpec era d -> RelSpec era d
mergeRelSpec (RelNever xs) (RelNever ys) = RelNever (xs ++ ys)
mergeRelSpec d@(RelNever _) _ = d
mergeRelSpec _ d@(RelNever _) = d
mergeRelSpec RelAny x = x
mergeRelSpec x RelAny = x
mergeRelSpec (RelEqual r x) y = mergeRelSpec (RelOper r x (Just x) Set.empty) y
mergeRelSpec y (RelEqual r x) = mergeRelSpec y (RelOper r x (Just x) Set.empty)
mergeRelSpec a@(RelOper r must1 may1 cant1) b@(RelOper _ must2 may2 cant2) =
  dropT $
    explain ("merging " ++ show a ++ "\n        " ++ show b) $
      relOper r (Set.union must1 must2) (interSectM may1 may2) (Set.union cant1 cant2)

-- | Check that RelOper invariants hold
relOper :: Ord d => Rep era d -> Set d -> Maybe (Set d) -> Set d -> Typed (RelSpec era d)
relOper r sup sub disj =
  explain
    ("Checking RelSpec self consistency\n   " ++ show (RelOper r sup sub disj))
    (help sup sub disj)
  where
    help must (Just may) cant | must == may && Set.disjoint must cant = pure $ RelEqual r must
    help must Nothing cant =
      if Set.disjoint must cant
        then pure $ RelOper r must Nothing cant
        else pure $ RelNever ["'must' " ++ synopsis (SetR r) must ++ " Is not disjoint from: 'disj' " ++ synopsis (SetR r) cant]
    help must (Just may) cant = case (Set.isSubsetOf must may, Set.disjoint must cant) of
      (True, True) ->
        let potentialAns = RelOper r must (Just may) cant
         in case sizeForRel potentialAns of
              sz@(SzRng lo hi) | lo > hi -> failT ["potentialAns " ++ show potentialAns ++ " has unrealizable size " ++ show sz]
              SzNever xs -> failT xs
              _ -> pure potentialAns
      (False, True) -> failT ["'must' " ++ synopsis (SetR r) must ++ " Is not a subset of: 'may' " ++ synopsis (SetR r) may]
      (True, False) -> failT ["'must' " ++ synopsis (SetR r) must ++ " Is not disjoint from: 'cant' " ++ synopsis (SetR r) cant]
      (False, False) ->
        failT
          [ "'must' " ++ synopsis (SetR r) must ++ " Is not disjoint from: 'cant' " ++ synopsis (SetR r) cant
          , "'must' " ++ synopsis (SetR r) must ++ " Is not a subset of: 'may' " ++ synopsis (SetR r) may
          ]

-- | The interpretation of (Just set) is set, and of Nothing is the universe (all possible sets)
interSectM :: Ord a => Maybe (Set a) -> Maybe (Set a) -> Maybe (Set a)
interSectM Nothing Nothing = Nothing
interSectM Nothing x = x
interSectM x Nothing = x
interSectM (Just x) (Just y) = Just (Set.intersection x y)

-- | test that a set 's' meets the RelSpec
runRelSpec :: Ord t => Set t -> RelSpec era t -> Bool
runRelSpec s (RelEqual _ set) = s == set
runRelSpec _ RelAny = True
runRelSpec _ (RelNever xs) = errorMess "RelNever in call to runRelSpec" xs
runRelSpec s (RelOper _ sup Nothing disj) = Set.isSubsetOf sup s && Set.disjoint s disj
runRelSpec s (RelOper _ sup (Just sub) disj) = Set.isSubsetOf sup s && Set.isSubsetOf s sub && Set.disjoint s disj

-- | Compute the Size that bounds the size of a set generated from a RelSpec
sizeForRel :: RelSpec era dom -> Size
sizeForRel (RelEqual _ s) = SzExact (Set.size s)
sizeForRel RelAny = SzAny
sizeForRel (RelNever _) = SzAny
sizeForRel (RelOper _ sup Nothing _) = SzLeast (Set.size sup)
sizeForRel (RelOper _ sup (Just sub) _) | Set.null sup = SzMost (Set.size sub)
sizeForRel (RelOper _ sup (Just sub) disj) = SzRng (Set.size sup) (Set.size (Set.difference sub disj))

-- | return a generator that always generates things that meet the RelSpec
genFromRelSpec :: forall era t. Ord t => [String] -> Int -> Gen t -> RelSpec era t -> Gen (Set t)
genFromRelSpec msgs n g spec =
  let msg = ("genFromRelSpec " ++ show n ++ " " ++ show spec)
   in case spec of
        RelNever xs -> errorMess "RelNever in genFromSpec" (msgs ++ xs)
        RelAny -> setSized (msg : msgs) n g
        RelEqual _ s -> pure s
        RelOper _ sup Nothing dis ->
          -- add things (not in disj) to 'sup' until you get to size 'n'
          fixSet (msg : msgs) 1000 n (suchThatErr (msg : msgs) g (`Set.notMember` dis)) sup
        RelOper _ sup (Just sub) dis ->
          let choices = Set.difference sub dis
              m = Set.size choices
           in -- add things (from choices) to 'sup' until you get to size 'n'
              case compare m n of
                EQ -> pure choices
                LT ->
                  errorMess
                    ( "Size inconsistency. We need "
                        ++ show n
                        ++ ". The most we can get from (sub-disj) is "
                        ++ show m
                    )
                    (msg : msgs)
                GT -> fixSet (("choices " ++ show (Set.size choices)) : msg : msgs) 1000 n (itemFromSet (msg : msgs) choices) sup

-- | Generate a random RelSpec
genRelSpec :: Ord dom => [String] -> Int -> Gen dom -> Rep era dom -> Gen (RelSpec era dom)
genRelSpec _ 0 _ r = pure $ RelEqual r Set.empty
genRelSpec msg n genD r = do
  smaller <- choose (1, n)
  larger <- choose (n, n + atleastdelta)
  let msgs = ("genRelSpec " ++ show n ++ " " ++ show r) : msg
  frequency
    [
      ( 1
      , do
          sup <- setSized ("sup of RelOper Nothing" : msgs) smaller genD
          dis <- someSet (suchThatErr ("dis of RelOper Nothing" : msgs) genD (`Set.notMember` sup))
          monadTyped (relOper r sup Nothing dis)
      )
    ,
      ( 2
      , do
          sup <- setSized ("sup of RelOper Just" : msgs) smaller genD
          sub <- superSetFromSetWithSize ("sub of RelOper Just" : msgs) larger genD sup
          dis <- setSized ("dis of RelOper Some" : msgs) 3 (suchThatErr msgs genD (`Set.notMember` sub))
          monadTyped (relOper r sup (Just sub) dis)
      )
    , (1, RelEqual r <$> setSized ("RelEqual" : msgs) n genD)
    , (1, pure RelAny)
    ]

-- | Generate another set which is disjoint from the input 'set'
--   Note that the empty set is always a solution.
--   These sets tend to be rather small (size <= atleastdelta)
genDisjoint :: Ord a => Set a -> Gen a -> Gen (Set a)
genDisjoint set gen = help atleastdelta Set.empty
  where
    help n answer | n < 0 = pure answer
    help n answer = do
      x <- gen
      help (n - 1) (if Set.member x set then answer else Set.insert x answer)

-- | Generate another RelSpec, guaranteed to be consistent with the input
--   Where (consistent a b) means:  (a <> b) =/= (RelNever _)
--   See the property test 'genConsistent'
genConsistentRelSpec :: [String] -> Gen dom -> RelSpec era dom -> Gen (RelSpec era dom)
genConsistentRelSpec msg g x = case x of
  RelOper r sup Nothing disj ->
    frequency
      [ (1, pure RelAny)
      ,
        ( 1
        , do
            disj2 <- genDisjoint sup g
            sup2 <- genDisjoint (disj <> disj2) g
            pure $ RelOper r sup2 Nothing disj2
        )
      ,
        ( 1
        , do
            sub2 <- (`Set.difference` disj) <$> superSetFromSet g sup
            sup2 <- subsetFromSet ((show x ++ " gen sub") : msgs) sup
            disj2 <- genDisjoint sub2 g
            pure $ RelOper r sup2 (Just sub2) disj2
        )
      ]
  RelOper r sup (Just sub) disj ->
    frequency
      [ (1, pure RelAny)
      ,
        ( 1
        , do
            disj2 <- genDisjoint sub g
            sup2 <- subsetFromSet ((show x ++ " gen sup") : msgs) sub
            pure $ RelOper r sup2 Nothing disj2
        )
      ,
        ( 1
        , do
            sub2 <- (`Set.difference` disj) <$> superSetFromSet g sup
            sup2 <- subsetFromSet ((show x ++ " gen sup") : msgs) sup
            disj2 <- genDisjoint (sub <> sub2) g
            pure $ RelOper r sup2 (Just (sub <> sub2)) disj2
        )
      ]
  RelEqual r s ->
    frequency
      [ (1, pure $ RelEqual r s)
      , (1, pure RelAny)
      , (1, RelOper r s (Just s) <$> genDisjoint s g)
      ]
  RelAny -> pure RelAny
  RelNever _ -> error "RelNever in genConsistentRelSpec"
  where
    msgs = ("genConsistentRelSpec " ++ show x) : msg

-- ==================
-- Actual property tests for Relpec

testConsistentRel :: Gen Property
testConsistentRel = do
  n <- chooseInt (3, 10)
  s1 <- genRelSpec ["from genConsistent " ++ show n] n (choose (1, 1000)) IntR
  s2 <- genConsistentRelSpec ["from genConsistent " ++ show n] (choose (1, 1000)) s1
  case s1 <> s2 of
    RelNever ms -> pure $ counterexample (unlines (["genConsistent fails", show s1, show s2] ++ ms)) False
    _ -> pure $ counterexample "" True

testSoundRelSpec :: Gen Property
testSoundRelSpec = do
  n <- chooseInt (3, 10)
  s1 <- genRelSpec ["from testSoundRelSpec " ++ show n] n (choose (1, 10000)) Word64R
  ans <- genFromRelSpec @TT ["from testSoundRelSpec " ++ show n] n (choose (1, 10000)) s1
  pure $ counterexample ("spec=" ++ show s1 ++ "\nans=" ++ show ans) (runRelSpec ans s1)

testMergeRelSpec :: Gen Property
testMergeRelSpec = do
  let msg = ["testMergeRelSpec"]
  n <- chooseInt (0, 10)
  s1 <- genRelSpec (("genRelSpec") : msg) n (choose (1, 1000)) Word64R
  s2 <- genConsistentRelSpec (("genConsistentRepSpec " ++ show s1) : msg) (choose (1, 1000)) s1
  let s3 = (s1 <> s2)
  case s3 of
    RelNever xs -> trace (unlines ("inconsistent merge" : xs)) (pure $ counterexample "" True)
    s4 -> do
      let size = sizeForRel s4
      m <- genFromSize size
      ans <- genFromRelSpec ["testMergeRelSpec " ++ show s1 ++ " " ++ show s2] m (choose (1, 1000)) s4
      pure $
        counterexample
          ( "s1="
              ++ show s1
              ++ "\ns2="
              ++ show s2
              ++ "\ns1<>s2="
              ++ show s4
              ++ "\nans="
              ++ show ans
              ++ "\nrun s1="
              ++ show (runRelSpec ans s1)
              ++ "\nrun s2="
              ++ show (runRelSpec ans s2)
              ++ "\nrun s4="
              ++ show (runRelSpec ans s4)
          )
          (runRelSpec ans s4 && runRelSpec ans s2 && runRelSpec ans s1)

consistent :: (LiftT a, Semigroup a) => a -> a -> Maybe a
consistent x y = case runTyped (liftT (x <> y)) of
  Left _ -> Nothing
  Right spec -> Just spec

tryManyMerge :: Gen (Int, Int, [String])
tryManyMerge = do
  n <- chooseInt (3, 10)
  xs <- vectorOf 60 (genRelSpec ["foo1"] n (choose (1, 100)) IntR)
  ys <- vectorOf 60 (genRelSpec ["foo2"] n (choose (1, 100)) IntR)
  let ok RelAny = False
      ok _ = True
      check (x, y, m) = do
        let size = sizeForRel m
        i <- genFromSize size
        let wordsX =
              [ "s1<>s2 Size = " ++ show (sizeForRel m)
              , "s1<>s2 = " ++ show m
              , "s2 Size = " ++ show (sizeForRel x)
              , "s2 = " ++ show x
              , "s1 Size = " ++ show (sizeForRel y)
              , "s1 = " ++ show y
              , "GenFromRelSpec " ++ show i ++ " n=" ++ show n
              ]
        z <- genFromRelSpec @TT wordsX i (choose (1, 100)) m
        pure (x, runRelSpec z x, y, runRelSpec z y, z, runRelSpec z m, m)
      showAns (s1, run1, s2, run2, v, run3, s3) =
        unlines
          [ "\ns1 = " ++ show s1
          , "s1 Size = " ++ show (sizeForRel s1)
          , "s2 = " ++ show s2
          , "s2 Size = " ++ show (sizeForRel s2)
          , "s1 <> s2 = " ++ show s3
          , "s1<>s2 Size = " ++ show (sizeForRel s3)
          , "v = genFromRelSpec (s1 <> s2) = " ++ show v
          , "runRelSpec v s1 = " ++ show run1
          , "runRelSpec v s2 = " ++ show run2
          , "runRelSpec v (s1 <> s2) = " ++ show run3
          ]
      pr x@(_, a, _, b, _, c, _) = if not (a && b && c) then Just (showAns x) else Nothing
  let trips = [(x, y, m) | x <- xs, y <- ys, ok x && ok y, Just m <- [consistent x y]]
  ts <- mapM check trips
  pure $ (n, length trips, Maybe.catMaybes (map pr ts))

reportManyMerge :: IO ()
reportManyMerge = do
  (n, passed, bad) <- generate tryManyMerge
  if null bad
    then putStrLn ("passed " ++ show passed ++ " tests. Spec size " ++ show n)
    else do mapM_ putStrLn bad; error "TestFails"

-- | Generates two random RelSpec, and then merges them. Raises an error
observeMerge :: Gen (RelSpec era Int)
observeMerge = do
  n <- chooseInt (3, 10)
  x <- genRelSpec ["observeMerge"] n (choose (1, 100)) IntR
  y <- genRelSpec ["observeMerge"] n (choose (1, 100)) IntR
  monadTyped (liftT (x <> y))

-- ==========================================================

-- | Indicates which constraints (if any) the range of a Map must adhere to
--   There are 3 cases RngSum, RngProj, and RngRel. They are all mutually inconsistent.
--   So while any Map may constrain its range, it can only choose ONE of the cases.
data RngSpec era rng where
  -- \^ The set must have Adds instance and add up to 'rng'
  RngSum ::
    Adds rng =>
    SumCond ->
    rng ->
    RngSpec era rng
  -- | The range must sum upto 'c' through the projection witnessed by the (Sums t c) class
  RngProj ::
    Sums x c =>
    SumCond ->
    Rep era c ->
    c ->
    RngSpec era x
  -- | The range has exactly these elements
  RngElem :: Eq r => Rep era r -> [r] -> RngSpec era r
  -- | The range must hold on the relation specified
  RngRel :: Ord x => RelSpec era x -> RngSpec era x
  -- | There are no constraints on the range (random generator will do)
  RngAny :: RngSpec era rng
  -- | Something was inconsistent
  RngNever :: [String] -> RngSpec era rng

instance Show (RngSpec era t) where show = showRngSpec

instance Monoid (RngSpec era rng) where mempty = RngAny

instance Semigroup (RngSpec era rng) where
  (<>) = mergeRngSpec

instance LiftT (RngSpec era a) where
  liftT (RngNever xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = RngNever s
  dropT (Typed (Right x)) = x

showRngSpec :: RngSpec era t -> String
showRngSpec (RngSum cond r) = sepsP ["RngSum", show cond, show r]
showRngSpec (RngProj cond r c) = sepsP ["RngProj", show cond, show r, show c]
showRngSpec (RngElem r cs) = sepsP ["RngElem", show r, synopsis (ListR r) cs]
showRngSpec (RngRel x) = sepsP ["RngRel", show x]
showRngSpec RngAny = "RngAny"
showRngSpec (RngNever _) = "RngNever"

mergeRngSpec :: forall r era. RngSpec era r -> RngSpec era r -> (RngSpec era r)
mergeRngSpec RngAny x = x
mergeRngSpec x RngAny = x
mergeRngSpec (RngRel RelAny) x = x
mergeRngSpec x (RngRel RelAny) = x
mergeRngSpec _ (RngNever xs) = RngNever xs
mergeRngSpec (RngNever xs) _ = RngNever xs
mergeRngSpec a@(RngElem _ xs) b@(RngElem _ ys) =
  if xs == ys
    then a
    else RngNever ["The RngSpec's are inconsistent.\n  " ++ show a ++ "\n  " ++ show b, "The elements are not the same"]
mergeRngSpec a@(RngElem _xrep xs) b@(RngSum cond tot) =
  let computed = sumAdds xs
   in if runCond cond computed tot
        then a
        else
          RngNever
            [ "The RngSpec's are inconsistent.\n  " ++ show a ++ "\n  " ++ show b
            , "The computed sum("
                ++ show computed
                ++ ") and the constrained total("
                ++ show tot
                ++ ") are not the same"
            ]
mergeRngSpec b@(RngSum _ _) a@(RngElem _ _) = mergeRngSpec a b
-- Given the right Sums instance we can probably compare RngElem and RngProj TODO

mergeRngSpec a@(RngSum c1 x) b@(RngSum c2 y) =
  if x == y
    then RngSum (c1 <> c2) x
    else RngNever ["The RngSpec's are inconsistent.\n  " ++ show a ++ "\n  " ++ show b, show x ++ " =/= " ++ show y]
mergeRngSpec a@(RngProj c1 r1 x) b@(RngProj c2 r2 y) =
  case testEql r1 r2 of
    Just Refl ->
      if x == y
        then RngProj (c1 <> c2) r1 x
        else RngNever ["The RngSpec's are inconsistent.\n  " ++ show a ++ "\n  " ++ show b, show x ++ " =/= " ++ show y]
    Nothing -> RngNever [show a, show b, "The RngSpec's are inconsistent.", show r1 ++ " =/= " ++ show r2]
mergeRngSpec a@(RngRel r1) b@(RngRel r2) =
  case r1 <> r2 of
    RelNever xs -> RngNever (["The RngSpec's are inconsistent.\n  " ++ show a ++ "\n  " ++ show b] ++ xs)
    r3 -> RngRel r3
mergeRngSpec a b = RngNever ["The RngSpec's are inconsistent.\n  " ++ show a ++ "\n  " ++ show b]

-- ===================================================================

-- | Compute the Size that is appropriate for a RngSpec
sizeForRng :: RngSpec era dom -> Size
sizeForRng (RngRel x) = sizeForRel x
sizeForRng (RngSum LTH tot) = SzRng 1 (addCount tot - 2)
sizeForRng (RngSum _ tot) = SzRng 1 (addCount tot - 1)
sizeForRng (RngProj LTH _ tot) = SzRng 1 (addCount tot - 2)
sizeForRng (RngProj _ _ tot) = SzRng 1 (addCount tot - 1)
sizeForRng (RngElem _ xs) = SzExact (length xs)
sizeForRng RngAny = SzAny
sizeForRng (RngNever _) = SzAny

-- ------------------------------------------
-- generators for test functions.

-- | Generate an arbitrary size [r] for a particular size 'n'
--   The generated list is consistent with the RngSpec given as input.
genFromRngSpec :: forall era r. [String] -> Int -> Gen r -> RngSpec era r -> Gen [r]
genFromRngSpec msgs n genr x = case x of
  (RngNever xs) -> errorMess "RngNever in genFromRngSpec" (xs ++ msgs)
  RngAny -> vectorOf n genr
  (RngSum cond tot) -> partitionBy msgs cond n tot
  (RngProj cond _ tot) -> do
    rs <- partitionBy (msg : msgs) cond n tot
    mapM (genT msgs) rs
  (RngRel relspec) -> Set.toList <$> genFromRelSpec (msg : msgs) n genr relspec
  (RngElem _ xs) -> pure xs
  where
    msg = "genFromRngSpec " ++ show n ++ " " ++ show x

-- | Generate a random RngSpec, appropriate for a given size. In order to accomodate any SumCOnd
--   (EQL, LTH, LTE, GTE, GTH) in RngSum and RngProj, we make the total a bit larger than 'n'
genRngSpec ::
  forall w c era.
  ( Adds w
  , Sums w c
  , Era era
  ) =>
  Int ->
  Gen w ->
  Rep era w ->
  Rep era c ->
  Gen (RngSpec era w)
genRngSpec 0 _ repw _ = pure $ RngRel (RelEqual repw Set.empty)
genRngSpec n g repw repc = do
  wtotal <- fromCount @w <$> choose (n + 3, 50) -- A bit larger than 'n'
  ctotal <- fromCount @c <$> choose (n + 3, 50)
  frequency
    [ (3, do (c, tot) <- genCond wtotal; pure (RngSum c tot))
    , (2, do (c, tot) <- genCond ctotal; pure (RngProj c repc tot))
    , (4, RngRel <$> genRelSpec @w ["genRngSpec "] n g repw)
    , (1, pure RngAny)
    , (2, RngElem repw <$> vectorOf n (genRep repw))
    ]

runRngSpec :: [r] -> RngSpec era r -> Bool
runRngSpec _ (RngNever _) = False
runRngSpec _ RngAny = True
runRngSpec ll (RngElem _ xs) = ll == xs
runRngSpec ll (RngSum cond tot) = runCond cond (sumAdds ll) tot
runRngSpec ll (RngProj cond _ tot) = runCond cond (projAdds ll) tot
runRngSpec ll (RngRel rspec) = runRelSpec (Set.fromList ll) rspec

-- ------------------------------------------
-- generators for RngSpec

instance Sums Word64 Coin where
  getsum n = Coin $ fromIntegral n
  genT _ (Coin n) = pure (fromIntegral n)

instance Sums Coin Word64 where
  getsum (Coin n) = fromIntegral n
  genT _ n = pure (Coin (fromIntegral n))

genCond :: Adds t => t -> Gen (SumCond, t)
genCond total = frequency (map fix [EQL, LTH, LTE, GTH, GTE])
  where
    fix LTH = (1, pure (LTH, add total (fromCount 1)))
    fix sumcond = (1, pure (sumcond, total))

genConsistentSumCond :: SumCond -> Gen SumCond
genConsistentSumCond x = case x of
  EQL -> pure EQL
  LTH -> elements [LTH]
  LTE -> elements [EQL]
  GTH -> elements [GTE]
  GTE -> elements [EQL, GTH]
  CondNever xs -> errorMess "CondNever in genConsistentSumCond" xs
  CondAny -> elements [EQL, LTE, GTH, GTE]

genConsistentRngSpec ::
  ( Adds w
  , Sums w c
  , Era era
  ) =>
  Int ->
  Gen w ->
  Rep era w ->
  Rep era c ->
  Gen (RngSpec era w, RngSpec era w)
genConsistentRngSpec n g repw repc = do
  x1 <- genRngSpec n g repw repc
  x2 <- case x1 of
    RngAny -> genRngSpec n g repw repc
    RngRel RelAny -> genRngSpec n g repw repc
    RngRel x -> RngRel <$> genConsistentRelSpec msgs g x
    RngSum c tot -> do c2 <- genConsistentSumCond c; elements [RngSum c2 tot, RngSum c tot]
    RngProj c r tot -> do c2 <- genConsistentSumCond c; elements [RngProj c2 r tot, RngProj c r tot]
    RngElem _ xs -> pure (RngSum EQL (sumAdds xs))
    RngNever xs -> errorMess "RngNever in genConsistentRngSpec" xs
  pure (x1, x2)
  where
    msgs = [seps ["genConsistentRngSpec", show repw, show repc]]

-- Tests

testConsistentRng :: Gen Property
testConsistentRng = do
  n <- chooseInt (3, 10)
  (s1, s2) <- genConsistentRngSpec @_ @_ @TT n (choose (1, 1000)) Word64R CoinR
  case s1 <> s2 of
    RngNever ms -> pure $ counterexample (unlines (["genConsistentRng fails", show s1, show s2] ++ ms)) False
    _ -> pure $ counterexample "" True

testSoundRngSpec :: Gen Property
testSoundRngSpec = do
  n <- choose (2, 8)
  spec <- genRngSpec n (choose (1, 1000)) Word64R CoinR
  list <- genFromRngSpec @TT ["testSoundRngSpec " ++ show spec] n (choose (1, 1000)) spec
  pure $
    counterexample
      ("spec=" ++ show spec ++ "\nlist=" ++ show list)
      (runRngSpec list spec)

testMergeRngSpec :: Gen Property
testMergeRngSpec = do
  (s1, s2) <- genConsistentRngSpec 3 (choose (1, 1000)) Word64R CoinR
  case s1 <> s2 of
    RngNever _ -> trace ("inconsistent RngSpec " ++ show s1 ++ " " ++ show s2) (pure (counterexample "" True))
    s3 -> do
      let size = sizeForRng s3
      n <- genFromSize size
      let wordsX =
            [ "s1=" ++ show s1
            , "s2=" ++ show s2
            , "s3=" ++ show s3
            , "size=" ++ show size
            , "n=" ++ show n
            , "testMergeRngSpec"
            ]
      list <- genFromRngSpec @TT wordsX n (choose (1, 1000)) s3
      pure $
        counterexample
          ( "s1="
              ++ show s1
              ++ "\n  s2="
              ++ show s2
              ++ "\n  s3="
              ++ show s3
              ++ "\n  size="
              ++ show size
              ++ "\n  n="
              ++ show n
              ++ "\n  list="
              ++ synopsis (ListR Word64R) list
              ++ "\n  run1="
              ++ show (runRngSpec list s1)
              ++ "\n run2="
              ++ show (runRngSpec list s2)
          )
          (runRngSpec list s1 && runRngSpec list s2)

-- =====================================================

data Unique dom rng = Unique String (Gen (Map dom rng))

instance Show (Unique dom rng) where
  show (Unique s _) = s

-- | Indicates which constraints (if any) a Map must adhere to
data MapSpec era dom rng where
  -- \^ The map may be constrained 3 ways. 1) Its size(Size) 2) its domain(RelSpec) 3) its range(RngSpec)
  MapSpec ::
    Size ->
    RelSpec era dom ->
    RngSpec era rng ->
    MapSpec era dom rng
  -- | Currently used to generate from (Component x fields) Pred. The plan
  --   is to replace it with some sort of FieldSpec soon.
  MapUnique :: Rep era (Map dom rng) -> Unique dom rng -> MapSpec era dom rng
  -- | Something is inconsistent
  MapNever :: [String] -> MapSpec era dom rng

instance Ord d => Show (MapSpec w d r) where show = showMapSpec

instance (Ord dom) => Semigroup (MapSpec era dom rng) where (<>) = mergeMapSpec

instance (Ord dom) => Monoid (MapSpec era dom rng) where mempty = MapSpec SzAny RelAny RngAny

instance LiftT (MapSpec era a b) where
  liftT (MapNever xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = MapNever s
  dropT (Typed (Right x)) = x

showMapSpec :: MapSpec era dom rng -> String
showMapSpec (MapSpec w d r) = sepsP ["MapSpec", show w, showRelSpec d, showRngSpec r]
showMapSpec (MapUnique _ (Unique nm _)) = "(MapUnique " ++ nm ++ ")"
showMapSpec (MapNever _) = "MapNever"

mergeMapSpec :: Ord dom => MapSpec era dom rng -> MapSpec era dom rng -> MapSpec era dom rng
mergeMapSpec spec1 spec2 = case (spec1, spec2) of
  (MapNever s, MapNever t) -> MapNever (s ++ t)
  (MapNever _, y) -> y
  (x, MapNever _) -> x
  (MapSpec SzAny RelAny RngAny, x) -> x
  (x, MapSpec SzAny RelAny RngAny) -> x
  (MapSpec s1 d1 r1, MapSpec s2 d2 r2) -> case mergeRngSpec r1 r2 of
    RngNever msgs -> MapNever (["The MapSpec's are inconsistent.", "  " ++ show spec1, "  " ++ show spec2] ++ msgs)
    r -> case mergeRelSpec d1 d2 of
      RelNever msgs -> MapNever (["The MapSpec's are inconsistent.", "  " ++ show spec1, "  " ++ show spec2] ++ msgs)
      d -> dropT (explain ("While merging\n   " ++ show spec1 ++ "\n   " ++ show spec2) (mapSpec (s1 <> s2) d r))
  (MapUnique _ x, y) ->
    MapNever
      [ "(Unique " ++ show x
      , "is never mergeable which another MapSpec such as"
      , showMapSpec y
      ]
  (y, MapUnique _ x) ->
    MapNever
      [ "(Unique " ++ show x
      , "is never mergeable which another MapSpec such as"
      , showMapSpec y
      ]

-- | Use 'mapSpec' instead of 'MapSpec' to check size consistency at creation time.
--   Runs in the type monad, so errors are caught and reported as Solver-time errors.
--   This should avoid many Gen-time errors, as many of those are cause by size
--   inconsistencies. We can also use this in mergeMapSpec, to catch size
--   inconsistencies there as well as (\ a b c -> dropT (mapSpec a b c)) has the same
--   type as MapSpec, but pushes the reports of inconsistencies into MapNever.
mapSpec :: Ord dom => Size -> RelSpec era dom -> RngSpec era rng -> Typed (MapSpec era dom rng)
mapSpec sz1 rel rng = case (sz1 <> sz2 <> sz3) of
  SzNever xs ->
    failT
      ( [ "Creating " ++ show (MapSpec sz1 rel rng) ++ " fails."
        , "It has size inconsistencies."
        , "  " ++ show rel ++ " has size " ++ show sz2
        , "  " ++ show rng ++ " has size " ++ show sz3
        ]
          ++ xs
      )
  size -> pure (MapSpec size rel rng)
  where
    sz2 = sizeForRel rel
    sz3 = sizeForRng rng

-- ------------------------------------------
-- MapSpec test functions

-- | test a Map against a MapSpec
runMapSpec :: Ord d => Map d r -> MapSpec era d r -> Bool
runMapSpec _ (MapNever xs) = errorMess "MapNever in runMapSpec" xs
runMapSpec _ (MapSpec SzAny RelAny RngAny) = True
runMapSpec m (MapSpec sz dom rng) =
  runSize (Map.size m) sz
    && runRelSpec (Map.keysSet m) dom
    && runRngSpec (Map.elems m) rng
runMapSpec _ (MapUnique _ _) = error "Remove MapUnique, replace with RelSpec (RelEqual)"

-- | compute a Size that bounds a MapSpec
sizeForMapSpec :: MapSpec era d r -> Size
sizeForMapSpec (MapSpec sz _ _) = sz
sizeForMapSpec (MapNever _) = SzAny
sizeForMapSpec (MapUnique _ _) = SzAny

-- ----------------------------------------
-- MapSpec generators

-- | Generate a random MapSpec
genMapSpec ::
  forall era dom w c.
  (Ord dom, Era era, Adds w, Sums w c) =>
  Gen dom ->
  Rep era dom ->
  Rep era w ->
  Rep era c ->
  Gen (MapSpec era dom w)
genMapSpec genD repd repw repc = frequency [(1, pure mempty), (6, genmapspec)]
  where
    genmapspec = do
      n <- chooseInt (1, 15)
      relspec <- genRelSpec ["genMapSpec"] n genD repd
      rngspec <- genRngSpec n (genRep @era repw) repw repc
      pure (MapSpec (SzExact n) relspec rngspec)

-- | Generate a (Map d t) from a (MapSpec era d r)
genFromMapSpec :: forall era w dom. Ord dom => (V era (Map dom w)) -> [String] -> Gen dom -> Gen w -> MapSpec era dom w -> Gen (Map dom w)
genFromMapSpec nm _ _ _ (MapNever xs) = errorMess ("genFromMapSpec " ++ (show nm) ++ " (MapNever _) fails") xs
genFromMapSpec _ _ _ _ (MapUnique _ (Unique _ gen)) = gen
genFromMapSpec nm msgs genD genR ms@(MapSpec size rel rng) = do
  n <- genFromSize size
  dom <-
    genFromRelSpec
      (("GenFromMapSpec " ++ (show nm) ++ "\n   " ++ show ms) : msgs)
      n
      genD
      rel
  rangelist <-
    genFromRngSpec
      (("genFromMapSpec " ++ (show nm) ++ "\n   " ++ show ms) : msgs)
      n
      genR
      rng
  let dsize = Set.size dom
      rsize = length rangelist
  if dsize == rsize
    then pure (Map.fromList (zip (Set.toList dom) rangelist))
    else
      trace ("Dsize=" ++ show (Set.size dom) ++ " =/=  Rsize=" ++ show (length rangelist)) $
        (pure (Map.fromList (zip (Set.toList dom) rangelist)))

genMapSpecIsSound :: Gen Property
genMapSpecIsSound = do
  spec <- genMapSpec (chooseInt (1, 1000)) IntR Word64R CoinR
  mp <- genFromMapSpec @TT (V "mapSpecIsSound" (MapR IntR Word64R) No) [] (choose (1, 10000)) (choose (1, 10000)) spec
  pure $ counterexample ("spec = " ++ show spec ++ "\nmp = " ++ show mp) (runMapSpec mp spec)

-- ===================================================================================

data SetSpec era a = (Ord a) => SetSpec Size (RelSpec era a) | SetNever [String]

instance Show (SetSpec era a) where show = showSetSpec

instance Ord a => Semigroup (SetSpec era a) where
  (<>) = mergeSetSpec

instance (Ord a) => Monoid (SetSpec era a) where
  mempty = SetSpec SzAny RelAny

instance LiftT (SetSpec era t) where
  liftT (SetNever xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = SetNever s
  dropT (Typed (Right x)) = x

showSetSpec :: SetSpec era a -> String
showSetSpec (SetSpec s r) = sepsP ["SetSpec", show s, show r]
showSetSpec (SetNever _) = "SetNever"

mergeSetSpec :: Ord a => SetSpec era a -> SetSpec era a -> SetSpec era a
mergeSetSpec s1 s2 = case (s1, s2) of
  (SetNever xs, SetNever ys) -> SetNever (xs ++ ys)
  (SetNever xs, _) -> SetNever xs
  (_, SetNever ys) -> SetNever ys
  (SetSpec SzAny RelAny, x) -> x
  (x, SetSpec SzAny RelAny) -> x
  (SetSpec s11 r1, SetSpec s22 r2) -> case r1 <> r2 of
    RelNever xs -> SetNever (["The SetSpec's are inconsistent.", "  " ++ show s1, "  " ++ show s2] ++ xs)
    r3 -> dropT (explain ("While merging\n  " ++ show s1 ++ "\n  " ++ show s2) $ setSpec (s11 <> s22) r3)

-- | Test the size consistency while building a SetSpec
setSpec :: Ord t => Size -> RelSpec era t -> Typed (SetSpec era t)
setSpec sz1 rel = case (sz1 <> sz2) of
  SzNever xs ->
    failT
      ( [ "Creating " ++ show (SetSpec sz1 rel) ++ " fails."
        , "It has size inconsistencies."
        , "  " ++ show rel ++ " has size " ++ show sz2
        , "  " ++ "the expected size is " ++ show sz1
        ]
          ++ xs
      )
  size -> pure (SetSpec size rel)
  where
    sz2 = sizeForRel rel

runSetSpec :: Set a -> SetSpec era a -> Bool
runSetSpec s (SetSpec sz rel) = runSize (Set.size s) sz && runRelSpec s rel
runSetSpec _ (SetNever msgs) = errorMess "runSetSpec applied to SetNever" msgs

sizeForSetSpec :: SetSpec era a -> Size
sizeForSetSpec (SetSpec sz _) = sz
sizeForSetSpec (SetNever _) = SzAny

genSetSpec :: Ord s => [String] -> Gen s -> Rep era s -> Gen (SetSpec era s)
genSetSpec msgs genS repS = do
  size <- chooseInt (0, 10)
  r <- genRelSpec ("from genSetSpec" : msgs) size genS repS
  pure (SetSpec (SzExact size) r)

genFromSetSpec :: forall era a. [String] -> Gen a -> SetSpec era a -> Gen (Set a)
genFromSetSpec msgs genS (SetSpec sz rp) = do
  n <- genFromSize sz
  genFromRelSpec ("genFromSetSpec" : msgs) n genS rp
genFromSetSpec _ _ (SetNever msgs) = errorMess "genFromSetSpec applied to SetNever" msgs

genSetSpecIsSound :: Gen Property
genSetSpecIsSound = do
  spec <- genSetSpec msgs (chooseInt (1, 1000)) IntR
  mp <- genFromSetSpec @TT msgs (choose (1, 10000)) spec
  pure $ counterexample ("spec = " ++ show spec ++ "\nmp = " ++ show mp) (runSetSpec mp spec)
  where
    msgs = ["genSetSpecIsSound"]

-- =============================================================

data SumSpec t where
  SumSpec :: Adds t => SumCond -> Maybe t -> Maybe t -> SumSpec t
  SumNever :: [String] -> SumSpec t

instance Show t => Show (SumSpec t) where show = showSumSpec

instance (Adds t) => Semigroup (SumSpec t) where (<>) = mergeSumSpec
instance (Adds t) => Monoid (SumSpec t) where mempty = SumSpec CondAny Nothing Nothing

instance LiftT (SumSpec t) where
  liftT (SumNever xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = SumNever s
  dropT (Typed (Right x)) = x

sizeForSumSpec :: SumSpec t -> Size
sizeForSumSpec _ = SzAny

showM :: Show a => Maybe a -> String
showM Nothing = "Nothing"
showM (Just x) = "(Just " ++ show x ++ ")"

showSumSpec :: SumSpec a -> String
showSumSpec (SumSpec scond x r) = "(SumSpec " ++ showM x ++ show scond ++ showM r ++ ")"
showSumSpec (SumNever _) = "SumNever"

sameMaybe :: (Show x, Eq x) => Maybe x -> Maybe x -> Either [String] (Maybe x)
sameMaybe Nothing Nothing = Right Nothing
sameMaybe (Just x) Nothing = Right (Just x)
sameMaybe Nothing (Just x) = Right (Just x)
sameMaybe (Just x) (Just y) =
  if x == y
    then Right (Just x)
    else Left ["Not the same in sameMaybe: " ++ show (x, y)]

mergeSumSpec :: (Adds t) => SumSpec t -> SumSpec t -> SumSpec t
mergeSumSpec (SumNever xs) (SumNever ys) = SumNever (xs ++ ys)
mergeSumSpec (SumNever xs) _ = SumNever xs
mergeSumSpec _ (SumNever xs) = SumNever xs
mergeSumSpec (SumSpec sc1 x a) (SumSpec sc2 y b) = case (sameMaybe x y, sameMaybe a b, sc1 <> sc2) of
  (Right _, Right _, CondNever xs) -> SumNever xs
  (Right z, Right c, sc3) -> SumSpec sc3 z c
  (Left z, Right _, CondNever w) -> SumNever (w ++ z)
  (Left z, Right _, _) -> SumNever z
  (Right _, Left c, CondNever w) -> SumNever (w ++ c)
  (Right _, Left c, _) -> SumNever c
  (Left z, Left c, CondNever w) -> SumNever (w ++ z ++ c)
  (Left z, Left c, _) -> SumNever (z ++ c)

-- | Interpretation of SumSpec
genFromSumSpec :: (Adds t, Era era) => [String] -> Rep era t -> SumSpec t -> Typed (Gen t)
genFromSumSpec ms rep spec =
  let msg = seps ["From genFromSum", show rep, show spec]
      msgs = msg : ms
   in explain msg $ case spec of
        (SumNever zs) -> failT (msg : zs)
        (SumSpec _ Nothing Nothing) -> pure $ genRep rep
        (SumSpec _ (Just t) Nothing) -> pure $ pure t -- Recall this comes from (x :=: y) so no SumCond is involved
        (SumSpec cond Nothing (Just tot)) -> pure $ adjust msgs cond 1 tot
        (SumSpec cond (Just x) (Just tot)) ->
          if runCond cond x tot
            then pure $ pure x
            else failT (["Sum condition not met: " ++ show x ++ show cond ++ show tot] ++ ms)

-- =======================================================
-- Specifications for Lists

data ElemSpec era t where
  -- \^ The set must have Adds instance and add up to 'tot'
  ElemSum ::
    Adds t =>
    SumCond ->
    t ->
    ElemSpec era t
  -- | The range must sum upto 'c' through the projection witnessed by the (Sums t c) class
  ElemProj ::
    Sums x c =>
    SumCond ->
    Rep era c ->
    c ->
    ElemSpec era x
  -- | The range has exactly these elements
  ElemEqual :: Eq t => Rep era t -> [t] -> ElemSpec era t
  ElemNever :: [String] -> ElemSpec era t
  ElemAny :: ElemSpec era t

instance Show (ElemSpec era a) where show = showElemSpec

instance Semigroup (ElemSpec era a) where
  (<>) = mergeElemSpec

instance Monoid (ElemSpec era a) where
  mempty = ElemAny

instance LiftT (ElemSpec era t) where
  liftT (ElemNever xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = ElemNever s
  dropT (Typed (Right x)) = x

showElemSpec :: ElemSpec era a -> String
showElemSpec (ElemSum c tot) = sepsP ["ElemSum", show c, show tot]
showElemSpec (ElemProj c r tot) = sepsP ["ElemProj", show c, show r, show tot]
showElemSpec (ElemEqual r xs) = sepsP ["ElemEqual", show r, synopsis (ListR r) xs]
showElemSpec (ElemNever _) = "ElemNever"
showElemSpec ElemAny = "ElemAny"

mergeElemSpec :: ElemSpec era a -> ElemSpec era a -> ElemSpec era a
mergeElemSpec (ElemNever xs) (ElemNever ys) = ElemNever (xs ++ ys)
mergeElemSpec (ElemNever xs) _ = ElemNever xs
mergeElemSpec _ (ElemNever ys) = ElemNever ys
mergeElemSpec ElemAny x = x
mergeElemSpec x ElemAny = x
mergeElemSpec a@(ElemEqual r xs) b@(ElemEqual _ ys) =
  if xs == ys
    then ElemEqual r xs
    else
      ElemNever
        [ "The ElemSpec's are inconsistent."
        , "  " ++ show a
        , "  " ++ show b
        , synopsis (ListR r) xs ++ " =/= " ++ synopsis (ListR r) ys
        ]
mergeElemSpec a@(ElemEqual _ xs) b@(ElemSum cond tot) =
  let computed = sumAdds xs
   in if runCond cond computed tot
        then a
        else
          ElemNever
            [ "The ElemSpec's are inconsistent."
            , "  " ++ show a
            , "  " ++ show b
            , "The computed sum("
                ++ show computed
                ++ ") and the constrained total("
                ++ show tot
                ++ ") are not the same"
            ]
mergeElemSpec b@(ElemSum _ _) a@(ElemEqual _ _) = mergeElemSpec a b
-- Given the right Sums instance we can probably compare ElemEqual and ElemProj TODO

mergeElemSpec a@(ElemSum c1 x) b@(ElemSum c2 y) =
  if x == y
    then ElemSum (c1 <> c2) x
    else
      ElemNever
        [ "The ElemSpec's are inconsistent."
        , "  " ++ show a
        , "  " ++ show b
        , show x ++ " =/= " ++ show y
        ]
mergeElemSpec a@(ElemProj c1 r1 x) b@(ElemProj c2 r2 y) =
  case testEql r1 r2 of
    Just Refl ->
      if x == y
        then ElemProj (c1 <> c2) r1 x
        else ElemNever ["The ElemSpec's are inconsistent.", "  " ++ show a, "  " ++ show b, show x ++ " =/= " ++ show y]
    Nothing -> ElemNever ["The ElemSpec's are inconsistent.", "  " ++ show a, "  " ++ show b]
mergeElemSpec a b = ElemNever ["The ElemSpec's are inconsistent.", "  " ++ show a, "  " ++ show b]

sizeForElemSpec :: ElemSpec era a -> Size
sizeForElemSpec (ElemNever _) = SzAny
sizeForElemSpec ElemAny = SzAny
sizeForElemSpec (ElemEqual _ x) = SzExact (length x)
sizeForElemSpec (ElemSum LTH tot) = SzLeast (addCount tot - 2)
sizeForElemSpec (ElemSum _ tot) = SzLeast (addCount tot - 1)
sizeForElemSpec (ElemProj LTH _ tot) = SzLeast (addCount tot - 2)
sizeForElemSpec (ElemProj _ _ tot) = SzLeast (addCount tot - 1)

runElemSpec :: [a] -> ElemSpec era a -> Bool
runElemSpec xs spec = case spec of
  ElemNever _ -> False -- ErrorMess "ElemNever in runElemSpec" []
  ElemAny -> True
  ElemEqual _ ys -> xs == ys
  ElemSum cond tot -> runCond cond (sumAdds xs) tot
  ElemProj cond _ tot -> runCond cond (projAdds xs) tot

genElemSpec ::
  forall w c era.
  (Adds w, Sums w c, Era era) =>
  Size ->
  Rep era w ->
  Rep era c ->
  Gen (ElemSpec era w)
genElemSpec (SzRng count j) repw repc
  | count == j && count >= 1 =
      frequency
        [
          ( 2
          , do
              total <- greater count -- Total must be bigger than count
              (c, tot) <- genCond total
              pure (ElemSum c tot)
          )
        ,
          ( 2
          , do
              total <- greater count
              (c, tot) <- genCond total
              pure (ElemProj c repc tot)
          )
        , (2, ElemEqual repw <$> vectorOf count (genRep repw))
        , (1, pure ElemAny)
        ]
genElemSpec size repw _ =
  frequency
    [ (3, ElemEqual repw <$> (do count <- genFromSize size; vectorOf count (genRep repw)))
    , (1, pure ElemAny)
    ]

genFromElemSpec ::
  forall era r.
  [String] ->
  Gen r ->
  Int ->
  ElemSpec era r ->
  Gen [r]
genFromElemSpec msgs genr n x = case x of
  (ElemNever xs) -> errorMess "RngNever in genFromElemSpec" xs
  ElemAny -> vectorOf n genr
  (ElemEqual _ xs) -> pure xs
  (ElemSum cond tot) -> partitionBy msgs cond n tot
  (ElemProj cond _ tot) -> do
    rs <- partitionBy (msg : msgs) cond n tot
    mapM (genT msgs) rs
  where
    msg = "genFromElemSpec " ++ show n ++ " " ++ show x

-- ========================================

-- | Specs for lists have two parts, the Size, and the elements
data ListSpec era t where
  ListSpec :: Size -> ElemSpec era t -> ListSpec era t
  ListNever :: [String] -> ListSpec era t

instance Show (ListSpec era a) where show = showListSpec

instance Semigroup (ListSpec era a) where
  (<>) = mergeListSpec

instance Monoid (ListSpec era a) where
  mempty = ListSpec SzAny ElemAny

instance LiftT (ListSpec era t) where
  liftT (ListNever xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = ListNever s
  dropT (Typed (Right x)) = x

showListSpec :: ListSpec era a -> String
showListSpec (ListSpec s xs) = sepsP ["ListSpec", show s, show xs]
showListSpec (ListNever _) = "ListNever"

mergeListSpec :: ListSpec era a -> ListSpec era a -> ListSpec era a
mergeListSpec (ListNever xs) (ListNever ys) = ListNever (xs ++ ys)
mergeListSpec (ListNever xs) (ListSpec _ _) = ListNever xs
mergeListSpec (ListSpec _ _) (ListNever xs) = ListNever xs
mergeListSpec a@(ListSpec s1 e1) b@(ListSpec s2 e2) =
  case e1 <> e2 of
    ElemNever xs ->
      ListNever (["The ListSpec's are inconsistent.", "  " ++ show a, "  " ++ show b] ++ xs)
    e3 -> dropT (explain ("While merging\n  " ++ show a ++ "\n  " ++ show b) $ listSpec (s1 <> s2) e3)

-- | Test the size consistency while building a ListSpec
listSpec :: Size -> ElemSpec era t -> Typed (ListSpec era t)
listSpec sz1 el = case (sz1 <> sz2) of
  SzNever xs ->
    failT
      ( [ "Creating " ++ show (ListSpec sz1 el) ++ " fails."
        , "It has size inconsistencies."
        , "  " ++ show el ++ " has size " ++ show sz2
        , "  " ++ "the expected size is " ++ show sz1
        ]
          ++ xs
      )
  size -> pure (ListSpec size el)
  where
    sz2 = sizeForElemSpec el

sizeForListSpec :: ListSpec era t -> Size
sizeForListSpec (ListSpec sz _) = sz
sizeForListSpec (ListNever _) = SzAny

runListSpec :: [a] -> ListSpec era a -> Bool
runListSpec xs spec = case spec of
  ListNever _ -> False
  ListSpec sx es -> runSize (length xs) sx && runElemSpec xs es

genListSpec ::
  forall w c era.
  (Adds w, Sums w c, Era era) =>
  Rep era w ->
  Rep era c ->
  Gen (ListSpec era w)
genListSpec repw repc = do
  size <- genSize
  e <- genElemSpec size repw repc
  pure (ListSpec size e)

genFromListSpec ::
  forall era r.
  [String] ->
  Gen r ->
  ListSpec era r ->
  Gen [r]
genFromListSpec _ _ (ListNever xs) = errorMess "ListNever in genFromListSpec" xs
genFromListSpec msgs genr (ListSpec size e) = do
  n <- genFromSize size
  genFromElemSpec ("genFromListSpec" : msgs) genr n e

-- List and Elem tests

testSoundElemSpec :: Gen Property
testSoundElemSpec = do
  size <- genSize
  spec <- genElemSpec size Word64R CoinR
  n <- genFromSize size
  list <- genFromElemSpec @TT ["testSoundElemSpec"] (choose (1, 1000)) n spec
  pure $
    counterexample
      ("size=" ++ show size ++ "\nspec=" ++ show spec ++ "\nlist=" ++ synopsis (ListR Word64R) list)
      (runElemSpec list spec)

testSoundListSpec :: Gen Property
testSoundListSpec = do
  spec <- genListSpec Word64R CoinR
  list <- genFromListSpec @TT ["testSoundListSpec"] (choose (1, 1000)) spec
  pure $
    counterexample
      ("spec=" ++ show spec ++ "\nlist=" ++ synopsis (ListR Word64R) list)
      (runListSpec list spec)

-- ================================================
-- Synthetic classes used to control the size of
-- things in the tests.

class (Arbitrary t, Adds t) => TestAdd t where
  anyAdds :: Gen t
  pos :: Gen t

instance TestAdd Word64 where
  anyAdds = choose (0, 12)
  pos = choose (1, 12)

instance TestAdd Coin where
  anyAdds = Coin <$> choose (0, 8)
  pos = Coin <$> choose (1, 8)

instance TestAdd Int where
  anyAdds = chooseInt (0, atmostany)
  pos = chooseInt (1, atmostany)

-- =============================================
-- Some simple generators tied to TestAdd class

-- | Only the size of the set uses TestAdd
genSet :: Ord t => Int -> Gen t -> Gen (Set t)
genSet n gen = do
  xs <- vectorOf 20 gen
  pure (Set.fromList (take n (List.nub xs)))

testSet :: TestAdd t => Gen (Set t)
testSet = do
  n <- pos @Int
  Set.fromList <$> vectorOf n anyAdds

someSet :: Ord t => Gen t -> Gen (Set t)
someSet g = do
  n <- pos @Int
  Set.fromList <$> vectorOf n g

someMap :: forall era t d. (Ord d, TestAdd t, Era era) => Rep era d -> Gen (Map d t)
someMap r = do
  n <- pos @Int
  rs <- vectorOf n anyAdds
  ds <- vectorOf n (genRep r)
  pure $ Map.fromList (zip ds rs)

-- ===================================
-- Some proto-tests, to be fixed soon

aMap :: Era era => Gen (MapSpec era Int Word64)
aMap = genMapSpec (chooseInt (1, 1000)) IntR Word64R CoinR

testm :: Gen (MapSpec TT Int Word64)
testm = do
  a <- aMap @TT
  b <- aMap
  monadTyped (liftT (a <> b))

aList :: Era era => Gen (ListSpec era Word64)
aList = genListSpec Word64R CoinR

testl :: Gen (ListSpec TT Word64)
testl = do
  a <- aList @TT
  b <- aList
  monadTyped (liftT (a <> b))

-- =======================================================================

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
--      vLeft  :: String -> SumCond -> Integer -> SumV
--                SumsTo x <= 4 + 6 + 9 ===> (vLeft x LTE 19) == (SumVSize x (AtMost 19))
--      vRight :: Integer -> SumCond -> Integer -> String -> SumV
--                SumsTo 8 < 2 + x + 3 ===> (vRight 8 LTH 5 x) == (SumVSize x (AtLeast 4))
--   But internally we store the information as a String and a Size (I.e. a range of Int)
data SumV where
  SumVSize :: String -> Size -> SumV
  SumVAny :: SumV
  SumVNever :: [String] -> SumV

vLeft :: String -> SumCond -> Int -> SumV
vLeft s cond n = uncurry SumVSize (tripToSize (s, cond, n))

vRight :: Int -> SumCond -> Int -> String -> SumV
vRight n cond m s = uncurry SumVSize (tripToSize (s, switch cond, n - m))
  where
    switch :: SumCond -> SumCond
    switch EQL = EQL
    switch LTH = GTH
    switch LTE = GTH
    switch GTH = LTH
    switch GTE = LTH
    switch x = x

instance Show SumV where show = showSumV

instance Semigroup SumV where (<>) = mergeSumV
instance Monoid SumV where mempty = SumVAny

instance LiftT SumV where
  liftT (SumVNever xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = SumVNever s
  dropT (Typed (Right x)) = x

showSumV :: SumV -> String
showSumV SumVAny = "SumVAny"
showSumV (SumVSize s size) = sepsP ["SumVSize", s, show size]
showSumV (SumVNever _) = "SumVNever"

mergeSumV :: SumV -> SumV -> SumV
mergeSumV (SumVNever xs) (SumVNever ys) = SumVNever (xs ++ ys)
mergeSumV x@(SumVNever _) _ = x
mergeSumV _ x@(SumVNever _) = x
mergeSumV SumVAny x = x
mergeSumV x SumVAny = x
mergeSumV a@(SumVSize s1 size1) b@(SumVSize s2 size2) =
  if s1 == s2
    then case size1 <> size2 of
      (SzNever xs) -> SumVNever (xs ++ [show a ++ " " ++ show a ++ " are inconsistent."])
      size3 -> SumVSize s1 size3
    else
      SumVNever
        [ "vars " ++ s1 ++ " and " ++ s2 ++ " are not the same."
        , show a ++ " " ++ show b ++ " are inconsistent."
        ]

genSumV :: Gen SumV
genSumV =
  frequency
    [ (1, vLeft <$> elements ["x", "y"] <*> genSumCond <*> choose (-5, 25))
    , (2, vRight <$> choose (-5, 25) <*> genSumCond <*> choose (-5, 25) <*> elements ["x", "y"])
    ]

genNonNegSumV :: Gen SumV
genNonNegSumV =
  frequency
    [ (1, vLeft <$> elements ["x", "y"] <*> genSumCond <*> choose (0, 30))
    ,
      ( 2
      , do
          n <- choose (1, 30)
          cond <- genSumCond
          m <- choose (0, n - 1)
          v <- elements ["x", "y"]
          pure $ vRight n cond m v
      )
    ]

genFromSumV :: forall c. Adds c => [String] -> SumV -> Gen c
genFromSumV _ SumVAny = pure zero
genFromSumV msgs s@(SumVSize _ size) = fromI (("genFromSumV " ++ show s) : msgs) <$> genFromIntRange size
genFromSumV msgs (SumVNever _) = errorMess ("genFromSumV applied to SumVNever") msgs

genFromNonNegSumV :: forall c. Adds c => [String] -> SumV -> Gen c
genFromNonNegSumV _ SumVAny = pure zero
genFromNonNegSumV msgs s@(SumVSize _ size) = fromI (("genFromSumV " ++ show s) : msgs) <$> genFromSize size
genFromNonNegSumV msgs (SumVNever _) = errorMess ("genFromSumV applied to SumVNever") msgs

tripToSize :: (String, SumCond, Int) -> (String, Size)
tripToSize (s, EQL, n) = (s, SzExact n)
tripToSize (s, LTH, n) = (s, SzMost (n - 1))
tripToSize (s, LTE, n) = (s, SzMost n)
tripToSize (s, GTH, n) = (s, SzLeast (n + 1))
tripToSize (s, GTE, n) = (s, SzLeast n)
tripToSize (s, CondAny, _) = (s, SzAny)
tripToSize (s, CondNever xs, _) = (s, SzNever xs)

genSumCond :: Gen SumCond
genSumCond = elements [EQL, LTH, LTE, GTH, GTE, CondAny]

runSumV :: forall c. Adds c => c -> SumV -> Bool
runSumV c (SumVSize _ size) = runSize (toI c) size
runSumV _ SumVAny = True
runSumV _ (SumVNever _) = False

tryManySumV :: Gen SumV -> ([String] -> SumV -> Gen Int) -> Gen (Int, [String])
tryManySumV genSum genFromSum = do
  xs <- vectorOf 25 genSum
  ys <- vectorOf 25 genSum
  let check (x, y, m) = do
        z <- genFromSum ["test tryManySumV"] m
        pure (x, runSumV z x, y, runSumV z y, z, runSumV z m, m)
      showAns :: (SumV, Bool, SumV, Bool, Int, Bool, SumV) -> String
      showAns (s1, run1, s2, run2, v, run3, s3) =
        unlines
          [ "s1 = " ++ show s1
          , "s2 = " ++ show s2
          , "s1 <> s2 = " ++ show s3
          , "v = genFromRelSpec (s1 <> s2) = " ++ show v
          , "runSumV s1 v = " ++ show run1
          , "runSumV s2 v = " ++ show run2
          , "runSumV (s1 <> s2) v = " ++ show run3
          ]
      pr x@(_, a, _, b, _, c, _) = if not (a && b && c) then Just (showAns x) else Nothing
  let trips = [(x, y, m) | x <- xs, y <- ys, Just m <- [consistent x y]]
  ts <- mapM check trips
  pure $ (length trips, Maybe.catMaybes (map pr ts))

reportManySumV :: IO ()
reportManySumV = do
  (passed, bad) <- generate (tryManySumV genSumV genFromSumV)
  if null bad
    then putStrLn ("passed " ++ show passed ++ " tests.")
    else do mapM_ putStrLn bad; error "TestFails"

reportManyNonNegSumV :: IO ()
reportManyNonNegSumV = do
  (passed, bad) <- generate (tryManySumV genNonNegSumV genFromNonNegSumV)
  if null bad
    then putStrLn ("passed " ++ show passed ++ " tests.")
    else do mapM_ putStrLn bad; error "TestFails"

-- ========================================================

main :: IO ()
main =
  defaultMain $
    testGroup
      "Spec tests"
      [ testProperty "test Size generators" testSoundSize
      , testProperty "test merging Size" testMergeSize
      , testProperty "we generate consistent RelSpecs" testConsistentRel
      , testProperty "test RelSpec generators" testSoundRelSpec
      , testProperty "test mergeRelSpec" testMergeRelSpec
      , testProperty "test Consistent RngSpec generators" testConsistentRng
      , testProperty "test RngSpec generators" testSoundRngSpec
      , testProperty "test mergeRngSpec" testMergeRngSpec
      , testProperty "test More Sound Merge Rng" reportManyMerge
      , testProperty "test MapSpec generators" genMapSpecIsSound
      , testProperty "test SetSpec generators" genSetSpecIsSound
      , testProperty "test ElemSpec generators" testSoundElemSpec
      , testProperty "test ListSpec generators" testSoundListSpec
      , testProperty "test Sound MergeSumV" reportManySumV
      , testProperty "test Sound non-negative MergeSumV" reportManyNonNegSumV
      ]

-- :main --quickcheck-replay=740521
