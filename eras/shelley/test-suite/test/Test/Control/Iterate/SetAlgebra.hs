{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- This option is on for only one reason, It allows us to make a list of
-- functions, written as (\ w x y z -> Property) where each function uses
-- a different set of the arguments. By typeing the list as
-- [Int -> Key -> Map Key Int -> Set key -> Property], I can specify
-- the type of each of the parameters to the anonymous functions, without
-- repeating them for each of the over 100 items in the list.

module Test.Control.Iterate.SetAlgebra where

import Control.Iterate.BaseTypes (List (..), Sett (..), fromPairs)
import Control.Iterate.BiMap (BiMap (..))
import Control.Iterate.Collect
import Control.Iterate.Exp (Exp (..), Query (..), domElem, lift, rngSnd)
import Control.Iterate.SetAlgebra
  ( compute,
    domEq,
    fifo,
    intersectDomP,
    intersectDomPLeft,
    lifo,
    runBool,
    runSet,
    sameDomain,
    (⨝),
  )
import Control.SetAlgebra
import Data.Char (ord)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- =========================================================
-- Some examples of Exp and tests
-- let's build a few things to test with

-- ==============================================================================================
-- Consider one of transactions that needs to compute the following.
-- ((dom stkcreds) ◁ delegs) ▷ (dom stpools)
-- We could express this in datalog as follows
-- ans(x,y) <- skcreds(x,z) and delegs(x,y) and stpools(y,w)
-- Or if collections: stkcreds, delegs, and stpools were lists of pairs as a comprehension
-- [ (x,y) | (x,z) <- skcreds, (x',y) <- delegs, x==x', (y',w) <- stpools, y=y' ]
-- This runs in time and space proportional to: size(dom skcreds) + size(dom delegs) + size(dom stpools) (perhaps even worse)
-- Or if  stkcreds, delegs, and stpools were Data.Map, we could use the Collection monad.
-- Even better, this will run in time and space proportional to: size((dom skcreds) ∩ (dom delegs))
-- See the example with timing above.

foo skcreds delegs stpools = materialize MapR $
  do
    (x, _z, y) <- skcreds ⨝ delegs
    y `element` stpools
    one (x, y)

-- Even better,  stkcreds, delegs, and stpools can be any binary type construtors in the Iter class.

foo ::
  (Iter s, Iter d, Iter p, Ord a, Ord b1) =>
  s a b2 ->
  d a b1 ->
  p b1 b3 ->
  Map a b1
example :: Exp (Map Int Char)
example = ((dom stkcred) ◁ deleg) ▷ (dom stpool)

stkcred :: Map Int [Char]
deleg :: Map Int Char
stpool :: Map Char Int
stkcred = Map.fromList [(5, "a"), (6, "q"), (12, "r")]

deleg = Map.fromList [(n, chars !! n) | n <- [1 .. 10]]

stpool = Map.fromList [('A', 99), ('C', 12), ('F', 42), ('R', 33), ('Z', 99)]

--  ((txins txb ⋪ utxo) ∪ txouts txb)
test33 :: () -> Exp (Map Int Char)
test33 () = ((Set.fromList [4, 7, 9] ⋪ m12) ∪ m22)

-- =============== Build a few maps ===================

chars :: String
chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdeghijklmnopqrstuvwxyz0123456789"

nchars :: Int
nchars = length chars

m0 :: Map.Map Int Char
m0 = Map.fromList [(1, 'a'), (2, 'z'), (4, 'g')]

m12 :: Map.Map Int Char
m12 = Map.fromList [(n, chars !! n) | n <- [0 .. length chars - 1]]

m22 :: Map.Map Int Char
m22 = Map.fromList [(57 + n, chars !! n) | n <- [0 .. length chars - 1]]

mN :: Int -> Int -> Map.Map Int Char
mN start size = Map.fromList [(n, chars !! (n `mod` nchars)) | n <- [start .. start + size]]

-- | Some really big Maps, with very small overlap.
m5, m6 :: Map.Map Int Char
m5 = mN 1 10000000
m6 = mN 9999995 10000000

b0 :: Bimap Int Char
b0 = biMapFromList (\l _r -> l) [(1, 'a'), (2, 'z'), (4, 'g')]

-- ============ Some small Maps to And, Or, Diff, Guard, Project with =========

l1, l2 :: [(Int, String)]
l1 = [(1, "a"), (4, "d"), (5, "e"), (10, "j"), (12, "l"), (21, "v"), (26, "z")]
l2 = [(3, "c"), (4, "d"), (5, "e"), (6, "f"), (10, "j"), (11, "k"), (21, "v")]

l3 :: [(Int, Int)]
l3 = [(4, 12), (9, 13), (12, 44), (55, 22)]

evens :: Sett Int ()
evens = fromList SetR (\l _r -> l) [(n, ()) | n <- [2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26]]

l4 :: [(Int, String)]
l4 = [(1, "m"), (2, "a"), (5, "z"), (6, "b"), (7, "r"), (12, "w"), (34, "v"), (50, "q"), (51, "l"), (105, "Z")]

l5 :: [(String, Int)]
l5 = [("a", 101), ("b", 102), ("c", 103), ("f", 104), ("m", 105), ("q", 107), ("s", 106), ("w", 108), ("y", 109), ("zz", 110)]

-- =================== Some sample (Exp t) =============================

ex1 :: Exp Bool
ex1 = 5 ∈ (dom m12)

ex2 :: Exp Bool
ex2 = 70 ∈ (dom m12)

ex3 :: Exp (Map Int Char)
ex3 = m0 ∪ (singleton 3 'b')

ex4, ex5, ex6 :: Exp (Map Int Char)
ex4 = (setSingleton 2) ⋪ m0
ex5 = dom (singleton 2 'z') ⋪ m0
ex6 = rng (singleton 'z' 2) ⋪ m0

ex7 :: Exp Bool
ex7 = 70 ∉ (dom m12)

z1 :: Map Int String
z1 = Map.fromList [(3, "c"), (4, "d"), (5, "e"), (6, "f"), (10, "j"), (11, "k"), (21, "v")]

z2 :: Set.Set Int
z2 = Set.fromList [4, 6, 11, 13, 2]

z3 :: Map Int String
z3 = Map.fromList [(9, "3"), (10, "j"), (30, "a")]

z4 :: Map Int String
z4 = Map.fromList [(3, "c"), (5, "e"), (10, "j"), (21, "v"), (9, "3"), (30, "a")]

ex8 :: Set.Set Int
ex8 = (eval (z2 ➖ dom z1))

-- ===================== test that compute works ======================

-- Test that computing  x::(Exp t) computes to the given object with type t.

evalTest :: (Show t, Eq t) => String -> Exp t -> t -> TestTree
evalTest nm expr ans = testCase name (assertEqual name (compute expr) ans)
  where
    name = (show expr ++ " where Map? = " ++ nm)

-- Test that (eval x) and runSet(x) get the same answers

eval_compile :: (Show (f k v), Ord k, Eq (f k v)) => Exp (f k v) -> TestTree
eval_compile expr = testCase name (assertEqual name (compute expr) (runSet expr))
  where
    name = ("compute and runSet of " ++ show expr ++ " are the same")

eval_tests :: TestTree
eval_tests =
  testGroup
    "eval tests"
    [ evalTest "m12" (5 ∈ (dom m12)) True,
      evalTest "m12" (70 ∈ (dom m12)) False,
      evalTest "m0" (m0 ∪ (singleton 3 'b')) (Map.fromList [(1, 'a'), (2, 'z'), (3, 'b'), (4, 'g')]),
      evalTest "m0" ((setSingleton 2) ⋪ m0) (Map.fromList [(1, 'a'), (4, 'g')]),
      evalTest "m0" (dom (singleton 2 'z') ⋪ m0) (Map.fromList [(1, 'a'), (4, 'g')]),
      evalTest "m0" (rng (singleton 'z' 2) ⋪ m0) (Map.fromList [(1, 'a'), (4, 'g')]),
      evalTest "m0" ((Map.fromList [(1, 'a'), (2, 'n'), (3, 'r')]) ∪ (singleton 2 'b')) (Map.fromList [(1 :: Int, 'a'), (2, 'n'), (3, 'r')]),
      evalTest "m0" ([(1, 'a'), (3, 'r')] ∪ (singleton 3 'b')) (UnSafeList [(1 :: Int, 'a'), (3, 'r')]),
      evalTest "m0" (70 ∉ (dom m12)) True,
      evalTest "((dom stkcred) ◁ deleg) ▷ (dom stpool)" (((dom stkcred) ◁ deleg) ▷ (dom stpool)) (Map.fromList [(5, 'F')]),
      evalTest "Range exclude 1" (l4 ⋫ Set.empty) (UnSafeList l4),
      evalTest "Range exclude 2" (l4 ⋫ Fail) (UnSafeList l4),
      evalTest
        "Range exclude 3"
        (l4 ⋫ (Set.fromList ["m", "Z"]))
        (UnSafeList [(2, "a"), (5, "z"), (6, "b"), (7, "r"), (12, "w"), (34, "v"), (50, "q"), (51, "l")]),
      evalTest "DomExclude Union" ((z2 ⋪ z1) ∪ z3) z4,
      evalTest "Set difference" (z2 ➖ dom z1) (Sett (Set.fromList [2 :: Int, 13])),
      eval_compile (((dom stkcred) ◁ deleg) ▷ (dom stpool)),
      eval_compile (l4 ⋫ (Set.fromList ["m", "Z"])),
      eval_compile (m0 ∪ (singleton 3 'b')),
      eval_compile ((setSingleton 2) ⋪ m0)
    ]

-- =============== test of KeysEqual and its variants =====================

tree1, tree2, tree3 :: Map Int Int
tree1 = Map.fromList [(i, i :: Int) | i <- [1 .. 20]]
tree2 = Map.fromList [(i, i :: Int) | i <- (reverse [2 .. 20]) ++ [1]]
tree3 = Map.fromList [(i, i :: Int) | i <- [1 .. 19]]

set1 :: Set.Set Int
set1 = Set.fromList [1 .. 20]

keysEqTests :: TestTree
keysEqTests =
  testGroup
    "keysEqual tests"
    ( zipWith
        tst
        [(1 :: Int) ..]
        [ (keysEqual tree1 tree2, True),
          (keysEqual tree2 tree1, True),
          (keysEqual tree1 tree3, False),
          (sameDomain tree1 tree2, True),
          (sameDomain tree2 tree1, True),
          (sameDomain tree1 tree3, False),
          (eval (tree1 ≍ tree2), True),
          (eval (tree1 ≍ tree3), False),
          (eval (tree1 ≍ set1), True),
          (eval (tree3 ≍ set1), False)
        ]
    )
  where
    tst n (x, y) = testCase ("keysEqual " ++ show n) (assertEqual ("keysEqual " ++ show n) y x)

-- ========================== test that various Compound iterators work ================

testcase :: (Eq k, Eq v, Show k, Show v, Iter f) => String -> f k v -> [(k, v)] -> TestTree
testcase nm col ans = testCase nm (assertEqual nm ans (runCollect (fifo col) [] (:)))

fromListD :: (Ord k, Iter f) => BaseRep f k v -> [(k, v)] -> Query k v
fromListD rep xs = BaseD rep (fromList rep (\l _r -> l) xs)

-- Tests where we vary how we represent l1 and l2 (the f in (Iter f) )
-- and see that we always get the same answer no matter how we store the data of l1 and l2

testAnd1, testAnd2, testOr, testDiff1, testDiff2 :: Iter g => String -> BaseRep g Int String -> TestTree
testAnd1 nm rep =
  testcase
    nm
    (AndD (fromListD rep l1) (fromListD rep l2))
    [(4, ("d", "d")), (5, ("e", "e")), (10, ("j", "j")), (21, ("v", "v"))]
testAnd2 nm rep =
  testCase
    nm
    ( assertEqual
        nm
        (runCollect (lifo (AndD (fromListD rep l1) (fromListD rep l2))) [] (:))
        (reverse [(4, ("d", "d")), (5, ("e", "e")), (10, ("j", "j")), (21, ("v", "v"))])
    )
testOr nm rep =
  testcase
    nm
    (OrD (fromListD rep l1) (fromListD rep l2) (lift (\x y -> x ++ "-" ++ y)))
    [(1, "a"), (3, "c"), (4, "d-d"), (5, "e-e"), (6, "f"), (10, "j-j"), (11, "k"), (12, "l"), (21, "v-v"), (26, "z")]
testDiff1 nm rep =
  testcase
    nm
    (DiffD (fromListD rep l1) (fromListD rep l2))
    [(1, "a"), (12, "l"), (26, "z")]
testDiff2 nm rep = testcase nm (DiffD (fromListD rep l2) (fromListD rep l1)) [(3, "c"), (6, "f"), (11, "k")]

-- ==========================================================================
-- tests where we vary both the data, and how it is represented.

testGuard :: (Show b, Iter f, Ord b) => String -> BaseRep f Int b -> [(Int, b)] -> TestTree
testGuard nm rep f =
  testcase
    nm
    (GuardD (fromListD rep f) (domElem evens))
    (filter (even . fst) f)

testProj :: (Show k, Ord k, Iter f) => String -> BaseRep f k [Char] -> [(k, [Char])] -> TestTree
testProj nm rep f =
  testcase
    nm
    (ProjectD (fromListD rep f) (lift (\_x y -> ord (y !! 0))))
    [(k, ord (v !! 0)) | (k, v) <- f]

-- =============================================================================
-- tests where we AndP l1 and l3, and use different type of data for l1 from l3
-- We use the second projection in AndP, that is the value will come from l3

testAndP :: (Iter f, Iter g) => String -> BaseRep f Int String -> BaseRep g Int Int -> TestTree
testAndP nm rep1 rep2 =
  testcase
    nm
    (AndPD (fromListD rep1 l1) (fromListD rep2 l3) rngSnd)
    [(4, 12), (12, 44)]

testChain :: (Iter f, Iter g) => String -> BaseRep f Int String -> BaseRep g String Int -> TestTree
testChain nm rep1 rep2 =
  testcase
    nm
    (ChainD (fromListD rep1 l4) (fromListD rep2 l5) (lift (\x (y, v) -> (x, y, v))))
    [(1, (1, "m", 105)), (2, (2, "a", 101)), (6, (6, "b", 102)), (12, (12, "w", 108)), (50, (50, "q", 107))]

testChain2 :: (Iter f, Iter g) => String -> BaseRep f String Int -> BaseRep g Int String -> TestTree
testChain2 nm rep1 rep2 =
  testcase
    nm
    (ChainD (fromListD rep1 l5) (fromListD rep2 l4) (lift (\x (y, v) -> (x, y, v))))
    [("m", ("m", 105, "Z"))]

-- This test inspired by set expression in EpochBoundary.hs
testEpochEx :: TestTree
testEpochEx =
  testCase
    "Epoch Boundary Example"
    ( assertEqual
        "Epoch Boundary Example"
        (Map.fromList [(6, True)])
        (eval (DRestrict (Dom (RRestrict (Base MapR delegs) (SetSingleton hk))) (Base MapR state)))
    )
  where
    delegs = Map.fromList [(5 :: Int, "a"), (6, "b"), (12, "c"), (14, "e"), (20, "f"), (25, "g")]
    hk = "b"
    state = Map.fromList [(n, even n) | n <- [1 .. 13]]

iter_tests :: TestTree
iter_tests =
  testGroup
    "Iterator tests"
    [ testAnd1 "(And l1 l2) as List, fifo" ListR,
      testAnd1 "(And l1 l2) as Map, fifo" MapR,
      testAnd1 "(And l1 l2) as BiMap, fifo" BiMapR,
      testAnd2 "(And l1 l2) as List, lifo" ListR,
      testAnd2 "(And l1 l2) as Map, lifo" MapR,
      testAnd2 "(And l1 l2) as BiMap, lifo" BiMapR,
      testOr "(Or l1 l2) as List" ListR,
      testOr "(Or l1 l2) as Map" MapR,
      testOr "(Or l1 l2) as BiMap" BiMapR,
      testDiff1 "(Diff l1 l2) as List" ListR, -- (Diff is not symmetric)
      testDiff2 "(Diff l2 l1) as List" ListR,
      testDiff1 "(Diff l1 l2) as Map" MapR,
      testDiff2 "(Diff l2 l1) as Map" MapR,
      testDiff1 "(Diff l1 l2) as BiMap" BiMapR,
      testDiff2 "(Diff l2 l1) as BiMap" BiMapR,
      testGuard "(Guard l1 even) as List" ListR l1,
      testGuard "(Guard l1 even) as Map" MapR l1,
      testGuard "(Guard l1 even) as BiMap" BiMapR l1,
      testGuard "(Guard l2 even) as List" ListR l2,
      testGuard "(Guard l2 even) as Map" MapR l2,
      testGuard "(Guard l2 even) as BiMap" BiMapR l2,
      testProj "(Proj l1 ord) as List" ListR l1,
      testProj "(Proj l1 ord) as Map" MapR l1,
      testProj "(Proj l1 ord) as BiMap" BiMapR l1,
      testProj "(Proj l2 ord) as List" ListR l2,
      testProj "(Proj l2 ord) as Map" MapR l2,
      testProj "(Proj l2 ord) as BiMap" BiMapR l2,
      testAndP "(AndP l1:List l3:Map ord)" ListR MapR,
      testAndP "(AndP l1:Map l3:List ord)" MapR ListR,
      testAndP "(AndP l1:Map l3:List Map)" MapR MapR,
      testAndP "(AndP l1:BiMap l3:List Map)" BiMapR MapR,
      testChain "(Chain l4:List l5:Map)" ListR MapR,
      testChain "(Chain l4:Map l5:List)" MapR ListR,
      testChain "(Chain l4:Map l5:List Map)" MapR MapR,
      testChain "(Chain l4:BiMap l5:List Map)" BiMapR MapR,
      testChain2 "(Chain2 l5:List l4:Map)" ListR MapR,
      testChain2 "(Chain2 l5:Map l4:List)" MapR ListR,
      testChain2 "(Chain2 l5:Map l4:List Map)" MapR MapR,
      testChain2 "(Chain2 l5:BiMap l4:List Map)" BiMapR MapR,
      testEpochEx
    ]

intersect2ways :: Map Int Char -> Map Int String -> Char -> Bool
intersect2ways delegs stake hk =
  (materialize MapR (do (x, y, z) <- delegs `domEq` stake; when (y == hk); one (x, z)))
    == (intersectDomPLeft (\_k v2 -> v2 == hk) stake delegs)

intersectDomPLeftTest :: TestTree
intersectDomPLeftTest = testProperty "intersect2ways" intersect2ways

ledgerStateProp :: Map Int Bool -> Map Int Char -> Map Char String -> Bool
ledgerStateProp xx yy zz =
  (materialize MapR (do (x, _, y) <- xx `domEq` yy; y `element` zz; one (x, y)))
    == (intersectDomP (\_k v -> Map.member v zz) xx yy)

ledgerStateTest :: TestTree
ledgerStateTest = testProperty "ledgerStateExample2ways" ledgerStateProp

threeWay :: Map Int Char -> Map Int String -> Char -> Bool
threeWay delegs stake hk =
  ((runSet (dom (delegs ▷ Set.singleton hk) ◁ stake)))
    == (intersectDomPLeft (\_k v2 -> v2 == hk) stake delegs)
    && (runSet (dom (delegs ▷ Set.singleton hk) ◁ stake))
      == materialize MapR (do (x, y, z) <- delegs `domEq` stake; when ((y == hk)); one (x, z))

threeWayTest :: TestTree
threeWayTest = testProperty "eval-materialize-intersectDom" threeWay

-- ==============================================================================
-- Slow property tests show that (compute e) and (runExp e) have the same answer.
-- The function (runExp), which uses compile, can be much slower than compute.
-- The reason for including (runExp) is that every query can be answered using
-- runExp, but only Queries which have data structure specific implementations
-- can use (compute). See the big case analysis in the function (compute).
-- ==============================================================================

-- Concrete types in the arbitrary monad to use to run slow tests.

newtype Key = Key Int
  deriving (Eq, Ord)

instance Show Key where
  show (Key n) = "k" ++ show n

-- ----------------------------
newtype Range = Range Int
  deriving (Eq, Ord, Num)

instance Show Range where
  show (Range n) = show n

instance Semigroup Range where (Range x) <> (Range y) = Range (x + y)

instance Monoid Range where mempty = Range 0

-- ===========================================================
-- helper functions to construct related types and Properties.

flip_rng :: (Ord b, Num b) => List a b -> List b b
flip_rng (UnSafeList xs) = fromPairs (+) (map (\(a, b) -> (b, b)) xs)

bimap :: (Ord k, Ord v) => Map k v -> BiMap v k v
bimap xs = biMapFromList (\earlier later -> later) (Map.toList xs)

duplicate :: Ord a => Set.Set a -> Map.Map a a
duplicate s = foldr (\a m -> Map.insert a a m) Map.empty s

btest :: Exp Bool -> Property
btest expr = (compute expr) === (runBool expr)

qtest :: (Ord key, Eq (f key a), Show (f key a)) => Exp (f key a) -> Property
qtest expr = (compute expr) === (runSet expr)

-- ======================================================

type STest =
  Key -> -- k
  Range -> -- v
  Map Key Range -> -- m1
  Map Key Range -> -- m2
  Set.Set Key -> -- s1
  Set.Set Key -> -- s2
  Set.Set Range -> -- rs
  List Key Range -> -- ls
  Property

slowFastEquiv :: TestTree
slowFastEquiv = testGroup "slowFastEquiv" (map f many)
  where
    f (prop, name) = testProperty name prop

-- Here is where we need to turn on -fno-warn-unused-matches
-- Note how the typing (STest) fixes the type of each lambda expression,
-- even though some tests do not mention some of the variables.

many :: [(STest, String)]
many =
  [ (\k v m1 m2 s1 s2 rs ls -> qtest (Dom (Base SetR (Sett s1))), "slow1"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Dom (Base MapR m1)), "slow2"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Dom (Base SetR (Sett s1))), "slow3"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Dom (Base MapR m1)), "slow4"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Dom (Singleton k v)), "slow5"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Dom (SetSingleton k)), "slow6"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Dom (Base MapR m1)), "slow7"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Dom (RRestrict (Base MapR m1) (SetSingleton v))), "slow8"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Dom (RRestrict (Base MapR m1) (Base SetR (Sett rs)))), "slow9"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Dom (RExclude (Base MapR m1) (SetSingleton v))), "slow10"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Dom (RExclude (Base MapR m1) (Base SetR (Sett rs)))), "slow11"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Dom (DRestrict (SetSingleton k) (Base MapR m1))), "slow12"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Dom (DRestrict (Base SetR (Sett s1)) (Base MapR m1))), "slow13"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Dom (DExclude (SetSingleton k) (Base MapR m1))), "slow14"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Dom (DExclude (Base SetR (Sett s1)) (Base MapR m1))), "slow15"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Rng (Base SetR (Sett s1))), "slow16"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Rng (Singleton k v)), "slow17"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Rng (SetSingleton k)), "slow18"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Rng (Base MapR m1)), "slow19"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DRestrict (Base SetR (Sett s1)) (Base MapR m1)), "slow21"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DRestrict (SetSingleton k) (Base MapR m1)), "slow22"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DRestrict (Singleton k ()) (Base MapR m1)), "slow23"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DRestrict (Dom (Base MapR m2)) (Base MapR m1)), "slow24"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DRestrict (Dom (RRestrict (Base MapR m1) (SetSingleton v))) (Base MapR m2)), "slow25"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DRestrict (Dom (RRestrict (Base MapR m1) (Base SetR (Sett rs)))) (Base MapR m2)), "slow26"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DRestrict (Base SetR (Sett s1)) (Base MapR m1)), "slow27"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DRestrict (Base SetR (Sett s1)) (Base SetR (Sett s2))), "slow28"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DRestrict (Base SetR (Sett s1)) (Base ListR ls)), "slow29"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DRestrict (Dom (Base MapR m1)) (Base ListR ls)), "slow30"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DRestrict (SetSingleton k) (Base ListR ls)), "slow31"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DRestrict (Dom (Singleton k v)) (Base ListR ls)), "slow32"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DRestrict (Rng (Singleton k v)) (Base ListR (flip_rng ls))), "slow33"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DExclude (SetSingleton k) (Base MapR m1)), "slow35"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DExclude (Dom (Singleton k v)) (Base MapR m1)), "slow36"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DExclude (Rng (Singleton v k)) (Base MapR m1)), "slow37"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DExclude (Base SetR (Sett s1)) (Base MapR m1)), "slow38"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DExclude (Dom (Base MapR m1)) (Base MapR m2)), "slow39"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DExclude (SetSingleton k) (Base BiMapR (bimap m1))), "slow40"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DExclude (Dom (Singleton k v)) (Base BiMapR (bimap m1))), "slow41"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (DExclude (Rng (Singleton v k)) (Base BiMapR (bimap m1))), "slow42"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (RExclude (Base BiMapR (bimap m1)) (SetSingleton v)), "slow44"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (RExclude (Base BiMapR (bimap m1)) (Dom (Singleton v k))), "slow45"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (RExclude (Base BiMapR (bimap m1)) (Rng (Singleton k v))), "slow46"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (RExclude (Base MapR m1) (Base SetR (Sett rs))), "slow47"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (RExclude (Base MapR m1) (SetSingleton v)), "slow48"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (RExclude (Base ListR ls) (Base SetR (Sett rs))), "slow49"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (RExclude (Base ListR ls) (Base SingleR Fail)), "slow50"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (RRestrict (Base MapR m1) (SetSingleton v)), "slow52"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (RRestrict (DRestrict (Dom (Base MapR m1)) (Base MapR m1)) (Dom (Base MapR (duplicate rs)))), "slow53"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (RRestrict (DRestrict (Dom (Base MapR m1)) (Base MapR m2)) (Dom (Base ListR (flip_rng ls)))), "slow54"),
    (\k v m1 m2 s1 s2 rs ls -> btest (Elem k (Dom (Base ListR ls))), "slow56"),
    (\k v m1 m2 s1 s2 rs ls -> btest (Elem k (Base SetR (Sett s1))), "slow57"),
    (\k v m1 m2 s1 s2 rs ls -> btest (Elem k (Dom (Singleton k v))), "slow58"),
    (\k v m1 m2 s1 s2 rs ls -> btest (Elem k (Rng (Singleton v k))), "slow59"),
    (\k v m1 m2 s1 s2 rs ls -> btest (Elem k (SetSingleton k)), "slow60"),
    (\k v m1 m2 s1 s2 rs ls -> btest (Elem k (UnionOverrideLeft (Base SetR (Sett s1)) (Base SetR (Sett s2)))), "slow61"),
    (\k v m1 m2 s1 s2 rs ls -> btest (Elem k (UnionOverrideRight (Base SetR (Sett s1)) (Base SetR (Sett s2)))), "slow62"),
    (\k v m1 m2 s1 s2 rs ls -> btest (Elem k (UnionPlus (Base SetR (Sett s1)) (Base SetR (Sett s2)))), "slow63"),
    (\k v m1 m2 s1 s2 rs ls -> btest (Elem k (Intersect (Base SetR (Sett s1)) (Base SetR (Sett s2)))), "slow64"),
    (\k v m1 m2 s1 s2 rs ls -> btest (Elem k (DRestrict (Dom (Base SetR (Sett s1))) (Dom (Base MapR m1)))), "slow106"),
    (\k v m1 m2 s1 s2 rs ls -> btest (Elem k (DExclude (Dom (Base SetR (Sett s1))) (Dom (Base MapR m1)))), "slow107"),
    (\k v m1 m2 s1 s2 rs ls -> btest (NotElem k (Dom (Base ListR ls))), "slow66"),
    (\k v m1 m2 s1 s2 rs ls -> btest (NotElem k (Base SetR (Sett s1))), "slow67"),
    (\k v m1 m2 s1 s2 rs ls -> btest (NotElem k (Dom (Singleton k v))), "slow68"),
    (\k v m1 m2 s1 s2 rs ls -> btest (NotElem k (Rng (Singleton v k))), "slow69"),
    (\k v m1 m2 s1 s2 rs ls -> btest (NotElem k (SetSingleton k)), "slow70"),
    (\k v m1 m2 s1 s2 rs ls -> btest (NotElem k (UnionOverrideLeft (Base SetR (Sett s1)) (Base SetR (Sett s2)))), "slow71"),
    (\k v m1 m2 s1 s2 rs ls -> btest (NotElem k (UnionOverrideRight (Base SetR (Sett s1)) (Base SetR (Sett s2)))), "slow72"),
    (\k v m1 m2 s1 s2 rs ls -> btest (NotElem k (UnionPlus (Base SetR (Sett s1)) (Base SetR (Sett s2)))), "slow73"),
    (\k v m1 m2 s1 s2 rs ls -> btest (NotElem k (Intersect (Base SetR (Sett s1)) (Base SetR (Sett s2)))), "slow74"),
    (\k v m1 m2 s1 s2 rs ls -> btest (Subset (Base SetR (Sett s1)) (Base SetR (Sett s2))), "slow76"),
    (\k v m1 m2 s1 s2 rs ls -> btest (Subset (Base SetR (Sett s1)) (Base MapR m1)), "slow77"),
    (\k v m1 m2 s1 s2 rs ls -> btest (Subset (Base SetR (Sett s1)) (Dom (Base MapR m1))), "slow78"),
    (\k v m1 m2 s1 s2 rs ls -> btest (Subset (Base MapR m1) (Base MapR m2)), "slow79"),
    (\k v m1 m2 s1 s2 rs ls -> btest (Subset (Dom (Base MapR m1)) (Dom (Base MapR m2))), "slow80"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Intersect (Base SetR (Sett s1)) (Base SetR (Sett s2))), "slow82"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Intersect (Base MapR m1) (Base MapR m2)), "slow83"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (UnionOverrideLeft (Base ListR ls) (Singleton k v)), "slow85"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (UnionOverrideLeft (Base MapR m1) (Base MapR m2)), "slow86"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (UnionOverrideLeft (Base SetR (Sett s1)) (Base SetR (Sett s2))), "slow87"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (UnionOverrideLeft (DExclude (SetSingleton k) (Base MapR m1)) (Base MapR m2)), "slow88"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (UnionOverrideLeft (DExclude (Base SetR (Sett s1)) (Base MapR m1)) (Base MapR m2)), "slow89"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (UnionOverrideRight (Base ListR ls) (Singleton k v)), "slow91"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (UnionOverrideRight (Base MapR m1) (Base MapR m2)), "slow92"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (UnionOverrideRight (Base SetR (Sett s1)) (Base SetR (Sett s2))), "slow93"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (UnionPlus (Base MapR m1) (Base MapR m2)), "slow95"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (UnionPlus (Base SetR (Sett s1)) (Base SetR (Sett s2))), "slow96"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (Singleton k v), "slow98"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (SetSingleton k), "slow99"),
    (\k v m1 m2 s1 s2 rs ls -> btest (KeyEqual (Base MapR m1) (Base MapR m2)), "slow100"),
    (\k v m1 m2 s1 s2 rs ls -> btest (KeyEqual (Base BiMapR (bimap m1)) (Base BiMapR (bimap m2))), "slow101"),
    (\k v m1 m2 s1 s2 rs ls -> btest (KeyEqual (Dom (Base MapR m1)) (Dom (Base MapR m2))), "slow102"),
    (\k v m1 m2 s1 s2 rs ls -> btest (KeyEqual (Dom (Base BiMapR (bimap m1))) (Dom (Base BiMapR (bimap m2)))), "slow103"),
    (\k v m1 m2 s1 s2 rs ls -> btest (KeyEqual (Base SetR (Sett s1)) (Base SetR (Sett s2))), "slow104"),
    (\k v m1 m2 s1 s2 rs ls -> btest (KeyEqual (Base MapR m1) (Base SetR (Sett s1))), "slow105"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (SetDiff (Base SetR (Sett s1)) (Base SetR (Sett s2))), "slow108"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (SetDiff (Base SetR (Sett s1)) (Base MapR m2)), "slow109"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (SetDiff (Base SetR (Sett s1)) (Dom (Base MapR m2))), "slow110"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (SetDiff (Base MapR m1) (Dom (Base MapR m2))), "slow111"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (SetDiff (Base MapR m1) (Base MapR m2)), "slow112"),
    (\k v m1 m2 s1 s2 rs ls -> qtest (SetDiff (Base MapR m1) (Base SetR (Sett s2))), "slow113")
  ]

-- ==================================================
-- Arbitrary instances for the slow tests.

genKey :: Gen Key
genKey = fmap Key (choose (1, 12)) -- Keep the set of Key and Range pretty small so Maps share keys

genRange :: Gen Range
genRange = fmap Range (choose (1, 20)) -- The Range type can have a slightly larger set

genSize :: Gen Int -- Sizes should favor middle sized numbers
genSize =
  frequency
    [ (1, return 0),
      (2, return 1),
      (5, return 2),
      (5, return 3),
      (4, return 4),
      (3, return 5),
      (2, return 6),
      (1, return 7)
    ]

genPair :: Gen k -> Gen v -> Gen (k, v)
genPair k v = (,) <$> k <*> v

genList :: Ord k => Gen k -> Gen v -> Gen (List k v)
genList k v = do
  n <- genSize
  xs <- vectorOf n (genPair k v)
  pure $ fromPairs (\old new -> new) xs

genMap :: Ord k => Gen k -> Gen v -> Gen (Map k v)
genMap k v = do
  n <- genSize
  xs <- vectorOf n (genPair k v)
  pure (Map.fromList xs)

genSett :: Ord k => Gen k -> Gen (Sett k ())
genSett k = do
  n <- genSize
  xs <- vectorOf n k
  pure (Sett (Set.fromList xs))

genBiMap :: (Ord k, Ord v) => Gen k -> Gen v -> Gen (Bimap k v)
genBiMap k v = do
  m <- genMap k v
  pure (bimap m)

instance Arbitrary Key where
  arbitrary = genKey

instance Arbitrary Range where
  arbitrary = genRange

instance Arbitrary (List Key Range) where
  arbitrary = genList genKey genRange

instance Arbitrary (Sett Key ()) where
  arbitrary = genSett genKey

-- ========================================
-- BiMap tests. BiMaps have two parts that
-- should encode the same information. Test
-- that every randomly generated one does.
-- =========================================

flatten :: (Ord k) => Map.Map v (Set.Set k) -> Map.Map k v
flatten m = Map.foldrWithKey accum Map.empty m
  where
    accum val setk ans = Set.foldr accum2 ans setk
      where
        accum2 key m2 = Map.insert key val m2

ok :: (Ord k, Ord v) => BiMap v k v -> Bool
ok (MkBiMap forwrd backwrd) = forwrd == (flatten backwrd)

okfromList :: [(Int, Int)] -> Bool
okfromList xs = ok (biMapFromList (\earlier later -> later) xs)

biMapTest :: TestTree
biMapTest = testProperty "BiMap Consistent" okfromList

-- ====================================================
-- Tie all the tests together
-- ====================================================

setAlgTest :: TestTree
setAlgTest =
  testGroup
    "Set Algebra Tests"
    [ eval_tests,
      keysEqTests,
      iter_tests,
      intersectDomPLeftTest,
      ledgerStateTest,
      threeWayTest,
      slowFastEquiv,
      biMapTest
    ]
