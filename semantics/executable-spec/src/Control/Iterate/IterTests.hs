{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IterTests where

import Control.Iterate.Collect
import Control.Iterate.SetAlgebra(Iter(..),
                  Exp(..),BaseRep(..),List(..),Sett(..),Single(..),
                  dom, range, singleton, setSingleton, (◁), (⋪), (▷), (⋫), (∪), (⨃), (∈), (∉),(|>),(<|),
                  element, lifo, fifo,
                  Fun, apply,
                  BiMap(..), Bimap, biMapEmpty, biMapFromList,removeval,
                  Fun(..), Pat(..), Expr(..), Lam(..), evaluate, reify,
                  constant, domElem, second, lift, rngSnd,
                  Query(..), materialize, fromList, eval, run, compile,(⨝)                 )


import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Set as Set
import Data.Char(ord)
import Test.HUnit


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
  do (x, _z, y) <- skcreds ⨝ delegs
     y `element` stpools
     one (x,y)

-- Even better,  stkcreds, delegs, and stpools can be any binary type construtors in the Iter class.

foo :: (Iter s, Iter d, Iter p, Ord a, Ord b1) =>
       s a b2 ->
       d a b1 ->
       p b1 b3 ->
       Map a b1


example :: Exp (Map Int Char)
example = ((dom stkcred) ◁ deleg) ▷ (dom stpool)

stkcred :: Map Int [Char]
deleg :: Map Int Char
stpool :: Map Char Int

stkcred = Map.fromList[(5,"a"),(6,"q"),(12,"r")]
deleg = Map.fromList [ (n,chars !! n) | n <- [1..10]]
stpool = Map.fromList [('A',99),('C',12),('F',42),('R',33),('Z',99)]


-- =============== Build a few maps ===================

chars::String
chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdeghijklmnopqrstuvwxyz0123456789"

nchars :: Int
nchars = length chars

m0 :: Map.Map Int Char
m0 = Map.fromList [(1,'a'),(2,'z'),(4,'g')]

m1 :: Map.Map Int Char
m1 = Map.fromList [ (n,chars !! n) | n <- [0..length chars -1]]

m2 :: Map.Map Int Char
m2 = Map.fromList [ (57+n,chars !! n) | n <- [0..length chars -1]]

mN :: Int -> Int -> Map.Map Int Char
mN start size  = Map.fromList [ (n,chars !! (n `mod` nchars)) | n <- [start..start+size]]

-- | Some really big Maps, with very small overlap.

m5,m6 :: Map.Map Int Char
m5 = mN 1 10000000
m6 = mN 9999995 10000000

b0::Bimap Int Char
b0 = biMapFromList [(1,'a'),(2,'z'),(4,'g')]

-- ============ Some small Maps to And, Or, Diff, Guard, Project with =========

l1,l2 :: [(Int,String)]
l1 = [(1,"a"),(4,"d"),(5,"e"),(10,"j"),(12,"l"),(21,"v"),(26,"z")]
l2 = [(3,"c"),(4,"d"),(5,"e"),(6,"f"),(10,"j"),(11,"k"),(21,"v")]

l3 :: [(Int,Int)]
l3 = [(4,12),(9,13),(12,44),(55,22)]
evens :: Sett Int ()
evens = fromList SetR [(n,()) | n <- [2,4,6,8,10,12,14,16,18,20,22,24,26]]

l4 :: [(Int,String)]
l4 = [(1,"m"),(2,"a"),(5,"z"),(6,"b"),(7,"r"),(12,"w"),(34,"a"),(50,"q"),(51,"l"),(105,"Z")]

l5 :: [(String,Int)]
l5 = [("a",101),("b",102),("c",103),("f",104),("m",105),("q",107),("s",106),("w",108),("y",109),("zz",110)]

-- Chain l4 l5 =  [(1,(1,"m",105)),(2,(2,"a",101)),(6,(6,"b",102)),(12,(12,"w",108)),(34,(34,"a",101)),(50,(50,"q",107))]
-- Chain l5 l4 =  [("m",("m",105,"Z"))]


-- =================== Some sample (Exp t) =============================

ex1 :: Exp Bool
ex1 = 5 ∈ (dom m1)

ex2 :: Exp Bool
ex2 = 70 ∈ (dom m1)

ex3 :: Exp (Map Int Char)
ex3 = m0 ∪ (singleton 3 'b')

ex4,ex5,ex6 :: Exp(Map Int Char)
ex4 = (setSingleton 2) ⋪ m0
ex5 = dom(singleton 2 'z') ⋪ m0
ex6 = range(singleton 'z' 2) ⋪ m0

ex7::Exp Bool
ex7 = 70 ∉ (dom m1)


-- ===================== test that eval works ======================

evalTest :: (Show t,Eq t) => String -> Exp t -> t -> Test
evalTest nm expr ans = TestCase (assertEqual (show expr++" where Map? = "++nm) (eval expr) ans)

eval_compile :: (Show (f k v), Ord k, Eq (f k v)) => Exp (f k v) -> Test
eval_compile expr = TestCase (assertEqual ("eval and run.compile of "++show expr++" are the same") (eval expr) (run(compile expr)))


eval_tests :: Test
eval_tests = TestList
          [ evalTest "m1"  (5 ∈ (dom m1))                True
          , evalTest "m1"  (70 ∈ (dom m1))               False
          , evalTest "m0"  (m0 ∪ (singleton 3 'b'))      (Map.fromList [(1,'a'),(2,'z'),(3,'b'),(4,'g')])
          , evalTest "m0"  ((setSingleton 2) ⋪ m0)       (Map.fromList [(1,'a'),(4,'g')])
          , evalTest "m0"  (dom(singleton 2 'z') ⋪ m0)   (Map.fromList [(1,'a'),(4,'g')])
          , evalTest "m0"  (range(singleton 'z' 2) ⋪ m0) (Map.fromList [(1,'a'),(4,'g')])
          , evalTest "m0"  (70 ∉ (dom m1))                True
          , evalTest "((dom stkcred) ◁ deleg) ▷ (dom stpool)"  (((dom stkcred) ◁ deleg) ▷ (dom stpool)) (Map.fromList [(5,'F')])
          , eval_compile  (((dom stkcred) ◁ deleg) ▷ (dom stpool))
          ]


-- ========================== test that various Compound iterators work ================

testcase :: (Eq k, Eq v, Show k, Show v, Iter f) => String -> f k v -> [(k, v)] -> Test
testcase nm col ans = TestCase (assertEqual nm ans (runCollect (fifo col) [] (:)))

fromListD :: (Ord k,Iter f) => BaseRep f k v -> [(k,v)] -> Query k v
fromListD rep xs = BaseD rep (fromList rep xs)

-- Tests where we vary how we represent l1 and l2 (the f in (Iter f) )
-- and see that we always get the same answer no matter how we store the data of l1 and l2

testAnd1,testAnd2,testOr,testDiff1,testDiff2 :: Iter g => String -> BaseRep g Int String -> Test

testAnd1 nm rep = testcase nm (AndD (fromListD rep l1) (fromListD rep l2))
                              [(4,("d","d")),(5,("e","e")),(10,("j","j")),(21,("v","v"))]


testAnd2 nm rep = TestCase (assertEqual nm (runCollect (lifo (AndD (fromListD rep l1) (fromListD rep l2))) [] (:))
                                           (reverse  [(4,("d","d")),(5,("e","e")),(10,("j","j")),(21,("v","v"))]))

testOr nm rep = testcase nm (OrD (fromListD rep l1) (fromListD rep l2) (lift (\x y -> x++"-"++y)))
                            [(1,"a"),(3,"c"),(4,"d-d"),(5,"e-e"),(6,"f"),(10,"j-j"),(11,"k"),(12,"l"),(21,"v-v"),(26,"z")]

testDiff1 nm rep = testcase nm (DiffD (fromListD rep l1) (fromListD rep l2))
                               [(1,"a"),(12,"l"),(26,"z")]

testDiff2 nm rep = testcase nm (DiffD (fromListD rep l2) (fromListD rep l1)) [(3,"c"),(6,"f"),(11,"k")]


-- ==========================================================================
-- tests where we vary both the data, and how it is represented.

testGuard :: (Show b, Iter f, Ord b) => String -> BaseRep f Int b -> [(Int, b)] -> Test
testGuard nm rep f = testcase nm (GuardD (fromListD rep f) (domElem evens))
                                 (filter (even . fst) f)

testProj :: (Show k, Ord k, Iter f) => String -> BaseRep f k [Char] -> [(k, [Char])] -> Test
testProj nm rep f = testcase nm (ProjectD (fromListD rep f) (lift (\ _x y -> ord (y!!0))))
                                [ (k,ord(v!!0)) | (k,v) <- f ]

-- =============================================================================
-- tests where we AndP l1 and l3, and use different type of data for l1 from l3
-- We use the second projection in AndP, that is the value will come from l3

testAndP :: (Iter f, Iter g) => String -> BaseRep f Int String -> BaseRep g Int Int -> Test
testAndP nm rep1 rep2 =  testcase nm (AndPD (fromListD rep1 l1) (fromListD rep2 l3) rngSnd)
                                     [(4,12),(12,44)]

testChain:: (Iter f, Iter g) => String -> BaseRep f Int String -> BaseRep g String Int -> Test
testChain nm rep1 rep2 = testcase nm (ChainD (fromListD rep1 l4) (fromListD rep2 l5) (lift (\ x (y,v) -> (x,y,v))))
                                    [(1,(1,"m",105)),(2,(2,"a",101)),(6,(6,"b",102)),(12,(12,"w",108)),(34,(34,"a",101)),(50,(50,"q",107))]

testChain2:: (Iter f, Iter g) => String -> BaseRep f String Int -> BaseRep g Int String -> Test
testChain2 nm rep1 rep2 = testcase nm (ChainD (fromListD rep1 l5) (fromListD rep2 l4) (lift (\ x (y,v) -> (x,y,v))))
                                    [("m",("m",105,"Z"))]



iter_tests :: Test
iter_tests = TestList
  [ testAnd1 "(And l1 l2) as List, fifo"  ListR
  , testAnd1 "(And l1 l2) as Map, fifo"   MapR
  , testAnd1 "(And l1 l2) as BiMap, fifo" BiMapR
  , testAnd2 "(And l1 l2) as List, lifo"  ListR
  , testAnd2 "(And l1 l2) as Map, lifo"   MapR
  , testAnd2 "(And l1 l2) as BiMap, lifo" BiMapR

  , testOr "(Or l1 l2) as List" ListR
  , testOr "(Or l1 l2) as Map" MapR
  , testOr "(Or l1 l2) as BiMap" BiMapR

  , testDiff1 "(Diff l1 l2) as List" ListR    -- (Diff is not symmetric)
  , testDiff2 "(Diff l2 l1) as List" ListR
  , testDiff1 "(Diff l1 l2) as Map" MapR
  , testDiff2 "(Diff l2 l1) as Map" MapR
  , testDiff1 "(Diff l1 l2) as BiMap" BiMapR
  , testDiff2 "(Diff l2 l1) as BiMap" BiMapR

  , testGuard "(Guard l1 even) as List" ListR l1
  , testGuard "(Guard l1 even) as Map" MapR l1
  , testGuard "(Guard l1 even) as BiMap" BiMapR l1
  , testGuard "(Guard l2 even) as List" ListR l2
  , testGuard "(Guard l2 even) as Map" MapR l2
  , testGuard "(Guard l2 even) as BiMap" BiMapR l2

  , testProj "(Proj l1 ord) as List" ListR l1
  , testProj "(Proj l1 ord) as Map" MapR l1
  , testProj "(Proj l1 ord) as BiMap" BiMapR l1
  , testProj "(Proj l2 ord) as List" ListR l2
  , testProj "(Proj l2 ord) as Map" MapR l2
  , testProj "(Proj l2 ord) as BiMap" BiMapR l2

  ,testAndP "(AndP l1:List l3:Map ord)" ListR MapR
  ,testAndP "(AndP l1:Map l3:List ord)" MapR ListR
  ,testAndP "(AndP l1:Map l3:List Map)" MapR MapR
  ,testAndP "(AndP l1:BiMap l3:List Map)" BiMapR MapR

  ,testChain "(Chain l4:List l5:Map)" ListR MapR
  ,testChain "(Chain l4:Map l5:List)" MapR ListR
  ,testChain "(Chain l4:Map l5:List Map)" MapR MapR
  ,testChain "(Chain l4:BiMap l5:List Map)" BiMapR MapR

  ,testChain2 "(Chain2 l5:List l4:Map)" ListR MapR
  ,testChain2 "(Chain2 l5:Map l4:List)" MapR ListR
  ,testChain2 "(Chain2 l5:Map l4:List Map)" MapR MapR
  ,testChain2 "(Chain2 l5:BiMap l4:List Map)" BiMapR MapR
  ]

alltests :: Test
alltests = TestList [ eval_tests, iter_tests ]
