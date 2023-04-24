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
module Test.Cardano.Ledger.Constrained.PairSpec where

import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Core (Era (..))
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import Debug.Trace (trace)
import Lens.Micro hiding (set)
import Test.Cardano.Ledger.Constrained.Ast (Pred (..), Sum (..), Term (..), runPred)
import Test.Cardano.Ledger.Constrained.Classes (
  Adds (..),
  Sums (..),
  genFromAddsSpec,
  genFromNonNegAddsSpec,
  projAdds,
  sumAdds,
 )
import Test.Cardano.Ledger.Constrained.Combinators (
  addUntilSize,
  errorMess,
  fixSet,
  setSized,
  subsetFromSet,
  suchThatErr,
  superSetFromSet,
  superSetFromSetWithSize,
 )
import Test.Cardano.Ledger.Constrained.Env (Access (No), V (..), emptyEnv, storeVar)
import Test.Cardano.Ledger.Constrained.Monad
import Test.Cardano.Ledger.Constrained.Size (
  AddsSpec (..),
  OrdCond (..),
  Size (..),
  atLeastDelta,
  atMostAny,
  genFromIntRange,
  genFromNonNegIntRange,
  genFromSize,
  runSize,
  seps,
  sepsP,
  vLeft,
  vRight,
 )
import Test.Cardano.Ledger.Constrained.TypeRep (
  Rep (..),
  genRep,
  synopsis,
  testEql,
  (:~:) (Refl),
 )
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Generic.Proof (BabbageEra, Standard)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Tasty
import Test.Tasty.QuickCheck hiding (total)

import Test.Cardano.Ledger.Constrained.Spec
import Test.Cardano.Ledger.Constrained.Combinators(mapFromSubset)

-- ===========================================================
{- TODO, possible extensions and improvements, so we don't forget
1) Redo Size with: data Size = SzNever [String] | SzRange Int (Maybe Int) Move to own file
2) Add newtype Never = Never [String], then instead of (XXXNever xs) we use (XX (Never xs))
3) class Specify
4) A better story about fields in constraints. Maybe add FieldSpec type.
-}


-- ===================================

-- | A map 'm1' meets the '(PairSpec _ _ m2)' specification if every
--   (key,value) pair in 'm2' is in 'm1'.
data PairSpec era a b where
  PairSpec :: (Ord a,Eq b) => Rep era a -> Rep era b -> Map a b -> PairSpec era a b
  PairNever :: [String] -> PairSpec era a b
  PairAny :: PairSpec era a b

anyPairSpec :: PairSpec era d r -> Bool
anyPairSpec PairAny = True
anyPairSpec (PairSpec _ _ m) = Map.null m
anyPairSpec _ = False

instance Monoid (PairSpec era a b) where
  mempty = PairAny

instance Semigroup (PairSpec era dom rng) where
  (<>) = mergePairSpec

instance Show (PairSpec era dom rng) where
  show = showPairSpec

instance LiftT (PairSpec era dom rng) where
  liftT (PairNever xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = PairNever s
  dropT (Typed (Right x)) = x

showPairSpec :: PairSpec era dom rng -> String
showPairSpec (PairNever _) = "PairNever"
showPairSpec PairAny = "PairAny"
showPairSpec (PairSpec dom rng mp) = sepsP ["PairSpec", show dom, show rng,synopsis (MapR dom rng) mp]

mergePairSpec :: PairSpec era a b -> PairSpec era a b -> PairSpec era a b
mergePairSpec (PairNever xs) (PairNever ys) = PairNever (xs ++ ys)
mergePairSpec d@(PairNever _) _ = d
mergePairSpec _ d@(PairNever _) = d
mergePairSpec PairAny x = x
mergePairSpec x PairAny = x
mergePairSpec a@(PairSpec d r m1) b@(PairSpec _ _ m2) =
 let accum (Right zs) key v =
        case Map.lookup key zs of
          Nothing -> Right (Map.insert key v zs)
          Just u ->
            if u==v
               then Right zs
               else Left ["The PairSpecs","   "++show a++" and","   "++show b++
                           " are inconsistent."
                         ,"The key "++synopsis d key++" has multiple values: "++
                          synopsis r v++" and "++synopsis r u]
     accum (Left xs) _ _ = Left xs
 in case Map.foldlWithKey' accum (Right m1) m2 of
      Left xs -> PairNever xs
      Right m3 -> PairSpec d r m3

sizeForPairSpec :: PairSpec era dom rng -> Size
sizeForPairSpec PairAny = SzAny
sizeForPairSpec (PairNever _) = SzAny
sizeForPairSpec (PairSpec _ _ m) = SzLeast(Map.size m)

runPairSpec :: (Ord dom, Eq rng)  => Map dom rng -> PairSpec era dom rng -> Bool
runPairSpec _ PairAny = True
runPairSpec _ (PairNever xs) = errorMess "PairNever in call to runPairSpec" xs
runPairSpec m1 (PairSpec _ _ m2) = Map.isSubmapOf m2 m1

genPairSpec :: (Era era,Ord dom,Eq rng) => Rep era dom -> Rep era rng -> Gen (PairSpec era dom rng)
genPairSpec domr rngr = frequency
  [(1,pure PairAny)
  ,(1,pure(PairSpec domr rngr Map.empty))
  ,(4,PairSpec domr rngr <$> (Map.singleton <$> genRep domr <*> genRep rngr)) 
  ,(2, do d1 <- genRep domr
          d2 <- genRep domr
          r1 <- genRep rngr
          r2 <- genRep rngr
          pure(PairSpec domr rngr (Map.fromList[(d1,r1),(d2,r2)])))         
  ]

genConsistentPairSpec :: (Ord dom,Era era,Eq rng) =>
  Rep era dom -> Rep era rng -> PairSpec era dom rng -> Gen(PairSpec era dom rng)
genConsistentPairSpec domr rngr (PairNever xs) = errorMess "PairNever in genConsistentPairSpec" xs
genConsistentPairSpec domr rngr PairAny = genPairSpec domr rngr
genConsistentPairSpec domr rngr (PairSpec d r m) | Map.null m = genPairSpec domr rngr
genConsistentPairSpec _ _ (PairSpec d r m) = frequency
  [(1,pure PairAny)
  ,(1,do { n <- choose(0,Map.size m - 1); pure(PairSpec d r (Map.deleteAt n m))})
  ,(1,do d1 <-suchThatErr ["genConsistentPairSpec"] (genRep d) (not . (`Map.member` m))
         r1 <- genRep r
         pure(PairSpec d r (Map.insert d1 r1 m)))
  ]

genFromPairSpec:: (Era era,Ord dom) => [String] -> PairSpec era dom rng -> Gen (Map dom rng)
genFromPairSpec msgs p@(PairSpec domr rngr mp) = do
  n <- (+ (Map.size mp)) <$> choose(0,10)
  mapFromSubset (msgs ++ ["genFromPairSpec "++show p]) mp n (genRep domr) (genRep rngr)

-- ============================================
-- tests for Pair Spec

testConsistentPair :: Gen Property
testConsistentPair = do
  s1 <- genPairSpec @TT IntR IntR
  s2 <- genConsistentPairSpec IntR IntR s1
  case s1 <> s2 of
    PairNever ms ->
      pure $ counterexample (unlines (["genConsistentPair fails", show s1, show s2] ++ ms)) False
    _ -> pure $ property True


testSoundPairSpec :: Gen Property
testSoundPairSpec = do
  s1 <- genPairSpec IntR Word64R
  ans <- genFromPairSpec @TT ["testSoundPairSpec"] s1
  pure $ counterexample ("spec=" ++ show s1 ++ "\nans=" ++ show ans) (runPairSpec ans s1)


testMergePairSpec :: Gen Property
testMergePairSpec = do
  let msg = ["testMergePairSpec"]
  s1 <- genPairSpec Word64R IntR 
  s2 <- genConsistentPairSpec Word64R IntR s1
  case s1 <> s2 of
    PairNever _ -> pure (property True) -- This test is an implication (consistent (s1 <> s2) => ...)
    s4 -> do
      ans <- genFromPairSpec @TT ["testMergePairSpec " ++ show s1 ++ " " ++ show s2] s4
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
              ++ show (runPairSpec ans s1)
              ++ "\nrun s2="
              ++ show (runPairSpec ans s2)
              ++ "\nrun s4="
              ++ show (runPairSpec ans s4)
          )
          (runPairSpec ans s4 && runPairSpec ans s2 && runPairSpec ans s1)

manyMergePairSpec :: Gen (Int, [String])
manyMergePairSpec = do
  xs <- vectorOf 60 (genPairSpec Word64R IntR)
  ys <- vectorOf 60 (genPairSpec Word64R IntR)
  let ok PairAny = False
      ok _ = True
      check (x, y, m) = do
        let size = sizeForPairSpec m
        i <- genFromSize size
        let wordsX =
              [ "s1<>s2 Size = " ++ show (sizeForPairSpec m)
              , "s1<>s2 = " ++ show m
              , "s2 Size = " ++ show (sizeForPairSpec x)
              , "s2 = " ++ show x
              , "s1 Size = " ++ show (sizeForPairSpec y)
              , "s1 = " ++ show y
              , "GenFromPairSpec " ++ show i
              ]
        z <- genFromPairSpec @TT wordsX m
        pure (x, runPairSpec z x, y, runPairSpec z y, z, runPairSpec z m, m)
      showAns (s1, run1, s2, run2, v, run3, s3) =
        unlines
          [ "\ns1 = " ++ show s1
          , "s1 Size = " ++ show (sizeForPairSpec s1)
          , "s2 = " ++ show s2
          , "s2 Size = " ++ show (sizeForPairSpec s2)
          , "s1 <> s2 = " ++ show s3
          , "s1<>s2 Size = " ++ show (sizeForPairSpec s3)
          , "v = genFromPairSpec (s1 <> s2) = " ++ show v
          , "runPairSpec v s1 = " ++ show run1
          , "runPairSpec v s2 = " ++ show run2
          , "runPairSpec v (s1 <> s2) = " ++ show run3
          ]
      pr x@(_, a, _, b, _, c, _) = if not (a && b && c) then Just (showAns x) else Nothing
  let trips = [(x, y, m) | x <- xs, y <- ys, ok x && ok y, Just m <- [consistent x y]]
  ts <- mapM check trips
  pure $ (length trips, Maybe.mapMaybe pr ts)

reportManyMergePairSpec :: IO ()
reportManyMergePairSpec = do
  (passed, bad) <- generate manyMergePairSpec
  if null bad
    then putStrLn ("passed " ++ show passed ++ " tests")
    else do mapM_ putStrLn bad; error "TestFails"


main3 :: IO ()
main3 =
  defaultMain $
    testGroup "PairSpec test"
     [ testProperty "test sound PairSpec" testSoundPairSpec
     , testProperty "test ConsistentPair" testConsistentPair
     , testProperty "test merge PairSpec" testMergePairSpec
     , testProperty "test More consistent PairSpec" reportManyMergePairSpec
     ]

-- =============================================================

genFromMapSpec2 ::
  forall era w dom.
  (Era era, Ord dom) =>
  (V era (Map dom w)) ->
  [String] ->
  Gen dom ->
  Gen w ->
  MapSpec era dom w ->
  PairSpec era dom w ->
  Gen (Map dom w)
genFromMapSpec2 nm msgs genD genR ms@(MapSpec size rel rng) (PairSpec dr rr m) = do
  n <- genFromSize size
  dom <-
    genFromRelSpec
      (("GenFromMapSpec " ++ (show nm) ++ "\n   " ++ show ms) : msgs)
      genD
      n
      rel
  rangelist <-
    genFromRngSpec
      (("genFromMapSpec " ++ (show nm) ++ "\n   " ++ show ms) : msgs)
      genR
      n
      rng
  let domainlist = Set.toList dom
      (doms,rngs) = Map.foldlWithKey' accum (domainlist,rangelist) m
      accum (ds,rs) k v = (remove dr "domain" k ds, remove rr "range" v rs)
  pure (Map.union m (Map.fromList (zip doms rngs)))

remove :: Eq a => Rep era a -> [Char] -> a -> [a] -> [a]
remove rep part x (y:ys) =
  if x==y then ys else y:(remove rep part x ys)
remove rep part x [] =
  errorMess ("The "++part++" mustSet does not contain "++synopsis rep x++
             " which appears in the "++part++" of the PairSpec.") ["genFromMapSpec"]

-- requireAll :: [(Bool, [String])] -> Typed a -> Typed a

-- | Use 'mapSpec' instead of 'MapSpec' to check size and PairSpec consistency at creation time.
--   Runs in the type monad, so errors are caught and reported as Solver-time errors.
--   This should avoid many Gen-time errors, as many of those are cause by size
--   inconsistencies. We can also use this in mergeMapSpec, to catch size
--   inconsistencies there as well as (\ a b c -> dropT (mapSpec a b c)) has the same
--   type as MapSpec, but pushes the reports of inconsistencies into MapNever.
mapSpec2 :: Ord d => Size -> RelSpec era d -> PairSpec era d r-> RngSpec era r -> Typed (MapSpec era d r,PairSpec era d r)
mapSpec2 sz1 rel pair rng =
  let sz2 = sizeForRel rel
      sz3 = sizeForRng rng
      sz4 = sizeForPairSpec pair
  in case sz1 <> sz2 <> sz3 <> sz4 of
      SzNever xs -> failT
        ( [ "Creating " ++ show (MapSpec sz1 rel rng) ++ " fails."
          , "It has size inconsistencies."
          , "  " ++ show rel ++ " has size " ++ show sz2
          , "  " ++ show pair ++ " has size " ++ show sz4
          , "  " ++ show rng ++ " has size " ++ show sz3
          ]
          ++ xs )
      size ->
        case (rel,pair,rng) of
          (_,p,_) | anyPairSpec p -> pure (MapSpec size rel rng,pair)
          (rel@(RelOper _ mustd _ _),PairSpec d r m,rng@(RngRel (RelOper _ mustr _ _))) ->
             requireAll
              [(not(Map.keysSet m `Set.isSubsetOf` mustd),
                 [ "Creating " ++ show (MapSpec sz1 rel rng) ++ " fails."
                 , "It has PairSpec inconsistencies. The domain of"
                 , "   "++synopsis (MapR d r) m++" is not a subset of the of the mustSet"
                 , "   "++synopsis (SetR d) mustd ])
              ,(not(Set.fromList(Map.elems m) `Set.isSubsetOf` mustr),
                 [ "Creating " ++ show (MapSpec sz1 rel rng) ++ " fails."
                 , "It has PairSpec inconsistencies. The range of"
                 , "   "++synopsis (MapR d r) m++" is not a subset of the of the mustSet"
                 , "   "++synopsis (SetR r) mustr ])              
                 ] 
              (pure (MapSpec size rel rng,pair))
          (rel,PairSpec d r m,_) -> failT
                 [ "Creating " ++ show (MapSpec sz1 rel rng) ++ " fails."
                 , "This spec has a non-PairAny PairSpec"
                 , "   "++show pair
                 , "so to be consistent it must have both a RelOper RelSpec, and a RngRel RelSpec."
                 , "But it does not:"
                 , "   RelSpec = "++show rel
                 , "   RngSpec = "++show rng]
          