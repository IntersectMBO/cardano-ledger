{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.Examples.MapMember where

import Constrained.API
import Constrained.TheKnot
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.QuickCheck hiding (forAll)

-- ===============================================
-- Three Strategies that work

mapMemberA ::
  (Ord k, IsNormalType k, IsNormalType v, HasSpec k, HasSpec v) =>
  Map k v ->
  Term k ->
  Term v ->
  Pred
mapMemberA m key val =
  And
    [ assert $ member_ key (dom_ (lit m))
    , dependsOn val key
    , (caseOn (lookup_ key (lit m)))
        -- Nothing
        (branch $ \_ -> FalsePred (pure "key not in map, mapMember1"))
        -- Just
        (branch $ \x -> assert $ val ==. x)
    ]

mapMemberB ::
  (Ord k, IsNormalType v, HasSpec k, HasSpec v, IsNormalType v, IsNormalType k) =>
  Map k v ->
  Term k ->
  Term v ->
  Pred
mapMemberB m key val =
  And
    [ assert $ member_ key (dom_ (lit m))
    , assert $ cJust_ val ==. lookup_ key (lit m)
    ]

mapMemberC ::
  (Ord k, HasSpec k, HasSpec v, IsNormalType v, IsNormalType v, IsNormalType k) =>
  Map k v ->
  Term k ->
  Term v ->
  Pred
mapMemberC m key val =
  And
    [ assert $ member_ key (dom_ (lit m))
    , forAll (lit m) $ \p -> match p $ \k v -> whenTrue (key ==. k) (assert $ val ==. v)
    ]

-- ===============================================
-- Two Strategies that don't work

mapMemberBad1 ::
  (Ord k, HasSpec k, HasSpec v, IsNormalType v, IsNormalType k) =>
  Term [(k, v)] ->
  Term k ->
  Term v ->
  Pred
mapMemberBad1 m key val =
  And
    [ dependsOn key m
    , dependsOn val m
    , dependsOn val key
    , assert $ elem_ (pair_ key val) m
    ]

mapMemberBad2 ::
  (Ord k, HasSpec k, HasSpec v, IsNormalType v, IsNormalType k) =>
  Map k v ->
  Term k ->
  Term v ->
  Pred
mapMemberBad2 m key val =
  satisfies
    (pair_ key val)
    ( constrained $ \p ->
        [ dependsOn key p -- This causes a cycle
        , dependsOn val p
        , assert $ elem_ p (lit (Map.toList m))
        ]
    )

-- ====================================================
-- Now some specs that produce results

mm :: Map Int Int
mm = Map.fromList [(x, x + x) | x <- [2 .. 5]]

spec1 :: Specification (Int, Int)
spec1 = constrained $ \ [var|p|] ->
  match p $ \ [var|k|] [var|v|] -> mapMemberA mm k v

spec2 :: Specification (Int, Int)
spec2 = constrained $ \ [var|p|] ->
  match p $ \ [var|k|] [var|v|] -> mapMemberB mm k v

spec3 :: Specification (Int, Int)
spec3 = constrained $ \ [var|p|] ->
  match p $ \ [var|k|] [var|v|] -> mapMemberC mm k v

-- ================================================
-- These do not work and return errors.

spec4 :: Specification (Int, Int)
spec4 = constrained $ \ [var|p|] ->
  match p $ \ [var|k|] [var|v|] -> mapMemberBad1 (Lit (Map.toList mm)) k v

-- FIX ME Cycle in Graph
spec5 :: Specification (Int, Int)
spec5 = constrained $ \ [var|p|] ->
  match p $ \ [var|k|] [var|v|] -> mapMemberBad2 mm k v

spec6 :: Specification (Int, Int)
spec6 = constrained' $ \x y ->
  elem_ (pair_ x y) (Lit [(3, 4), (7, 8), (3, 1), (6, 8), (22, 9), (1, 0), (34, 567), (7, 99)])

pp :: Pred
pp = Assert (elem_ (pair_ @Int (Lit 3) (Lit (1 :: Int))) (Lit [(3, 4), (7, 8), (3, 1)]))

bar :: Term a -> Maybe String
bar (FromGeneric (Pair x y)) = Just $ show (x, y)
bar _ = Nothing
