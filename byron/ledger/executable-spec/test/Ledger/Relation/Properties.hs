{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ledger.Relation.Properties
    (testRelation)
  where

import           Data.Bimap          (Bimap)
import qualified Data.Bimap          as Bimap
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set

import           Hedgehog            (Gen, MonadTest, Property, PropertyT,
                                      classify, forAll, property, withTests,
                                      (===))

import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

import           Test.Tasty.Hedgehog

import           Ledger.Core         hiding ((<|))

import           Test.Tasty          (TestTree, testGroup)

--------------------------------------------------------------------------------
-- Properties on Relations
--------------------------------------------------------------------------------

-- | (dom r) ∩ s == dom (s ◁ r)
--
--  By restricting the domain of 'Relation r' to 's', the new 'Domain r'
--  is an intersection of the original 'Domain r' with 's'
propDomainRestrictionAndIntersection
  :: (MonadTest m, Relation r, Ord (Domain r))
  => Domain r -> Set (Domain r) -> r -> m ()
propDomainRestrictionAndIntersection x s r
  = Set.member x (dom r `Set.intersection` s)
    ===
    Set.member x (dom (s ◁ r))

-- | (dom r ∩ s) ◁ r == s ◁ r
--
--  Restricting the domain of 'Relation r' to 's'
--  is the same as restricting  the domain to 'dom r ∩ s'.
propDomainRestrictionAndIntersectionB
  :: (MonadTest m, Relation r, Ord (Domain r), Eq (Range r), Show (Range r))
  => (Domain r -> r -> Maybe (Range r))
  -> Domain r
  -> Set (Domain r)
  -> r
  -> m ()
propDomainRestrictionAndIntersectionB find x s r
  = find x ((dom r `Set.intersection` s) ◁ r)
    ===
    find x (s ◁ r)

-- | (dom r \\ s) ◁ r == s ⋪ r
--
--  Excluding a from the domain of 'Relation r'
--  is the same as restricting the domain to 'dom r \\ s'.
propDomainExclusionAndSetDifference
  :: (MonadTest m, Relation r, Ord (Domain r), Eq (Range r), Show (Range r))
  => (Domain r -> r -> Maybe (Range r))
  -> Domain r
  -> Set (Domain r)
  -> r
  -> m ()
propDomainExclusionAndSetDifference find x s r
  = find x ((dom r `Set.difference` s) ◁ r)
    ===
    find x (s ⋪ r)

-- | (dom r1 ∪ s) ⋪ (r1 ∪ r2) == (dom r1 ∪ s) ⋪ r2
--
--  Excluding 'dom r1 ∪ s' from the domain of 'r1 ∪ r2'
--  is the same as excluding 'dom r1 ∪ s' from the domain of just r2
--  (since 'dom r1 ⋪ r1' gives an empty relation)
propDomainExclusionAndUnion
  :: (MonadTest m, Relation r, Ord (Domain r), Ord (Range r), Show (Range r))
  => (Domain r -> r -> Maybe (Range r))
  -> Domain r
  -> Set (Domain r)
  -> r
  -> r
  -> m ()
propDomainExclusionAndUnion find x s r1 r2
  = find x ((dom r1 `Set.union` s) ⋪ (r1 ∪ r2))
    ===
    find x ((dom r1 `Set.union` s) ⋪ r2)


-- | s ◁ (r1 ∪ r2) == (s ◁ r1) ∪ (s ◁ r2)
--
-- Restricting the domain of 'r1 ∪ r2' to 's' is the same as restricting the
-- domains of r1 and r2 seperately and then taking the relation union of that.
{- TODO @uroboros keep this?
propDomainRestrictionAndUnion
  :: (MonadTest m, Relation r, Ord (Domain r), Ord (Range r), Show (Range r))
  => (Domain r -> r -> Maybe (Range r))
  -> Domain r
  -> Set (Domain r)
  -> r
  -> r
  -> m ()
propDomainRestrictionAndUnion find x s r1 r2
  = find x (s ◁ (r1 ∪ r2))
    ===
    find x ((s ◁ r1) ∪ (s ◁ r2))
-}

--------------------------------------------------------------------------------
-- Functions abstracted over 'Relation r'
--------------------------------------------------------------------------------

-- | Combined property abstracted over 'Relation r'
propRelation_
  :: ( Relation r, Ord (Domain r), Ord (Range r)
     , Show r, Show (Domain r), Show (Range r))
  => Gen r
  -> Gen [Domain r]
  -> (Domain r -> r -> Maybe (Range r))
  -> PropertyT IO ()
propRelation_ genRelation_ genList_ find_
  = do
      r1 <- forAll $ genRelation_
      s <- forAll $ Set.fromList <$> genList_
      -- pick an element from 's ∪ dom r'
      x <- forAll $ Gen.element (Set.toList $ s `Set.union` dom r1)

      classify "(dom r) ∩ s == dom (s ◁ r)" $
               Set.member x (dom r1 `Set.intersection` s)
      propDomainRestrictionAndIntersection x s r1

      classify "(dom r ∩ s) ◁ r == s ◁ r" $
               Set.member x (dom ((dom r1 `Set.intersection` s) ◁ r1))
      propDomainRestrictionAndIntersectionB find_ x s r1

      classify "(dom r \\ s) ◁ r == s ⋪ r" $
               Set.member x (dom ((dom r1 `Set.difference` s) ◁ r1))
      propDomainExclusionAndSetDifference find_ x s r1

      r2 <- forAll $ genRelation_
      -- pick an element from 's ∪ dom r1 ∪ dom r2'
      x' <- forAll $ Gen.element (Set.toList $ s `Set.union` dom r1 `Set.union` dom r2)

      classify "(dom r1 ∪ s) ⋪ (r1 ∪ r2) == (dom r1 ∪ s) ⋪ r2" $
               Set.member x' (dom ((dom r1 `Set.union` s) ⋪ (r1 ∪ r2)))
      propDomainExclusionAndUnion find_ x' s r1 r2

      {- TODO @uroboros fails for Bimap, but could make it pass with
         more a more restrictive Bimap generator.

      classify "s ◁ (r1 ∪ r2) == (s ◁ r1) ∪ (s ◁ r2)" $
                Set.member x' (dom (s ◁ (r1 ∪ r2)))
      propDomainRestrictionAndUnion find_ x' s r1 r2
      -}

genList :: Gen [Int]
genList = Gen.list (Range.constant 5 100)
                   (Gen.integral (Range.constant 0 50))

genTuples :: Gen [(Int, Int)]
genTuples = zip <$> genList <*> genList

--------------------------------------------------------------------------------
-- Test 'instance Relation Map'
--------------------------------------------------------------------------------

genMap :: Gen (Map Int Int)
genMap = Map.fromList <$> genTuples

propRelationMap :: Property
propRelationMap
  = withTests 500 $ property $ propRelation_ genMap genList find
    where
      find :: Int -> Map Int Int -> Maybe Int
      find = Map.lookup

--------------------------------------------------------------------------------
-- Test 'instance Relation Set'
--------------------------------------------------------------------------------

genSet :: Gen (Set (Int, Int))
genSet = Set.fromList <$> genTuples

propRelationSet :: Property
propRelationSet
  = withTests 500 $ property $ propRelation_ genSet genList find
    where
      find :: Int -> Set (Int,Int) -> Maybe Int
      find x r = case Set.toList (Set.filter (\(a,_) -> a == x) r) of
                  []        -> Nothing
                  ((_,v):_) -> Just v

--------------------------------------------------------------------------------
-- Test 'instance Relation Bimap'
--------------------------------------------------------------------------------

genBimap :: Gen (Bimap Int Int)
genBimap = Bimap.fromList <$> genTuples

propRelationBimap :: Property
propRelationBimap
  = withTests 500 $ property $ propRelation_ genBimap genList find
    where
      find :: Int -> Bimap Int Int -> Maybe Int
      find = Bimap.lookup

--------------------------------------------------------------------------------
-- Test 'instance Relation *'
--------------------------------------------------------------------------------

testRelation :: TestTree
testRelation = testGroup "Test Relation instances"
  [ testProperty "Relation - Set instance" propRelationSet
  , testProperty "Relation - Map instance" propRelationMap
  , testProperty "Relation - Bimap instance" propRelationBimap
  ]
