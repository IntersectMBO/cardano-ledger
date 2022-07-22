{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Byron.Spec.Ledger.Relation.Properties (testRelation) where

import Byron.Spec.Ledger.Core hiding ((<|))
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Map.Strict (Map)
import Data.Set (Set, union, (\\))
import Hedgehog (Gen, MonadTest, Property, PropertyT, forAll, property, withTests, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog

--------------------------------------------------------------------------------
-- Properties on Relations
--------------------------------------------------------------------------------

-- | (dom r) ∩ s == dom (s ◁ r)
--
--  By restricting the domain of 'Relation r' to 's', the new 'Domain r'
--  is an intersection of the original 'Domain r' with 's'
propDomainRestrictionAndIntersection ::
  (MonadTest m, Relation r, Ord (Domain r), Show (Domain r)) =>
  Set (Domain r) ->
  r ->
  m ()
propDomainRestrictionAndIntersection s r =
  dom r ∩ s === dom (s ◁ r)

-- | (dom r ∩ s) ◁ r == s ◁ r
--
--  Restricting the domain of 'Relation r' to 's'
--  is the same as restricting the domain to 'dom r ∩ s'.
propDomainRestrictionAndIntersectionB ::
  (MonadTest m, Relation r, Eq r, Show r, Ord (Domain r)) =>
  Set (Domain r) ->
  r ->
  m ()
propDomainRestrictionAndIntersectionB s r =
  (dom r ∩ s) ◁ r === s ◁ r

-- | r ▷ (range r ∩ s) == r ▷ s
--
--  By restricting the range of 'Relation r' to 's', the new 'Range r'
--  is an intersection of the original 'Range r' with 's'
propRangeRestrictionAndIntersection ::
  (MonadTest m, Relation r, Eq r, Show r, Ord (Range r)) =>
  Set (Range r) ->
  r ->
  m ()
propRangeRestrictionAndIntersection s r =
  r ▷ (range r ∩ s) === r ▷ s

-- | (range r) ∩ s == range (r ▷ s)
--
--  By restricting the range of 'Relation r' to 's', the new 'Range r'
--  is an intersection of the original 'Range r' with 's'
propRangeRestrictionAndIntersectionB ::
  (MonadTest m, Relation r, Ord (Range r), Show (Range r)) =>
  Set (Range r) ->
  r ->
  m ()
propRangeRestrictionAndIntersectionB s r =
  (range r) ∩ s === range (r ▷ s)

-- | (dom r \\ s) ◁ r == s ⋪ r
--
--  Excluding a set 's' from the domain of 'Relation r'
--  is the same as restricting the domain to 'dom r \\ s'.
propDomainExclusionAndSetDifference ::
  (MonadTest m, Relation r, Eq r, Show r, Ord (Domain r)) =>
  Set (Domain r) ->
  r ->
  m ()
propDomainExclusionAndSetDifference s r =
  (dom r \\ s) ◁ r === s ⋪ r

-- | (dom r1 ∪ s) ⋪ (r1 ∪ r2) == (dom r1 ∪ s) ⋪ r2
--
--  Excluding 'dom r1 ∪ s' from the domain of 'r1 ∪ r2'
--  is the same as excluding 'dom r1 ∪ s' from the domain of just r2
--  (since 'dom r1 ⋪ r1' gives an empty relation)
propDomainExclusionAndUnion ::
  ( MonadTest m,
    Relation r,
    Eq r,
    Show r,
    Ord (Domain r),
    Ord (Range r)
  ) =>
  Set (Domain r) ->
  r ->
  r ->
  m ()
propDomainExclusionAndUnion s r1 r2 =
  (dom r1 `union` s) ⋪ (r1 ∪ r2)
    === (dom r1 `union` s) ⋪ r2

--------------------------------------------------------------------------------
-- Property helpers
--------------------------------------------------------------------------------

propRelation ::
  (Show r, Show (Domain r)) =>
  Gen (Set (Domain r)) ->
  Gen r ->
  (Set (Domain r) -> r -> PropertyT IO ()) ->
  Property
propRelation genS genR prop =
  withTests 500 $
    property $ do
      (s, r) <- (,) <$> forAll genS <*> forAll genR
      prop s r

propRelations ::
  (Show r, Show (Domain r)) =>
  Gen (Set (Domain r)) ->
  Gen r ->
  (Set (Domain r) -> r -> r -> PropertyT IO ()) ->
  Property
propRelations genS genR prop =
  withTests 500 $
    property $ do
      (s, r1, r2) <- (,,) <$> forAll genS <*> forAll genR <*> forAll genR
      prop s r1 r2

genInt :: Gen Int
genInt = Gen.integral (Range.constant 0 100)

genSetOf :: Ord a => Gen a -> Gen (Set a)
genSetOf genA = Gen.set aRange genA

aRange :: Range.Range Int
aRange = Range.constant 0 50

genIntS :: Gen (Set Int)
genIntS = genSetOf genInt

genMap :: Gen (Map Int Int)
genMap = Gen.map aRange $ (,) <$> genInt <*> genInt

genSet :: Gen (Set (Int, Int))
genSet = genSetOf ((,) <$> genInt <*> genInt)

genPairsList :: Gen [(Int, Int)]
genPairsList = Gen.list aRange ((,) <$> genInt <*> genInt)

genBimap :: Gen (Bimap Int Int)
genBimap = Bimap.fromList <$> genPairsList

--------------------------------------------------------------------------------
-- Property Tests
--------------------------------------------------------------------------------

testRelation :: TestTree
testRelation =
  testGroup
    "Test Relation instances"
    [ testGroup
        "Relation - Set"
        [ testPropertyNamed
            "DomainRestrictionAndIntersection"
            "domain-restriction-and-intersection"
            (propRelation genIntS genSet propDomainRestrictionAndIntersection),
          testPropertyNamed
            "DomainRestrictionAndIntersectionB"
            "domain-restriction-and-intersection-b"
            (propRelation genIntS genSet propDomainRestrictionAndIntersectionB),
          testPropertyNamed
            "DomainExclusionAndSetDifference"
            "domain-exclusion-and-set-difference"
            (propRelation genIntS genSet propDomainExclusionAndSetDifference),
          testPropertyNamed
            "RangeRestrictionAndIntersection"
            "range-restriction-and-intersection"
            (propRelation genIntS genSet propRangeRestrictionAndIntersection),
          testPropertyNamed
            "RangeRestrictionAndIntersectionB"
            "range-restriction-and-intersection-b"
            (propRelation genIntS genSet propRangeRestrictionAndIntersectionB)
        ],
      testGroup
        "Relation - Map"
        [ testPropertyNamed
            "DomainRestrictionAndIntersection"
            "domain-restriction-and-intersection"
            (propRelation genIntS genMap propDomainRestrictionAndIntersection),
          testPropertyNamed
            "DomainRestrictionAndIntersectionB"
            "domain-restriction-and-intersection-b"
            (propRelation genIntS genMap propDomainRestrictionAndIntersectionB),
          testPropertyNamed
            "DomainExclusionAndSetDifference"
            "domain-exclusion-and-set-difference"
            (propRelation genIntS genMap propDomainExclusionAndSetDifference),
          testPropertyNamed
            "RangeRestrictionAndIntersection"
            "range-restriction-and-intersection"
            (propRelation genIntS genMap propRangeRestrictionAndIntersection),
          testPropertyNamed
            "RangeRestrictionAndIntersectionB"
            "range-restriction-and-intersection-b"
            (propRelation genIntS genMap propRangeRestrictionAndIntersectionB)
        ],
      testGroup
        "Relation - Bimap"
        [ testPropertyNamed
            "DomainRestrictionAndIntersection"
            "domain-restriction-and-intersection"
            (propRelation genIntS genBimap propDomainRestrictionAndIntersection),
          testPropertyNamed
            "DomainRestrictionAndIntersectionB"
            "domain-restriction-and-intersection-b"
            (propRelation genIntS genBimap propDomainRestrictionAndIntersectionB),
          testPropertyNamed
            "DomainExclusionAndSetDifference"
            "domain-exclusion-and-set-difference"
            (propRelation genIntS genBimap propDomainExclusionAndSetDifference),
          testPropertyNamed
            "RangeRestrictionAndIntersection"
            "range-restriction-and-intersection"
            (propRelation genIntS genBimap propRangeRestrictionAndIntersection),
          testPropertyNamed
            "RangeRestrictionAndIntersectionB"
            "range-restriction-and-intersection-b"
            (propRelation genIntS genBimap propRangeRestrictionAndIntersectionB)
        ],
      testGroup
        "Relation - Pairs list"
        [ testPropertyNamed
            "DomainRestrictionAndIntersection"
            "domain-restriction-and-intersection"
            (propRelation genIntS genPairsList propDomainRestrictionAndIntersection),
          testPropertyNamed
            "DomainRestrictionAndIntersectionB"
            "domain-restriction-and-intersection-b"
            (propRelation genIntS genPairsList propDomainRestrictionAndIntersectionB),
          testPropertyNamed
            "DomainExclusionAndSetDifference"
            "domain-exclusion-and-set-difference"
            (propRelation genIntS genPairsList propDomainExclusionAndSetDifference),
          testPropertyNamed
            "RangeRestrictionAndIntersection"
            "range-restriction-and-intersection"
            (propRelation genIntS genPairsList propRangeRestrictionAndIntersection),
          testPropertyNamed
            "RangeRestrictionAndIntersectionB"
            "range-restriction-and-intersection-b"
            (propRelation genIntS genPairsList propRangeRestrictionAndIntersectionB)
        ],
      testGroup
        "Relations"
        [ testPropertyNamed
            "Set instance"
            "set-instance"
            (propRelations genIntS genSet propDomainExclusionAndUnion),
          testPropertyNamed
            "Map instance"
            "map-instance"
            (propRelations genIntS genMap propDomainExclusionAndUnion),
          testPropertyNamed
            "Bimap instance"
            "bimap-instance"
            (propRelations genIntS genBimap propDomainExclusionAndUnion)
        ]
    ]
