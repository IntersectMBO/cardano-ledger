{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.UMap where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid ()
import Data.Semigroup (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.UMap hiding (Del, Ptr, Rew)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Prelude hiding (lookup)

ee :: UMap Coin Cred Pool Ptr
ee = empty

-- ======================================
-- local types for Testing

newtype Coin = Coin Integer
  deriving (Ord, Eq)
  deriving (Monoid, Semigroup) via (Sum Integer)

instance Show Coin where show (Coin n) = "(Coin " ++ show n ++ ")"

instance (Show coin, Show cred, Show pool, Show ptr) => Show (View coin cred pool ptr x y) where
  show (Rewards x) = "(Rewards (" ++ show x ++ "))"
  show (Delegations x) = "(Delegations (" ++ show x ++ "))"
  show (Ptrs x) = "(Ptrs (" ++ show x ++ "))"

type Cred = Int

type Ptr = Int

type Pool = Char

type UnifiedMap = UMap Coin Cred Pool Ptr

-- ======================================================
-- Generators to make Arbitrary instances for Testing

genPool :: Gen Pool
genPool = elements ['a' .. 'z']

genCred :: Gen Cred
genCred = chooseInt (1, 20)

genPtr :: Gen Ptr
genPtr = chooseInt (1, 25)

genCoin :: Gen Coin
genCoin = (Coin . fromIntegral) <$> chooseInt (0, 100)

instance Arbitrary Coin where arbitrary = genCoin

genAction :: Gen Action
genAction =
  oneof
    [ Insert Rew <$> genCred <*> genCoin,
      Insert Del <$> genCred <*> genPool,
      Insert Ptr <$> genPtr <*> genCred,
      Delete Rew <$> genCred,
      Delete Del <$> genCred,
      Delete Ptr <$> genPtr
    ]

instance Arbitrary Action where
  arbitrary = genAction

-- ======================================================================================
-- First order data to describe actions on UMap, for which we make Arbitrary instances,

data ViewTag k v where
  Rew :: ViewTag Cred Coin
  Del :: ViewTag Cred Pool
  Ptr :: ViewTag Ptr Cred

instance Show (ViewTag k v) where
  show Rew = "Rew"
  show Del = "Del"
  show Ptr = "Ptr"

data Action where
  Insert :: ViewTag k v -> k -> v -> Action
  Delete :: ViewTag k v -> k -> Action

instance Show Action where
  show (Insert Rew k v) = "(Insert Rew " ++ show k ++ " " ++ show v ++ ")"
  show (Insert Del k v) = "(Insert Del " ++ show k ++ " " ++ show v ++ ")"
  show (Insert Ptr k v) = "(Insert Ptr " ++ show k ++ " " ++ show v ++ ")"
  show (Delete Rew k) = "(Delete Rew " ++ show k ++ ")"
  show (Delete Del k) = "(Delete Del " ++ show k ++ ")"
  show (Delete Ptr k) = "(Delete Ptr " ++ show k ++ ")"

-- | Perform the Action on UnifiedMap 'x'
reify :: Action -> UnifiedMap -> UnifiedMap
reify (Insert Rew k v) x = insert k v (Rewards x)
reify (Insert Del k v) x = insert k v (Delegations x)
reify (Insert Ptr k v) x = insert k v (Ptrs x)
reify (Delete Rew k) x = delete k (Rewards x)
reify (Delete Del k) x = delete k (Delegations x)
reify (Delete Ptr k) x = delete k (Ptrs x)

-- Perform applicable actions on Data.Map of the right type.

reifyRew :: Action -> Map Cred Coin -> Map Cred Coin
reifyRew (Insert Rew k v) x = Map.insert k v x
reifyRew (Delete Rew k) x = Map.delete k x
reifyRew _ x = x

reifyDel :: Action -> Map Cred Pool -> Map Cred Pool
reifyDel (Insert Del k v) x = Map.insert k v x
reifyDel (Delete Del k) x = Map.delete k x
reifyDel _ x = x

reifyPtr :: Action -> Map Ptr Cred -> Map Ptr Cred
reifyPtr (Insert Ptr k v) x = Map.insert k v x
reifyPtr (Delete Ptr k) x = Map.delete k x
reifyPtr _ x = x

-- | Sequence all the actions to make a new UnifiedMap
runActions :: [Action] -> UnifiedMap
runActions xs = foldr reify empty xs

runRew :: [Action] -> Map Cred Coin
runRew xs = foldr reifyRew Map.empty xs

runDel :: [Action] -> Map Cred Pool
runDel xs = foldr reifyDel Map.empty xs

runPtr :: [Action] -> Map Ptr Cred
runPtr xs = foldr reifyPtr Map.empty xs

actions :: Gen [Action]
actions = do
  n <- chooseInt (1, 50)
  vectorOf n genAction

act :: IO UnifiedMap
act = runActions <$> generate actions

-- ========================================================

invariant :: Cred -> Ptr -> UnifiedMap -> Bool
invariant = umInvariant

bisimulateRew :: [Action] -> Bool
bisimulateRew acts = (runRew acts) == unUnify (Rewards (runActions acts))

bisimulateDel :: [Action] -> Bool
bisimulateDel acts = (runDel acts) == unUnify (Delegations (runActions acts))

bisimulatePtr :: [Action] -> Bool
bisimulatePtr acts = (runPtr acts) == unUnify (Ptrs (runActions acts))

-- ================

loop :: Ord k => Maybe (k, v, View coin cr pl ptr k v) -> Map k v -> Map k v
loop Nothing ans = ans
loop (Just (k, v, um)) ans = loop (next um) (Map.insert k v ans)

iterRew :: [Action] -> Bool
iterRew acts = runRew acts == loop (next um) Map.empty
  where
    um = Rewards (runActions acts)

iterDel :: [Action] -> Bool
iterDel acts = runDel acts == loop (next um) Map.empty
  where
    um = Delegations (runActions acts)

iterPtr :: [Action] -> Bool
iterPtr acts = runPtr acts == loop (next um) Map.empty
  where
    um = Ptrs (runActions acts)

-- =================

finish ::
  Ord cred =>
  Maybe (a, b, View coin cred pool ptr k v) ->
  Maybe (a, b, Map k v)
finish Nothing = Nothing
finish (Just (k, v, rews)) = Just (k, v, unUnify rews)

lubRew :: [Action] -> Cred -> Bool
lubRew acts k = mapLub k (runRew acts) == finish (leastUpperBound k um)
  where
    um = Rewards (runActions acts)

lubDel :: [Action] -> Cred -> Bool
lubDel acts k = mapLub k (runDel acts) == finish (leastUpperBound k um)
  where
    um = Delegations (runActions acts)

lubPtr :: [Action] -> Ptr -> Bool
lubPtr acts k = mapLub k (runPtr acts) == finish (leastUpperBound k um)
  where
    um = Ptrs (runActions acts)

-- ======================

delRew :: [Action] -> Cred -> Bool
delRew acts k = Map.delete k (runRew acts) == unUnify (delete' k (Rewards (runActions acts)))

delDel :: [Action] -> Cred -> Bool
delDel acts k = Map.delete k (runDel acts) == unUnify (delete' k (Delegations (runActions acts)))

delPtr :: [Action] -> Ptr -> Bool
delPtr acts k = Map.delete k (runPtr acts) == unUnify (delete' k (Ptrs (runActions acts)))

-- =======================

insertRew :: [Action] -> Cred -> Coin -> Bool
insertRew acts k c = Map.insertWith (<>) k c (runRew acts) == unUnify (insertWith' (<>) k c (Rewards (runActions acts)))

insertDel :: [Action] -> Cred -> Pool -> Bool
insertDel acts k p = Map.insertWith comb k p (runDel acts) == unUnify (insertWith' comb2 k p (Delegations (runActions acts)))
  where
    comb new _old = new
    comb2 _old new = new -- Data.Map uses the reverse order than SetAlgebra inserts

insertPtr :: [Action] -> Ptr -> Cred -> Bool
insertPtr acts k p = Map.insertWith comb k p (runPtr acts) == unUnify (insertWith' comb2 k p (Ptrs (runActions acts)))
  where
    comb _new old = old
    comb2 old _new = old -- Data.Map uses the reverse order than SetAlgebra inserts

-- ===========================================

lookRew :: [Action] -> Cred -> Bool
lookRew acts k = Map.lookup k (runRew acts) == (lookup k (Rewards (runActions acts)))

lookDel :: [Action] -> Cred -> Bool
lookDel acts k = Map.lookup k (runDel acts) == (lookup k (Delegations (runActions acts)))

lookPtr :: [Action] -> Ptr -> Bool
lookPtr acts k = Map.lookup k (runPtr acts) == (lookup k (Ptrs (runActions acts)))

-- ====================================

nullRew :: [Action] -> Bool
nullRew acts = Map.null (runRew acts) == (isNull (Rewards (runActions acts)))

nullDel :: [Action] -> Bool
nullDel acts = Map.null (runDel acts) == (isNull (Delegations (runActions acts)))

nullPtr :: [Action] -> Bool
nullPtr acts = Map.null (runPtr acts) == (isNull (Ptrs (runActions acts)))

-- ====================================

domRew :: [Action] -> Bool
domRew acts = Map.keysSet (runRew acts) == (domain (Rewards (runActions acts)))

domDel :: [Action] -> Bool
domDel acts = Map.keysSet (runDel acts) == (domain (Delegations (runActions acts)))

domPtr :: [Action] -> Bool
domPtr acts = Map.keysSet (runPtr acts) == (domain (Ptrs (runActions acts)))

-- ========================

rngRew :: [Action] -> Bool
rngRew acts = Set.fromList (Map.elems (runRew acts)) == (range (Rewards (runActions acts)))

rngDel :: [Action] -> Bool
rngDel acts = Set.fromList (Map.elems (runDel acts)) == (range (Delegations (runActions acts)))

rngPtr :: [Action] -> Bool
rngPtr acts = Set.fromList (Map.elems (runPtr acts)) == (range (Ptrs (runActions acts)))

-- ==========================================
uLeftRew :: [Action] -> Cred -> Coin -> Bool
uLeftRew acts k v =
  Map.unionWith (\l _r -> l) (runRew acts) (Map.singleton k v)
    == unUnify (Rewards (Rewards (runActions acts) ∪ (k, v)))

uLeftDel :: [Action] -> Cred -> Pool -> Bool
uLeftDel acts k v =
  Map.unionWith (\l _r -> l) (runDel acts) (Map.singleton k v)
    == unUnify (Delegations (Delegations (runActions acts) ∪ (k, v)))

uLeftPtr :: [Action] -> Ptr -> Cred -> Bool
uLeftPtr acts k v =
  Map.unionWith (\l _r -> l) (runPtr acts) (Map.singleton k v)
    == unUnify (Ptrs (Ptrs (runActions acts) ∪ (k, v)))

uRightRew :: [Action] -> Map Cred Coin -> Bool
uRightRew acts m =
  Map.unionWith (\_l r -> r) (runRew acts) m
    == unUnify (Rewards (Rewards (runActions acts) ⨃ m))

uRightDel :: [Action] -> Map Cred Pool -> Bool
uRightDel acts m =
  Map.unionWith (\_l r -> r) (runDel acts) m
    == unUnify (Delegations (Delegations (runActions acts) ⨃ m))

uRightPtr :: [Action] -> Map Ptr Cred -> Bool
uRightPtr acts m =
  Map.unionWith (\_l r -> r) (runPtr acts) m
    == unUnify (Ptrs (Ptrs (runActions acts) ⨃ m))

uSumRew :: [Action] -> Map Cred Coin -> Bool
uSumRew acts m =
  Map.unionWith (<>) (runRew acts) m
    == unUnify (Rewards (Rewards (runActions acts) ∪+ m))

-- ======================

domExRew :: [Action] -> Set Cred -> Bool
domExRew acts dom = Map.withoutKeys (runRew acts) dom == unUnify (Rewards (dom ⋪ (Rewards (runActions acts))))

domExDel :: [Action] -> Set Cred -> Bool
domExDel acts dom = Map.withoutKeys (runDel acts) dom == unUnify (Delegations (dom ⋪ (Delegations (runActions acts))))

domExPtr :: [Action] -> Set Ptr -> Bool
domExPtr acts dom = Map.withoutKeys (runPtr acts) dom == unUnify (Ptrs (dom ⋪ (Ptrs (runActions acts))))

-- ============================

rngExRew :: [Action] -> Set Coin -> Bool
rngExRew acts rng = Map.filter ok (runRew acts) == unUnify (Rewards ((Rewards (runActions acts)) ⋫ rng))
  where
    ok x = not (Set.member x rng)

rngExDel :: [Action] -> Set Pool -> Bool
rngExDel acts rng = Map.filter ok (runDel acts) == unUnify (Delegations (Delegations (runActions acts) ⋫ rng))
  where
    ok x = not (Set.member x rng)

rngExPtr :: [Action] -> Set Cred -> Bool
rngExPtr acts rng = Map.filter ok (runPtr acts) == unUnify (Ptrs ((Ptrs (runActions acts)) ⋫ rng))
  where
    ok x = not (Set.member x rng)

-- =================================

memberRew :: [Action] -> Cred -> Bool
memberRew acts k = Map.member k (runRew acts) == (member k (Rewards (runActions acts)))

memberDel :: [Action] -> Cred -> Bool
memberDel acts k = Map.member k (runDel acts) == (member k (Delegations (runActions acts)))

memberPtr :: [Action] -> Ptr -> Bool
memberPtr acts k = Map.member k (runPtr acts) == (member k (Ptrs (runActions acts)))

-- ==========================================

domRestrictRew :: [Action] -> Map Cred Coin -> Bool
domRestrictRew acts m =
  Map.intersection m (runRew acts)
    == (domRestrict (Rewards (runActions acts)) m)

domRestrictDel :: [Action] -> Map Cred Pool -> Bool
domRestrictDel acts m =
  Map.intersection m (runDel acts)
    == (domRestrict (Delegations (runActions acts)) m)

domRestrictPtr :: [Action] -> Map Ptr Cred -> Bool
domRestrictPtr acts m =
  Map.intersection m (runPtr acts)
    == ( domRestrict
           ( Ptrs
               (runActions acts)
           )
           m
       )

-- ===============================

unifyRoundTrip1 :: Map Cred Coin -> Map Cred Pool -> Map Ptr Cred -> Bool
unifyRoundTrip1 x y z = (x == x') && (y == y') && (z == z')
  where
    umap = unify x y z
    x' = unUnify (Rewards umap)
    y' = unUnify (Delegations umap)
    z' = unUnify (Ptrs umap)

unifyRoundTrip2 :: [Action] -> Bool
unifyRoundTrip2 acts = umap1 == umap2
  where
    umap1 = runActions acts
    x' = unUnify (Rewards umap1)
    y' = unUnify (Delegations umap1)
    z' = unUnify (Ptrs umap1)
    umap2 = unify x' y' z'

sizeUMap :: Map Cred Coin -> Map Cred Pool -> Map Ptr Cred -> Bool
sizeUMap x y z = (Map.size x == x') && (Map.size y == y') && (Map.size z == z')
  where
    umap = unify x y z
    x' = size (Rewards umap)
    y' = size (Delegations umap)
    z' = size (Ptrs umap)

-- ==============================================================

testPropertyN :: Testable prop => Int -> TestName -> prop -> TestTree
testPropertyN n name prop = testProperty name (withMaxSuccess n prop)

alltests :: TestTree
alltests =
  testGroup
    "UMap tests"
    [ testProperty "invariant empty" (\x y -> invariant x y empty),
      testPropertyN 1000 "invariant Actions" (\x y xs -> invariant x y (runActions xs)),
      testPropertyN 1000 "bisimulation Reward map" bisimulateRew,
      testPropertyN 1000 "bisimulation Delegation map" bisimulateDel,
      testPropertyN 1000 "bisimulation Ptr map" bisimulatePtr,
      testPropertyN 1000 "next iterates over Rewards" iterRew,
      testPropertyN 1000 "next iterates over Delegtions" iterDel,
      testPropertyN 1000 "next iterates over Ptrs" iterPtr,
      testPropertyN 1000 "leastUpperBound Rewards" lubRew,
      testPropertyN 1000 "leastUpperBound Delegations" lubDel,
      testPropertyN 1000 "leastUpperBound Ptrs" lubPtr,
      testPropertyN 1000 "delete Rewards" delRew,
      testPropertyN 1000 "delete Delegations" delDel,
      testPropertyN 1000 "delete Ptrs" delPtr,
      testPropertyN 1000 "insertWith (<>) Rewards" insertRew,
      testPropertyN 1000 "insertWith (\\ old new -> new) Delegations" insertDel,
      testPropertyN 1000 "insertWith (\\ old new -> old) Ptrs" insertPtr,
      testPropertyN 1000 "lookup Rewards" lookRew,
      testPropertyN 1000 "lookup Delegations" lookDel,
      testPropertyN 1000 "lookup Ptrs" lookPtr,
      testPropertyN 1000 "null Rewards" nullRew,
      testPropertyN 1000 "null Delegations" nullDel,
      testPropertyN 1000 "null Ptrs" nullPtr,
      testPropertyN 1000 "domain  Rewards" domRew,
      testPropertyN 1000 "domain Delegations" domDel,
      testPropertyN 1000 "domain Ptrs" domPtr,
      testPropertyN 1000 "range  Rewards" rngRew,
      testPropertyN 1000 "range Delegations" rngDel,
      testPropertyN 1000 "range Ptrs" rngPtr,
      testPropertyN 1000 "left biased (x  ∪ (k,v)) Rewards" uLeftRew,
      testPropertyN 1000 "left biased (x  ∪ (k,v)) Delegations" uLeftDel,
      testPropertyN 1000 "left biased (x  ∪ (k,v)) Ptrs" uLeftPtr,
      testPropertyN 1000 "right biased (x ⨃ y) Rewards" uRightRew,
      testPropertyN 1000 "right biased (x ⨃ y) Delegations" uRightDel,
      testPropertyN 1000 "right biased (x ⨃ y) Ptrs" uRightPtr,
      testPropertyN 1000 "monoidal sum (x  ∪+ y) Ptrs" uSumRew,
      testPropertyN 1000 "domain exclude (x ⋪ y) Rewards" domExRew,
      testPropertyN 1000 "domain exclude (x ⋪ y) Delegations" domExDel,
      testPropertyN 1000 "domain exclude (x ⋪ y) Ptrs" domExPtr,
      testPropertyN 1000 "range exclude (x  ⋫ y) Rewards" rngExRew,
      testPropertyN 1000 "range exclude (x  ⋫ y) Delegations" rngExDel,
      testPropertyN 1000 "range exclude (x  ⋫ y) Ptrs" rngExPtr,
      testPropertyN 1000 "member Rewards" memberRew,
      testPropertyN 1000 "member Delegations" memberDel,
      testPropertyN 1000 "member Ptrs" memberPtr,
      testPropertyN 1000 "domain restrict Rewards" domRestrictRew,
      testPropertyN 1000 "domain restrict Delegations" domRestrictDel,
      testPropertyN 1000 "domain restrict Ptrs" domRestrictPtr,
      testPropertyN 1000 "unify unUnify roundtrip" unifyRoundTrip1,
      testPropertyN 1000 "unUnify unify roundtrip" unifyRoundTrip2,
      testPropertyN 1000 "size is accurate" sizeUMap
    ]
