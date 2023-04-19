{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.UMapSpec where

import Cardano.Ledger.Credential (Credential, Ptr)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.UMap (
  RDPair (RDPair, rdReward),
  UMap,
  View (Delegations, Ptrs, RewardDeposits),
  compactRewView,
  delView,
  delete,
  delete',
  domRestrict,
  domain,
  empty,
  insert,
  insert',
  isNull,
  member,
  ptrView,
  range,
  rdPairView,
  size,
  umInvariant,
  unUnify,
  unView,
  unify,
  (∪),
  (∪+),
  (⋪),
  (⋫),
  (⨃),
 )
import qualified Cardano.Ledger.UMap as UMap (lookup)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary (
  genInsertDeleteRoundtripDelegation,
  genInsertDeleteRoundtripPtr,
  genInsertDeleteRoundtripRDPair,
  genInvariantNonEmpty,
  genRightPreferenceUMap,
  genValidTriples,
 )

data Action
  = InsertRDPair (Credential 'Staking StandardCrypto) RDPair
  | InsertDelegation (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto)
  | InsertPtr Ptr (Credential 'Staking StandardCrypto)
  | DeleteRDPair (Credential 'Staking StandardCrypto)
  | DeleteDelegation (Credential 'Staking StandardCrypto)
  | DeletePtr Ptr
  deriving (Show)

instance Arbitrary Action where
  arbitrary =
    oneof
      [ InsertRDPair <$> arbitrary <*> arbitrary
      , InsertDelegation <$> arbitrary <*> arbitrary
      , InsertPtr <$> arbitrary <*> arbitrary
      , DeleteRDPair <$> arbitrary
      , DeleteDelegation <$> arbitrary
      , DeletePtr <$> arbitrary
      ]

genRDPair :: Gen Action
genRDPair = InsertRDPair <$> arbitrary <*> arbitrary

genDelegation :: Gen Action
genDelegation = InsertDelegation <$> arbitrary <*> arbitrary

genPtr :: Gen Action
genPtr = InsertPtr <$> arbitrary <*> arbitrary

reify :: Action -> UMap StandardCrypto -> UMap StandardCrypto
reify = \case
  InsertRDPair k v -> insert k v . RewardDeposits
  InsertDelegation k v -> insert k v . Delegations
  InsertPtr k v -> insert k v . Ptrs
  DeleteRDPair k -> delete k . RewardDeposits
  DeleteDelegation k -> delete k . Delegations
  DeletePtr k -> delete k . Ptrs

reifyRDPair :: Action -> UMap StandardCrypto -> UMap StandardCrypto
reifyRDPair = \case
  InsertRDPair k v -> insert k v . RewardDeposits
  DeleteRDPair k -> delete k . RewardDeposits
  _ -> id

reifyDelegation :: Action -> UMap StandardCrypto -> UMap StandardCrypto
reifyDelegation = \case
  InsertDelegation k v -> insert k v . Delegations
  DeleteDelegation k -> delete k . Delegations
  _ -> id

reifyPtr :: Action -> UMap StandardCrypto -> UMap StandardCrypto
reifyPtr = \case
  InsertPtr k v -> insert k v . Ptrs
  DeletePtr k -> delete k . Ptrs
  _ -> id

runActions :: [Action] -> UMap StandardCrypto -> UMap StandardCrypto
runActions actions umap = foldr reify umap actions

runRDPairs :: [Action] -> UMap StandardCrypto -> UMap StandardCrypto
runRDPairs actions umap = foldr reifyRDPair umap actions

runDelegations :: [Action] -> UMap StandardCrypto -> UMap StandardCrypto
runDelegations actions umap = foldr reifyDelegation umap actions

runPtrs :: [Action] -> UMap StandardCrypto -> UMap StandardCrypto
runPtrs actions umap = foldr reifyPtr umap actions

sizeTest ::
  ( Map.Map (Credential 'Staking StandardCrypto) RDPair
  , Map.Map (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto)
  , Map.Map Ptr (Credential 'Staking StandardCrypto)
  ) ->
  IO ()
sizeTest (rdPairs, delegs, ptrs) = do
  let
    umap = unify rdPairs delegs ptrs
    rdPairsSize = size (RewardDeposits umap)
    delegsSize = size (Delegations umap)
    ptrsSize = size (Ptrs umap)
  Map.size rdPairs `shouldBe` rdPairsSize
  Map.size delegs `shouldBe` delegsSize
  Map.size ptrs `shouldBe` ptrsSize

unifyRoundTripTo ::
  ( Map.Map (Credential 'Staking StandardCrypto) RDPair
  , Map.Map (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto)
  , Map.Map Ptr (Credential 'Staking StandardCrypto)
  ) ->
  IO ()
unifyRoundTripTo (rdPairs, delegs, ptrs) = do
  let umap = unify rdPairs delegs ptrs
  rdPairView umap `shouldBe` rdPairs
  delView umap `shouldBe` delegs
  ptrView umap `shouldBe` ptrs

unifyRoundTripFrom :: [Action] -> Property
unifyRoundTripFrom actions =
  let
    umap = runActions actions empty
    rdPairs = rdPairView umap
    delegs = delView umap
    ptrs = ptrView umap
   in
    umap === unify rdPairs delegs ptrs

spec :: Spec
spec = do
  describe "UMap" $ do
    describe "Invariant" $ do
      prop "Empty" (\(cred :: Credential 'Staking StandardCrypto) ptr -> umInvariant cred ptr empty)
      prop "Non-empty" $
        forAll
          genInvariantNonEmpty
          (\(cred, ptr, umap) -> umInvariant cred ptr umap)
      prop "Non-empty with insert and delete actions" $
        forAll
          ((,) <$> genInvariantNonEmpty <*> arbitrary)
          (\((cred, ptr, umap), actions) -> umInvariant cred ptr $ runActions actions umap)
    describe "Unify roundtrip" $ do
      prop "To" $ forAll genValidTriples unifyRoundTripTo
      prop "From" unifyRoundTripFrom
    describe "Insert-delete roundtrip" $ do
      prop "RDPair" $
        forAll
          genInsertDeleteRoundtripRDPair
          (\(umap, k, v) -> umap === unView (delete' k (insert' k v (RewardDeposits umap))))
      prop "Delegations" $
        forAll
          genInsertDeleteRoundtripDelegation
          (\(umap, k, v) -> umap === unView (delete' k (insert' k v (Delegations umap))))
      prop "Ptrs" $
        forAll
          genInsertDeleteRoundtripPtr
          (\(umap, k, v) -> umap === unView (delete' k (insert' k v (Ptrs umap))))
    prop "Size" $ forAll genValidTriples sizeTest
    describe "Membership" $ do
      prop
        "RewardDeposits"
        ( \(umap :: UMap StandardCrypto, cred) ->
            member cred (RewardDeposits umap) === Map.member cred (rdPairView umap)
        )
      prop
        "Delegations"
        ( \(umap :: UMap StandardCrypto, cred) ->
            member cred (Delegations umap) === Map.member cred (delView umap)
        )
      prop
        "Ptrs"
        ( \(umap :: UMap StandardCrypto, ptr) ->
            member ptr (Ptrs umap) === Map.member ptr (ptrView umap)
        )
    describe "Bisimulation" $ do
      prop
        "RewardDeposits"
        ( \actions ->
            unUnify (RewardDeposits (runRDPairs actions empty))
              === unUnify (RewardDeposits (runActions actions empty))
        )
      prop
        "Delegations"
        ( \actions ->
            delView (runDelegations actions empty)
              === delView (runActions actions empty)
        )
      prop
        "Ptrs"
        ( \actions ->
            ptrView (runPtrs actions empty)
              === ptrView (runActions actions empty)
        )
    describe "Null" $ do
      prop
        "RewardDeposits"
        ( \actions ->
            Map.null (rdPairView (runRDPairs actions empty))
              === isNull (RewardDeposits $ runActions actions empty)
        )
      prop
        "Delegations"
        ( \actions ->
            Map.null (delView (runDelegations actions empty))
              === isNull (Delegations $ runActions actions empty)
        )
      prop
        "Ptrs"
        ( \actions ->
            Map.null (ptrView (runPtrs actions empty))
              === isNull (Ptrs $ runActions actions empty)
        )
    describe "Lookup" $ do
      prop
        "RewardDeposits"
        ( \actions cred ->
            Map.lookup cred (rdPairView (runRDPairs actions empty))
              === UMap.lookup cred (RewardDeposits $ runActions actions empty)
        )
      prop
        "Delegations"
        ( \actions cred ->
            Map.lookup cred (delView (runDelegations actions empty))
              === UMap.lookup cred (Delegations $ runActions actions empty)
        )
      prop
        "Ptrs"
        ( \actions ptr ->
            Map.lookup ptr (ptrView (runPtrs actions empty))
              === UMap.lookup ptr (Ptrs $ runActions actions empty)
        )
    describe "Domain" $ do
      prop
        "RewardDeposits"
        ( \actions ->
            Map.keysSet (rdPairView (runRDPairs actions empty))
              === domain (RewardDeposits $ runActions actions empty)
        )
      prop
        "Delegations"
        ( \actions ->
            Map.keysSet (delView (runDelegations actions empty))
              === domain (Delegations $ runActions actions empty)
        )
      prop
        "Ptrs"
        ( \actions ->
            Map.keysSet (ptrView (runPtrs actions empty))
              === domain (Ptrs $ runActions actions empty)
        )
    describe "Range" $ do
      prop
        "RewardDeposits"
        ( \actions ->
            Set.fromList (Map.elems (rdPairView (runRDPairs actions empty)))
              === range (RewardDeposits $ runActions actions empty)
        )
      prop
        "Delegations"
        ( \actions ->
            Set.fromList (Map.elems (delView (runDelegations actions empty)))
              === range (Delegations $ runActions actions empty)
        )
      prop
        "Ptrs"
        ( \actions ->
            Set.fromList (Map.elems (ptrView (runPtrs actions empty)))
              === range (Ptrs $ runActions actions empty)
        )
    describe "Union (left preference)" $ do
      prop
        "RewardDeposits"
        ( \actions cred rdPair ->
            Map.unionWith const (rdPairView (runRDPairs actions empty)) (Map.singleton cred rdPair)
              === unUnify (RewardDeposits (RewardDeposits (runActions actions empty) ∪ (cred, rdPair)))
        )
      prop
        "Delegations"
        ( \actions cred pool ->
            Map.unionWith const (delView (runDelegations actions empty)) (Map.singleton cred pool)
              === unUnify (Delegations (Delegations (runActions actions empty) ∪ (cred, pool)))
        )
      prop
        "Ptrs"
        ( \actions cred ptr ->
            Map.unionWith const (ptrView (runPtrs actions empty)) (Map.singleton cred ptr)
              === unUnify (Ptrs (Ptrs (runActions actions empty) ∪ (cred, ptr)))
        )
    describe "Union (right preference)" $ do
      prop
        "RewardDeposits (domain of map on the right has to be subset of RewardDeposits View)"
        $ forAll
          genRightPreferenceUMap
          ( \(umap, m) ->
              Map.unionWith (\(RDPair _ leftDep) (RDPair rightRD _) -> RDPair rightRD leftDep) (rdPairView umap) m
                === rdPairView (RewardDeposits umap ⨃ m)
          )
      prop
        "Delegations"
        ( \actions m ->
            Map.unionWith (\_ x -> x) (delView (runDelegations actions empty)) m
              === unUnify (Delegations (Delegations (runActions actions empty) ⨃ m))
        )
      prop
        "Ptrs"
        ( \actions m ->
            Map.unionWith (\_ x -> x) (ptrView (runPtrs actions empty)) m
              === unUnify (Ptrs (Ptrs (runActions actions empty) ⨃ m))
        )
    prop
      "Monoidal Rewards (domain of map on the right has to be subset of RewardDeposits View)"
      $ forAll
        genRightPreferenceUMap
        ( \(umap, m) ->
            Map.unionWith (<>) (compactRewView umap) (rdReward <$> m)
              === compactRewView (RewardDeposits umap ∪+ (rdReward <$> m))
        )
    describe "Domain exclusion" $ do
      prop
        "RewardDeposits"
        ( \actions dom ->
            Map.withoutKeys (rdPairView (runRDPairs actions empty)) dom
              === unUnify (RewardDeposits (dom ⋪ RewardDeposits (runActions actions empty)))
        )
      prop
        "Delegations"
        ( \actions dom ->
            Map.withoutKeys (delView (runDelegations actions empty)) dom
              === unUnify (Delegations (dom ⋪ Delegations (runActions actions empty)))
        )
      prop
        "Ptrs"
        ( \actions dom ->
            Map.withoutKeys (ptrView (runPtrs actions empty)) dom
              === unUnify (Ptrs (dom ⋪ Ptrs (runActions actions empty)))
        )
    describe "Range exclusion" $ do
      prop
        "RewardDeposits"
        ( \actions rng ->
            Map.filter (not . flip Set.member rng) (rdPairView (runRDPairs actions empty))
              === unUnify (RewardDeposits (RewardDeposits (runActions actions empty) ⋫ rng))
        )
      prop
        "Delegations"
        ( \actions rng ->
            Map.filter (not . flip Set.member rng) (delView (runDelegations actions empty))
              === unUnify (Delegations (Delegations (runActions actions empty) ⋫ rng))
        )
      prop
        "Ptrs"
        ( \actions rng ->
            Map.filter (not . flip Set.member rng) (ptrView (runPtrs actions empty))
              === unUnify (Ptrs (Ptrs (runActions actions empty) ⋫ rng))
        )
    describe "Domain restriction" $ do
      prop
        "RewardDeposits"
        ( \actions (m :: Map.Map (Credential 'Staking StandardCrypto) RDPair) ->
            Map.intersection m (rdPairView (runRDPairs actions empty))
              === domRestrict (RewardDeposits (runActions actions empty)) m
        )
      prop
        "Delegations"
        ( \actions (m :: Map.Map (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto)) ->
            Map.intersection m (delView (runDelegations actions empty))
              === domRestrict (Delegations (runActions actions empty)) m
        )
      prop
        "Ptrs"
        ( \actions (m :: Map.Map Ptr (Credential 'Staking StandardCrypto)) ->
            Map.intersection m (ptrView (runPtrs actions empty))
              === domRestrict (Ptrs (runActions actions empty)) m
        )
