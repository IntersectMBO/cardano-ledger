{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.UMapSpec where

import Cardano.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Cardano.Ledger.Coin (Coin, CompactForm)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Credential (Credential, Ptr)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.DRep (DRep)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.UMap (
  RDPair (RDPair, rdReward),
  StakeCredentials (..),
  UMElem (UMElem),
  UMap (UMap, umElems, umPtrs),
  UView (DRepUView, PtrUView, RewDepUView, SPoolUView),
  addCompact,
  compactRewardMap,
  dRepMap,
  delete,
  delete',
  depositMap,
  domRestrict,
  domRestrictedMap,
  domRestrictedStakeCredentials,
  domain,
  empty,
  insert,
  insert',
  invPtrMap,
  member,
  nullUView,
  ptrMap,
  range,
  rdDeposit,
  rdPairMap,
  rewardMap,
  sPoolMap,
  size,
  toStakeCredentials,
  umInvariant,
  unUView,
  unUnify,
  unify,
  (∪),
  (∪+),
  (⋪),
  (⋫),
  (⨃),
 )
import qualified Cardano.Ledger.UMap as UMap (lookup)
import Control.Exception (assert)
import Data.Foldable (Foldable (fold))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary (
  genInsertDeleteRoundtripDRep,
  genInsertDeleteRoundtripPtr,
  genInsertDeleteRoundtripRDPair,
  genInsertDeleteRoundtripSPool,
  genInvariantNonEmpty,
  genRightPreferenceUMap,
  genValidTuples,
  genValidUMap,
  genValidUMapWithCreds,
 )

data Action
  = InsertRDPair (Credential 'Staking StandardCrypto) RDPair
  | InsertPtr Ptr (Credential 'Staking StandardCrypto)
  | InsertSPool (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto)
  | InsertDRep (Credential 'Staking StandardCrypto) (DRep StandardCrypto)
  | DeleteRDPair (Credential 'Staking StandardCrypto)
  | DeletePtr Ptr
  | DeleteSPool (Credential 'Staking StandardCrypto)
  | DeleteDRep (Credential 'Staking StandardCrypto)
  deriving (Show)

instance Arbitrary Action where
  arbitrary =
    oneof
      [ InsertRDPair <$> arbitrary <*> arbitrary
      , InsertPtr <$> arbitrary <*> arbitrary
      , InsertSPool <$> arbitrary <*> arbitrary
      , DeleteRDPair <$> arbitrary
      , DeletePtr <$> arbitrary
      , DeleteSPool <$> arbitrary
      , DeleteDRep <$> arbitrary
      ]

genRDPair :: Gen Action
genRDPair = InsertRDPair <$> arbitrary <*> arbitrary

genPtr :: Gen Action
genPtr = InsertPtr <$> arbitrary <*> arbitrary

genSPool :: Gen Action
genSPool = InsertSPool <$> arbitrary <*> arbitrary

genDRep :: Gen Action
genDRep = InsertDRep <$> arbitrary <*> arbitrary

reify :: Action -> UMap StandardCrypto -> UMap StandardCrypto
reify = \case
  InsertRDPair k v -> insert k v . RewDepUView
  InsertPtr k v -> insert k v . PtrUView
  InsertSPool k v -> insert k v . SPoolUView
  InsertDRep k v -> insert k v . DRepUView
  DeleteRDPair k -> delete k . RewDepUView
  DeletePtr k -> delete k . PtrUView
  DeleteSPool k -> delete k . SPoolUView
  DeleteDRep k -> delete k . DRepUView

reifyRDPair :: Action -> UMap StandardCrypto -> UMap StandardCrypto
reifyRDPair = \case
  InsertRDPair k v -> insert k v . RewDepUView
  DeleteRDPair k -> delete k . RewDepUView
  _ -> id

reifyPtr :: Action -> UMap StandardCrypto -> UMap StandardCrypto
reifyPtr = \case
  InsertPtr k v -> insert k v . PtrUView
  DeletePtr k -> delete k . PtrUView
  _ -> id

reifySPool :: Action -> UMap StandardCrypto -> UMap StandardCrypto
reifySPool = \case
  InsertSPool k v -> insert k v . SPoolUView
  DeleteSPool k -> delete k . SPoolUView
  _ -> id

reifyDRep :: Action -> UMap StandardCrypto -> UMap StandardCrypto
reifyDRep = \case
  InsertDRep k v -> insert k v . DRepUView
  DeleteDRep k -> delete k . DRepUView
  _ -> id

runActions :: [Action] -> UMap StandardCrypto -> UMap StandardCrypto
runActions actions umap = foldr reify umap actions

runRDPairs :: [Action] -> UMap StandardCrypto -> UMap StandardCrypto
runRDPairs actions umap = foldr reifyRDPair umap actions

runPtrs :: [Action] -> UMap StandardCrypto -> UMap StandardCrypto
runPtrs actions umap = foldr reifyPtr umap actions

runSPools :: [Action] -> UMap StandardCrypto -> UMap StandardCrypto
runSPools actions umap = foldr reifySPool umap actions

runDReps :: [Action] -> UMap StandardCrypto -> UMap StandardCrypto
runDReps actions umap = foldr reifyDRep umap actions

sizeTest ::
  ( Map.Map (Credential 'Staking StandardCrypto) RDPair
  , Map.Map Ptr (Credential 'Staking StandardCrypto)
  , Map.Map (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto)
  , Map.Map (Credential 'Staking StandardCrypto) (DRep StandardCrypto)
  ) ->
  IO ()
sizeTest (rdPairs, ptrs, sPools, dReps) = do
  let
    umap = unify rdPairs ptrs sPools dReps
    rdPairsSize = size (RewDepUView umap)
    ptrsSize = size (PtrUView umap)
    sPoolSize = size (SPoolUView umap)
    dRepSize = size (DRepUView umap)
  Map.size rdPairs `shouldBe` rdPairsSize
  Map.size ptrs `shouldBe` ptrsSize
  Map.size sPools `shouldBe` sPoolSize
  Map.size dReps `shouldBe` dRepSize

unifyRoundTripTo ::
  ( Map.Map (Credential 'Staking StandardCrypto) RDPair
  , Map.Map Ptr (Credential 'Staking StandardCrypto)
  , Map.Map (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto)
  , Map.Map (Credential 'Staking StandardCrypto) (DRep StandardCrypto)
  ) ->
  IO ()
unifyRoundTripTo (rdPairs, ptrs, sPools, dReps) = do
  let umap = unify rdPairs ptrs sPools dReps
  rdPairMap umap `shouldBe` rdPairs
  ptrMap umap `shouldBe` ptrs
  sPoolMap umap `shouldBe` sPools
  dRepMap umap `shouldBe` dReps

unifyRoundTripFrom :: [Action] -> Property
unifyRoundTripFrom actions =
  let
    umap = runActions actions empty
    rdPairs = rdPairMap umap
    ptrs = ptrMap umap
    sPools = sPoolMap umap
    dReps = dRepMap umap
   in
    umap === unify rdPairs ptrs sPools dReps

oldUnionRewAgg ::
  UView c (Credential 'Staking c) RDPair ->
  Map (Credential 'Staking c) (CompactForm Coin) ->
  UMap c
(RewDepUView UMap {umElems, umPtrs}) `oldUnionRewAgg` aggRewMap = UMap newUmElem umPtrs
  where
    newUmElem =
      let
        result = Map.mergeWithKey f id (const Map.empty) umElems aggRewMap
        f _k (UMElem p1 s deposit drep) delta = Just (UMElem (addC delta p1) s deposit drep)
       in
        -- We use Map.empty below because aggRewMap is a subset of umElems, we never add anything here.
        assert (Map.valid result) result
    addC :: CompactForm Coin -> StrictMaybe RDPair -> StrictMaybe RDPair
    addC newR = \case
      SNothing -> SNothing
      SJust (RDPair r d) -> SJust $ RDPair (addCompact r newR) d

spec :: Spec
spec = do
  describe "UMap" $ do
    describe "Invariant" $ do
      prop "Empty" (\(cred :: Credential 'Staking StandardCrypto) ptr -> umInvariant cred ptr empty)
      prop "Non-empty" $
        forAll
          genInvariantNonEmpty
          (\(cred, ptr, umap) -> umInvariant cred ptr umap)
      xprop "Non-empty with insert and delete actions" $
        forAll
          ((,) <$> genInvariantNonEmpty <*> arbitrary)
          (\((cred, ptr, umap), actions) -> umInvariant cred ptr $ runActions actions umap)
    describe "Unify roundtrip" $ do
      prop "To" $ forAll genValidTuples unifyRoundTripTo
      prop "From" unifyRoundTripFrom
    describe "Insert-delete roundtrip" $ do
      prop "RDPair" $
        forAll
          genInsertDeleteRoundtripRDPair
          (\(umap, k, v) -> umap === unUView (delete' k (insert' k v (RewDepUView umap))))
      prop "PtrUView" $
        forAll
          genInsertDeleteRoundtripPtr
          (\(umap, k, v) -> umap === unUView (delete' k (insert' k v (PtrUView umap))))
      prop "SPoolUView" $
        forAll
          genInsertDeleteRoundtripSPool
          (\(umap, k, v) -> umap === unUView (delete' k (insert' k v (SPoolUView umap))))
      prop "DRepUView" $
        forAll
          genInsertDeleteRoundtripDRep
          (\(umap, k, v) -> umap === unUView (delete' k (insert' k v (DRepUView umap))))
    prop "Size" $ forAll genValidTuples sizeTest
    describe "Membership" $ do
      prop
        "RewDepUView"
        ( \(umap :: UMap StandardCrypto, cred) ->
            member cred (RewDepUView umap) === Map.member cred (rdPairMap umap)
        )
      prop
        "PtrUViews"
        ( \(umap :: UMap StandardCrypto, ptr) ->
            member ptr (PtrUView umap) === Map.member ptr (ptrMap umap)
        )
      prop
        "SPoolUView"
        ( \(umap :: UMap StandardCrypto, cred) ->
            member cred (SPoolUView umap) === Map.member cred (sPoolMap umap)
        )
      prop
        "DRepUView"
        ( \(umap :: UMap StandardCrypto, cred) ->
            member cred (DRepUView umap) === Map.member cred (dRepMap umap)
        )
    describe "Bisimulation" $ do
      prop
        "RewDepUView"
        ( \actions ->
            rdPairMap (runRDPairs actions empty)
              === rdPairMap (runActions actions empty)
        )
      prop
        "PtrUView"
        ( \actions ->
            ptrMap (runPtrs actions empty)
              === ptrMap (runActions actions empty)
        )
      prop
        "SPoolUView"
        ( \actions ->
            sPoolMap (runSPools actions empty)
              === sPoolMap (runActions actions empty)
        )
      prop
        "DRepUView"
        ( \actions ->
            dRepMap (runDReps actions empty)
              === dRepMap (runActions actions empty)
        )
    describe "Null" $ do
      prop
        "RewDepUView"
        ( \actions ->
            Map.null (rdPairMap (runRDPairs actions empty))
              === nullUView (RewDepUView $ runActions actions empty)
        )
      prop
        "PtrUView"
        ( \actions ->
            Map.null (ptrMap (runPtrs actions empty))
              === nullUView (PtrUView $ runActions actions empty)
        )
      prop
        "SPoolUView"
        ( \actions ->
            Map.null (sPoolMap (runSPools actions empty))
              === nullUView (SPoolUView $ runActions actions empty)
        )
      prop
        "DRepUView"
        ( \actions ->
            Map.null (dRepMap (runDReps actions empty))
              === nullUView (DRepUView $ runActions actions empty)
        )
    describe "Lookup" $ do
      prop
        "RewDepUView"
        ( \actions cred ->
            Map.lookup cred (rdPairMap (runRDPairs actions empty))
              === UMap.lookup cred (RewDepUView $ runActions actions empty)
        )
      prop
        "PtruView"
        ( \actions ptr ->
            Map.lookup ptr (ptrMap (runPtrs actions empty))
              === UMap.lookup ptr (PtrUView $ runActions actions empty)
        )
      prop
        "SPoolUView"
        ( \actions cred ->
            Map.lookup cred (sPoolMap (runSPools actions empty))
              === UMap.lookup cred (SPoolUView $ runActions actions empty)
        )
      prop
        "DRepUView"
        ( \actions cred ->
            Map.lookup cred (dRepMap (runDReps actions empty))
              === UMap.lookup cred (DRepUView $ runActions actions empty)
        )
    describe "Domain" $ do
      prop
        "RewDepUView"
        ( \actions ->
            Map.keysSet (rdPairMap (runRDPairs actions empty))
              === domain (RewDepUView $ runActions actions empty)
        )
      prop
        "PtrUView"
        ( \actions ->
            Map.keysSet (ptrMap (runPtrs actions empty))
              === domain (PtrUView $ runActions actions empty)
        )
      prop
        "SPoolUView"
        ( \actions ->
            Map.keysSet (sPoolMap (runSPools actions empty))
              === domain (SPoolUView $ runActions actions empty)
        )
      prop
        "DRepUView"
        ( \actions ->
            Map.keysSet (dRepMap (runDReps actions empty))
              === domain (DRepUView $ runActions actions empty)
        )
    describe "Range" $ do
      prop
        "RewDepUView"
        ( \actions ->
            Set.fromList (Map.elems (rdPairMap (runRDPairs actions empty)))
              === range (RewDepUView $ runActions actions empty)
        )
      prop
        "PtrUView"
        ( \actions ->
            Set.fromList (Map.elems (ptrMap (runPtrs actions empty)))
              === range (PtrUView $ runActions actions empty)
        )
      prop
        "SPoolUView"
        ( \actions ->
            Set.fromList (Map.elems (sPoolMap (runSPools actions empty)))
              === range (SPoolUView $ runActions actions empty)
        )
      prop
        "DRepUView"
        ( \actions ->
            Set.fromList (Map.elems (dRepMap (runDReps actions empty)))
              === range (DRepUView $ runActions actions empty)
        )
    describe "Union (left preference)" $ do
      prop
        "RewDepUView"
        ( \actions cred rdPair ->
            Map.unionWith const (rdPairMap (runRDPairs actions empty)) (Map.singleton cred rdPair)
              === unUnify (RewDepUView (RewDepUView (runActions actions empty) ∪ (cred, rdPair)))
        )
      prop
        "PtrUView"
        ( \actions cred ptr ->
            Map.unionWith const (ptrMap (runPtrs actions empty)) (Map.singleton cred ptr)
              === unUnify (PtrUView (PtrUView (runActions actions empty) ∪ (cred, ptr)))
        )
      prop
        "SPoolUView"
        ( \actions cred pool ->
            Map.unionWith const (sPoolMap (runSPools actions empty)) (Map.singleton cred pool)
              === unUnify (SPoolUView (SPoolUView (runActions actions empty) ∪ (cred, pool)))
        )
      prop
        "DRepUView"
        ( \actions cred pool ->
            Map.unionWith const (dRepMap (runDReps actions empty)) (Map.singleton cred pool)
              === unUnify (DRepUView (DRepUView (runActions actions empty) ∪ (cred, pool)))
        )
    describe "Union (right preference)" $ do
      prop
        "RewDepUView (domain of map on the right has to be subset of RewDepUView View)"
        $ forAll
          genRightPreferenceUMap
          ( \(umap, m) ->
              Map.unionWith
                (\(RDPair _ leftDep) (RDPair rightRD _) -> RDPair rightRD leftDep)
                (rdPairMap umap)
                m
                === rdPairMap (RewDepUView umap ⨃ m)
          )
      prop
        "PtrUView"
        ( \actions m ->
            Map.unionWith (\_ x -> x) (ptrMap (runPtrs actions empty)) m
              === unUnify (PtrUView (PtrUView (runActions actions empty) ⨃ m))
        )
      prop
        "SPoolUView"
        ( \actions m ->
            Map.unionWith (\_ x -> x) (sPoolMap (runSPools actions empty)) m
              === unUnify (SPoolUView (SPoolUView (runActions actions empty) ⨃ m))
        )
      prop
        "DRepUView"
        ( \actions m ->
            Map.unionWith (\_ x -> x) (dRepMap (runDReps actions empty)) m
              === unUnify (DRepUView (DRepUView (runActions actions empty) ⨃ m))
        )
    prop
      "Monoidal Rewards (domain of map on the right has to be subset of RewDepUView View)"
      $ forAll
        genRightPreferenceUMap
        ( \(umap, m) ->
            Map.unionWith (<>) (compactRewardMap umap) (rdReward <$> m)
              === compactRewardMap (RewDepUView umap ∪+ (rdReward <$> m))
        )
    describe "Domain exclusion" $ do
      prop
        "RewDepUView"
        ( \actions dom ->
            Map.withoutKeys (rdPairMap (runRDPairs actions empty)) dom
              === unUnify (RewDepUView (dom ⋪ RewDepUView (runActions actions empty)))
        )
      prop
        "PtrUView"
        ( \actions dom ->
            Map.withoutKeys (ptrMap (runPtrs actions empty)) dom
              === unUnify (PtrUView (dom ⋪ PtrUView (runActions actions empty)))
        )
      prop
        "SPoolUView"
        ( \actions dom ->
            Map.withoutKeys (sPoolMap (runSPools actions empty)) dom
              === unUnify (SPoolUView (dom ⋪ SPoolUView (runActions actions empty)))
        )
      prop
        "DRepUView"
        ( \actions dom ->
            Map.withoutKeys (dRepMap (runDReps actions empty)) dom
              === unUnify (DRepUView (dom ⋪ DRepUView (runActions actions empty)))
        )
    describe "Range exclusion" $ do
      prop
        "RewDepUView"
        ( \actions rng ->
            Map.filter (not . flip Set.member rng) (rdPairMap (runRDPairs actions empty))
              === unUnify (RewDepUView (RewDepUView (runActions actions empty) ⋫ rng))
        )
      prop
        "PtrUView"
        ( \actions rng ->
            Map.filter (not . flip Set.member rng) (ptrMap (runPtrs actions empty))
              === unUnify (PtrUView (PtrUView (runActions actions empty) ⋫ rng))
        )
      prop
        "SPoolUView"
        ( \actions rng ->
            Map.filter (not . flip Set.member rng) (sPoolMap (runSPools actions empty))
              === unUnify (SPoolUView (SPoolUView (runActions actions empty) ⋫ rng))
        )
      prop
        "DRepUView"
        ( \actions rng ->
            Map.filter (not . flip Set.member rng) (dRepMap (runDReps actions empty))
              === unUnify (DRepUView (DRepUView (runActions actions empty) ⋫ rng))
        )
    describe "Domain restriction" $ do
      prop
        "RewDepUView"
        ( \actions (m :: Map.Map (Credential 'Staking StandardCrypto) RDPair) ->
            Map.intersection m (rdPairMap (runRDPairs actions empty))
              === domRestrict (RewDepUView (runActions actions empty)) m
        )
      prop
        "PtrUView"
        ( \actions (m :: Map.Map Ptr (Credential 'Staking StandardCrypto)) ->
            Map.intersection m (ptrMap (runPtrs actions empty))
              === domRestrict (PtrUView (runActions actions empty)) m
        )
      prop
        "SPoolUView"
        ( \actions (m :: Map.Map (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto)) ->
            Map.intersection m (sPoolMap (runSPools actions empty))
              === domRestrict (SPoolUView (runActions actions empty)) m
        )
      prop
        "DRepUView"
        ( \actions (m :: Map.Map (Credential 'Staking StandardCrypto) (DRep StandardCrypto)) ->
            Map.intersection m (dRepMap (runDReps actions empty))
              === domRestrict (DRepUView (runActions actions empty)) m
        )
    describe "Old and new implementation is equivalent" $ do
      prop
        "unionRewAgg or ∪+ === oldUnionRewAgg"
        $ forAll
          genRightPreferenceUMap
          ( \(umap, m) ->
              RewDepUView umap ∪+ (rdReward <$> m) === RewDepUView umap `oldUnionRewAgg` (rdReward <$> m)
          )
    describe "StakeCredentials" $ do
      prop "toStakeCredentials as full domRestrictedStakeCredentials" $
        forAll genValidUMap $ \umap ->
          toStakeCredentials umap
            `shouldBe` domRestrictedStakeCredentials (Map.keysSet (umElems umap)) umap

      prop "domRestrictedStakeCredentials" $ forAll genValidUMapWithCreds $ \(umap, creds) ->
        domRestrictedStakeCredentials creds umap
          `shouldBe` StakeCredentials
            { scRewards = rewardMap umap `Map.restrictKeys` creds
            , scDeposits = depositMap umap `Map.restrictKeys` creds
            , scSPools = sPoolMap umap `Map.restrictKeys` creds
            , scDReps = dRepMap umap `Map.restrictKeys` creds
            , scPtrs = Map.filter (`Set.member` creds) $ ptrMap umap
            , scPtrsInverse = invPtrMap umap `Map.restrictKeys` creds
            }
      prop "domRestrictedStakeCredentials with domRestrictedMap" $
        forAll genValidUMapWithCreds $ \(umap, creds) ->
          let rdmap = domRestrictedMap creds (RewDepUView umap)
              ptrs = invPtrMap umap `Map.restrictKeys` creds
           in domRestrictedStakeCredentials creds umap
                `shouldBe` StakeCredentials
                  { scRewards = Map.map (fromCompact . rdReward) rdmap
                  , scDeposits = Map.map (fromCompact . rdDeposit) rdmap
                  , scSPools = domRestrictedMap creds (SPoolUView umap)
                  , scDReps = domRestrictedMap creds (DRepUView umap)
                  , scPtrs = domRestrictedMap (fold ptrs) (PtrUView umap)
                  , scPtrsInverse = ptrs
                  }
