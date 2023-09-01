{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.STS where

import Cardano.Crypto.Hash.Class (sizeHash)
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxCert
import qualified Cardano.Ledger.Crypto as CC (Crypto (HASH))
import Cardano.Ledger.PoolParams
import Cardano.Ledger.Shelley.Rules hiding (epochNo, slotNo)
import qualified Cardano.Ledger.UMap as UM
import Control.State.Transition.Extended
import qualified Data.ByteString as BS
import Data.Default.Class (Default (def))
import Data.Foldable
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
import Test.Cardano.Ledger.Alonzo.EraMapping ()
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad
import Test.Cardano.Ledger.Constrained.Preds.CertState
import Test.Cardano.Ledger.Constrained.Preds.PParams
import Test.Cardano.Ledger.Constrained.Preds.Universes
import Test.Cardano.Ledger.Constrained.Rewrite
import Test.Cardano.Ledger.Constrained.Shrink
import Test.Cardano.Ledger.Constrained.Solver
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars hiding (delegations, rewards)
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Shelley.Utils
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

------------------------------------------------------------------------
-- Generator Setup
------------------------------------------------------------------------

applySubst :: Subst era -> [Pred era] -> [Pred era]
applySubst = map . substPredWithVarTest

genFromConstraints :: Era era => Proof era -> OrderInfo -> [Pred era] -> RootTarget era s t -> Gen t
genFromConstraints proof order cs target = do
  subst <- genSubstFromConstraints proof order cs
  env <- monadTyped $ substToEnv subst emptyEnv
  monadTyped (runTarget env target)

genSubstFromConstraints :: Era era => Proof era -> OrderInfo -> [Pred era] -> Gen (Subst era)
genSubstFromConstraints proof order cs = do
  (_, graph) <- compileGenWithSubst order emptySubst cs
  result <- genDependGraph False proof graph
  case result of
    Left errs -> error $ unlines errs
    Right subst -> pure subst

shrinkFromConstraints :: Era era => OrderInfo -> [Pred era] -> RootTarget era t t -> t -> [t]
shrinkFromConstraints order cs target val = do
  -- Solve variables that are uniquely determined by the
  -- target and the constraints (e.g. "helper variables")
  let env = saturateEnv (getTarget val target emptyEnv) cs
  -- Create a Dependency graph from the predicates
  graph <- monadTyped $ compile order $ rewrite cs
  -- Shrink the environment using the standard environment
  -- shrinker
  env' <- shrinkEnv graph env
  -- Turn the environment back into a value
  monadTyped $ runTarget env' target

-- | Add variables to the environment that are uniquely determined by the constraints.
saturateEnv :: Era era => Env era -> [Pred era] -> Env era
saturateEnv env0 preds = go env0 preds
  where
    go env [] = env
    go env (p : ps)
      | Just (x, v) <- solveUnknown env p = saturateEnv (storeName x v env) preds
      | otherwise = go env ps

-- | Try to find a solution for a single variable from a predicate in an environment
solveUnknown :: forall era. Era era => Env era -> Pred era -> Maybe (Name era, Payload era)
solveUnknown env p = case p of
  SumsTo _ (Var x@(V _ rep acc)) EQL sums
    | unknown (Name x)
    , knownSums sums
    , Right v <- runTyped (sumAdds <$> mapM (runSum env) sums) ->
        Just (Name x, Payload rep v acc)
  Component (direct -> tm) (AnyF (Field s rep reps lens) : _)
    | knownTerm tm
    , unknown x
    , Right r <- runTyped (runTerm env tm) ->
        Just (x, Payload rep (r ^. lens) acc)
    where
      acc = Yes reps lens
      x = Name (V s rep acc)
  Component r (_ : flds) ->
    solveUnknown env (Component r flds)
  tm :=: Var x@(V _ rep acc)
    | knownTerm tm
    , unknown (Name x)
    , Right v <- runTyped (runTerm env tm) ->
        Just (Name x, Payload rep v acc)
  Var x@(V _ rep acc) :=: tm
    | knownTerm tm
    , unknown (Name x)
    , Right v <- runTyped (runTerm env tm) ->
        Just (Name x, Payload rep v acc)
  _ -> Nothing
  where
    known = isJust . flip findName env
    unknown = not . known
    knownTerm :: forall t. Term era t -> Bool
    knownTerm = all known . varsOfTerm mempty
    knownSums :: forall r. [Sum era r] -> Bool
    knownSums = all known . foldl' varsOfSum mempty

lookupTerm ::
  Testable p =>
  Subst (ConwayEra StandardCrypto) ->
  Term (ConwayEra StandardCrypto) t ->
  (t -> p) ->
  Property
lookupTerm sub term cont =
  let lkp = do
        env <- substToEnv sub emptyEnv
        runTarget env (Simple term)
   in case runTyped lkp of
        Left errors -> counterexample ("lookupTerm: " ++ show errors) False
        Right val -> property $ cont val

------------------------------------------------------------------------
-- Generator helpers
------------------------------------------------------------------------

conwayProof :: Proof (ConwayEra StandardCrypto)
conwayProof = Conway Standard

-- PParams ----------------------------------------------------------------

pParamsT ::
  RootTarget
    (ConwayEra StandardCrypto)
    (PParamsF (ConwayEra StandardCrypto))
    (PParamsF (ConwayEra StandardCrypto))
pParamsT = Lensed (pparams conwayProof) id

pParamsPs :: Subst (ConwayEra StandardCrypto) -> [Pred (ConwayEra StandardCrypto)]
pParamsPs s = applySubst s $ pParamsPreds conwayProof

pParamsGen :: Subst (ConwayEra StandardCrypto) -> Gen (PParamsF (ConwayEra StandardCrypto))
pParamsGen s = genFromConstraints conwayProof standardOrderInfo (pParamsPs s) pParamsT

-- TODO: this is currently broken because it fails to find `maxTxSize`. This can be fixed
-- by building a proper target for PParams but I don't yet know how to do that.
pParamsShrink ::
  Subst (ConwayEra StandardCrypto) ->
  PParamsF (ConwayEra StandardCrypto) ->
  [PParamsF (ConwayEra StandardCrypto)]
pParamsShrink s = shrinkFromConstraints standardOrderInfo (pParamsPs s) pParamsT

-- Govcert ----------------------------------------------------------------

genConwayGovCert ::
  PParams (ConwayEra StandardCrypto) ->
  VState (ConwayEra StandardCrypto) ->
  Gen (ConwayGovCert StandardCrypto)
genConwayGovCert pp vs =
  oneof $
    [ do
        key <- arbitrary `suchThat` (`Map.notMember` vsDReps vs)
        ConwayRegDRep key (pp ^. ppDRepDepositL) <$> arbitrary
    ]
      ++ [ do
          (k, dep) <- elements $ Map.toList (vsDReps vs)
          pure $ ConwayUnRegDRep k (drepDeposit dep)
         | mempty /= vsDReps vs
         ]
      ++ [ ConwayAuthCommitteeHotKey <$> arbitrary <*> arbitrary
         , ConwayResignCommitteeColdKey <$> arbitrary
         ]

shrinkConwayGovCert ::
  PParams (ConwayEra StandardCrypto) ->
  VState (ConwayEra StandardCrypto) ->
  ConwayGovCert StandardCrypto ->
  [ConwayGovCert StandardCrypto]
shrinkConwayGovCert pp vs =
  filter validGovCert . shrink
  where
    validGovCert cert =
      case cert of
        ConwayRegDRep key dep _ -> Map.notMember key (vsDReps vs) && dep == pp ^. ppDRepDepositL
        ConwayUnRegDRep key dep -> fmap drepDeposit (Map.lookup key (vsDReps vs)) == Just dep
        ConwayUpdateDRep key _ -> Map.member key (vsDReps vs)
        ConwayAuthCommitteeHotKey _ _ -> True
        ConwayResignCommitteeColdKey _ -> True

genPoolCert ::
  Network ->
  EpochNo ->
  PParams (ConwayEra StandardCrypto) ->
  PState (ConwayEra StandardCrypto) ->
  Gen (PoolCert StandardCrypto)
genPoolCert net epochNo pParams pState =
  oneof $
    [RegPool <$> genPoolParams]
      ++ [ RetirePool <$> genRetirePool <*> genRetireEpoch
         | not $ null rpools
         , epochNo + 1 < maxEp
         ]
  where
    maxEp = pParams ^. (withEraPParams conwayProof ppEMaxL)
    rpools = Map.keys $ psStakePoolParams pState
    genRetireEpoch = (min maxEp) . (1 + epochNo +) <$> arbitrary
    genRetirePool = elements rpools

    maxMetaLen = fromIntegral (sizeHash ([] @(CC.HASH StandardCrypto)))

    adjustRewardAcnt rwd = rwd {getRwdNetwork = net}
    adjustMetadata SNothing = SNothing
    adjustMetadata (SJust m) = SJust $ m {pmHash = BS.take maxMetaLen (pmHash m)}

    genPoolParams = do
      pp <- arbitrary
      pure $
        pp
          { ppRewardAcnt = adjustRewardAcnt $ ppRewardAcnt pp
          , ppMetadata = adjustMetadata $ ppMetadata pp
          }

genDelegCert ::
  PParams (ConwayEra StandardCrypto) ->
  DState (ConwayEra StandardCrypto) ->
  Gen (ConwayDelegCert StandardCrypto)
genDelegCert pp ds =
  oneof $
    [gen_RegCert]
      ++ [gen_UnRegCert | hasRegisteredKeys noReward]
      ++ [gen_DelegCert | hasRegisteredKeys unrestricted]
      ++ [gen_RegDelegCert]
  where
    delegView = delegations ds
    rewardView = rewards ds
    delegKeys = Set.toList $ UM.domain delegView

    hasRegisteredKeys p = not $ null $ filter p delegKeys
    arbitraryRegisteredKey p = elements $ filter p delegKeys

    noReward k = case UM.lookup k rewardView of
      Nothing -> True
      Just (UM.RDPair (UM.fromCompact -> r) _) -> r == mempty

    unrestricted _ = True

    gen_RegCert = ConwayRegCert <$> arbitrary <*> pure (SJust $ pp ^. ppKeyDepositL)

    gen_UnRegCert = do
      key <- arbitraryRegisteredKey noReward
      let UM.RDPair _ (UM.fromCompact -> val) = fromJust $ UM.lookup key rewardView
      pure $ ConwayUnRegCert key (SJust val)

    gen_DelegCert = ConwayDelegCert <$> arbitraryRegisteredKey unrestricted <*> arbitrary

    gen_RegDelegCert = ConwayRegDelegCert <$> arbitrary <*> arbitrary <*> pure (pp ^. ppKeyDepositL)

------------------------------------------------------------------------
-- Properties
------------------------------------------------------------------------

prop_GOVCERT :: Subst (ConwayEra StandardCrypto) -> Property
prop_GOVCERT sub =
  forAll (pParamsGen sub) $ \(unPParams -> pParams) ->
    lookupTerm sub currentEpoch $ \epochNo ->
      forAllShrink (vStateGen sub) (vStateShrink sub) $ \vState ->
        forAllShrink
          (genConwayGovCert pParams vState)
          (shrinkConwayGovCert pParams vState)
          $ \signal ->
            -- TODO: this can be generalized when we have more examples
            runShelleyBase $ do
              res <- applySTS @(ConwayGOVCERT Conway) $ TRC (ConwayGovCertEnv pParams epochNo, vState, signal)
              pure $ case res of
                Left pfailure -> counterexample (show pfailure) $ property False
                Right _ -> property True -- There are no actual invariants on `VState` that we can check
                -- so we are happy just checking that we actually do _something_ here
  where
    -- VState
    vStatePs s = applySubst s $ vstatePreds conwayProof
    vStateGen s = genFromConstraints conwayProof standardOrderInfo (vStatePs s) vstateT
    vStateShrink s = shrinkFromConstraints standardOrderInfo (vStatePs s) vstateT

prop_POOL :: Subst (ConwayEra StandardCrypto) -> Property
prop_POOL sub =
  lookupTerm sub network $ \net ->
    lookupTerm sub currentEpoch $ \epochNo ->
      lookupTerm sub currentSlot $ \slotNo ->
        forAll (pParamsGen sub) $ \(unPParams -> pParams) ->
          forAllShrink (pStateGen sub) (pStateShrink sub) $ \pState ->
            -- TODO: this should eventually be replaced by constraints based generator
            forAll (genPoolCert net epochNo pParams pState) $ \poolCert ->
              runShelleyBase $ do
                res <- applySTS @(ConwayPOOL Conway) $ TRC (PoolEnv slotNo pParams, pState, poolCert)
                pure $ case res of
                  Left pfailure -> counterexample (show pfailure) $ property False
                  Right _ -> property True -- TODO: checking here! (requires splitting checking constraints from pstate)
  where
    -- PState
    pStatePs s = applySubst s $ pstatePreds conwayProof
    pStateGen s = genFromConstraints conwayProof standardOrderInfo (pStatePs s) pstateT
    pStateShrink s = shrinkFromConstraints standardOrderInfo (pStatePs s) pstateT

prop_DELEG :: Subst (ConwayEra StandardCrypto) -> Property
prop_DELEG sub =
  forAll (pParamsGen sub) $ \(unPParams -> pParams) ->
    forAllShrink (dStateGen sub) (dStateShrink sub) $ \dState ->
      -- TODO: this should eventually be replaced by constraints based generator
      forAll (genDelegCert pParams dState) $ \delegCert ->
        runShelleyBase $ do
          res <- applySTS @(ConwayDELEG Conway) $ TRC (pParams, dState, delegCert)
          pure $ case res of
            Left pfailure -> counterexample (show pfailure) $ property False
            Right _ -> property True -- TODO: checking here! (requires splitting checking constraints from pstate)
  where
    -- PState
    dStatePs s = applySubst s $ certStatePreds conwayProof
    dStateGen s = genFromConstraints conwayProof standardOrderInfo (dStatePs s) dstateT
    dStateShrink s = shrinkFromConstraints standardOrderInfo (dStatePs s) dstateT

------------------------------------------------------------------------
-- Test Tree
------------------------------------------------------------------------

genUniverse :: IO (Subst (ConwayEra StandardCrypto))
genUniverse = generate (genSubstFromConstraints (Conway Standard) standardOrderInfo (universePreds def (Conway Standard)))

-- NOTE: these tests can be slow (~10 seconds) because it takes time to generate
-- the initial universe. Reducing the number of tests will NOT help.
tests_STS :: TestTree
tests_STS =
  withResource genUniverse (const $ pure ()) $ \io_subst ->
    testGroup
      "STS property tests"
      [ testProperty "prop_GOVCERT" $ idempotentIOProperty (prop_GOVCERT <$> io_subst)
      , testProperty "prop_POOL" $ idempotentIOProperty (prop_POOL <$> io_subst)
      , testProperty "prop_DELEG" $ idempotentIOProperty (prop_DELEG <$> io_subst)
      ]

runTest :: (Subst (ConwayEra StandardCrypto) -> Property) -> IO ()
runTest prop = do
  univ <- genUniverse
  quickCheck $ prop univ
