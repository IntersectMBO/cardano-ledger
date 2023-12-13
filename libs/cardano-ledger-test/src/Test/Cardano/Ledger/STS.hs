{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.STS where

import Cardano.Crypto.Hash.Class (sizeHash)
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential
import qualified Cardano.Ledger.Crypto as CC (Crypto (HASH))
import Cardano.Ledger.Keys
import Cardano.Ledger.PoolDistr
import Cardano.Ledger.PoolParams
import Cardano.Ledger.Pretty
import Cardano.Ledger.Shelley.Rules hiding (epochNo, slotNo)
import Cardano.Ledger.TxIn (TxId (..))
import qualified Cardano.Ledger.UMap as UM
import Control.Monad.Reader
import Control.State.Transition.Extended
import qualified Data.ByteString as BS
import Data.Default.Class (Default (def))
import Data.Foldable
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import GHC.Stack
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
import Test.Cardano.Ledger.Alonzo.EraMapping ()
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad
import Test.Cardano.Ledger.Constrained.Preds.CertState
import Test.Cardano.Ledger.Constrained.Preds.LedgerState
import Test.Cardano.Ledger.Constrained.Preds.PParams
import Test.Cardano.Ledger.Constrained.Preds.Universes
import Test.Cardano.Ledger.Constrained.Rewrite
import Test.Cardano.Ledger.Constrained.Shrink
import Test.Cardano.Ledger.Constrained.Solver
import Test.Cardano.Ledger.Constrained.Tests
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars hiding (delegations, drepDeposit, rewards)
import qualified Test.Cardano.Ledger.Constrained.Vars as Vars
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Shelley.Utils
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Type.Reflection (typeRep)

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
  Component (direct -> tm) (AnyF (Field s rep reps l) : _)
    | knownTerm tm
    , unknown x
    , Right r <- runTyped (runTerm env tm) ->
        Just (x, Payload rep (r ^. l) acc)
    where
      acc = Yes reps l
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
  Subst era ->
  Term era t ->
  (t -> p) ->
  Property
lookupTerm sub term cont =
  let lkp = do
        env <- substToEnv sub emptyEnv
        runTarget env (Simple term)
   in case runTyped lkp of
        Left errors -> counterexample ("lookupTerm: " ++ show errors) False
        Right val -> property $ cont val

genShrinkFromConstraints ::
  Era era =>
  Proof era ->
  Subst era ->
  (Proof era -> [Pred era]) ->
  (Proof era -> [Pred era]) ->
  RootTarget era t t ->
  GenShrink t
genShrinkFromConstraints proof sub generationPreds checkPreds target =
  ( genFromConstraints proof standardOrderInfo ps target
  , shrinkFromConstraints standardOrderInfo cps target
  )
  where
    -- Pad with randoms here to make sure that the unconstrained variables in
    -- the target are still shrunk and don't go missing when we shrink.
    cps = applySubst sub $ checkPreds proof ++ [Random (Var v) | Name v <- HashSet.toList $ varsOfTarget mempty target]
    ps = applySubst sub $ generationPreds proof ++ checkPreds proof

------------------------------------------------------------------------
-- Generator helpers
------------------------------------------------------------------------

conwayProof :: Proof (ConwayEra StandardCrypto)
conwayProof = Conway Standard

-- PParams ----------------------------------------------------------------

pParamsT ::
  Reflect era =>
  RootTarget
    era
    (PParamsF era)
    (PParamsF era)
pParamsT = Lensed (pparams reify) id

pParamsPs :: Reflect era => Subst era -> [Pred era]
pParamsPs s = applySubst s $ pParamsPreds reify

pParamsGen :: Reflect era => Subst era -> Gen (PParamsF era)
pParamsGen s = genFromConstraints reify standardOrderInfo (pParamsPs s) pParamsT

-- TODO: this is currently broken because it fails to find `maxTxSize`. This can be fixed
-- by building a proper target for PParams but I don't yet know how to do that.
-- NOTE: Can't we use the
-- Proj :: Lens' b t -> Rep era t -> Term era b -> Term era t
-- on var currPParams :: Era era => Proof era -> Term era (PParamsF era)
-- and the var
-- maxTxSize :: Era era => Proof era -> Term era Natural
-- to do something?
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
    , ConwayAuthCommitteeHotKey <$> arbitrary <*> arbitrary
    , ConwayResignCommitteeColdKey <$> arbitrary <*> arbitrary
    ]
      ++ [ do
          (k, dep) <- elements $ Map.toList (vsDReps vs)
          pure $ ConwayUnRegDRep k (drepDeposit dep)
         | mempty /= vsDReps vs
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
        ConwayResignCommitteeColdKey _ _ -> True

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
         , epochNo + 1 < maxEpochNo
         ]
  where
    EpochInterval maxEp = pParams ^. (withEraPParams conwayProof ppEMaxL)
    maxEpochNo = EpochNo (fromIntegral maxEp)
    rpools = Map.keys $ psStakePoolParams pState
    genRetireEpoch = (min maxEpochNo) . (1 + epochNo +) <$> arbitrary
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
    [ gen_RegCert
    , gen_RegDelegCert
    ]
      ++ [gen_UnRegCert | hasRegisteredKeys noReward]
      ++ [gen_DelegCert | hasRegisteredKeys unrestricted]
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

-- TODO: this should be doable with constraints, at least for the `VotingProcedures` part
genGovProceedure ::
  era ~ ConwayEra StandardCrypto =>
  PropEnv era ->
  GovEnv era ->
  Proposals era ->
  Gen (GovProcedures era)
genGovProceedure PropEnv {} GovEnv {} _snapshot =
  GovProcedures <$> arbitrary <*> arbitrary

------------------------------------------------------------------------
-- Constraints
------------------------------------------------------------------------

-- TODO: I'm sure this is some form of Iso thing in la-la-lens land
toCompF :: (HasCallStack, Compactible v) => Map.Map k v -> Map.Map k (UM.CompactForm v)
toCompF = fmap $ fromJust . UM.toCompact

fromCompF :: Compactible v => Map.Map k (UM.CompactForm v) -> Map.Map k v
fromCompF = fmap UM.fromCompact

-- RatifyEnv --------------------------------------------------------------

ratifyEnvT :: PropEnv era -> RootTarget (ConwayEra StandardCrypto) (RatifyEnv (ConwayEra StandardCrypto)) (RatifyEnv (ConwayEra StandardCrypto))
ratifyEnvT PropEnv {..} =
  Invert
    "RatifyEnv"
    (typeRep @(RatifyEnv (ConwayEra StandardCrypto)))
    (\sd spd drepd drepst cs -> RatifyEnv (toCompF sd) (PoolDistr spd) (toCompF drepd) drepst peEpochNo cs)
    :$ Lensed
      stakeDistrV
      (lens (fromCompF . reStakeDistr) (\re x -> re {reStakeDistr = toCompF x}))
    :$ Lensed
      stakePoolDistrV
      (lens reStakePoolDistr (\re x -> re {reStakePoolDistr = x}) . unPoolDistrL)
    :$ Lensed
      drepDistrV
      (lens (fromCompF . reDRepDistr) (\re x -> re {reDRepDistr = toCompF x}))
    :$ Lensed
      drepStateV
      (lens reDRepState $ \re x -> re {reDRepState = x})
    :$ Lensed
      committeeStateV
      (lens reCommitteeState $ \re x -> re {reCommitteeState = x})

drepDistrV :: Term (ConwayEra StandardCrypto) (Map.Map (DRep StandardCrypto) Coin)
drepDistrV = Var $ V "drepDistr" (MapR DRepR CoinR) No

stakeDistrV :: Term (ConwayEra StandardCrypto) (Map.Map (Credential 'Staking StandardCrypto) Cardano.Ledger.Coin.Coin)
stakeDistrV = Var $ V "stakeDistr" (MapR CredR CoinR) No

stakePoolDistrV :: Term (ConwayEra StandardCrypto) (Map.Map (KeyHash 'StakePool StandardCrypto) (IndividualPoolStake StandardCrypto))
stakePoolDistrV = Var $ V "stakePoolDistr" (MapR PoolHashR IPoolStakeR) No

drepStateV :: Term (ConwayEra StandardCrypto) (Map.Map (Credential 'DRepRole StandardCrypto) (DRepState StandardCrypto))
drepStateV = Var $ V "drepState" (MapR VCredR DRepStateR) No

committeeStateV :: Term (ConwayEra StandardCrypto) (CommitteeState (ConwayEra StandardCrypto))
committeeStateV = Var $ V "committeeState" CommitteeStateR No

ratifyEnvGenPreds :: Proof era -> [Pred (ConwayEra StandardCrypto)]
ratifyEnvGenPreds _ =
  [ Random drepDistrV
  , Dom stakeDistrV :⊆: credsUniv
  , Random stakePoolDistrV
  , Dom drepStateV :⊆: voteUniv
  , Random committeeStateV
  , Random ppUpdateChildren
  , Random hardForkChildren
  ]

ratifyEnvCheckPreds :: Proof era -> [Pred (ConwayEra StandardCrypto)]
ratifyEnvCheckPreds _ = []

-- RatifyState ------------------------------------------------------------

ratifyStateT :: RootTarget (ConwayEra StandardCrypto) (RatifyState (ConwayEra StandardCrypto)) (RatifyState (ConwayEra StandardCrypto))
ratifyStateT =
  Invert "RatifyState" (typeRep @(RatifyState (ConwayEra StandardCrypto))) RatifyState
    :$ Shift enactStateT (lens rsEnactState $ \rs x -> rs {rsEnactState = x})
    :$ Lensed removedV (lens rsRemoved $ \rs x -> rs {rsRemoved = x})
    :$ Lensed enactedV (lens rsEnacted $ \rs x -> rs {rsEnacted = x})
    :$ Lensed delayedV (lens rsDelayed $ \rs x -> rs {rsDelayed = x})

removedV :: Term (ConwayEra StandardCrypto) (Set.Set (GovActionId StandardCrypto))
removedV = Var $ V "removed" (SetR GovActionIdR) No

enactedV :: Term (ConwayEra StandardCrypto) (Set.Set (GovActionId StandardCrypto))
enactedV = Var $ V "enacted" (SetR GovActionIdR) No

delayedV :: Era era => Term era Bool
delayedV = Var $ V "delayed" BoolR No

-- TODO: it's possible that these should depend on the variables in the environment!
ratifyStateGenPreds :: Proof (ConwayEra StandardCrypto) -> [Pred (ConwayEra StandardCrypto)]
ratifyStateGenPreds p =
  [ Random removedV
  , Random enactedV
  , Random delayedV
  ]
    ++ enactStateGenPreds p

-- TODO: there should probably be more constraints here!
ratifyStateCheckPreds :: Proof era -> [Pred era]
ratifyStateCheckPreds p = enactStateCheckPreds p

-- GovEnv -----------------------------------------------------------------

govEnvT :: forall era. Reflect era => PropEnv era -> RootTarget era (GovEnv era) (GovEnv era)
govEnvT PropEnv {..} =
  Invert "GovEnv" (typeRep @(GovEnv era)) (\txid pgovact -> GovEnv txid peEpochNo pePParams pgovact)
    :$ Lensed txIdV (lens geTxId $ \ge x -> ge {geTxId = x})
    :$ Shift prevGovActionIdsT (lens gePrevGovActionIds $ \ge x -> ge {gePrevGovActionIds = x})

txIdV :: Reflect era => Term era (TxId (EraCrypto era))
txIdV = Var $ V "txId" TxIdR No

govEnvGenPreds :: Reflect era => Proof era -> [Pred era]
govEnvGenPreds p = Random txIdV : prevGovActionIdsGenPreds p

govEnvCheckPreds :: Proof era -> [Pred era]
govEnvCheckPreds p = prevGovActionIdsCheckPreds p

-- GovSnapshots -----------------------------------------------------------

proposalsSnapshotGenPreds :: Era era => Proof era -> [Pred era]
proposalsSnapshotGenPreds _ =
  [ Random prevProposals
  ]

proposalsSnapshotsCheckPreds :: Proof era -> [Pred era]
proposalsSnapshotsCheckPreds _ = []

------------------------------------------------------------------------
-- Properties
------------------------------------------------------------------------

-- Setup ------------------------------------------------------------------

data PropEnv era = PropEnv
  { peNetwork :: Network
  , pePParams :: PParams era
  , peEpochNo :: EpochNo
  , peSlotNo :: SlotNo
  }

extendSub :: Reflect era => PropEnv era -> Subst era -> Subst era
extendSub PropEnv {..} sub =
  ( extend
      (pparamsVar reify)
      (Lit (PParamsR reify) $ PParamsF reify pePParams)
      sub
  )

type GenShrink a = (Gen a, a -> [a])

stsProperty ::
  forall r era env st sig fail.
  ( Reflect era
  , Environment (EraRule r era) ~ env
  , State (EraRule r era) ~ st
  , Signal (EraRule r era) ~ sig
  , PredicateFailure (EraRule r era) ~ fail
  , STS (EraRule r era)
  , BaseM (EraRule r era) ~ ReaderT Globals Identity
  , PrettyA st
  , PrettyA sig
  , PrettyA env
  , PrettyA fail
  ) =>
  Subst era ->
  (PropEnv era -> GenShrink env) ->
  (PropEnv era -> GenShrink st) ->
  (PropEnv era -> st -> GenShrink sig) ->
  (PropEnv era -> env -> st -> sig -> st -> Property) ->
  Property
stsProperty sub genEnv genSt genSig prop =
  lookupTerm sub network $ \peNetwork ->
    lookupTerm sub currentEpoch $ \peEpochNo ->
      lookupTerm sub currentSlot $ \peSlotNo ->
        forAllBlind (pParamsGen sub) $ \(unPParams -> pePParams) ->
          let pEnv = PropEnv {..}
           in uncurry forAllShrinkBlind (genEnv pEnv) $ \env ->
                counterexample (show $ ppString "env = " <> prettyA env) $
                  uncurry forAllShrinkBlind (genSt pEnv) $ \st ->
                    counterexample (show $ ppString "st = " <> prettyA st) $
                      uncurry forAllShrinkBlind (genSig pEnv st) $ \sig ->
                        counterexample (show $ ppString "sig = " <> prettyA sig) $
                          runShelleyBase $ do
                            res <- applySTS @(EraRule r era) $ TRC (env, st, sig)
                            pure $ case res of
                              Left pfailures -> counterexample (show $ ppList prettyA pfailures) $ property False
                              Right st' ->
                                counterexample (show $ ppString "st' = " <> prettyA st') $
                                  prop pEnv env st sig st'

checkConstraints ::
  era ~ ConwayEra StandardCrypto =>
  Proof era ->
  Subst (ConwayEra StandardCrypto) ->
  (Proof era -> [Pred era]) ->
  RootTarget era s t ->
  s ->
  Property
checkConstraints proof sub preds target val =
  let cs = applySubst sub $ preds proof
      env = saturateEnv (getTarget val target emptyEnv) cs
      check p = counterexample ("Failed: " ++ showPred p) $ ensureTyped (runPred env p) id
   in counterexample ("-- Solution --\n" ++ showEnv env) $
        conjoin $
          map check cs

-- STS properties ---------------------------------------------------------

prop_GOVCERT :: Subst (ConwayEra StandardCrypto) -> Property
prop_GOVCERT sub =
  stsProperty @"GOVCERT"
    sub
    (\PropEnv {..} -> (pure $ ConwayGovCertEnv pePParams peEpochNo, const []))
    ( \pe ->
        genShrinkFromConstraints
          conwayProof
          (extendSub pe sub)
          vstateGenPreds
          (\p -> (Vars.drepDeposit p :=: Lit CoinR (Coin 7)) : vstateCheckPreds p)
          vstateT
    )
    -- drepDeposit is defined in the PParams stage, which is not used here
    -- TODO: this should eventually be replaced by constraints based generator
    (\PropEnv {..} st -> (genConwayGovCert pePParams st, shrinkConwayGovCert pePParams st))
    $ \pe _env _st _sig st' -> checkConstraints conwayProof (extendSub pe sub) vstateCheckPreds vstateT st'

prop_POOL :: Subst (ConwayEra StandardCrypto) -> Property
prop_POOL sub =
  stsProperty @"POOL"
    sub
    (\PropEnv {..} -> (pure $ PoolEnv peSlotNo pePParams, const []))
    ( \pe ->
        genShrinkFromConstraints
          conwayProof
          (extendSub pe sub)
          pstateGenPreds
          pstateCheckPreds
          pstateT
    )
    -- TODO: this should eventually be replaced by constraints based generator
    (\PropEnv {..} st -> (genPoolCert peNetwork peEpochNo pePParams st, const []))
    $ \pe _env _st _sig st' -> checkConstraints conwayProof (extendSub pe sub) pstateCheckPreds pstateT st'

prop_DELEG :: Subst (ConwayEra StandardCrypto) -> Property
prop_DELEG sub =
  stsProperty @"DELEG"
    sub
    (\PropEnv {..} -> (pure $ pePParams, const []))
    (\pe -> genShrinkFromConstraints conwayProof (extendSub pe sub) certStateGenPreds certStateCheckPreds dstateT)
    -- TODO: this should eventually be replaced by constraints based generator
    (\PropEnv {..} st -> (genDelegCert pePParams st, const []))
    $ \pe _env _st _sig st' -> checkConstraints conwayProof (extendSub pe sub) certStateCheckPreds dstateT st'

prop_ENACT :: Subst (ConwayEra StandardCrypto) -> Property
prop_ENACT sub =
  stsProperty @"ENACT"
    sub
    (const (pure (), const []))
    (\pe -> genShrinkFromConstraints conwayProof (extendSub pe sub) enactStateGenPreds enactStateCheckPreds enactStateT)
    -- TODO: this is a bit suspect, there are preconditions on these signals in the spec so we
    -- shouldn't expect this to go through so easily.
    (\_ _ -> (EnactSignal <$> arbitrary <*> arbitrary, const []))
    $ \pe _env _st _sig st' -> checkConstraints conwayProof (extendSub pe sub) enactStateCheckPreds enactStateT st'

prop_RATIFY :: Subst (ConwayEra StandardCrypto) -> Property
prop_RATIFY sub =
  stsProperty @"RATIFY"
    sub
    (\pe -> genShrinkFromConstraints conwayProof (extendSub pe sub) ratifyEnvGenPreds ratifyEnvCheckPreds (ratifyEnvT pe))
    (\pe -> genShrinkFromConstraints conwayProof (extendSub pe sub) ratifyStateGenPreds ratifyStateCheckPreds ratifyStateT)
    (\_ _ -> (RatifySignal <$> arbitrary, const []))
    -- TODO: we should probably check more things here
    $ \pe _env _st _sig st' -> checkConstraints conwayProof (extendSub pe sub) ratifyStateCheckPreds ratifyStateT st'

prop_GOV :: Subst (ConwayEra StandardCrypto) -> Property
prop_GOV sub =
  stsProperty @"GOV"
    sub
    (\pe -> genShrinkFromConstraints conwayProof (extendSub pe sub) govEnvGenPreds govEnvCheckPreds (govEnvT pe))
    (\pe -> genShrinkFromConstraints conwayProof (extendSub pe sub) proposalsSnapshotGenPreds proposalsSnapshotsCheckPreds govRuleStateT)
    (\_ _ -> (arbitrary, const []))
    -- TODO: we should probably check more things here
    $ \pe _env _st _sig st' -> checkConstraints conwayProof (extendSub pe sub) proposalsSnapshotsCheckPreds govRuleStateT st'

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
      , testProperty "prop_ENACT" $ idempotentIOProperty (prop_ENACT <$> io_subst)
      , testProperty "prop_RATIFY" $ idempotentIOProperty (prop_RATIFY <$> io_subst)
      -- , testProperty "prop_GOV" $ idempotentIOProperty (prop_GOV <$> io_subst)
      ]

------------------------------------------------------------------------
-- Development helpers
------------------------------------------------------------------------

runTest :: (Subst (ConwayEra StandardCrypto) -> Property) -> IO ()
runTest prop = do
  univ <- genUniverse
  quickCheck $ prop univ
