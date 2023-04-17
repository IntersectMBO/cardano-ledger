{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- Orphan Arbitrary instance for OrderInfo
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Constrained.Tests where

import Control.Arrow (first)
import Control.Monad
import Data.Foldable (fold)
import Data.Group
import Data.List (intercalate, isPrefixOf)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (subtract)

import Test.Cardano.Ledger.Constrained.Ast

import Cardano.Ledger.Coin
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.Shelley
import Test.Cardano.Ledger.Constrained.Classes
import Test.Cardano.Ledger.Constrained.Combinators
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad
import Test.Cardano.Ledger.Constrained.Rewrite
import Test.Cardano.Ledger.Constrained.Shrink
import Test.Cardano.Ledger.Constrained.Size (OrdCond (..), Size (SzRng), runOrdCond)
import Test.Cardano.Ledger.Constrained.Solver
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Generic.Proof (Standard)
import Test.QuickCheck hiding (getSize, total)

{-

One property that we'd like to test is the soundness property for the constraint solver: if it
successfully generates a solution to a set of constraints, this solution *actually* satisfies the
constraints.

In order to test this we need to generate random satisfiable constraints. The approach we take is to
generate constraints in a given fixed environment, and take care to only generate ones that hold in
this particular environment.

To ensure we generate constraints that have a linear dependency graph we keep track of which
variables in the environment are solved by an already generated constraint and the "depth" at which
it was solved. When generating constraints between solved variables we require the depth to respect
the dependency order. For instance, a constraints S ⊆ T (assuming we solve supersets before subsets)
must have `depthOf T < depthOf S`.

This doesn't always work, because the solver substitutes equalities in a way that does not respect
our dependencies (`Rewrite.removeEqual`)

The soundness property discards cases where we fail to find a solution to the constraints, but it's
still interesting to know when this happens, since we try our best to generate solvable constraints.
There is a strict version of the property (`prop_soundness' True`) that fails instead. Currently it
fails in these cases:
  - When the existence of a solution to a later variable depends on the value picked for an earlier
    variable. For instance, [Sized 3 A, B ⊆ A, C ⊆ B, Sized 1 C]. Here B needs to be solved with a
    non-empty set for C to have a solution.

Current limitations of the tests
  - Only CoinR and SetR CoinR types (and Word64R for size constraints)
  - Only generates Sized, Subset, Disjoint, and SumsTo(SumSet) constraints

Known limitations of the code that the tests avoid
  - Can't solve `Sized n (Rng X)` (see TODO/sizedRng)
  - Random X, N < Σ sum X can pick X = {} (N :: Coin) and then crash on the sum constraint
    (TODO/negativeCoin)

-}

-- Generators ---

data GenEnv era = GenEnv
  { gOrder :: OrderInfo
  , gEnv :: Env era
  , gSolved :: Map (Name era) Depth
  -- ^ Which variables in the environment have we "solved", i.e. generated
  --   a constraint that allows solving them. Also track the "depth" of
  --   the variable in the dependency order (depth = 1 + depth of
  --   dependencies).
  }
  deriving (Show)

type Depth = Int

instance Arbitrary OrderInfo where
  arbitrary = OrderInfo <$> arbitrary <*> arbitrary <*> arbitrary
  shrink info = [standardOrderInfo | info /= standardOrderInfo]

addVar :: V era t -> t -> GenEnv era -> GenEnv era
addVar var val env = env {gEnv = storeVar var val $ gEnv env}

markSolved :: Set (Name era) -> Depth -> GenEnv era -> GenEnv era
markSolved solved d env = env {gSolved = Map.unionWith max new (gSolved env)}
  where
    new = Map.fromSet (const d) solved

addSolvedVar :: V era t -> t -> Depth -> GenEnv era -> GenEnv era
addSolvedVar var val d = markSolved (Set.singleton $ Name var) d . addVar var val

depthOfName :: GenEnv era -> Name era -> Depth
depthOfName env x = Map.findWithDefault 0 x (gSolved env)

depthOf :: GenEnv era -> Term era t -> Depth
depthOf env t = maximum $ 0 : map (depthOfName env) (Set.toList $ vars t)

depthOfSum :: GenEnv era -> Sum era c -> Depth
depthOfSum env = \case
  SumMap t -> depthOf env t
  SumList t -> depthOf env t
  One t -> depthOf env t
  Project _ t -> depthOf env t

genLiteral :: forall era t. Era era => GenEnv era -> Rep era t -> Gen t
genLiteral env rep =
  case rep of
    SetR erep -> setLiteral erep
    MapR _ _ -> unconstrained rep -- TODO: more clever generation for maps
    _ -> unconstrained rep
  where
    unconstrained :: forall a. Rep era a -> Gen a
    unconstrained r = genRep r

    setLiteral :: forall a. Ord a => Rep era a -> Gen (Set a)
    setLiteral erep = do
      let knownSets = [val | (_, val) <- envVarsOfType (gEnv env) (SetR erep)]
          gen = oneof $ genRep (SetR erep) : map pure knownSets
      set1 <- gen
      set2 <- gen
      op <- elements [const, const id, Set.union, Set.intersection, Set.difference, flip Set.difference]
      pure $ op set1 set2

genFreshVarName :: GenEnv era -> Gen String
genFreshVarName = elements . varNames
  where
    allTheNames = [s ++ [c] | s <- "" : allTheNames, c <- ['A' .. 'Z']]
    varNames env = take 10 $ filter (`Map.notMember` vmap) allTheNames
      where
        Env vmap = gEnv env

envVarsOfType :: Env era -> Rep era t -> [(V era t, t)]
envVarsOfType (Env env) rep = concatMap wellTyped $ Map.toList env
  where
    wellTyped (name, Payload rep' val access) =
      case testEql rep rep' of
        Just Refl -> [(V name rep access, val)]
        Nothing -> []

data VarSpec
  = -- | Must contain a variable (either unsolved, or solved at least the given depth).
    --   Requiring a minimum depth let's us avoid introducing cycles in
    --   already solved variables.
    VarTerm Depth
  | -- | Can only use solved variables
    KnownTerm
  deriving (Eq, Show)

genTerm :: Era era => GenEnv era -> Rep era t -> VarSpec -> Gen (Term era t, GenEnv era)
genTerm env rep vspec = genTerm' env rep (const True) (genLiteral env rep) vspec

genTerm' :: forall era t. Era era => GenEnv era -> Rep era t -> (t -> Bool) -> Gen t -> VarSpec -> Gen (Term era t, GenEnv era)
genTerm' env rep valid genLit vspec =
  frequency $
    [(5, genFixed) | vspec == KnownTerm]
      ++ [(5, genExistingVar) | not $ null allowedVars]
      ++ [(1, genFreshVar) | VarTerm {} <- [vspec]]
      ++ [(2, genDom krep) | SetR krep <- [rep]]
      ++ [(2, genRng vrep) | SetR vrep <- [rep]]
  where
    isValid (_, val) = valid val
    existingVars = map fst $ filter isValid $ envVarsOfType (gEnv env) rep
    allowedVars = case vspec of
      VarTerm d -> filter ((>= d) . depthOfName env . Name) existingVars
      KnownTerm -> filter ((`Map.member` gSolved env) . Name) existingVars

    genFixed = (,env) . Lit rep <$> genLit
    genExistingVar = (,env) . Var <$> elements allowedVars

    genFreshVar = do
      name <- genFreshVarName env
      val <- genLit
      let var = V name rep No
      pure (Var var, addVar var val env)

    genDom :: forall k. (t ~ Set k, Ord k) => Rep era k -> Gen (Term era (Set k), GenEnv era)
    genDom krep = do
      TypeInEra vrep <- genValType
      (m, env') <-
        genTerm'
          env
          (MapR krep vrep)
          (valid . Map.keysSet)
          (genLit >>= genMapLiteralWithDom env vrep)
          vspec
      pure (Dom m, env')

    genRng :: forall v. (t ~ Set v, Ord v) => Rep era v -> Gen (Term era (Set v), GenEnv era)
    genRng vrep = do
      TypeInEra krep <- genKeyType
      (m, env') <-
        genTerm'
          env
          (MapR krep vrep)
          (valid . Set.fromList . Map.elems)
          (genLit >>= genMapLiteralWithRng env krep)
          vspec
      pure (Rng m, env')

genMapLiteralWithDom :: Era era => GenEnv era -> Rep era v -> Set k -> Gen (Map k v)
genMapLiteralWithDom env vrep keys = do
  let genVal = genLiteral env vrep
  traverse (\_ -> genVal) (Map.fromSet (const ()) keys)

genMapLiteralWithRng :: (Era era, Ord k) => GenEnv era -> Rep era k -> Set v -> Gen (Map k v)
genMapLiteralWithRng env krep vals = do
  keys <- setSized [] (Set.size vals) (genLiteral env krep)
  valsList <- shuffle $ Set.toList vals
  pure $ Map.fromList $ zip (Set.toList keys) valsList

data TypeInEra era where
  TypeInEra :: (Show t, Ord t) => Rep era t -> TypeInEra era

genKeyType :: Gen (TypeInEra era)
genKeyType = elements [TypeInEra IntR]

genValType :: Gen (TypeInEra era)
genValType = elements [TypeInEra CoinR, TypeInEra DeltaCoinR] -- Possible future use , TypeInEra (PairR CoinR CoinR)]

genBaseType :: Gen (TypeInEra era)
genBaseType = oneof [genKeyType, genValType]

genType :: Gen (TypeInEra era)
genType =
  oneof
    [ genBaseType
    , setR <$> genValType
    , listR <$> genValType
    , mapR <$> genKeyType <*> genValType
    ]
  where
    setR (TypeInEra t) = TypeInEra (SetR t)
    listR (TypeInEra t) = TypeInEra (ListR t)
    mapR (TypeInEra s) (TypeInEra t) = TypeInEra (MapR s t)

-- | Unsatisfiable constraint returned if we fail during constraint generation.
errPred :: [String] -> Pred era
errPred errs = Lit (ListR stringR) ["Errors:"] :=: Lit (ListR stringR) errs

-- This is very ad hoc
setWithSum :: Int -> Gen (Set Int)
setWithSum n = fixUp <$> arbitrary
  where
    fixUp s
      | missing == 0 = s
      | otherwise =
          if Set.notMember missing s
            then Set.insert missing s
            else Set.singleton n
      where
        missing = n - sum s

listWithSum :: Coin -> Gen [Coin]
listWithSum n = fixUp <$> arbitrary
  where
    fixUp s
      | missing == Coin 0 = s
      | otherwise = missing : s
      where
        missing = n ~~ fold s

mapWithSum :: Coin -> Gen (Map Coin Coin)
mapWithSum n = do
  rng <- listWithSum n
  dom <- Set.toList <$> setSized [] (length rng) arbitrary
  pure $ Map.fromList $ zip dom rng

genFromOrdCond :: (Arbitrary c, Adds c) => OrdCond -> Bool -> c -> Gen c
genFromOrdCond EQL _ n = pure n
genFromOrdCond cond canBeNegative n =
  suchThatErr
    ["genFromOrdCond " ++ show cond ++ " " ++ show n]
    ( frequency $
        [(1, pure $ minus ["genFromOrdCond"] n (fromI [] 1)) | n > zero || canBeNegative]
          ++ [ (1, pure n)
             , (1, pure $ add n (fromI [] 1))
             , (10, arbitrary)
             ]
    )
    (flip (runOrdCond cond) n)

genPredicate :: forall era. Era era => GenEnv era -> Gen (Pred era, GenEnv era)
genPredicate env =
  frequency $
    [(1, fixedSizedC)]
      ++ [(1, varSizedC)]
      ++ [(1, eqC)]
      ++ [(1, subsetC)]
      ++ [(1, disjointC)]
      ++ [(1, sumsToC)]
      ++ [(1, hasDomC)]
  where
    withValue ::
      forall t.
      Gen (Term era t, GenEnv era) ->
      (Term era t -> t -> GenEnv era -> Gen (Pred era, GenEnv era)) ->
      Gen (Pred era, GenEnv era)
    withValue genTm k = do
      (tm, env') <- genTm
      case runTyped $ runTerm (gEnv env') tm of
        Left errs -> pure (errPred errs, env')
        Right val -> k tm val env'

    goodSized (Sized _ Rng {}, _) = False -- TODO/sizedRng
    goodSized _ = True

    -- Fixed size
    fixedSizedC = flip suchThat goodSized $ do
      TypeInEra rep <- genValType
      withValue (genTerm env (SetR rep) (VarTerm 1)) $ \set val env' ->
        let n = ExactSize (getSize val)
         in pure (Sized n set, markSolved (vars set) 1 env')

    -- Fresh variable for size.
    varSizedC = do
      TypeInEra rep <- genValType
      withValue (genTerm env (SetR rep) KnownTerm) $ \set val env' -> do
        name <- genFreshVarName env'
        let var = V name SizeR No
            intn = getSize val
        pure (Sized (Var var) set, addSolvedVar var (SzRng intn intn) (1 + depthOf env' set) env')

    eqC = do
      TypeInEra rep <- genType
      withValue (genTerm env rep KnownTerm) $ \lhs val env' -> do
        let d = 1 + depthOf env' lhs
        (rhs, env'') <- genTerm' env' rep (== val) (pure val) (VarTerm d)
        pure (lhs :=: rhs, markSolved (vars rhs) d env'')

    subsetC
      | setBeforeSubset (gOrder env) = do
          -- Known superset
          TypeInEra rep <- genValType
          withValue (genTerm env (SetR rep) KnownTerm) $ \sup val env' -> do
            let d = 1 + depthOf env' sup
            (sub, env'') <-
              genTerm'
                env'
                (SetR rep)
                (`Set.isSubsetOf` val)
                (subsetFromSet ["From genPredicate subsetC"] val)
                (VarTerm d)
            pure (sub `Subset` sup, markSolved (vars sub) d env'')
      | otherwise = do
          -- Known subset
          TypeInEra rep <- genValType
          withValue (genTerm env (SetR rep) KnownTerm) $ \sub val env' -> do
            let d = 1 + depthOf env' sub
            (sup, env'') <-
              genTerm'
                env'
                (SetR rep)
                (Set.isSubsetOf val)
                (Set.union val <$> genRep (SetR rep))
                (VarTerm d)
            pure (sub `Subset` sup, markSolved (vars sup) d env'')

    -- Disjoint, left KnownTerm and right VarTerm
    disjointC = do
      TypeInEra rep <- genValType
      withValue (genTerm env (SetR rep) KnownTerm) $ \lhs val env' -> do
        let d = 1 + depthOf env' lhs
        (rhs, env'') <-
          genTerm'
            env'
            (SetR rep)
            (Set.disjoint val)
            ((`Set.difference` val) <$> genRep (SetR rep))
            (VarTerm d)
        pure (Disjoint lhs rhs, markSolved (vars rhs) d env'')

    -- SumsTo constraint, only Map Int Int at the moment.
    sumsToC
      | sumBeforeParts (gOrder env) =
          -- Known sum
          withValue (genTerm' env CoinR (> Coin 10) ((<> Coin 10) <$> arbitrary) KnownTerm) $ \sumTm val env' -> do
            let d = 1 + depthOf env' sumTm

                genParts [] env0 = pure ([], env0)
                genParts (n : ns) env0 = do
                  (t, env1) <-
                    genTerm'
                      env0
                      (MapR CoinR CoinR)
                      ((== n) . fold)
                      (mapWithSum n)
                      (VarTerm d)
                  first (SumMap t :) <$> genParts ns env1
            -- TODO: It's unclear what the requirements are on the parts when solving sumBeforeParts.
            -- The solver fails if you have multiple unknown variables. Generating only a single part
            -- for now, since sumBeforeParts is anyway disabled due to TODO/SumSet.
            -- count <- choose (1, min 3 val)
            let count = 1
            small <- genSmall @Coin
            partSums <- partition (fromI [] small) ["sumdToC in Tests.hs"] count val
            (parts, env'') <- genParts partSums env'
            -- At some point we should generate a random TestCond other than EQL
            pure (SumsTo (fromI [] small) sumTm EQL parts, markSolved (foldMap (varsOfSum mempty) parts) d env'')
      | otherwise =
          oneof
            [ sumCKnownSets CoinR False
            , sumCKnownSets DeltaCoinR True
            ]

    -- Avoid constraints where the same variable appears in multiple parts, since we can't solve
    -- these when the variable is unknown.
    uniqueVars (SumsTo _ _ _ parts) =
      and
        [ Set.disjoint us vs
        | us : vss <- List.tails $ map (varsOfSum mempty) parts
        , vs <- vss
        ]
    uniqueVars _ = True

    sumCKnownSets ::
      forall c.
      (Adds c, Arbitrary c) =>
      Rep era c ->
      Bool ->
      Gen (Pred era, GenEnv era)
    sumCKnownSets rep canBeNegative = flip suchThat (uniqueVars . fst) $ do
      let genParts 0 env0 k = k [] zero env0
          genParts n env0 k = do
            TypeInEra krep <- genKeyType
            withValue (genTerm env0 (MapR krep rep) KnownTerm) $ \expr val env1 ->
              genParts (n - 1) env1 $ \parts tot ->
                k (SumMap expr : parts) (add (sumAdds val) tot)
      count <- choose (1, 3 :: Int)
      genParts count env $ \parts tot env' -> do
        cmp <- elements $ [EQL, LTE, GTH, GTE] ++ [LTH | canBeNegative] -- TODO/negativeCoin
        let d = 1 + maximum (0 : map (depthOfSum env') parts)
        (sumTm, env'') <-
          genTerm'
            env'
            rep
            (flip (runOrdCond cmp) tot)
            (genFromOrdCond cmp canBeNegative tot)
            (VarTerm d)
        small <- genSmall @c
        pure (SumsTo (fromI [] small) sumTm cmp parts, markSolved (vars sumTm) d env'')

    hasDomC =
      oneof
        [ do
            -- Known map
            TypeInEra krep <- genKeyType
            TypeInEra vrep <- genValType
            withValue (genTerm env (MapR krep vrep) KnownTerm) $ \mapTm val env' -> do
              let d = 1 + depthOf env' mapTm
              (domTm, env'') <-
                genTerm'
                  env'
                  (SetR krep)
                  (== Map.keysSet val)
                  (pure $ Map.keysSet val)
                  (VarTerm d)
              pure (Dom mapTm :=: domTm, env'')
        , do
            -- Known domain
            TypeInEra krep <- genKeyType
            TypeInEra vrep <- genValType
            withValue (genTerm env (SetR krep) KnownTerm) $ \domTm val env' -> do
              let d = 1 + depthOf env' domTm
              (mapTm, env'') <-
                genTerm'
                  env'
                  (MapR krep vrep)
                  ((val ==) . Map.keysSet)
                  (genMapLiteralWithDom env vrep val)
                  (VarTerm d)
              pure (domTm :=: Dom mapTm, env'')
        ]

genPreds :: Era era => GenEnv era -> Gen ([Pred era], GenEnv era)
genPreds = \env -> do
  n <- choose (1, 40)
  loop (n :: Int) env
  where
    loop n env
      | n == 0 = pure ([], env)
      | otherwise = do
          (pr, env') <- genPredicate env
          first (pr :) <$> loop (n - 1) env'

withEq :: Rep era t -> (Eq t => a) -> Maybe a
withEq (SetR r) cont = withEq r cont
withEq (MapR kr vr) cont = join $ withEq kr (withEq vr cont)
withEq CoinR cont = Just cont
withEq _ _ = Nothing

-- We can't drop constraints due to dependency limitations. There needs to be at least one
-- constraint to solve each variable. We can replace constraints by Random though!
shrinkPreds :: ([Pred era], GenEnv era) -> [([Pred era], GenEnv era)]
shrinkPreds (preds, env) =
  [ (preds', env')
  | preds' <- shrinkList shrinkPred preds
  , let defined = foldMap def preds'
        env' = env {gEnv = pruneEnv defined (gEnv env)}
  , depCheck mempty preds'
  ]
  where
    -- Shrink to a Random constraint defining the same variables. The makes sure we respect the
    -- OrderInfo.
    shrinkPred Random {} = []
    shrinkPred c = filter (checkDefs c) $ shrinkToValue c ++ shrinkToRandom c

    checkDefs orig shrunk = def orig == def shrunk && not (null $ def shrunk)

    shrinkToValue (Lit {} :=: Var {}) = []
    shrinkToValue c =
      [ c'
      | Name x@(V _ r _) <- Set.toList $ def c
      , Right v <- [runTyped $ runTerm (gEnv env) (Var x)]
      , Just c' <- [withEq r $ Lit r v :=: Var x]
      ]

    shrinkToRandom c = [r | r@(Random Var {}) <- randoms c]

    randoms (Sized n t) = [Random n, Random t]
    randoms (s `Subset` t) = [Random s, Random t]
    randoms (s :=: t) = [Random s, Random t]
    randoms (Disjoint s t) = [Random s, Random t]
    randoms _ = []

    pruneEnv defs (Env vmap) = Env $ Map.filterWithKey (\name _ -> Set.member name defNames) vmap
      where
        defNames = Set.map (\(Name (V name _ _)) -> name) defs

    depCheck _ [] = True
    depCheck solved preds'
      | null rdy = False
      | otherwise = depCheck (foldMap def rdy <> solved) rest
      where
        (rdy, rest) = List.partition canSolve preds'
        canSolve c = Set.isSubsetOf (use c) solved

    deps c = accumdep (gOrder env) mempty c
    def = Map.keysSet . deps
    use = fold . deps

-- Tests ---

type TestEra = ShelleyEra Standard

testProof :: Proof TestEra
testProof = Shelley Standard

testEnv :: Env TestEra
testEnv = Env $ Map.fromList [("A", Payload CoinR (Coin 5) No)]

ensureRight :: Testable prop => Either [String] a -> (a -> prop) -> Property
ensureRight (Left errs) _ = counterexample (unlines errs) False
ensureRight (Right x) prop = property $ prop x

ifRight :: Testable prop => Either [String] a -> (a -> prop) -> Property
ifRight Left {} _ = False ==> False
ifRight (Right x) prop = property $ prop x

ensureTyped :: Testable prop => Typed a -> (a -> prop) -> Property
ensureTyped = ensureRight . runTyped

ifTyped :: Testable prop => Typed a -> (a -> prop) -> Property
ifTyped = ifRight . runTyped

initEnv :: OrderInfo -> GenEnv TestEra
initEnv info =
  GenEnv
    { gOrder = info
    , gEnv = emptyEnv
    , gSolved = mempty
    }

showVal :: Rep era t -> t -> String
showVal (SetR r) t = "{" ++ intercalate ", " (map (synopsis r) (Set.toList t)) ++ "}"
showVal (MapR kr vr) t = "{" ++ intercalate ", " [synopsis kr k ++ " -> " ++ synopsis vr v | (k, v) <- Map.toList t] ++ "}"
showVal rep t = synopsis rep t

showTerm :: Term era t -> String
showTerm (Lit rep t) = showVal rep t
showTerm (Dom t) = "(Dom " ++ showTerm t ++ ")"
showTerm (Rng t) = "(Rng " ++ showTerm t ++ ")"
showTerm t = show t

showPred :: Pred era -> String
showPred (sub `Subset` sup) = showTerm sub ++ " ⊆ " ++ showTerm sup
showPred (sub :=: sup) = showTerm sub ++ " == " ++ showTerm sup
showPred (Disjoint s t) = "Disjoint " ++ showTerm s ++ " " ++ showTerm t
showPred pr = show pr

showEnv :: Env era -> String
showEnv (Env vmap) = unlines $ map pr (Map.toList vmap)
  where
    pr (name, Payload rep t _) = name ++ " :: " ++ show rep ++ " -> " ++ showVal rep t

predConstr :: Pred era -> String
predConstr Sized {} = "Sized"
predConstr (_ :=: _) = ":=:"
predConstr (_ `Subset` _) = "Subset"
predConstr Disjoint {} = "Disjoint"
predConstr SumsTo {} = "SumsTo"
predConstr Random {} = "Random"
predConstr Component {} = "Component"
predConstr CanFollow {} = "CanFollow"
predConstr Member {} = "Member"
predConstr NotMember {} = "NotMember"
predConstr MapMember {} = "MapMember"
predConstr NotMapMember {} = "NotMapMember"
predConstr (:<-:) {} = ":<-:"
predConstr List {} = "List"
predConstr Choose {} = "Choose"
predConstr Maybe {} = "Maybe"
predConstr GenFrom {} = "GenFrom"

constraintProperty :: Maybe Int -> Bool -> [String] -> OrderInfo -> ([Pred TestEra] -> DependGraph TestEra -> Env TestEra -> Property) -> Property
constraintProperty timeout strict whitelist info prop =
  forAllShrink (genPreds $ initEnv info) shrinkPreds $ \(preds, genenv) ->
    counterexample ("\n-- Order --\n" ++ show info) $
      counterexample ("\n-- Constraints --\n" ++ unlines (map showPred preds)) $
        counterexample ("-- Model solution --\n" ++ showEnv (gEnv genenv)) $
          maybe id within timeout $
            checkTyped (compile info preds) $ \graph ->
              forAllBlind (genDependGraph False testProof graph) . flip checkRight $ \subst ->
                let env = errorTyped $ substToEnv subst emptyEnv
                    n = let Env e = gEnv genenv in Map.size e
                 in tabulate "Var count" [show n] $
                      tabulate "Constraint types" (map predConstr preds) $
                        prop preds graph env
  where
    checkTyped
      | strict = checkWhitelist . runTyped
      | otherwise = ifTyped
    checkRight
      | strict = checkWhitelist
      | otherwise = ifRight

    checkWhitelist :: Testable prop => Either [String] a -> (a -> prop) -> Property
    checkWhitelist (Left errs) _ =
      and
        [ not $ isPrefixOf white err
        | white <- whitelist
        , err <- errs
        ]
        ==> counterexample (unlines errs) False
    checkWhitelist (Right x) k = property $ k x

checkPredicates :: [Pred TestEra] -> Env TestEra -> Property
checkPredicates preds env =
  counterexample ("-- Solution --\n" ++ showEnv env) $
    conjoin $
      map checkPred preds
  where
    checkPred pr = counterexample ("Failed: " ++ showPred pr) $ ensureTyped (runPred env pr) id

runPreds :: [Pred TestEra] -> IO ()
runPreds ps = do
  let info = standardOrderInfo
  Right g <- pure $ runTyped $ compile info ps
  subst <- generate $ genDependGraph True testProof g
  print subst

-- | Generate a set of satisfiable constraints and check that we can generate a solution and that it
--   actually satisfies the constraints.
prop_soundness :: OrderInfo -> Property
prop_soundness = prop_soundness' False []

defaultWhitelist :: [String]
defaultWhitelist = ["Size specifications", "The SetSpec's are inconsistent", "The MapSpec's are inconsistent"]

-- | If argument is True, fail property if constraints cannot be solved. Otherwise discard unsolved
--   constraints.
prop_soundness' :: Bool -> [String] -> OrderInfo -> Property
prop_soundness' strict whitelist info =
  constraintProperty (Just 100000) strict whitelist info $ \preds _graph env ->
    checkPredicates preds env

-- | Check that shrinking is sound, i.e. that all shrink steps preserves constraint satisfaction.
prop_shrinking :: OrderInfo -> Property
prop_shrinking = prop_shrinking' False []

prop_shrinking' :: Bool -> [String] -> OrderInfo -> Property
prop_shrinking' strict whitelist info =
  constraintProperty Nothing strict whitelist info $ \preds graph env ->
    counterexample ("-- Original solution --\n" ++ showEnv env) $
      let envs = shrinkEnv graph env
       in tabulate "Shrinkings" [show $ length envs] $
            conjoin $
              map (checkPredicates preds) envs
