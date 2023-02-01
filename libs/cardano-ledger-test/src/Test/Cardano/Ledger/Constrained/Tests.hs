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
import Data.Foldable (fold)
import Data.List (intercalate, partition)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Test.Cardano.Ledger.Constrained.Ast

import Test.Cardano.Ledger.Constrained.Env
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.Shelley
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes
import Test.Cardano.Ledger.Constrained.Classes hiding (partition)
import Test.Cardano.Ledger.Constrained.Combinators
import Test.Cardano.Ledger.Constrained.Monad
import Test.Cardano.Ledger.Constrained.Spec
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Generic.Proof (Evidence (..), Proof (..))
import Test.QuickCheck hiding (Fixed, total)

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

The soundness property discards cases where we fail to find a solution to the constraints, but it's
still interesting to know when this happens, since we try our best to generate solvable constraints.
There is a strict version of the property (`prop_soundness' True`) that fails instead. Currently it
fails in these cases:
  - When the existence of a solution to a later variable depends on the value picked for an earlier
    variable. For instance, [Sized 3 A, B ⊆ A, C ⊆ B, Sized 1 C]. Here B needs to be solved with a
    non-empty set for C to have a solution.

Current limitations of the tests
  - Only IntR and SetR IntR types (and Word64R for size constraints)
  - Only generates Sized, :<=:, Disjoint, and SumsTo(SumSet) constraints

Known limitations of the code that the tests avoid
  - Constraints of the form `Sized X t` cannot be solved (with X unknown), even with sizeBeforeArg =
    False. On the other hand `Sized n X` is no problem regardless of the value of sizeBeforeArg. See
    TODO/sizeBeforeArg for where these constraints a disabled in the tests.
  - Superset constraints cannot be solved, meaning `setBeforeSubset` is always set to True (see
    TODO/supersetConstraints).
  - The generator for SumSet is wrong. It does `Set.fromList` on a partitioned list which doesn't
    account for duplicate elements (see TODO/SumSet).

-}

-- Generators ---

data GenEnv era = GenEnv { gOrder  :: OrderInfo
                         , gEnv    :: Env era
                         , gSolved :: Map (Name era) Depth
                            -- ^ Which variables in the environment have we "solved", i.e. generated
                            --   a constraint that allows solving them. Also track the "depth" of
                            --   the variable in the dependency order (depth = 1 + depth of
                            --   dependencies).
                         }
  deriving Show

type Depth = Int

instance Arbitrary OrderInfo where
  arbitrary   = do
    info <- OrderInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- TODO/supersetConstraints: RngSpec can't represent superset constraints at the moment, so
    -- setBeforeSubset = False doesn't work.
    -- TODO/SumSet: The generator for a Set with a given sum doesn't work (it doesn't account for
    -- duplicate elements), so we can't have sumBeforeParts.
    pure info{ setBeforeSubset = True
             , sumBeforeParts  = False }
  shrink info = [ standardOrderInfo | info /= standardOrderInfo ]

addVar :: V era t -> t -> GenEnv era -> GenEnv era
addVar var val env = env{ gEnv = storeVar var val $ gEnv env }

markSolved :: Set (Name era) -> Depth -> GenEnv era -> GenEnv era
markSolved solved d env = env{ gSolved = Map.unionWith max new (gSolved env) }
  where new = Map.fromSet (const d) solved

addSolvedVar :: V era t -> t -> Depth -> GenEnv era -> GenEnv era
addSolvedVar var val d = markSolved (Set.singleton $ Name var) d . addVar var val

depthOfName :: GenEnv era -> Name era -> Depth
depthOfName env x = Map.findWithDefault 0 x (gSolved env)

depthOf :: GenEnv era -> Term era t -> Depth
depthOf env t = maximum $ 0 : map (depthOfName env) (Set.toList $ vars t)

depthOfSum :: GenEnv era -> Sum era c -> Depth
depthOfSum env = \ case
  SumMap t  -> depthOf env t
  SumSet t  -> depthOf env t
  SumList t -> depthOf env t
  One t     -> depthOf env t
  Project t -> depthOf env t

genLiteral :: forall era t. Era era => GenEnv era -> Rep era t -> Gen (Literal era t)
genLiteral env rep =
  case rep of
    SetR erep -> setLiteral erep
    _         -> unconstrained rep
  where
    unconstrained :: forall a. Rep era a -> Gen (Literal era a)
    unconstrained r = Lit r <$> genRep r

    setLiteral :: forall a. Ord a => Rep era a -> Gen (Literal era (Set a))
    setLiteral erep = do
      let knownSets = [ val | (_, Lit _ val) <- envVarsOfType (gEnv env) (SetR erep) ]
          gen       = oneof $ genRep (SetR erep) : map pure knownSets
      set1 <- gen
      set2 <- gen
      op   <- elements [const, const id, Set.union, Set.intersection, Set.difference, flip Set.difference]
      pure $ Lit (SetR erep) $ op set1 set2

genFreshVarName :: GenEnv era -> Gen String
genFreshVarName env = elements varNames
  where
    Env vmap = gEnv env
    varNames = take 10 [ name | s <- "" : varNames
                              , c <- ['A'..'Z']
                              , let name = s ++ [c]
                              , Map.notMember name vmap
                       ]

envVarsOfType :: Env era -> Rep era t -> [(V era t, Literal era t)]
envVarsOfType (Env env) rep = concatMap wellTyped $ Map.toList env
  where
    wellTyped (name, Payload rep' val access) =
      case testEql rep rep' of
        Just Refl -> [(V name rep access, Lit rep val)]
        Nothing   -> []

data VarSpec = VarTerm Depth -- ^ Must contain a variable (either unsolved, or solved at least the given depth).
                             --   Requiring a minimum depth let's us avoid introducing cycles in
                             --   already solved variables.
             | KnownTerm     -- ^ Can only use solved variables
  deriving (Eq, Show)

genTerm :: Era era => GenEnv era -> Rep era t -> VarSpec -> Gen (Term era t, GenEnv era)
genTerm env rep vspec = genTerm' env rep (const True) (genLiteral env rep) vspec

genTerm' :: GenEnv era -> Rep era t -> (t -> Bool) -> Gen (Literal era t) -> VarSpec -> Gen (Term era t, GenEnv era)
genTerm' env rep valid genLit vspec = frequency $
  [ (5, genFixed)       | vspec == KnownTerm ] ++
  [ (5, genExistingVar) | not $ null allowedVars ] ++
  [ (1, genFreshVar)    | VarTerm{} <- [vspec] ]
  where
    isValid (_, Lit _ val) = valid val
    existingVars = map fst $ filter isValid $ envVarsOfType (gEnv env) rep
    allowedVars = case vspec of
      VarTerm d -> filter ((>= d) . depthOfName env . Name)   existingVars
      KnownTerm -> filter ((`Map.member` gSolved env) . Name) existingVars

    genFixed       = (, env) . Fixed <$> genLit
    genExistingVar = (, env) . Var <$> elements allowedVars

    genFreshVar = do
      name      <- genFreshVarName env
      Lit _ val <- genLit
      let var = V name rep No
      pure (Var var, addVar var val env)

data TypeInEra era where
  TypeInEra :: (Show t, Ord t) => Rep era t -> TypeInEra era

genType :: Gen (TypeInEra era)
genType = elements [TypeInEra IntR, TypeInEra (SetR IntR)]

genBaseType :: Gen (TypeInEra era)
genBaseType = elements [TypeInEra IntR]

-- | Unsatisfiable constraint returned if we fail during constraint generation.
errPred :: [String] -> Pred era
errPred errs = Fixed (Lit (ListR StringR) ["Errors:"]) :=: Fixed (Lit (ListR StringR) errs)

-- This is very ad hoc
setWithSum :: Int -> Gen (Set Int)
setWithSum n = fixUp <$> arbitrary
  where
    fixUp s
      | missing == 0 = s
      | otherwise    = if Set.notMember missing s then Set.insert missing s
                                                  else Set.singleton n
      where missing = n - sum s

genPred :: forall era. Era era => GenEnv era -> Gen (Pred era, GenEnv era)
genPred env = frequency $
  [ (1, fixedSizedC)         ] ++
  [ (1, varSizedC)   | False ] ++  -- TODO/sizeBeforeArg: we can't solve these at the moment
  [ (1, subsetC)             ] ++
  [ (1, disjointC)           ] ++
  [ (1, sumsToC)             ]
  where
    withValue :: forall t.
                 Gen (Term era t, GenEnv era)
              -> (Term era t -> t -> GenEnv era -> Gen (Pred era, GenEnv era))
              -> Gen (Pred era, GenEnv era)
    withValue genTm k = do
      (tm, env') <- genTm
      case runTyped $ runTerm (gEnv env') tm of
        Left errs -> pure (errPred errs, env')
        Right val -> k tm val env'

    -- Fixed size
    fixedSizedC = do
      TypeInEra rep <- genBaseType
      withValue (genTerm env (SetR rep) (VarTerm 1)) $ \ set val env' ->
        let n = Fixed $ Lit Word64R (getsize val) in
        pure (Sized n set, markSolved (vars set) 1 env')

    -- Fresh variable for size.
    varSizedC = do
      TypeInEra rep <- genBaseType
      withValue (genTerm env (SetR rep) KnownTerm) $ \ set val env' -> do
        name <- genFreshVarName env'
        let var = V name Word64R No
        pure (Sized (Var var) set, addSolvedVar var (getsize val) (1 + depthOf env' set) env')

    subsetC
      | setBeforeSubset (gOrder env) = do
        -- Known superset
        TypeInEra rep <- genBaseType
        withValue (genTerm env (SetR rep) KnownTerm) $ \ sup val env' -> do
          let d = 1 + depthOf env' sup
          (sub, env'') <- genTerm' env' (SetR rep)
                                        (`Set.isSubsetOf` val)
                                        (Lit (SetR rep) <$> subsetFromSet val)
                                        (VarTerm d)
          pure (sub :<=: sup, markSolved (vars sub) d env'')
      | otherwise = do
        -- Known subset
        TypeInEra rep <- genBaseType
        withValue (genTerm env (SetR rep) KnownTerm) $ \ sub val env' -> do
          let d = 1 + depthOf env' sub
          (sup, env'') <- genTerm' env' (SetR rep)
                                        (Set.isSubsetOf val)
                                        (Lit (SetR rep) . Set.union val <$> genRep (SetR rep))
                                        (VarTerm d)
          pure (sub :<=: sup, markSolved (vars sup) d env'')

    -- Disjoint, left KnownTerm and right VarTerm
    disjointC = do
      TypeInEra rep <- genBaseType
      withValue (genTerm env (SetR rep) KnownTerm) $ \ lhs val env' -> do
        let d = 1 + depthOf env' lhs
        (rhs, env'') <- genTerm' env' (SetR rep)
                                      (Set.disjoint val)
                                      (Lit (SetR rep) . (`Set.difference` val) <$> genRep (SetR rep))
                                      (VarTerm d)
        pure (Disjoint lhs rhs, markSolved (vars rhs) d env'')

    -- SumsTo constraint, only Set Int at the moment.
    sumsToC
      | sumBeforeParts (gOrder env) =
        -- Known sum
        withValue (genTerm' env IntR (> 10) (Lit IntR . (+ 10) . getPositive <$> arbitrary) KnownTerm) $ \ sumTm val env' -> do
          let d = 1 + depthOf env' sumTm

              genParts [] env0 = pure ([], env0)
              genParts (n : ns) env0 = do
                (t, env1) <- genTerm' env0 (SetR IntR)
                                           ((== n) . sum)
                                           (Lit (SetR IntR) <$> setWithSum n)
                                           (VarTerm d)
                first (SumSet t :) <$> genParts ns env1
          -- TODO: It's unclear what the requirements are on the parts when solving sumBeforeParts.
          -- The solver fails if you have multiple unknown variables. Generating only a single part
          -- for now, since sumBeforeParts is anyway disabled due to TODO/SumSet.
          -- count <- choose (1, min 3 val)
          let count = 1
          partSums <- intPartition 1 count val
          (parts, env'') <- genParts partSums env'
          pure (SumsTo sumTm parts, markSolved (foldMap (varsOfSum mempty) parts) d env'')
      | otherwise = do
        -- Known sets
        let genParts 0 env0 k = k [] 0 env0
            genParts n env0 k =
              withValue (genTerm env0 (SetR IntR) KnownTerm) $ \ set val env1 ->
                genParts (n - 1) env1 $ \ parts tot ->
                k (SumSet set : parts) (sum val + tot)
        count <- choose (1, 3 :: Int)
        genParts count env $ \ parts tot env' -> do
          let d = 1 + maximum (0 : map (depthOfSum env') parts)
          (sumTm, env'') <- genTerm' env' IntR
                                     (== tot)
                                     (pure $ Lit IntR tot)
                                     (VarTerm d)
          pure (SumsTo sumTm parts, markSolved (vars sumTm) d env'')

genPreds :: Era era => GenEnv era -> Gen ([Pred era], GenEnv era)
genPreds = \ env -> do
  n <- choose (1, 10)
  loop (n :: Int) env
  where
    loop n env
      | n == 0    = pure ([], env)
      | otherwise = do
        (pr, env') <- genPred env
        first (pr :) <$> loop (n - 1) env'

-- We can't drop constraints due to dependency limitations. There needs to be at least one
-- constraint to solve each variable. We can replace constraints by Random though!
shrinkPreds :: ([Pred era], GenEnv era) -> [([Pred era], GenEnv era)]
shrinkPreds (preds, env) =
  [ (preds', env') | preds' <- shrinkList shrinkPred preds
                   , let defined = foldMap def preds'
                         env'    = env{ gEnv = pruneEnv defined (gEnv env) }
                   , depCheck mempty preds'
                   ]
  where
    -- Shrink to a Random constraint defining the same variables. The makes sure we respect the
    -- OrderInfo.
    shrinkPred Random{} = []
    shrinkPred c        = [ r | r <- randoms c, def r == def c, not . null $ def r ]

    randoms (Sized n t)    = [Random n, Random t]
    randoms (s :<=: t)     = [Random s, Random t]
    randoms (s :=: t)      = [Random s, Random t]
    randoms (Disjoint s t) = [Random s, Random t]
    randoms _              = []

    pruneEnv defs (Env vmap) = Env $ Map.filterWithKey (\ name _ -> Set.member name defNames) vmap
      where
        defNames = Set.map (\ (Name (V name _ _)) -> name) defs

    depCheck _ [] = True
    depCheck solved preds'
      | null rdy  = False
      | otherwise = depCheck (foldMap def rdy <> solved) rest
      where
        (rdy, rest) = partition canSolve preds'
        canSolve c = Set.isSubsetOf (use c) solved

    def = Map.keysSet . accumdep (gOrder env) mempty
    use = fold        . accumdep (gOrder env) mempty

-- Tests ---

type TestEra = ShelleyEra C_Crypto

testProof :: Proof TestEra
testProof = Shelley Mock

testEnv :: Env TestEra
testEnv = Env $ Map.fromList [ ("A", Payload IntR 5 No) ]

ensureRight :: Testable prop => Either [String] a -> (a -> prop) -> Property
ensureRight (Left errs) _ = counterexample (unlines errs) False
ensureRight (Right x) prop = property $ prop x

ifRight :: Testable prop => Either [String] a -> (a -> prop) -> Property
ifRight Left{} _ = False ==> False
ifRight (Right x) prop = property $ prop x

ensureTyped :: Testable prop => Typed a -> (a -> prop) -> Property
ensureTyped = ensureRight . runTyped

ifTyped :: Testable prop => Typed a -> (a -> prop) -> Property
ifTyped = ifRight . runTyped

initEnv :: OrderInfo -> GenEnv TestEra
initEnv info = GenEnv { gOrder  = info
                      , gEnv    = emptyEnv
                      , gSolved = mempty
                      }

showVal :: Rep era t -> t -> String
showVal (SetR r) t = "{" ++ intercalate ", " (map (synopsis r) (Set.toList t)) ++ "}"
showVal rep t      = synopsis rep t

showTerm :: Term era t -> String
showTerm (Fixed (Lit rep t)) = showVal rep t
showTerm t                   = show t

showPred :: Pred era -> String
showPred (sub :<=: sup)  = showTerm sub ++ " ⊆ " ++ showTerm sup
showPred (Disjoint  s t) = "Disjoint " ++ showTerm s ++ " " ++ showTerm t
showPred pr              = show pr

showEnv :: Env era -> String
showEnv (Env vmap) = unlines $ map pr $ Map.toList vmap
  where pr (name, Payload rep t _) = name ++ " :: " ++ show rep ++ " -> " ++ showVal rep t

predConstr :: Pred era -> String
predConstr Sized{}    = "Sized"
predConstr (_ :=: _)  = ":=:"
predConstr (_ :<=: _) = ":<=:"
predConstr Disjoint{} = "Disjoint"
predConstr SumsTo{}   = "SumsTo"
predConstr Random{}   = "Random"
predConstr HasDom{}   = "HasDom"

-- | Generate a set of satisfiable constraints and check that we can generate a solution and that it
--   actually satisfies the constraints.
prop_soundness :: OrderInfo -> Property
prop_soundness = prop_soundness' False

-- | If argument is True, fail property if constraints cannot be solved. Otherwise discard unsolved
--   constraints.
prop_soundness' :: Bool -> OrderInfo -> Property
prop_soundness' strict info =
  forAllShrink (genPreds $ initEnv info) shrinkPreds                       $ \ (preds, genenv) ->
  counterexample ("\n-- Order --\n" ++ show info)                          $
  counterexample ("\n-- Constraints --\n" ++ unlines (map showPred preds)) $
  counterexample ("-- Model solution --\n" ++ showEnv (gEnv genenv))       $
  ensureTyped (compile info preds)                                         $ \ graph ->
  forAll (genDependGraph testProof graph) . flip checkRight                $ \ subst ->
  let env = substToEnv subst emptyEnv
      checkPred pr = counterexample ("Failed: " ++ show pr) $ ensureTyped (runPred env pr) id
      n = let Env e = gEnv genenv in Map.size e
  in
    tabulate "Var count"        [show n] $
    tabulate "Constraint count" [show $ length preds] $
    tabulate "Constraint types" (map predConstr preds) $
    counterexample ("-- Solution --\n" ++ showEnv env) $
    conjoin $ map checkPred preds
  where
    checkRight | strict    = ensureRight
               | otherwise = ifRight

