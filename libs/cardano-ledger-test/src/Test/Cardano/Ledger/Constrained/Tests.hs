{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Cardano.Ledger.Constrained.Tests where

import Control.Arrow (first)
import Data.List (intercalate, partition)
import qualified Data.Map as Map
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
variables in the environment are solved by an already generated constraint.

Current limitations
  - Only IntR and SetR IntR types (and Word64R for size constraints)
  - Only generates Sized and :<=: constraints
  - Only tests standardOrderInfo variable order

Current issues
  - Generates cyclic constraints of the form [Random A, B ⊆ A, A ⊆ B]
  - Property fails on constraints like [Sized 3 A, B ⊆ A, C ⊆ B, Sized 1 C]
    where the solver solves B with the empty set and then fails on the size constraint for C.
-}

-- Generators ---

data GenEnv era = GenEnv { gEnv    :: Env era
                         , gSolved :: Set (Name era)
                            -- ^ Which variables in the environment have we "solved", i.e. generated
                            --   a constraint that allows solving them.
                         }
  deriving Show

addVar :: V era t -> t -> GenEnv era -> GenEnv era
addVar var val env = env{ gEnv = storeVar var val $ gEnv env }

markSolved :: Set (Name era) -> GenEnv era -> GenEnv era
markSolved solved env = env{ gSolved = solved <> gSolved env }

addSolvedVar :: V era t -> t -> GenEnv era -> GenEnv era
addSolvedVar var val = markSolved (Set.singleton $ Name var) . addVar var val

genLiteral :: forall era t. Era era => GenEnv era -> Rep era t -> Gen (Literal era t)
genLiteral _env rep =
  case rep of
    SetR erep -> setLiteral erep
    _         -> unconstrained rep
  where
    unconstrained :: forall a. Rep era a -> Gen (Literal era a)
    unconstrained r = Lit r <$> genRep r

    setLiteral :: forall a. Ord a => Rep era a -> Gen (Literal era (Set a))
    setLiteral erep = unconstrained (SetR erep)

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

data VarSpec = VarTerm    -- ^ Must contain a variable (solved or unsolved)
             | KnownTerm  -- ^ Can only use solved variables
  deriving (Eq, Show)

genTerm :: Era era => GenEnv era -> Rep era t -> VarSpec -> Gen (Term era t, GenEnv era)
genTerm env rep vspec = sized $ \ n -> genSizedTerm n env rep vspec

genSizedTerm :: Era era => Int -> GenEnv era -> Rep era t -> VarSpec -> Gen (Term era t, GenEnv era)
genSizedTerm size env rep vspec = genSizedTerm' size env rep (const True) (genLiteral env rep) vspec

genSizedTerm' :: Int -> GenEnv era -> Rep era t -> (t -> Bool) -> Gen (Literal era t) -> VarSpec -> Gen (Term era t, GenEnv era)
genSizedTerm' _size env rep valid genLit vspec = frequency $
  [ (5, genFixed)       | vspec == KnownTerm ] ++
  [ (5, genExistingVar) | not $ null allowedVars ] ++
  [ (1, genFreshVar)    | vspec == VarTerm ]
  where
    isValid (_, Lit _ val) = valid val
    existingVars = map fst $ filter isValid $ envVarsOfType (gEnv env) rep
    allowedVars = case vspec of
      VarTerm   -> existingVars  -- Open terms can use solved variables
      KnownTerm -> filter ((`Set.member`    gSolved env) . Name) existingVars

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

genPred :: Era era => GenEnv era -> Gen (Maybe (Pred era, GenEnv era))
genPred env = sized $ \ n -> genSizedPred n env

genSizedPred :: forall era. Era era => Int -> GenEnv era -> Gen (Maybe (Pred era, GenEnv era))
genSizedPred size env = frequency
  [ (1, Just <$> fixedSizedC)
  , (0, Just <$> varSizedC)      -- TODO: we can't solve these at the moment
  , (1, Just <$> subsetC)
  ]
  where
    -- Fixed size
    fixedSizedC = do
      TypeInEra rep <- genBaseType
      (set, env')   <- genSizedTerm size env (SetR rep) VarTerm
      case runTyped $ runTerm (gEnv env') set of
        Left errs -> pure (errPred errs, env')
        Right val -> pure (Sized n set, markSolved (vars set) env')
          where n = Fixed $ Lit Word64R (getsize val)

    -- Fresh variable for size.
    varSizedC = do
      TypeInEra rep <- genBaseType
      (set, env')   <- genSizedTerm size env (SetR rep) KnownTerm
      case runTyped $ runTerm (gEnv env') set of
        Left errs -> pure (errPred errs, env')
        Right val -> do
          name <- genFreshVarName env'
          let var = V name Word64R No
          pure (Sized (Var var) set, addSolvedVar var (getsize val) env')

    -- Open subset, known superset
    subsetC = do
      TypeInEra rep <- genBaseType
      (sup, env') <- genSizedTerm size env (SetR rep) KnownTerm
      case runTyped $ runTerm (gEnv env') sup of
        Left errs -> pure (errPred errs, env')
        Right val -> do
          (sub, env'') <- genSizedTerm' size env' (SetR rep)
                                        (`Set.isSubsetOf` val)
                                        (Lit (SetR rep) <$> subsetFromSet val)
                                        VarTerm
          pure (sub :<=: sup, markSolved (vars sub) env'')

genPreds :: Era era => GenEnv era -> Gen ([Pred era], GenEnv era)
genPreds = \ env -> do
  n <- choose (1, 10)
  loop (n :: Int) env
  where
    loop n env
      | n == 0    = pure ([], env)
      | otherwise = genPred env >>= \ case
        Nothing         -> loop (n - 1) env
        Just (pr, env') -> first (pr :) <$> loop (n - 1) env'

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
    shrinkPred Random{}    = []
    shrinkPred (Sized _ t) = [Random t]
    shrinkPred (t :<=: _)  = [Random t]
    shrinkPred _           = []

    pruneEnv defs (Env vmap) = Env $ Map.filterWithKey (\ name _ -> Set.member name defNames) vmap
      where
        defNames = Set.map (\ (Name (V name _ _)) -> name) defs

    -- TODO: don't reimplement this here!
    depCheck _ [] = True
    depCheck solved preds'
      | null rdy  = False
      | otherwise = depCheck (foldMap def rdy <> solved) rest
      where
        (rdy, rest) = partition canSolve preds'
        canSolve c = Set.isSubsetOf (use c) solved

    use Random{}   = mempty
    use Sized{}    = mempty
    use (_ :<=: t) = vars t
    use _          = mempty

    def (Random t)  = vars t
    def (Sized _ t) = vars t
    def (t :<=: _)  = vars t
    def _ = mempty

-- Tests ---

type TestEra = ShelleyEra C_Crypto

testProof :: Proof TestEra
testProof = Shelley Mock

testEnv :: Env TestEra
testEnv = Env $ Map.fromList [ ("A", Payload IntR 5 No) ]

ensureRight :: Testable prop => Either [String] a -> (a -> prop) -> Property
ensureRight (Left errs) _ = counterexample (unlines errs) False
ensureRight (Right x) prop = property $ prop x

ensureTyped :: Testable prop => Typed a -> (a -> prop) -> Property
ensureTyped = ensureRight . runTyped

ifTyped :: Testable prop => Typed a -> (a -> prop) -> Property
ifTyped t prop =
  case runTyped t of
    Left{}  -> False ==> False
    Right x -> property $ prop x

initEnv :: GenEnv TestEra
initEnv = GenEnv { gEnv    = emptyEnv
                 , gSolved = mempty
                 }

showVal :: Rep era t -> t -> String
showVal (SetR r) t = "{" ++ intercalate ", " (map (synopsis r) (Set.toList t)) ++ "}"
showVal rep t      = synopsis rep t

showTerm :: Term era t -> String
showTerm (Fixed (Lit rep t)) = showVal rep t
showTerm t                   = show t

showPred :: Pred era -> String
showPred (sub :<=: sup) = showTerm sub ++ " ⊆ " ++ showTerm sup
showPred pr             = show pr

showEnv :: Env era -> String
showEnv (Env vmap) = unlines $ map pr $ Map.toList vmap
  where pr (name, Payload rep t _) = name ++ " :: " ++ show rep ++ " -> " ++ showVal rep t

-- | Generate a set of satisfiable constraints and check that we can generate a solution and that it
--   actually satisfies the constraints.
prop_soundness :: Property
prop_soundness =
  forAllShrink (genPreds initEnv) shrinkPreds                $ \ (preds, genenv) ->
  ensureTyped (compile standardOrderInfo preds)              $ \ graph ->
  forAll (genDependGraph testProof graph) . flip ensureRight $ \ subst ->
  let env = substToEnv subst emptyEnv
      checkPred pr = counterexample ("Failed: " ++ show pr) $ ensureTyped (runPred env pr) id
      n = let Env e = gEnv genenv in Map.size e
  in
    tabulate "Var count"        [show n] $
    tabulate "Constraint count" [show $ length preds] $
    counterexample ("\n-- Constraints --\n" ++ unlines (map showPred preds)) $
    counterexample ("-- Model solution --\n" ++ showEnv (gEnv genenv)) $
    counterexample ("-- Solution --\n" ++ showEnv env) $
    conjoin $ map checkPred preds

