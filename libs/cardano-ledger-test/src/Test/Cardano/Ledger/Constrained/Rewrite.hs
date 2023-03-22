{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Constrained.Rewrite (
  rewrite,
  compile,
  removeSameVar,
  removeEqual,
  DependGraph (..),
  accumdep,
  OrderInfo (..),
  standardOrderInfo,
  initialOrder,
  showGraph,
  listEq,
  cpeq,
  cteq,
  strategyRhsMap,
  mkNewVar,
  addP,
  addPred,
) where

import qualified Data.Array as A
import Data.Foldable (toList)
import Data.Graph (Graph, SCC (AcyclicSCC, CyclicSCC), Vertex, graphFromEdges, stronglyConnComp)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Env (Access (..), AnyF (..), Env (..), Field (..), Name (..), V (..))
import Test.Cardano.Ledger.Constrained.Monad (Typed (..), failT)
import Test.Cardano.Ledger.Constrained.TypeRep

-- ======================================================================

-- | Compute the names of all variables of type (Map a b) that appear on the
--   rhs of a (SumsTo test lhs rhs) that appear in a [Pred]
rhsMapNames :: Set (Name era) -> Pred era -> Set (Name era)
rhsMapNames ans (SumsTo _ _ _ rhs) = List.foldl' rhsSumNames ans rhs
rhsMapNames ans _ = ans

rhsSumNames :: Set (Name era) -> Sum era c -> Set (Name era)
rhsSumNames ans (SumMap (Var v)) = Set.insert (Name v) ans
rhsSumNames ans (Project _ (Var v)) = Set.insert (Name v) ans
rhsSumNames ans _ = ans

rhsMapNamesList :: Set (Name era) -> [Pred era] -> Set (Name era)
rhsMapNamesList ans ps = List.foldl' rhsMapNames ans ps

strategyRhsMap :: [Pred era] -> Set (Name era)
strategyRhsMap ps = rhsMapNamesList Set.empty ps

-- ============================================================
-- Conservative (approximate) Equality

-- | Test if two terms (of possibly different types) are equal
typedEq :: Term era a -> Term era b -> Bool
typedEq x y = case testEql (termRep x) (termRep y) of
  Just Refl -> cteq x y
  Nothing -> False

cEq :: Eq c => Term era c -> Term era a -> c -> a -> Bool
cEq t1 t2 c1 c2 = case testEql (termRep t1) (termRep t2) of
  Just Refl -> c1 == c2
  Nothing -> False

listEq :: (a -> b -> Bool) -> [a] -> [b] -> Bool
listEq _ [] [] = True
listEq eqf (x : xs) (y : ys) = eqf x y && listEq eqf xs ys
listEq _ _ _ = False

-- | Conservative Term equality
cteq :: Term era t -> Term era t -> Bool
cteq (Var x) (Var y) = Name x == Name y
cteq (Dom x) (Dom y) = typedEq x y
cteq (Rng x) (Rng y) = typedEq x y
cteq (Delta x) (Delta y) = typedEq x y
cteq (Negate x) (Negate y) = typedEq x y
cteq _ _ = False

-- | Conservative Pred equality
cpeq :: Pred era -> Pred era -> Bool
cpeq (Sized x a) (Sized y b) = cteq x y && typedEq a b
cpeq (x :=: a) (y :=: b) = typedEq x y && typedEq a b
cpeq (x :⊆: a) (y :⊆: b) = typedEq x y && typedEq a b
cpeq (Disjoint x a) (Disjoint y b) = typedEq x y && typedEq a b
cpeq (Random x) (Random y) = typedEq x y
cpeq (CanFollow x a) (CanFollow y b) = typedEq x y && typedEq a b
cpeq (SumsTo i x c xs) (SumsTo j y d ys) = cEq x y i j && typedEq x y && listEq cseq xs ys && c == d
cpeq (Component x xs) (Component y ys) = typedEq x y && listEq anyWeq xs ys
cpeq _ _ = False

-- | Conservative Sum equality
cseq :: Sum era c -> Sum era d -> Bool
cseq (One x) (One y) = typedEq x y
cseq (SumMap x) (SumMap y) = typedEq x y
cseq (SumList x) (SumList y) = typedEq x y
cseq (Project r1 x) (Project r2 y) = case testEql r1 r2 of
  Just Refl -> typedEq x y
  Nothing -> False
cseq _ _ = False

anyWeq :: AnyF era t -> AnyF era s -> Bool
anyWeq (AnyF (Field x y z)) (AnyF (Field a b c)) = Name (V x y z) == Name (V a b c)
anyWeq _ _ = False

-- ==================================================================================
-- Rewriting by replacing (Dom x) by a new varariabl xDom, and adding additional
-- [Pred], that relate xDom with other terms

mkNewVar :: forall era d r. Term era (Map d r) -> Term era (Set d)
mkNewVar (Var (V nm (MapR d _) _)) = newVar
  where
    newstring = nm ++ "Dom"
    newV = V newstring (SetR d) No
    newVar = Var newV
mkNewVar other = error ("mkNewVar should only be applied to variables: " ++ show other)

addP :: Pred era -> [Pred era] -> [Pred era]
addP p ps = List.nubBy cpeq (p : ps)

addPred :: Set (Name era) -> Pred era -> [Name era] -> [Pred era] -> [Pred era] -> [Pred era]
addPred bad orig names ans newps =
  if any (\x -> Set.member x bad) names
    then addP orig ans
    else foldr addP ans newps

removeSameVar :: [Pred era] -> [Pred era] -> [Pred era]
removeSameVar [] ans = reverse ans
removeSameVar ((Var v :=: Var u) : more) ans | Name v == Name u = removeSameVar more ans
removeSameVar ((Var v :⊆: Var u) : more) ans | Name v == Name u = removeSameVar more ans
removeSameVar (Disjoint (Var v@(V _ rep _)) (Var u) : more) ans | Name v == Name u = removeSameVar more ((Lit rep mempty :=: Var v) : ans)
removeSameVar (m : more) ans = removeSameVar more (m : ans)

removeEqual :: [Pred era] -> [Pred era] -> [Pred era]
removeEqual [] ans = reverse ans
removeEqual ((Var v :=: Var u) : more) ans | Name v == Name u = removeEqual more ans
removeEqual ((Var v :=: expr@Lit {}) : more) ans = removeEqual (map sub more) ((Var v :=: expr) : map sub ans)
  where
    sub = substPred [SubItem v expr]
removeEqual ((expr@Lit {} :=: Var v) : more) ans = removeEqual (map sub more) ((expr :=: Var v) : map sub ans)
  where
    sub = substPred [SubItem v expr]
removeEqual (m : more) ans = removeEqual more (m : ans)

removeTrivial :: forall era. [Pred era] -> [Pred era]
removeTrivial = filter (not . trivial)
  where
    trivial p | null (varsOfPred mempty p) =
      case runTyped $ runPred (Env mempty) p of
        Left {} -> False
        Right valid -> valid
    trivial (e1 :=: e2) = cteq e1 e2
    trivial _ = False

rewrite :: [Pred era] -> [Pred era]
rewrite cs = removeTrivial $ removeSameVar (removeEqual cs []) []

-- ==============================================================
-- Build a Dependency Graph that extracts an ordering on the
-- variables in the [Pred] we are trying to solve. The idea is that
-- solving for for a [Pred] will be easier if it contains only
-- one variable, and all other Terms are constants (Fixed (Lit rep x))

-- | An Ordering
newtype DependGraph era = DependGraph [(Name era, [Pred era])]

instance Show (DependGraph era) where
  show (DependGraph xs) = unlines (map f xs)
    where
      f (nm, cs) = pad n (shName nm) ++ " | " ++ showL show ", " cs
      n = maximum (map (length . shName . fst) xs) + 2
      shName (Name (V v rep _)) = v ++ ": " ++ show rep

-- =========================================================
-- Sketch of algorithm for creating a DependGraph
--
-- for every pair (name,[cond]) the variables of [cond] only contain name, and names
-- defined in previous pairs in the DependGraph. Can we find such an order?
-- To compute this from a [Pred era] we implement this algorithm
-- try prev choices constraints =
--   (c,more) <- pick choices
--   possible <- filter (only mentions (c:prev)) constraints
--   if null possible
--      then try prev (more ++ [(c,possible)]) constraints
--      else possible ++ try (c:prev) more (constraints - possible)
--
-- ===================================================================
-- Find an order to solve the variables in

mkDependGraph :: Int -> [(Name era, [Pred era])] -> [Name era] -> [Name era] -> [Pred era] -> Typed (DependGraph era)
mkDependGraph _ prev _ _ [] = pure (DependGraph (reverse prev))
mkDependGraph count prev choices badchoices specs
  | count <= 0 =
      failT
        [ "\nFailed to find an Ordering of variables to solve for.\nHandled Constraints\n"
        , show (DependGraph (reverse prev))
        , "\n  vars to be processed"
        , show choices
        , "\n  vars bad "
        , show badchoices
        , "\n  Still to be processed\n"
        , unlines (map show specs)
        ]
mkDependGraph count prev [] badchoices cs = mkDependGraph (count - 1) prev (reverse badchoices) [] cs
mkDependGraph count prev (n : more) badchoices cs =
  if null possible
    then mkDependGraph count prev more (n : badchoices) cs
    else mkDependGraph count ((n, List.nubBy cpeq possible) : prev) (reverse badchoices ++ more) [] notPossible
  where
    defined = Set.insert n (Set.fromList (map fst prev))
    ok constraint = Set.isSubsetOf (varsOfPred Set.empty constraint) defined
    (possible, notPossible) = List.partition ok cs

-- ============================================================
-- Create a Graph from which we can extract a DependGraph

-- | Add to the dependency map 'answer' constraints such that every Name in 'before'
--   preceeds every Name in 'after' in the order in which Names are solved for.
mkDeps :: Set (Name era) -> Set (Name era) -> Map (Name era) (Set (Name era)) -> Map (Name era) (Set (Name era))
mkDeps before after answer = Set.foldl' accum answer after
  where
    accum ans left = Map.insertWith (Set.union) left before ans

data OrderInfo = OrderInfo
  { sumBeforeParts :: Bool
  , sizeBeforeArg :: Bool
  , setBeforeSubset :: Bool
  }
  deriving (Show, Eq)

standardOrderInfo :: OrderInfo
standardOrderInfo =
  OrderInfo
    { sumBeforeParts = False
    , sizeBeforeArg = True
    , setBeforeSubset = True
    }

accumdep :: OrderInfo -> Map (Name era) (Set (Name era)) -> Pred era -> Map (Name era) (Set (Name era))
accumdep info answer c = case c of
  sub :⊆: set ->
    if setBeforeSubset info
      then mkDeps (vars set) (vars sub) answer
      else mkDeps (vars sub) (vars set) answer
  lhs :=: rhs -> mkDeps (vars lhs) (vars rhs) answer
  Disjoint left right -> mkDeps (vars left) (vars right) answer
  Sized size arg ->
    if sizeBeforeArg info
      then mkDeps (vars size) (vars arg) answer
      else mkDeps (vars arg) (vars size) answer
  SumsTo _ sm _ parts ->
    if sumBeforeParts info
      then mkDeps (vars sm) (List.foldl' varsOfSum Set.empty parts) answer
      else mkDeps (List.foldl' varsOfSum Set.empty parts) (vars sm) answer
  Component t cs -> mkDeps (componentVars cs) (vars t) answer
  -- One has to evaluate the terms in 'cs' first, otherwise 't' will be evaluated, and then it will
  -- be too late to overide the projections with the values of the vars in 'cs'
  other -> Set.foldl' accum answer (varsOfPred Set.empty other)
    where
      accum ans v = Map.insertWith (Set.union) v Set.empty ans

componentVars :: [AnyF era s] -> Set (Name era)
componentVars [] = Set.empty
componentVars (AnyF (Field n r a) : cs) = Set.insert (Name $ V n r a) $ componentVars cs
componentVars (AnyF (FConst _ _ _) : cs) = componentVars cs

-- =========================================================================
-- Create an initial Ordering. Build a Graph, then extract the Ordering

initialOrder :: forall era. OrderInfo -> [Pred era] -> Typed [Name era]
initialOrder info cs0 = do
  mmm <- flatOrError (stronglyConnComp listDep)
  -- pure $ trace ("\nGraph\n"++showGraph (show.getname) _graph1) (map getname mmm)
  pure $ map getname mmm
  where
    allvs = List.foldl' varsOfPred Set.empty cs0
    noDepMap = Set.foldl' (\ans n -> Map.insert n Set.empty ans) Map.empty allvs
    mapDep = List.foldl' (accumdep info) noDepMap cs0
    listDep = zipWith (\(x, m) n -> (n, x, Set.toList m)) (Map.toList mapDep) [0 ..]
    (_graph1, nodeFun, _keyf) = graphFromEdges listDep
    getname :: Vertex -> Name era
    getname x = n where (_node, n, _children) = nodeFun x
    flatOrError [] = pure []
    flatOrError (AcyclicSCC x : more) = (x :) <$> flatOrError more
    flatOrError (CyclicSCC xs : _) = failT [message, show info, unlines (map (("  " ++) . show) usesNames)]
      where
        names = map getname xs
        namesSet = Set.fromList names
        usesNames =
          [pr | pr <- cs0, any (`Set.member` namesSet) (varsOfPred Set.empty pr)]
        theCycle = case names of
          [] -> map show names
          (x : _) -> map show (names ++ [x])
        message = "Cycle in dependencies: " ++ List.intercalate " <= " theCycle

-- | Construct the DependGraph
compile :: OrderInfo -> [Pred era] -> Typed (DependGraph era)
compile info cs = do
  let simple = rewrite cs
  orderedNames <- initialOrder info simple
  mkDependGraph (length orderedNames) [] orderedNames [] simple

showGraph :: (Vertex -> String) -> Graph -> String
showGraph nameof g = unlines (map show (zip names (toList zs)))
  where
    (lo, hi) = A.bounds g
    names = map nameof [lo .. hi]
    zs = fmap (map nameof) g
