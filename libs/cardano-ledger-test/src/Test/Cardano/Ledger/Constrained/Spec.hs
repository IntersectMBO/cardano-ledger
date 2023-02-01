{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

--  {-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Cardano.Ledger.Constrained.Spec where

import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Data.Graph
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Pulse (foldlM')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe (cmpIndex)
import Data.Word (Word64)
import Debug.Trace (trace)
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (Adds (..), Sums (..))
import Test.Cardano.Ledger.Constrained.Combinators
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad
import Test.Cardano.Ledger.Constrained.TypeRep (
  Rep (..),
  genRep,
  genSizedRep,
  synopsis,
  testEql,
  (:~:) (Refl),
 )
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Generic.Proof (
  Mock,
  Proof (..),
  ShelleyEra,
 )
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.QuickCheck hiding (Fixed, total)
import Prelude hiding (subtract)

-- ===================================================================================
-- This is testing file, and sometimes it pays for solver to explain what it is doing
-- especially if it fails on some known input. This way the solver can leave a trace
-- of what it is doing, and why.

traceOn :: Bool
traceOn = False

ifTrace :: String -> a -> a
ifTrace message a = case traceOn of
  True -> trace message a
  False -> a

type Shell = ShelleyEra Mock

-- ==================================================

-- ======================================================================================
-- Transform a [Pred era] by introducing new variables.

mkNewVar :: Term era (Map d r) -> Term era (Set d)
mkNewVar (Var (V nm (MapR d _) _)) = newVar
  where
    newstring = nm ++ "Dom"
    newV = V newstring (SetR d) No
    newVar = Var newV
mkNewVar other = error ("mkNewVar should only be applied to variables: " ++ show other)

removeSameVar :: [Pred era] -> [Pred era] -> [Pred era]
removeSameVar [] ans = reverse ans
removeSameVar ((Var v :=: Var u) : more) ans | Name v == Name u = removeSameVar more ans
removeSameVar ((Var v :<=: Var u) : more) ans | Name v == Name u = removeSameVar more ans
removeSameVar (m : more) ans = removeSameVar more (m : ans)

removeEqual :: [Pred era] -> [Pred era] -> [Pred era]
removeEqual [] ans = reverse ans
removeEqual ((Var v :=: Var u) : more) ans | Name v == Name u = removeEqual more ans
removeEqual ((Var v :=: expr) : more) ans = removeEqual (map sub more) ((Var v :=: expr) : (map sub ans))
  where
    sub = substPred [SubItem v expr]
removeEqual (m : more) ans = removeEqual more (m : ans)

removeDom :: [Pred era] -> [Pred era]
removeDom cs = (List.foldl' help [] cs)
  where
    help :: [Pred era] -> Pred era -> [Pred era]
    help ans c = case c of
      Sized x (Dom old@(Var (V _ (MapR _ _) _))) -> foldr addPred ans [Sized x newVar, HasDom old newVar]
        where
          newVar = mkNewVar old
      (Dom left@(Var (V _ (MapR d1 _) _))) :=: (Dom right@(Var (V _ (MapR d2 _) _))) ->
        let leftVar = mkNewVar left
            rightVar = mkNewVar right
         in case testEql d1 d2 of
              Just Refl -> foldr addPred ans [leftVar :=: rightVar, HasDom left leftVar, HasDom right rightVar]
              Nothing -> ans
      x :=: (Dom old@(Var (V _ (MapR _ _) _))) -> foldr addPred ans [x :=: newVar, HasDom old newVar]
        where
          newVar = mkNewVar old
      (Dom old@(Var (V _ (MapR _ _) _))) :=: x -> foldr addPred ans [newVar :=: x, HasDom old newVar]
        where
          newVar = mkNewVar old
      (Dom left@(Var (V _ (MapR d1 _) _))) :<=: (Dom right@(Var (V _ (MapR d2 _) _))) ->
        let leftVar = mkNewVar left
            rightVar = mkNewVar right
         in case testEql d1 d2 of
              Just Refl -> foldr addPred ans [leftVar :<=: rightVar, HasDom left leftVar, HasDom right rightVar]
              Nothing -> ans
      x :<=: (Dom old@(Var (V _ (MapR _ _) _))) -> foldr addPred ans [x :<=: newVar, HasDom old newVar]
        where
          newVar = mkNewVar old
      (Dom old@(Var (V _ (MapR _ _) _))) :<=: x -> foldr addPred ans [newVar :<=: x, HasDom old newVar]
        where
          newVar = mkNewVar old
      Disjoint (Dom left@(Var (V _ (MapR d1 _) _))) (Dom right@(Var (V _ (MapR d2 _) _))) ->
        let leftVar = mkNewVar left
            rightVar = mkNewVar right
         in case testEql d1 d2 of
              Just Refl -> foldr addPred ans [Disjoint leftVar rightVar, HasDom left leftVar, HasDom right rightVar]
              Nothing -> ans
      Disjoint x (Dom old@(Var (V _ (MapR _ _) _))) -> foldr addPred ans [Disjoint x newVar, HasDom old newVar]
        where
          newVar = mkNewVar old
      Disjoint (Dom old@(Var (V _ (MapR _ _) _))) x -> foldr addPred ans [Disjoint newVar x, HasDom old newVar]
        where
          newVar = mkNewVar old
      ss@(SumsTo (Fixed _) _) -> addPred ss ans
      SumsTo x ys -> foldr addPred ans (SumsTo x ys2 : (concat new))
        where
          pairs = map remSum ys
          new = map snd pairs
          ys2 = map fst pairs
      other -> addPred other ans

remSum :: Sum era c -> (Sum era c, [Pred era])
remSum (SumMap old@(Var (V nm (MapR _ rng) _))) = (One newVar, [SumsTo newVar [SumMap old]])
  where
    newstring = nm ++ "Sum"
    newV = V newstring rng No
    newVar = Var newV
remSum (SumSet old@(Var (V nm (SetR rng) _))) = (One newVar, [SumsTo newVar [SumSet old]])
  where
    newstring = nm ++ "Sum"
    newV = V newstring rng No
    newVar = Var newV
remSum other = (other, [])

rewrite :: [Pred era] -> [Pred era]
rewrite cs = removeSameVar (removeEqual (removeDom cs) []) []

-- =========================
-- Do not add duplicates

addName :: Name era -> [Name era] -> [Name era]
addName n ns = List.nubBy cmpName (n : ns)

addPred :: Pred era -> [Pred era] -> [Pred era]
addPred x xs = List.nubBy cmpPred (x : xs)

cmpV :: V era t -> V era s -> Ordering
cmpV (V n1 r1 _) (V n2 r2 _) = case compare n1 n2 of
  EQ -> cmpIndex r1 r2
  other -> other

cmpName :: Name era -> Name era -> Bool
cmpName (Name v1) (Name v2) = cmpV v1 v2 == EQ

-- | conservative equality. If it returns True, they are really equal, if it returns False, then who knows?
cmpPred :: forall era. Pred era -> Pred era -> Bool
cmpPred (HasDom (Var x) (Var y)) (HasDom (Var a) (Var b)) = Name x == Name a && Name y == Name b
cmpPred (Sized (Var x) (Var y)) (Sized (Var a) (Var b)) = Name x == Name a && Name y == Name b
cmpPred ((Var x) :=: (Var y)) ((Var a) :=: (Var b)) = Name x == Name a && Name y == Name b
cmpPred ((Var x) :<=: (Var y)) ((Var a) :<=: (Var b)) = Name x == Name a && Name y == Name b
cmpPred (Disjoint (Var x) (Var y)) (Disjoint (Var a) (Var b)) = Name x == Name a && Name y == Name b
cmpPred (Random (Var x)) (Random (Var a)) = Name x == Name a
cmpPred (SumsTo (Var x@(V _ r1 _)) xs) (SumsTo (Var a@(V _ r2 _)) as) =
  case testEql r1 r2 of
    Just Refl -> Name x == Name a && length xs == length as && and (zipWith cmpSum xs as)
    Nothing -> False
cmpPred _ _ = False

cmpSum :: Sum era t -> Sum era t -> Bool
cmpSum (One (Var x)) (One (Var a)) = Name x == Name a
cmpSum (SumMap (Var x)) (SumMap (Var a)) = Name x == Name a
cmpSum (SumSet (Var x)) (SumSet (Var a)) = Name x == Name a
cmpSum _ _ = False

-- ==============================================================
-- Solving a list of constraints

-- | A solution
data DependGraph era = DependGraph [(Name era, [Pred era])]

instance Show (DependGraph era) where
  show (DependGraph xs) = unlines (map f xs)
    where
      f (nm, cs) = pad n (shName nm) ++ " | " ++ showL show ", " cs
      n = maximum (map (length . shName . fst) xs) + 2
      shName (Name (V v rep _)) = v ++ ": " ++ show rep

-- =========================================================
-- Sketch of algorithm for creating a DependGraph
--
-- for every pair (name,[cond]) the variables of [cond] only contain name, and names defined in previous lines
-- Can we find such an order? to compute this from a [Pred era]
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
    else mkDependGraph count ((n, List.nubBy cmpPred possible) : prev) more badchoices notPossible
  where
    defined = Set.insert n (Set.fromList (map fst prev))
    ok constraint = Set.isSubsetOf (varsOfPred Set.empty constraint) defined
    (possible, notPossible) = List.partition ok cs

-- | Add to the dependency map 'answer' constraints such that every Name in 'before'
--   preceeds every Name in 'after' in the order in which Names are solved for.
mkDeps :: Set (Name era) -> Set (Name era) -> Map (Name era) (Set (Name era)) -> Map (Name era) (Set (Name era))
mkDeps before after answer = Set.foldl' accum answer after
  where
    accum ans left = Map.insertWith (Set.union) left before ans

-- ===================================================

data OrderInfo = OrderInfo
  { sumBeforeParts :: Bool
  , sizeBeforeArg :: Bool
  , setBeforeSubset :: Bool
  , mapBeforeDom :: Bool
  }
  deriving (Show, Eq)

standardOrderInfo :: OrderInfo
standardOrderInfo =
  OrderInfo
    { sumBeforeParts = False
    , sizeBeforeArg = False
    , setBeforeSubset = True
    , mapBeforeDom = False
    }

accumdep :: OrderInfo -> Map (Name era) (Set (Name era)) -> Pred era -> Map (Name era) (Set (Name era))
accumdep info answer c = case c of
  sub :<=: set ->
    if setBeforeSubset info
      then mkDeps (vars set) (vars sub) answer
      else mkDeps (vars sub) (vars set) answer
  Disjoint left right -> mkDeps (vars left) (vars right) answer
  Sized size arg ->
    if sizeBeforeArg info
      then mkDeps (vars size) (vars arg) answer
      else mkDeps (vars arg) (vars size) answer
  HasDom mp dom ->
    if mapBeforeDom info
      then mkDeps (vars mp) (vars dom) answer
      else mkDeps (vars dom) (vars mp) answer
  SumsTo sm parts ->
    if sumBeforeParts info
      then mkDeps (vars sm) (List.foldl' varsOfSum Set.empty parts) answer
      else mkDeps (List.foldl' varsOfSum Set.empty parts) (vars sm) answer
  other -> Set.foldl' accum answer (varsOfPred Set.empty other)
    where
      accum ans v = Map.insertWith (Set.union) v Set.empty ans

initialOrder :: OrderInfo -> [Pred era] -> Typed [Name era]
initialOrder info cs0 = do
  mmm <- flatOrError (stronglyConnComp listDep)
  pure (map getname mmm)
  where
    allvs = List.foldl' varsOfPred Set.empty cs0
    noDepMap = Set.foldl' (\ans n -> Map.insert n Set.empty ans) Map.empty allvs
    mapDep = List.foldl' (accumdep info) noDepMap cs0
    listDep = zipWith (\(x, m) n -> (n, x, Set.toList m)) (Map.toList mapDep) [0 ..]
    (_graph1, nodeFun, _keyf) = graphFromEdges listDep
    getname x = n where (_node, n, _children) = nodeFun x
    flatOrError [] = pure []
    flatOrError (AcyclicSCC x : more) = (x :) <$> flatOrError more
    flatOrError (CyclicSCC xs : _) = failT [message, show info, unlines (map (("  " ++) . show) usesNames)]
      where
        names = map getname xs
        usesNames = map fst $ filter (any (`elem` names) . snd) $ map (\pr -> (pr, varsOfPred Set.empty pr)) cs0
        theCycle = map show (names ++ [head names])
        message = "Cycle in dependencies: " ++ List.intercalate " <= " theCycle

maybePartition :: (a -> Maybe b) -> [a] -> ([b], [a])
maybePartition _f [] = ([], [])
maybePartition f (a : more) = maybe (bs, a : as) (\b -> (b : bs, as)) (f a)
  where
    (bs, as) = maybePartition f more

compile :: OrderInfo -> [Pred era] -> Typed (DependGraph era)
compile info cs = do
  let simple = rewrite cs
  orderedNames <- initialOrder info simple
  mkDependGraph (length orderedNames) [] orderedNames [] simple

-- ==========================================================

-- | Indicates which constraints (if any) the range of a Map or a plain Set must adhere to
data RngSpec rng where
  -- \^ The set must have Adds instance and add up to 'rng'
  SumRng ::
    Adds rng =>
    rng ->
    RngSpec rng
  -- | The range must be equal to the set created by Set.fromlist [rng]
  Equal ::
    Eq rng =>
    [rng] ->
    RngSpec rng
  -- | The range must be a subset of (Set rng)
  SubsetRng ::
    Ord rng =>
    (Set rng) ->
    RngSpec rng
  -- | The range must NOT contain anything in this set
  DisjointRng ::
    Ord rng =>
    (Set rng) ->
    RngSpec rng
  -- | The range must sum upto 'c' through the projection witnessed by the (Sums t c) class
  ProjRng :: Sums x c => Rep era x -> c -> RngSpec x
  -- | There are no constraints on the range (random generator will do)
  None :: RngSpec rng
  -- | Something was inconsistent
  NeverRng :: [String] -> RngSpec rng

instance LiftT (RngSpec a) where
  liftT (NeverRng xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = NeverRng s
  dropT (Typed (Right x)) = x

showRngSpec :: Rep era t -> RngSpec t -> String
showRngSpec _ (SumRng r) = "(Sum " ++ show r ++ ")"
showRngSpec rep (Equal xs) = "(RngEqual " ++ synopsis (ListR rep) xs ++ ")"
showRngSpec rep (SubsetRng xs) = "(Subset " ++ synopsis (SetR rep) xs ++ ")"
showRngSpec rep (DisjointRng xs) = "(Disjoint " ++ synopsis (SetR rep) xs ++ ")"
showRngSpec _ None = "None"
showRngSpec _ (NeverRng _) = "NeverRng"
showRngSpec _ (ProjRng r c) = "(ProjRng " ++ show r ++ " " ++ show c ++ ")"

mergeRngSpec :: RngSpec r -> RngSpec r -> (RngSpec r)
mergeRngSpec None x = x
mergeRngSpec x None = x
mergeRngSpec (SumRng x) (SumRng y) | x == y = SumRng x
mergeRngSpec (DisjointRng x) (DisjointRng y) = DisjointRng (Set.union x y)
mergeRngSpec (Equal x) (Equal y) | x == y = Equal x
mergeRngSpec (SubsetRng x) (SubsetRng y) = SubsetRng (Set.intersection x y)
mergeRngSpec (Equal xs) (SubsetRng ys) | Set.isSubsetOf (Set.fromList xs) ys = Equal xs
mergeRngSpec (SubsetRng ys) (Equal xs) | Set.isSubsetOf (Set.fromList xs) ys = Equal xs
mergeRngSpec (Equal xs) (DisjointRng ys) | Set.disjoint (Set.fromList xs) ys = Equal xs
mergeRngSpec (DisjointRng ys) (Equal xs) | Set.disjoint (Set.fromList xs) ys = Equal xs
mergeRngSpec (SubsetRng xs) (DisjointRng ys) = SubsetRng (Set.difference xs ys)
mergeRngSpec (DisjointRng ys) (SubsetRng xs) = SubsetRng (Set.difference xs ys)
mergeRngSpec _ _ = NeverRng ["Inconsistent range specs in mergeRngSpec"]

instance Monoid (RngSpec rng) where mempty = None

instance Semigroup (RngSpec rng) where
  (<>) = mergeRngSpec

-- =====================================================

-- | Indicates which constraints (if any) a Map must adhere to
data MapSpec dom rng where
  -- \^ The map may be constrained 3 ways. 1) Its size (Maybe word64) 2) its domain 3) its range
  MapSpec ::
    (Maybe Word64) ->
    (Maybe (Set dom)) ->
    (RngSpec rng) ->
    -- | The map must be equal to the given Map
    MapSpec dom rng
  MapEqual ::
    Ord rng =>
    Map dom rng ->
    -- | Something is inconsistent
    MapSpec dom rng
  NeverMap :: [String] -> MapSpec dom rng

showMapSpec :: Rep era (Map dom rng) -> MapSpec dom rng -> String
showMapSpec (MapR _ rng) (MapSpec w Nothing r) =
  "(MapSpec " ++ show w ++ " Nothing " ++ showRngSpec rng r ++ ")"
showMapSpec (MapR dom rng) (MapSpec w (Just s) r) =
  "(MapSpec " ++ show w ++ " " ++ synopsis (SetR dom) s ++ " " ++ showRngSpec rng r ++ ")"
showMapSpec rep@(MapR _ _) (MapEqual m) = "(MapEqual " ++ synopsis rep m ++ ")"
showMapSpec _ (NeverMap _) = "NeverMap"

instance LiftT (MapSpec a b) where
  liftT (NeverMap xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = NeverMap s
  dropT (Typed (Right x)) = x

comb1 :: Ord a => (Maybe (Set a)) -> (Maybe (Set a)) -> Maybe (Set a)
comb1 Nothing x = x
comb1 x Nothing = x
comb1 (Just x) (Just y) = Just (Set.intersection x y)

comb2 :: (Maybe Word64) -> (Maybe Word64) -> Either [String] (Maybe Word64)
comb2 Nothing x = Right x
comb2 x Nothing = Right x
comb2 (Just s1) (Just s2) =
  if s1 == s2
    then Right (Just s1)
    else Left ["Can't be size " ++ show s1 ++ " " ++ show s2 ++ " at the same time."]

instance (Ord dom) => Semigroup (MapSpec dom rng) where
  (NeverMap s) <> (NeverMap t) = NeverMap (s ++ t)
  (NeverMap _) <> y = y
  x <> (NeverMap _) = x
  (MapEqual x) <> (MapEqual y) =
    if x == y then MapEqual x else NeverMap ["Two non-matching Exact"]
  (MapEqual x) <> m@(MapSpec _ _ _) = MapSpec Nothing (Just (Map.keysSet x)) (Equal (Map.elems x)) <> m
  m@(MapSpec _ _ _) <> (MapEqual x) = MapSpec Nothing (Just (Map.keysSet x)) (Equal (Map.elems x)) <> m
  (MapSpec s1 d1 r1) <> (MapSpec s2 d2 r2) = case comb2 s1 s2 of
    Left s -> NeverMap s
    Right s -> case mergeRngSpec r1 r2 of
      NeverRng msg -> NeverMap msg
      r -> MapSpec s (comb1 d1 d2) r

instance (Ord dom) => Monoid (MapSpec dom rng) where mempty = MapSpec Nothing Nothing None

-- =======================================================

solveMap :: forall dom rng era. V era (Map dom rng) -> Pred era -> Typed (MapSpec dom rng)
solveMap v1@(V _ r@(MapR dom rng) _) cond = explain ("Solving (" ++ show cond ++ ")") $ case cond of
  (Sized (Fixed (Lit Word64R n)) (Var v2))
    | Name v1 == Name v2 -> do
        -- With none <- hasOrd rng None
        pure (MapSpec (Just n) Nothing None)
  (Var v2 :=: expr) | Name v1 == Name v2 ->
    do
      With m <- evalWithOrd r expr rng
      pure (MapEqual m)
  (expr :=: v2@(Var _)) -> solveMap v1 (v2 :=: expr)
  -- delegations: (Map Cred Pool) | HasDomain delegations delegationsDom, (rng delegations) ⊆  pools
  (Rng (Var v2) :<=: expr)
    | Name v1 == Name v2 -> do
        dyn <- eval expr
        With n <- isDynSet dyn rng
        pure (MapSpec Nothing Nothing (SubsetRng n))

  -- e.g. poolDistr: (Map Pool Rational) | (Fixed 1 % 1) =∑= sum poolDistr
  (SumsTo expr [SumMap (Var v2)])
    | Name v1 == Name v2 -> do
        n <- evalWith rng expr
        case rng of
          IntR -> pure $ MapSpec Nothing Nothing (SumRng n)
          CoinR -> pure $ MapSpec Nothing Nothing (SumRng n)
          RationalR -> pure $ MapSpec Nothing Nothing (SumRng n)
          _ -> failT ["Type " ++ show rng ++ " does not have Summable instance"]
  -- (SumsTo _expr xs) | exactlyOne (isSumMap (Name v1)) xs -> error "STOP 1"

  (SumsTo expr xs) | exactlyOne (isMapVar (Name v1)) xs -> do
    let cRep = repOf expr
    t <- simplify expr
    With (Id tx) <- hasAdd cRep (Id t)
    explain ("Solving (" ++ show cond ++ ")") (solveSummands v1 tx xs)
  (Random (Var v2)) | Name v1 == Name v2 -> do
    With none2 <- hasOrd rng None
    pure $ MapSpec Nothing Nothing none2

  -- e.g poolDeposits: (Map Pool Coin)  | HasDomain poolDeposits poolDistrDom
  (HasDom (Var v2) expr) | Name v1 == Name v2 -> do
    dyn <- eval expr
    With set <- isDynSet dyn dom
    With emptyRngSpec <- hasOrd rng None
    pure $ MapSpec Nothing (Just set) emptyRngSpec
  other -> failT ["Cannot solve map condition: " ++ show other]

-- | Is the Sum a variable (of a map). Only SumMap and Project store maps.
isMapVar :: Name era -> Sum era c -> Bool
isMapVar n1 (SumMap (Var v2)) = n1 == Name v2
isMapVar n1 (Project (Var v2)) = n1 == Name v2
isMapVar _ _ = False

exactlyOne :: (a -> Bool) -> [a] -> Bool
exactlyOne _ [] = False
exactlyOne pp (x : xs) = pp x && all (not . pp) xs || exactlyOne pp xs

solveSummands :: Adds c => V era (Map dom rng) -> c -> [Sum era c] -> Typed (MapSpec dom rng)
solveSummands (V _ (MapR _ r) _) c [Project (Var (V _ (MapR _ r1) _))] = do
  Refl <- sameRep r r1
  pure (MapSpec Nothing Nothing (ProjRng r1 c))
solveSummands (V _ (MapR _ r) _) c [SumMap (Var (V _ (MapR _ r1) _))] = do
  Refl <- sameRep r r1
  pure (MapSpec Nothing Nothing (SumRng c))
solveSummands v c (s : ss) | isMapVar (Name v) s = solveSummands v c (ss ++ [s])
solveSummands v c (s : ss) = do
  d <- simplifySum s
  solveSummands v (subtract c d) ss
solveSummands v _ [] = failT ["Does not have exactly one summand with variable " ++ show (Name v)]

solveMaps :: Ord dom => V era (Map dom rng) -> [Pred era] -> Typed (MapSpec dom rng)
solveMaps v@(V nm (MapR _ _) _) cs = foldlM' accum (MapSpec Nothing Nothing None) cs
  where
    accum spec cond = do
      condspec <- solveMap v cond
      explain ("Solving Map constraint (" ++ show cond ++ ") for variable " ++ show nm) (liftT (spec <> condspec))

hasAdd :: Rep era t -> s t -> Typed (HasCond Adds (s t))
hasAdd IntR x = pure $ With x
hasAdd CoinR x = pure $ With x
hasAdd RationalR x = pure $ With x
hasAdd n _ = failT [show n ++ " does not have Adds instance"]

genMap ::
  forall a b era.
  (Ord a, Era era) =>
  Rep era (Map a b) ->
  MapSpec a b ->
  Typed (Gen (Map a b))
genMap rep@(MapR d r) cond = explain ("Producing Map generator for " ++ showMapSpec rep cond) $ case cond of
  (MapEqual m) -> Typed $ Right $ pure m -- or equivalently (pure $ pure m) which is confusing
  (NeverMap ss) -> failT ss
  (MapSpec Nothing Nothing None) -> pure $ genRep rep
  (MapSpec Nothing Nothing (NeverRng xs)) -> failT xs
  (MapSpec Nothing Nothing (Equal rs)) -> pure $ mapFromRange rs (genRep d)
  (MapSpec Nothing Nothing (SumRng tot)) -> pure $ do
    Positive n <- arbitrary
    ranges <- partition n tot
    mapFromRange ranges (genRep d)
  (MapSpec Nothing Nothing (ProjRng _ tot)) -> pure $ do
    Positive n <- arbitrary
    ranges <- partition n tot
    mapFromProj ranges (genRep d) (genT @b)
  (MapSpec Nothing Nothing (SubsetRng set)) -> pure $ do
    rs <- subsetFromSet set
    mapFromRange (Set.toList rs) (genRep d)
  (MapSpec Nothing Nothing (DisjointRng set)) -> pure $ do
    rs <- genRep (SetR r)
    mapFromRange (Set.toList (Set.difference rs set)) (genRep d)
  (MapSpec Nothing (Just _) (NeverRng xs)) -> failT xs
  (MapSpec Nothing (Just dom) None) -> pure $ mapFromSet dom (genRep r)
  (MapSpec Nothing (Just dom) (SumRng tot)) -> pure $ do
    ranges <- partition (Set.size dom) tot
    pure (Map.fromList $ zip (Set.toList dom) ranges)
  (MapSpec Nothing (Just dom) (ProjRng _r tot)) -> pure $ do
    ranges <- partition (Set.size dom) tot
    projs <- mapM genT ranges
    pure (Map.fromList $ zip (Set.toList dom) projs)
  (MapSpec Nothing (Just dom) (Equal rs)) ->
    pure $ pure (Map.fromList (zip (Set.toList dom) rs))
  (MapSpec Nothing (Just dom) (SubsetRng set)) ->
    pure $ mapFromSet dom (itemFromSet set)
  (MapSpec Nothing (Just dom) (DisjointRng set)) ->
    pure $ mapFromSet dom (suchThat (genRep r) (`Set.notMember` set))
  (MapSpec (Just n) Nothing None) -> pure $ genSizedRep (fromIntegral n) rep
  (MapSpec (Just _) Nothing (NeverRng xs)) -> failT xs
  (MapSpec (Just n) Nothing (SumRng tot)) -> pure $ do
    ranges <- partition (fromIntegral n) tot
    mapFromRange ranges (genRep d)
  (MapSpec (Just n) Nothing (ProjRng _r tot)) -> pure $ do
    ranges <- partition (fromIntegral n) tot
    mapFromProj ranges (genRep d) genT
  (MapSpec (Just n) Nothing (Equal rs))
    | (fromIntegral n) == length rs -> pure $ mapFromRange rs (genRep d)
    | otherwise -> failT ["Explicit size and exact range don't match"]
  (MapSpec (Just n) Nothing (SubsetRng rs)) ->
    pure $ mapSized (fromIntegral n) (genRep d) (itemFromSet rs)
  (MapSpec (Just n) Nothing (DisjointRng rs)) ->
    pure $ mapSized (fromIntegral n) (genRep d) (suchThat (genRep r) (`Set.notMember` rs))
  (MapSpec (Just _) (Just _) (NeverRng xs)) -> failT xs
  (MapSpec (Just n) (Just dom) None) ->
    pure $ mapSized (fromIntegral n) (itemFromSet dom) (genRep r)
  (MapSpec (Just n) (Just dom) (SumRng tot))
    | (fromIntegral n) == Set.size dom ->
        pure
          ( do
              ranges <- partition (fromIntegral n) tot
              pure (Map.fromList $ zip (Set.toList dom) ranges)
          )
    | otherwise -> failT ["Explicit size and dom set don't match"]
  (MapSpec (Just n) (Just dom) (ProjRng _r tot))
    | (fromIntegral n) == Set.size dom ->
        pure
          ( do
              ranges <- partition (fromIntegral n) tot
              projs <- mapM genT ranges
              pure (Map.fromList $ zip (Set.toList dom) projs)
          )
    | otherwise -> failT ["Explicit size and dom set don't match"]
  (MapSpec (Just n) (Just dom) (Equal rs))
    | (fromIntegral n) == length rs -> pure $ mapFromRange rs (itemFromSet dom)
    | otherwise -> failT ["Explicit size and exact range don't match"]
  (MapSpec (Just n) (Just dom) (SubsetRng rng)) ->
    pure $ mapSized (fromIntegral n) (itemFromSet dom) (itemFromSet rng)
  (MapSpec (Just n) (Just dom) (DisjointRng rng)) ->
    pure $ mapSized (fromIntegral n) (itemFromSet dom) (suchThat (genRep r) (`Set.notMember` rng))

--   _other -> failT ["Missing patterns FIX ME"]
-- ================================================

data SetSpec a = (Ord a) => SetSpec (Maybe Int) (RngSpec a) | NeverSet [String]

showSetSpec :: Rep era a -> SetSpec a -> String
showSetSpec rep (SetSpec m r) = "(SetSpec " ++ show m ++ " " ++ showRngSpec rep r ++ ")"
showSetSpec _ (NeverSet _) = "NeverSet"

instance LiftT (SetSpec t) where
  liftT (NeverSet xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = NeverSet s
  dropT (Typed (Right x)) = x

mergeSetSpec :: Ord a => SetSpec a -> SetSpec a -> SetSpec a
mergeSetSpec s1 s2 = case (s1, s2) of
  (NeverSet xs, NeverSet ys) -> NeverSet (xs ++ ys)
  (NeverSet xs, _) -> NeverSet xs
  (_, NeverSet ys) -> NeverSet ys
  (SetSpec Nothing xs, SetSpec Nothing ys) -> case mergeRngSpec xs ys of
    NeverRng x -> NeverSet x
    x -> SetSpec Nothing x
  (SetSpec (Just a) xs, SetSpec Nothing ys) -> case mergeRngSpec xs ys of
    NeverRng x -> NeverSet x
    x -> SetSpec (Just a) x
  (SetSpec Nothing xs, SetSpec (Just a) ys) -> case mergeRngSpec xs ys of
    NeverRng x -> NeverSet x
    x -> SetSpec (Just a) x
  (SetSpec (Just n) xs, SetSpec (Just m) ys)
    | n == m -> case mergeRngSpec xs ys of
        NeverRng x -> NeverSet x
        x -> SetSpec (Just n) x
    | otherwise -> NeverSet ["Two SetSpec with explicit sizes can't be merged: " ++ show (n, m)]

instance Ord a => Semigroup (SetSpec a) where
  (<>) = mergeSetSpec

instance (Ord a) => Monoid (SetSpec a) where
  mempty = SetSpec Nothing None

sumFromDyn :: Rep era t -> Dyn era -> Typed (HasCond Adds (Id t))
sumFromDyn rep (Dyn rep2 m) = case (testEql rep rep2) of
  Just Refl -> hasSummable rep2 (Id m)
  Nothing -> failT ["(Dyn " ++ show rep2 ++ " _) does not store expected type: " ++ show rep]

hasSummable :: Rep era t -> (s t) -> Typed (HasCond Adds (s t))
hasSummable IntR x = pure $ With x
hasSummable CoinR x = pure $ With x
hasSummable RationalR x = pure $ With x
hasSummable r _ = failT [show r ++ " does not have Adds instance."]

solveSet :: V era (Set a) -> Pred era -> Typed (SetSpec a)
solveSet v1@(V _ (SetR r) _) (Sized (Fixed (Lit Word64R n)) (Var v2))
  | Name v1 == Name v2 = do
      With none2 <- hasOrd r None
      pure (SetSpec (Just (fromIntegral n)) none2)
solveSet v1@(V _ (SetR r) _) (Var v2 :=: expr)
  | Name v1 == Name v2 = do
      dyn <- eval expr
      With set <- isDynSet dyn r
      pure $ SetSpec Nothing (Equal (Set.toList set))
solveSet v1 (expr :=: v2@(Var _)) = solveSet v1 (v2 :=: expr)
solveSet v1@(V _ (SetR r) _) (Var v2 :<=: expr)
  | Name v1 == Name v2 = do
      dyn <- eval expr
      With set <- isDynSet dyn r
      pure $ SetSpec Nothing (SubsetRng set)
solveSet v1@(V _ (SetR r) _) (Disjoint (Var v2) expr)
  | Name v1 == Name v2 = do
      dyn <- eval expr
      With set <- isDynSet dyn r
      pure $ SetSpec Nothing (DisjointRng set)
solveSet v1@(V _ (SetR _) _) (Disjoint expr (Var v2)) = solveSet v1 (Disjoint (Var v2) expr)
solveSet v1@(V _ (SetR _) _) cond@(_ :<=: Var v2)
  | Name v1 == Name v2 = failT ["Don't know how to solve superset constraint: " ++ show cond]
solveSet v1@(V _ (SetR r) _) (SumsTo expr [SumSet (Var v2)])
  | Name v1 == Name v2 = do
      dyn <- eval expr
      With (Id m2) <- sumFromDyn r dyn
      pure $ SetSpec Nothing (SumRng m2)
solveSet v1@(V _ (SetR _) _) (Random (Var v2))
  | Name v1 == Name v2 = pure $ SetSpec Nothing None
solveSet _ cond = failT ["Can't solveSet " ++ show cond]

solveSets :: V era (Set a) -> [Pred era] -> Typed (SetSpec a)
solveSets v@(V nm (SetR _) _) cs = foldlM' accum mempty cs
  where
    accum spec cond = do
      condspec <- solveSet v cond
      explain ("Solving Set constraint (" ++ show cond ++ ") for variable " ++ show nm) (liftT (spec <> condspec))

genSet ::
  (Ord a) =>
  Era era =>
  Rep era (Set a) ->
  SetSpec a ->
  Typed (Gen (Set a))
genSet rep@(SetR r) cond = explain ("Producing Set generator for " ++ showSetSpec r cond) $ case cond of
  NeverSet ss -> failT ss
  SetSpec _ (NeverRng xs) -> failT xs
  SetSpec Nothing None -> pure $ genRep rep
  SetSpec Nothing (SubsetRng x) -> pure $ subsetFromSet x
  SetSpec Nothing (DisjointRng x) ->
    -- TODO: might want to push the suchThat into the element generator here to improve
    --       distribution.
    pure $ suchThat (genRep rep) (Set.disjoint x)
  SetSpec Nothing (Equal xs) -> pure $ pure (Set.fromList xs)
  SetSpec Nothing (ProjRng _ _) -> failT ["FIX ME: SetSpec Nothing (ProjRng _ _)"]
  SetSpec Nothing (SumRng n) -> pure $ do
    i <- choose (2, 5) -- We don't want 'i'too large, or we might not be able to break 'n' into 'i' parts.
    xs <- partition i n
    pure (Set.fromList xs)
  SetSpec (Just n) None -> pure $ genSizedRep n rep
  SetSpec (Just n) (SubsetRng set)
    | Set.size set < n -> failT ["Cannot make subset of size " ++ show n ++ " from set of size " ++ show (Set.size set)]
    | otherwise -> pure $ subsetFromSetWithSize set n
  SetSpec (Just n) (DisjointRng set) -> pure $ setSized n (suchThat (genRep r) (`Set.notMember` set))
  SetSpec (Just _) (ProjRng _ _) -> failT ["FIX ME:  SetSpec (Just _) (ProjRng _ _)"]
  SetSpec (Just n) (Equal xs) ->
    if n == length xs
      then pure $ pure (Set.fromList xs)
      else failT ["Explicit size and exact set don't match up"]
  SetSpec (Just i) (SumRng n) -> pure $ do
    xs <- partition i n
    pure (Set.fromList xs)

-- =============================================================
data SumSpec t where
  SumSpec :: Adds t => Maybe t -> Maybe t -> SumSpec t
  -- ProjSpec :: Sums t c => MapSpec era t -> SumSpec t
  NeverSum :: [String] -> SumSpec t

showSumSpec :: SumSpec a -> String
showSumSpec (SumSpec m r) = "(SumSpec " ++ show m ++ " " ++ show r ++ ")"
showSumSpec (NeverSum _) = "NeverSum"

instance LiftT (SumSpec t) where
  liftT (NeverSum xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = NeverSum s
  dropT (Typed (Right x)) = x

sameMaybe :: (Show x, Eq x) => Maybe x -> Maybe x -> Either [String] (Maybe x)
sameMaybe Nothing Nothing = Right Nothing
sameMaybe (Just x) Nothing = Right (Just x)
sameMaybe Nothing (Just x) = Right (Just x)
sameMaybe (Just x) (Just y) =
  if x == y
    then Right (Just x)
    else Left ["Not the same in sameMaybe: " ++ show (x, y)]

mergeSumSpec :: (Adds t) => SumSpec t -> SumSpec t -> SumSpec t
mergeSumSpec (NeverSum xs) (NeverSum ys) = NeverSum (xs ++ ys)
mergeSumSpec (NeverSum xs) _ = NeverSum xs
mergeSumSpec _ (NeverSum xs) = NeverSum xs
mergeSumSpec (SumSpec x a) (SumSpec y b) = case (sameMaybe x y, sameMaybe a b) of
  (Right z, Right c) -> SumSpec z c
  (Left z, Right _) -> NeverSum z
  (Right _, Left c) -> NeverSum c
  (Left z, Left c) -> NeverSum (z ++ c)

instance
  (Adds t) =>
  Semigroup (SumSpec t)
  where
  (<>) = mergeSumSpec

instance
  (Adds t) =>
  Monoid (SumSpec t)
  where
  mempty = SumSpec Nothing Nothing

{-
data SumsOrAdds t where
   AddsCond :: Adds c => c -> SumsOrAdds c
   SumsCond :: Sums t c => t -> SumsOrAdds c

evalSum2 :: Rep era t -> Sum era t -> Typed (SumsOrAdds t)
evalSum2 rep (One expr) = do
  (Dyn rep2 m) <- eval expr
  case (testEql rep rep2) of
     Just Refl -> case rep2 of
       IntR -> pure $ AddsCond m
       CoinR -> pure $ AddsCond m
       RationalR -> pure $ AddsCond m
       _ -> failT [show rep2 ++ " does not have Adds instance."]
evalSum2 _rep (SumMap (Fixed (Lit (MapR _ _) m))) = pure $ AddsCond (Map.foldl' add zero m)
evalSum2 _rep (SumSet (Fixed (Lit (SetR _) m))) = pure $ AddsCond (Set.foldl' add zero m)
-- evalSum2 _rep (Project (Fixed (Lit (MapR _ r) m))) =  pure $ SumsCond (Map.foldl' accum zero m)
  -- where accum ans t = add ans (getsum t)
evalSum2 _rep x = failT ["Can't evalSum " ++ show x]

evalSums2 :: Adds t => Rep era t -> [Sum era t] -> Typed (HasCond Adds (Id t))
evalSums2 rep xs = foldlM' accum (With (Id zero)) xs
  where
    accum (With (Id n)) x = do
      choice <- evalSum2 rep x
      case choice of
        AddsCond m -> pure $ With (Id (add n m))
        SumsCond _ m -> pure $ With (Id (add n (getsum m)))
-}

evalSum :: Adds t => Rep era t -> Sum era t -> Typed (HasCond Adds (Id t))
evalSum rep (One expr) = do
  dyn <- eval expr
  sumFromDyn rep dyn
evalSum _rep (SumMap (Fixed (Lit (MapR _ _) m))) = pure $ With (Id (Map.foldl' add zero m))
evalSum _rep (SumSet (Fixed (Lit (SetR _) m))) = pure $ With (Id (Set.foldl' add zero m))
evalSum _rep (Project (Fixed (Lit (MapR _ _) m))) = pure $ With (Id (Map.foldl' accum zero m))
  where
    accum ans t = add ans (getsum t)
evalSum _rep x = failT ["Can't evalSum " ++ show x]

evalSums :: Adds t => Rep era t -> [Sum era t] -> Typed (HasCond Adds (Id t))
evalSums rep xs = foldlM' accum (With (Id zero)) xs
  where
    accum (With (Id n)) x = do
      With (Id m) <- evalSum rep x
      pure (With (Id (add n m)))

solveSum :: Adds t => V era t -> Pred era -> Typed (SumSpec t)
solveSum v1@(V _ r _) c = case c of
  (Sized expr (Var v2)) | Name v1 == Name v2 -> do
    dyn <- eval expr
    With (Id n) <- sumFromDyn r dyn
    pure $ SumSpec (Just n) Nothing
  (Sized (Var v2) expr) | Name v1 == Name v2 -> do
    dyn <- eval expr
    With (Id n) <- sumFromDyn r dyn
    pure $ SumSpec Nothing (Just n)
  (expr :=: (Var v2)) | Name v1 == Name v2 -> do
    dyn <- eval expr
    With (Id n) <- sumFromDyn r dyn
    pure $ SumSpec (Just n) Nothing
  ((Var v2) :=: expr) | Name v1 == Name v2 -> do
    dyn <- eval expr
    With (Id n) <- sumFromDyn r dyn
    pure $ SumSpec Nothing (Just n)
  (Random (Var v2)) | Name v1 == Name v2 -> pure $ SumSpec Nothing Nothing
  (SumsTo (Var v2@(V _ r2 _)) xs@(_ : _)) | Name v1 == Name v2 -> do
    Refl <- sameRep r r2
    With (Id n) <- evalSums r xs
    pure $ SumSpec Nothing (Just n)
  other -> failT ["Can't solveSum " ++ show other]

solveSums :: Adds t => V era t -> [Pred era] -> Typed (SumSpec t)
solveSums v@(V nm _ _) cs = foldlM' accum mempty cs
  where
    accum spec cond = do
      condspec <- solveSum v cond
      explain
        ("Solving Sum constraint (" ++ show cond ++ ") for variable " ++ show nm)
        (liftT (spec <> condspec))

genSum :: (Adds t, Era era) => Rep era t -> SumSpec t -> Typed (Gen t)
genSum rep spec = explain ("Producing Sum generator for " ++ showSumSpec spec) $ case spec of
  (NeverSum msgs) -> failT msgs
  (SumSpec Nothing Nothing) -> pure $ genRep rep
  (SumSpec (Just t) Nothing) -> pure $ pure t
  (SumSpec Nothing (Just tot)) -> pure $ pure tot
  (SumSpec (Just x) (Just tot)) ->
    if x == tot
      then pure $ pure x
      else failT ["Not the same in genSum: " ++ show (x, tot)]

isSumType :: forall era t. Rep era t -> Bool
isSumType rep = case hasSummable rep (Id (undefined :: t)) of
  (Typed (Right (With _))) -> True
  (Typed (Left _)) -> False

-- ===================================================

genPair :: Era era => (V era t, [Pred era]) -> Typed (Gen t)
genPair (v@(V _nm rep@(MapR _ _) _), cs) = (solveMaps v cs) >>= genMap rep
genPair (v@(V _nm rep@(SetR _) _), cs) = (solveSets v cs) >>= genSet rep
genPair (v@(V _nm rep _), cs) | isSumType rep = do
  With v2 <- hasSummable rep v
  xs <- solveSums v2 cs
  genSum rep xs
genPair (v1@(V _nm rep _), [Random (Var v2)]) | Name v1 == Name v2 = pure $ genRep rep
genPair ((V _nm rep _), cs) = failT ["No solution at type " ++ show rep ++ " for condtions " ++ show cs]

genOrFail ::
  Era era =>
  Either [String] (Subst era) ->
  (Name era, [Pred era]) ->
  Gen (Either [String] (Subst era))
genOrFail (Right subst) (Name v@(V _ rep _), conds) =
  case runTyped $
    ifTrace
      (pad 20 (show (Name v)) ++ " | " ++ showL show "," (map (substPred subst) conds))
      (genPair (v, map (substPred subst) conds)) of
    Right gen -> do
      t <- gen
      pure (Right (SubItem v (Fixed (Lit rep t)) : subst))
    Left msgs -> pure (Left msgs)
genOrFail (Left msgs) _ = pure (Left msgs)

genOrFailList ::
  Era era =>
  Either [String] (Subst era) ->
  [(Name era, [Pred era])] ->
  Gen (Either [String] (Subst era))
genOrFailList = foldlM' genOrFail

genDependGraph :: Proof era -> DependGraph era -> Gen (Either [String] (Subst era))
genDependGraph (Shelley _) (DependGraph pairs) = genOrFailList (Right []) pairs
genDependGraph (Allegra _) (DependGraph pairs) = genOrFailList (Right []) pairs
genDependGraph (Mary _) (DependGraph pairs) = genOrFailList (Right []) pairs
genDependGraph (Alonzo _) (DependGraph pairs) = genOrFailList (Right []) pairs
genDependGraph (Babbage _) (DependGraph pairs) = genOrFailList (Right []) pairs
genDependGraph (Conway _) (DependGraph pairs) = genOrFailList (Right []) pairs

-- ==========================================================
-- Extras

known :: Rep era s -> Literal era t -> Maybe s
known s (Lit r x) = case testEql s r of Nothing -> Nothing; Just Refl -> Just x

repOf :: Term era t -> Rep era t
repOf (Fixed (Lit r _)) = r
repOf (Var (V _ r _)) = r
repOf (Dom e) = case repOf e of MapR d _ -> SetR d
repOf (Rng e) = case repOf e of MapR _ r -> SetR r

-- =======================================================================

{-  Here is a total ordering of the variables

ghci> test6 False
ghci> test6 False
utxoCoin: Coin                        | Random utxoCoin
treasury: Coin                        | Random treasury
reserves: Coin                        | Random reserves
pools: (Set Pool )                    | Sized (Fixed 10) pools
poolDistrDom: (Set Pool )             | poolDistrDom ⊆  pools
regPools: (Map Pool (PoolParams c))   | HasDomain regPools poolDistrDom
retiringDom: (Set Pool )              | retiringDom ⊆  poolDistrDom
retiring: (Map Pool EpochNo)          | HasDomain retiring retiringDom
poolDistr: (Map Pool Rational)        | (Fixed 1 % 1) =∑= sum poolDistr, HasDomain poolDistr poolDistrDom
poolDeposits: (Map Pool Coin)         | HasDomain poolDeposits poolDistrDom
poolDepositsSum: Coin                 | poolDepositsSum =∑= sum poolDeposits
fees: Coin                            | Random fees
creds: (Set Cred )                    | Sized (Fixed 10) creds
stakeDepositsDom: (Set Cred )           | stakeDepositsDom ⊆  creds
delegationsDom: (Set Cred )           | delegationsDom ⊆  stakeDepositsDom, delegationsDom ⊆  creds
delegations: (Map Cred Pool)          | HasDomain delegations delegationsDom, (rng delegations) ⊆  pools
stakeDeposits: (Map Cred Coin)          | HasDomain stakeDeposits stakeDepositsDom
stakeDepositsSum: Coin                  | stakeDepositsSum =∑= sum stakeDeposits
deposits: Coin                        | deposits =∑= stakeDepositsSum + poolDepositsSum
rewards: (Map Cred Coin)              | HasDomain rewards stakeDepositsDom
rewardsSum: Coin                      | rewardsSum =∑= sum rewards
totalAda: Coin                        | totalAda =∑= utxoCoin + treasury + reserves + fees + stakeDepositsSum + poolDepositsSum + rewardsSum
-}

-- | Here is a transcription by hand, just to get a feel of what we have to generate.
foo :: forall era. Era era => Proof era -> Gen [P era]
foo _proof = do
  utxoCoinX <- genRep @era CoinR
  treasuryX <- genRep @era CoinR
  reservesX <- genRep @era CoinR
  poolsX <- setSized 10 (genRep @era PoolHashR)
  poolDistrDomX <- subsetFromSet poolsX
  regPoolsX <- mapFromSet poolDistrDomX (genRep @era PoolParamsR)
  retiringDomX <- subsetFromSet poolDistrDomX
  retiringX <- mapFromSet retiringDomX (genRep @era EpochR)
  rs <- partition (Set.size poolDistrDomX) (1 :: Rational)
  let poolDistrX = mapFromDomRange poolDistrDomX rs
  poolDepositsX <- mapFromSet poolDistrDomX (genRep @era CoinR)
  let poolDepositsSumX = sumMap poolDepositsX
  feesX <- genRep @era CoinR
  credsX <- setSized 10 (genRep @era CredR)
  keyDepositxDomX <- subsetFromSet credsX
  delegationsDomX <- subsetFromSet keyDepositxDomX
  delegationsX <- mapFromSet delegationsDomX (itemFromSet poolsX)
  stakeDepositsX <- mapFromSet keyDepositxDomX (genRep @era CoinR)
  let stakeDepositsSumX = sumMap stakeDepositsX
  let _depositsX = add poolDepositsSumX stakeDepositsSumX
  rewardsX <- mapFromSet keyDepositxDomX (genRep @era CoinR)
  let rewardsSumX = sumMap rewardsX
  let totalAdaX = List.foldl' add zero [utxoCoinX, treasuryX, reservesX, feesX, stakeDepositsSumX, poolDepositsSumX, rewardsSumX]
  pure
    [ p utxoCoin utxoCoinX
    , p treasury treasuryX
    , p reserves reservesX
    , p fees feesX
    , P (V "keyDepositSum" CoinR No) stakeDepositsSumX
    , P (V "poolDepositSum" CoinR No) poolDepositsSumX
    , P (V "rewardsSum" CoinR No) rewardsSumX
    , p totalAda totalAdaX
    , p poolsUniv poolsX
    , P (V "poolDistrDom" (SetR PoolHashR) No) poolDistrDomX
    , p regPools regPoolsX
    , p retiring retiringX
    , p poolDistrVar poolDistrX
    , p poolDeposits poolDepositsX
    , p credsUniv credsX
    , p rewards rewardsX
    , p delegations delegationsX
    , p stakeDeposits stakeDepositsX
    ]

poolDistrVar :: Term era (Map (KeyHash 'StakePool (EraCrypto era)) Rational)
poolDistrVar = Var (V "poolDistr" (MapR PoolHashR RationalR) No)

p :: Term era t -> t -> P era
p (Var v) t = P v t
p other _ = error ("p applied to non Var: " ++ show other)

sumMap :: Adds t => Map a t -> t
sumMap m = Map.foldl' add zero m
