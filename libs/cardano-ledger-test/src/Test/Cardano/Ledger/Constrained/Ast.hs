{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | The types that make up the Abstract Syntax Trees of the Language
module Test.Cardano.Ledger.Constrained.Ast where

import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Pretty
import Data.Char (toLower)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Universe as Univ (Any (..))
import Data.Word (Word64)
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Classes (Adds (..), Count (..), FromList (..), Sizeable (..), Sums (..))
import Test.Cardano.Ledger.Constrained.Env (Access (..), AnyF (..), Env (..), Field (..), Name (..), Payload (..), V (..), findVar, storeVar)
import Test.Cardano.Ledger.Constrained.Monad (Typed (..), failT)
import Test.Cardano.Ledger.Constrained.Size (OrdCond (..), Size (..), runOrdCond, runSize)
import Test.Cardano.Ledger.Constrained.TypeRep (Rep (..), synopsis, testEql, (:~:) (Refl))
import Test.QuickCheck (Gen)

-- ================================================

data Term era t where
  Lit :: Rep era t -> t -> Term era t
  Var :: V era t -> Term era t
  Dom :: Ord a => Term era (Map a b) -> Term era (Set a)
  Rng :: (Ord a, Ord b) => Term era (Map a b) -> Term era (Set b)
  ProjM :: (Ord a) => Lens' b t -> Rep era t -> Term era (Map a b) -> Term era (Map a t)
  ProjS :: (Ord b, Ord t) => Lens' b t -> Rep era t -> Term era (Set b) -> Term era (Set t)
  Delta :: Term era Coin -> Term era DeltaCoin
  Negate :: Term era DeltaCoin -> Term era DeltaCoin
  Restrict :: Ord a => Term era (Set a) -> Term era (Map a b) -> Term era (Map a b)

infix 4 :=:
infix 4 :<-:

data Pred era where
  Sized :: Sizeable t => Term era Size -> Term era t -> Pred era
  (:=:) :: Eq a => Term era a -> Term era a -> Pred era
  Subset :: Ord a => Term era (Set a) -> Term era (Set a) -> Pred era
  Disjoint :: Ord a => Term era (Set a) -> Term era (Set a) -> Pred era
  SumsTo :: Adds c => c -> Term era c -> OrdCond -> [Sum era c] -> Pred era
  Random :: Term era t -> Pred era
  Component :: Term era t -> [AnyF era t] -> Pred era
  CanFollow :: Count n => Term era n -> Term era n -> Pred era
  Member :: Ord a => Term era a -> Term era (Set a) -> Pred era
  NotMember :: Ord a => Term era a -> Term era (Set a) -> Pred era
  MapMember :: (Ord k, Eq v, Ord v) =>  Term era k -> Term era v -> Term era (Map k v) -> Pred era
  NotMapMember :: (Ord k, Eq v, Ord v) =>  Term era k -> Term era v -> Term era (Map k v) -> Pred era
  (:<-:) :: Term era t -> Target era t -> Pred era
  GenFrom :: Term era t -> Target era (Gen t) -> Pred era
  List :: FromList f t => Term era (f t) -> [Term era t] -> Pred era
  Choose :: Eq t => Size -> Term era [t] -> [(Target era t, [Pred era])] -> Pred era
  Maybe :: Term era (Maybe t) -> Target era t -> [Pred era] -> Pred era

data Sum era c where
  SumMap :: Adds c => Term era (Map a c) -> Sum era c
  SumList :: Adds c => Term era [c] -> Sum era c
  One :: Term era c -> Sum era c
  Project :: forall x c a era. Sums x c => Rep era c -> Term era (Map a x) -> Sum era c -- TODO uses lenses here

-- ====================================================================
-- Special patterns for building literal Terms of type Size and Word64

infix 4 :⊆:
pattern (:⊆:) :: forall era. (forall a. Ord a => Term era (Set a) -> Term era (Set a) -> Pred era)
pattern x :⊆: y = Subset x y

pattern ExactSize :: Int -> Term era Size
pattern ExactSize x <- (sameRng -> Just x)
  where
    ExactSize x = Lit SizeR (SzRng x x)

pattern AtLeast :: Int -> Term era Size
pattern AtLeast n = Lit SizeR (SzLeast n)

pattern Size :: Size -> Term era Size
pattern Size n = Lit SizeR n

pattern AtMost :: Int -> Term era Size
pattern AtMost n = Lit SizeR (SzMost n)

pattern Range :: Int -> Int -> Term era Size
pattern Range i j <- Lit SizeR (SzRng i j)
  where
    Range i j =
      if i <= j
        then Lit SizeR (SzRng i j)
        else
          error
            ( "Bad call to "
                ++ show (SzRng i j)
                ++ ". It is not the case that ("
                ++ show i
                ++ " <= "
                ++ show j
                ++ ")."
            )

sameRng :: Term era Size -> Maybe Int
sameRng (Lit SizeR (SzRng x y)) = if x == y then Just x else Nothing
sameRng _ = Nothing

pattern Word64 :: Word64 -> Term era Word64
pattern Word64 x = Lit Word64R x

-- ====================================================================

infixl 0 :$

data Target era t where
  Simple :: Term era t -> Target era t
  (:$) :: Target era (a -> b) -> Target era a -> Target era b
  Constr :: String -> (a -> b) -> Target era (a -> b)

infixl 0 ^$

-- | Version of (:$) That takes a Term on the right, rather than a Target
(^$) :: Target era (a -> t) -> Term era a -> Target era t
(^$) f x = f :$ Simple x

constTarget :: t -> Target era t
constTarget t = Constr "constTarget" (const t) ^$ (Lit UnitR ())

-- ===================================

showL :: (t -> String) -> [Char] -> [t] -> [Char]
showL _f _sep [] = ""
showL f _sep [t] = f t
showL f sep (t : ts) = f t ++ sep ++ showL f sep ts

instance Show (Term era t) where
  show (Lit r k) = synopsis r k
  show (Var (V nm _rep _)) = nm -- ++ "::" ++ show _rep
  show (Dom x) = "(Dom " ++ show x ++ ")"
  show (Rng x) = "(Rng " ++ show x ++ ")"
  show (ProjM _ r t) = "(ProjM " ++ show r ++ " " ++ show t ++ ")"
  show (ProjS _ r t) = "(ProjS " ++ show r ++ " " ++ show t ++ ")"
  show (Delta x) = "(Delta " ++ show x ++ ")"
  show (Negate x) = "(Negate " ++ show x ++ ")"
  show (Restrict r t) = "(Restrict " ++ show r ++ " " ++ show t ++ ")"
  showList xs ans = unlines (ans : (map show xs))

instance Show (Sum era c) where
  show (SumMap t) = "sum " ++ show t
  show (SumList t) = "sum " ++ show t
  show (One t) = show t
  show (Project crep t) = "Project " ++ show crep ++ " " ++ show t

instance Show (Pred era) where
  show (Sized n t) = "Sized " ++ show n ++ " " ++ show t
  show (x :=: y) = show x ++ " :=: " ++ show y
  show (Subset x y) = show x ++ " ⊆  " ++ show y
  show (Disjoint x y) = "Disjoint " ++ show x ++ " " ++ show y
  show (SumsTo i c cond m) = "SumsTo (" ++ show i ++ ") " ++ show c ++ show cond ++ showL show " + " m
  show (Random x) = "Random " ++ show x
  show (Component t ws) = "Component " ++ show t ++ " " ++ show ws
  show (CanFollow x y) = "CanFollow " ++ show x ++ " " ++ show y
  show (Member x y) = "Member " ++ show x ++ " " ++ show y
  show (NotMember x y) = "NotMember " ++ show x ++ " " ++ show y
  show (MapMember x y z) = "MapMember " ++ show x ++ " " ++ show y ++ " " ++ show z
  show (NotMapMember x y z) = "NotMapMember " ++ show x ++ " " ++ show y ++ " " ++ show z
  show (x :<-: y) = show x ++ " :<-: " ++ showT y
  show (GenFrom x y) = "GenFrom " ++ show x ++ " " ++ showT y
  show (List t xs) = "List " ++ show t ++ " [" ++ showL show ", " xs ++ "]"
  show (Choose s term xs) = unlines (("Choose " ++ show s ++ " " ++ show term) : (map showchoices xs))
    where
      showchoices (target, ps) = showAllTarget target ++ showT target ++ " | " ++ showL show ", " ps ++ ")"
  show (Maybe term target ps) = "Maybe " ++ show term ++ showAllTarget target ++ " | " ++ showL show ", " ps
  showList xs ans = unlines (ans : (map show xs))

showAllTarget :: Target era t -> [Char]
showAllTarget tar = "  (forall " ++ showL show " " (Set.toList (varsOfTarget Set.empty tar)) ++ ". "

instance Show (Target era t) where
  show (Constr nm _f) = nm
  show (Simple x) = show x
  show (f :$ x) = "(" ++ show f ++ " :$ " ++ showL pp " :$ " (args x) ++ ")"
    where
      pp :: Univ.Any (Target era) -> String
      pp (Univ.Any spec) = show spec

-- | "Print a Target as nested applications"
showT :: forall era t. Target era t -> String
showT (Constr nm _f) = nm
showT (Simple x) = show x
showT (f :$ x) = "(" ++ showT f ++ " " ++ showL pp " " (args x) ++ ")"
  where
    pp :: Univ.Any (Target era) -> String
    pp (Univ.Any spec) = showT spec

args :: Target era t -> [Univ.Any (Target era)]
args (x :$ xs) = Univ.Any x : args xs
args other = [Univ.Any other]

-- | Print a Target as a record showing the struture and names of all
--   the variables involved. This documents what is in scope where
--   the Target value was defined.
ppTarget :: Target era t -> PDoc
ppTarget x = targetRecord x []

targetRecord :: Target era t -> [(Text, PDoc)] -> PDoc
targetRecord (Constr n _) xs = ppRecord (pack n) xs
targetRecord (ts :$ t) xs = targetRecord ts (targetPair t : xs)
targetRecord (Simple e) [] = ppString (show e)
targetRecord other xs = ppRecord (nameOf other) xs

nameOf :: Target era t -> Text
nameOf (Constr cs _) = pack (map toLower cs ++ "T")
nameOf (Simple (Var (V n _ _))) = pack n
nameOf (Simple term) = pack (show term)
nameOf (x :$ _) = nameOf x

targetPair :: Target era t -> (Text, PDoc)
targetPair (Simple (Var (V n rep _))) = (pack n, ppString (show rep))
targetPair x = (nameOf x, targetRecord x [])

-- ===================================================
-- Computing the variables (V era t) in a Term, Pred, Target
-- Their are no binders in any of these, so this is not so difficult
-- But (V era t) may have different 't', so we hide 't' in 'Name'

varsOfTerm :: Set (Name era) -> Term era t -> Set (Name era)
varsOfTerm ans s = case s of
  Lit _ _ -> ans
  Var v@(V _ _ _) -> Set.insert (Name v) ans
  Dom x -> varsOfTerm ans x
  Rng x -> varsOfTerm ans x
  (ProjM _ _ x) -> varsOfTerm ans x
  (ProjS _ _ x) -> varsOfTerm ans x
  Delta x -> varsOfTerm ans x
  Negate x -> varsOfTerm ans x
  Restrict st mp -> varsOfTerm (varsOfTerm ans st) mp

vars :: Term era t -> Set (Name era)
vars x = varsOfTerm Set.empty x

varsOfTarget :: Set (Name era) -> Target era t -> Set (Name era)
varsOfTarget ans s = case s of
  (a :$ b) -> varsOfTarget (varsOfTarget ans a) b
  (Simple x) -> varsOfTerm ans x
  (Constr _ _) -> ans

varsOfPred :: Set (Name era) -> Pred era -> Set (Name era)
varsOfPred ans s = case s of
  Sized a b -> varsOfTerm (varsOfTerm ans a) b
  (a :=: b) -> varsOfTerm (varsOfTerm ans a) b
  (Subset a b) -> varsOfTerm (varsOfTerm ans a) b
  (Disjoint a b) -> varsOfTerm (varsOfTerm ans a) b
  SumsTo _ x _ xs -> List.foldl' varsOfSum (varsOfTerm ans x) xs
  Random x -> varsOfTerm ans x
  Component t cs -> varsOfTerm (List.foldl' varsOfComponent ans cs) t
    where
      varsOfComponent l (AnyF (Field n r a)) = Set.insert (Name $ V n r a) l
      varsOfComponent l (AnyF (FConst _ _ _)) = l
  CanFollow a b -> varsOfTerm (varsOfTerm ans a) b
  Member a b -> varsOfTerm (varsOfTerm ans a) b
  NotMember a b -> varsOfTerm (varsOfTerm ans a) b
  MapMember a b c -> varsOfTerm (varsOfTerm (varsOfTerm ans a) b) c
  NotMapMember a b c -> varsOfTerm (varsOfTerm (varsOfTerm ans a) b) c
  a :<-: b -> varsOfTarget (varsOfTerm ans a) b
  GenFrom a b -> varsOfTarget (varsOfTerm ans a) b
  List a bs -> List.foldl' varsOfTerm (varsOfTerm ans a) bs
  Choose _ term pairs -> varsOfTerm (varsOfPairs ans pairs) term
  Maybe term target ps -> varsOfTerm (varsOfPairs ans [(target, ps)]) term

varsOfPairs :: Set (Name era) -> [(Target era t2, [Pred era])] -> Set (Name era)
varsOfPairs ans1 [] = ans1
varsOfPairs ans1 ((t, ps) : more) = varsOfPairs (act ans1 t ps) more
  where
    act ans2 tar preds = Set.union (Set.difference (List.foldl' varsOfPred Set.empty preds) (varsOfTarget Set.empty tar)) ans2

varsOfSum :: Set (Name era) -> Sum era r -> Set (Name era)
varsOfSum ans (SumMap y) = varsOfTerm ans y
varsOfSum ans (SumList y) = varsOfTerm ans y
varsOfSum ans (One y) = varsOfTerm ans y
varsOfSum ans (Project _ x) = varsOfTerm ans x

-- =====================================================
-- Subtitution of (V era t) inside of (Spec era t)

type Subst era = [SubItem era]

substToEnv :: Subst era -> Env era -> Typed (Env era)
substToEnv [] ans = pure ans
substToEnv ((SubItem v (Lit _ t)) : more) ans =
  substToEnv more (storeVar v t ans)
substToEnv ((SubItem _ e) : _) _ = failT ["Not Literal expr in substToEnv: " ++ show e]

envToSubst :: Env era -> Subst era
envToSubst (Env env) =
  [ SubItem (V x rep acc) (Lit rep t)
  | (x, Payload rep t acc) <- Map.toList env
  ]

data SubItem era where
  SubItem :: V era t -> Term era t -> SubItem era

instance Show (SubItem era) where
  show (SubItem (V nm _rep _) expr) = pad 14 nm ++ " = " ++ show expr
  showList xs ans = unlines (ans : (map show xs))

pad :: Int -> String -> String
pad n x = x ++ replicate (n - length x) ' '

extend :: V era t -> Term era t -> Subst era -> Subst era
extend v k xs = (SubItem v k) : xs

findV :: Subst era -> V era t -> Term era t
findV [] v@(V _ _ _) = Var v -- If its not in the Subst, return the Var
findV (SubItem (V n2 rep2 _) kn : more) v@(V n1 rep1 _) =
  if n1 /= n2
    then findV more v
    else case testEql rep1 rep2 of
      Just Refl -> kn
      Nothing ->
        error
          ( "In findV, we found: "
              ++ n1
              ++ ", but the types did not match. "
              ++ show rep1
              ++ " =/= "
              ++ show rep2
          )

substTerm :: Subst era -> Term era t -> Term era t
substTerm sub (Var v) = findV sub v
substTerm _ (Lit r k) = Lit r k
substTerm sub (Dom x) = Dom (substTerm sub x)
substTerm sub (Rng x) = Rng (substTerm sub x)
substTerm sub (ProjM l r x) = ProjM l r (substTerm sub x)
substTerm sub (ProjS l r x) = ProjS l r (substTerm sub x)
substTerm sub (Delta x) = Delta (substTerm sub x)
substTerm sub (Negate x) = Negate (substTerm sub x)
substTerm sub (Restrict s m) = Restrict (substTerm sub s) (substTerm sub m)

substPred :: Subst era -> Pred era -> Pred era
substPred sub (Sized a b) = Sized (substTerm sub a) (substTerm sub b)
substPred sub (a :=: b) = substTerm sub a :=: substTerm sub b
substPred sub (a `Subset` b) = substTerm sub a `Subset` substTerm sub b
substPred sub (Disjoint a b) = Disjoint (substTerm sub a) (substTerm sub b)
substPred sub (SumsTo i a cond b) = SumsTo i (substTerm sub a) cond (map (substSum sub) b)
substPred sub (Random x) = Random (substTerm sub x)
substPred sub (Component t cs) = Component (substTerm sub t) (substComp <$> cs)
  where
    substComp (AnyF w@(Field n r a)) = AnyF $ case findV sub (V n r a) of
      (Lit rep x) -> FConst rep x a
      _ -> w
    substComp x@(AnyF (FConst _ _ _)) = x
substPred sub (CanFollow a b) = CanFollow (substTerm sub a) (substTerm sub b)
substPred sub (Member a b) = Member (substTerm sub a) (substTerm sub b)
substPred sub (NotMember a b) = NotMember (substTerm sub a) (substTerm sub b)
substPred sub (MapMember a b c) = MapMember (substTerm sub a) (substTerm sub b) (substTerm sub c)
substPred sub (NotMapMember a b c) = NotMapMember (substTerm sub a) (substTerm sub b) (substTerm sub c)
substPred sub (a :<-: b) = substTerm sub a :<-: substTarget sub b
substPred sub (GenFrom a b) = GenFrom (substTerm sub a) (substTarget sub b)
substPred sub (List a b) = List (substTerm sub a) (map (substTerm sub) b)
substPred sub (Choose sz t pairs) = Choose sz (substTerm sub t) (map (subPair sub) pairs)
  where
    subPair sub0 (tar, ps) = (tar, map (substPred (sub1 ++ sub0)) ps)
      where
        sub1 = substFromTarget tar
substPred sub (Maybe term target ps) = Maybe (substTerm sub term) target (map (substPred (sub1 ++ sub)) ps)
  where
    sub1 = substFromTarget target

substFromTarget :: Target era t -> Subst era
substFromTarget tar = map (\(Name v) -> SubItem v (Var v)) (Set.toList (varsOfTarget Set.empty tar))

substSum :: Subst era -> Sum era t -> Sum era t
substSum sub (SumMap x) = SumMap (substTerm sub x)
substSum sub (SumList x) = SumList (substTerm sub x)
substSum sub (One x) = One (substTerm sub x)
substSum sub (Project crep x) = Project crep (substTerm sub x)

substTarget :: Subst era -> Target era t -> Target era t
substTarget sub (Simple e) = Simple (substTerm sub e)
substTarget sub (a :$ b) = substTarget sub a :$ substTarget sub b
substTarget _ (Constr n f) = Constr n f

-- ======================================================
-- Symbolic evaluators

-- | Simplify Terms that only contain Literals (or constant) sub-Terms
simplify :: Term era t -> Typed t
simplify (Lit _ x) = pure x
simplify (Dom (Lit _ x)) = pure (Map.keysSet x)
simplify (Dom (ProjM _ _ t)) = simplify (Dom t)
simplify (Rng (Lit _ x)) = pure (Set.fromList (Map.elems x))
simplify (Rng (ProjM l _ (Lit _ m))) = pure (Set.fromList (Map.elems (Map.map (\x -> x ^. l) m)))
simplify (ProjM l _ (Lit _ x)) = pure (Map.map (\z -> z ^. l) x)
simplify (ProjS l _ (Lit _ x)) = pure (Set.map (\z -> z ^. l) x)
simplify (ProjS l _ t) = do
  s <- simplify t
  pure (Set.map (\z -> z ^. l) s)
simplify (Delta (Lit CoinR (Coin n))) = pure (DeltaCoin n)
simplify (Negate (Lit DeltaCoinR (DeltaCoin n))) = pure (DeltaCoin (-n))
simplify (Restrict s m) = do
  sv <- simplify s
  mv <- simplify m
  pure (Map.restrictKeys mv sv)
simplify x = failT ["Can't simplify term: " ++ show x ++ ", to a value."]

-- | Simplify constant Sum's
simplifySum :: Sum era c -> Typed c
simplifySum (One (Lit _ x)) = pure x
simplifySum (One (Delta (Lit CoinR (Coin n)))) = pure (DeltaCoin n)
simplifySum (One (Negate (Lit DeltaCoinR (DeltaCoin n)))) = pure (DeltaCoin (-n))
simplifySum (SumMap (Lit _ m)) = pure (Map.foldl' add zero m)
simplifySum (SumList (Lit _ m)) = pure (List.foldl' add zero m)
simplifySum (Project _ (Lit _ m)) = pure (List.foldl' (\ans x -> add ans (getSum x)) zero m)
simplifySum x = failT ["Can't simplify Sum: " ++ show x ++ ", to a value."]

simplifyTarget :: Target era t -> Typed t
simplifyTarget (Simple t) = simplify t
simplifyTarget (Constr _ f) = pure f
simplifyTarget (x :$ y) = do
  f <- simplifyTarget x
  z <- simplifyTarget y
  pure (f z)

-- | Fully evaluate a `Term`, looking up the variables in the `Env`.
runTerm :: Env era -> Term era t -> Typed t
runTerm _ (Lit _ x) = pure x
runTerm env (Dom x) = Map.keysSet <$> runTerm env x
runTerm env (Rng x) = Set.fromList . Map.elems <$> runTerm env x
runTerm env (Var v) = findVar v env
runTerm env (ProjM l _ x) = do
  m <- runTerm env x
  pure (Map.map (\z -> z ^. l) m)
runTerm env (ProjS l _ x) = do
  m <- runTerm env x
  pure (Set.map (\z -> z ^. l) m)
runTerm env (Delta x) = do
  Coin n <- runTerm env x
  pure (DeltaCoin n)
runTerm env (Negate x) = do
  DeltaCoin n <- runTerm env x
  pure (DeltaCoin (-n))
runTerm env (Restrict s m) = do
  sv <- runTerm env s
  mv <- runTerm env m
  pure (Map.restrictKeys mv sv)

runTarget :: Env era -> Target era t -> Typed t
runTarget env (Simple t) = runTerm env t
runTarget _ (Constr _ f) = pure f
runTarget env (x :$ y) = do
  f <- runTarget env x
  z <- runTarget env y
  pure (f z)

runPred :: Env era -> Pred era -> Typed Bool
runPred env (Sized w x) = do
  sz <- runTerm env w
  item <- runTerm env x
  pure (runSize (getSize item) sz)
runPred env (x :=: y) = do
  x2 <- runTerm env x
  y2 <- runTerm env y
  pure (x2 == y2)
runPred env (Disjoint x y) = do
  x2 <- runTerm env x
  y2 <- runTerm env y
  pure (Set.disjoint x2 y2)
runPred env (Subset x y) = do
  x2 <- runTerm env x
  y2 <- runTerm env y
  pure (Set.isSubsetOf x2 y2)
runPred env (SumsTo _ x cond ys) = do
  x2 <- runTerm env x
  is <- mapM (runSum env) ys
  let y2 = List.foldl' add zero is
  pure (runOrdCond cond x2 y2)
runPred _ (Random _) = pure True
runPred env (Component t cs) = do
  t' <- runTerm env t
  and <$> mapM (runComp env t') cs
runPred env (CanFollow x y) = do
  x2 <- runTerm env x
  y2 <- runTerm env y
  pure (canFollow x2 y2)
runPred env (Member x y) = do
  x2 <- runTerm env x
  y2 <- runTerm env y
  pure (Set.member x2 y2)
runPred env (NotMember x y) = do
  x2 <- runTerm env x
  y2 <- runTerm env y
  pure (Set.notMember x2 y2)
runPred env (MapMember x y z) = do
  x2 <- runTerm env x
  y2 <- runTerm env y
  z2 <- runTerm env z
  pure $ Set.member x2 (Map.keysSet  z2) && Set.member y2 (Set.fromList (Map.elems z2))
runPred env (NotMapMember x y z) = do
  x2 <- runTerm env x
  y2 <- runTerm env y
  z2 <- runTerm env z
  pure $ not (Set.member x2 (Map.keysSet z2)) || not (Set.member y2 (Set.fromList (Map.elems z2)))
runPred env (x :<-: y) = do
  _x2 <- runTerm env x
  _y2 <- runTarget env y
  pure True
runPred env (GenFrom x y) = do
  _x2 <- runTerm env x
  _y2 <- runTarget env y
  pure True
runPred env (List x y) = do
  x2 <- runTerm env x
  y2 <- mapM (runTerm env) y
  pure (x2 == makeFromList y2)
runPred env (Maybe x tar ps) = do
  m <- runTerm env x
  case m of
    Nothing -> pure True
    (Just y) -> do
      ans <- mapM (runPred (bind tar y env)) ps
      pure (and ans)
runPred _ (p@(Choose _ _ _)) = failT ["Choose predicate in runPred", show p]

bind :: Target era t -> t -> Env era -> Env era
bind (Simple (Var v)) x env = storeVar v x env
bind t _ _ = error ("Non simple Target in bind: " ++ show t)

runComp :: Env era -> s -> AnyF era s -> Typed Bool
runComp _ _ (AnyF (Field _ _ No)) = pure False
runComp _ _ (AnyF (FConst _ _ No)) = pure False
runComp env t (AnyF (Field n r a@(Yes _ l))) = do
  t' <- runTerm env $ Var (V n r a)
  pure $ t ^. l == t'
runComp _ t (AnyF (FConst _ v (Yes _ l))) = pure $ t ^. l == v

termRep :: Term era t -> Rep era t
termRep (Lit r _) = r
termRep (Var (V _ r _)) = r
termRep (Dom (termRep -> MapR r _)) = SetR r
termRep (Rng (termRep -> MapR _ r)) = SetR r
termRep (ProjM _ t (termRep -> MapR a _)) = MapR a t
termRep (ProjS _ t (termRep -> SetR _)) = SetR t
termRep (Delta _) = DeltaCoinR
termRep (Negate _) = DeltaCoinR
termRep (Restrict _ m) = termRep m

runSum :: Env era -> Sum era c -> Typed c
runSum env (SumMap t) = Map.foldl' add zero <$> runTerm env t
runSum env (SumList t) = List.foldl' add zero <$> runTerm env t
runSum env (One t) = runTerm env t
runSum env (Project _ t) = Map.foldl' accum zero <$> runTerm env t
  where
    accum ans x = add ans (getSum x)

makeTest :: Env era -> Pred era -> Typed (String, Bool, Pred era)
makeTest env c = do
  b <- runPred env c
  pure (show c ++ " => " ++ show b, b, c)
