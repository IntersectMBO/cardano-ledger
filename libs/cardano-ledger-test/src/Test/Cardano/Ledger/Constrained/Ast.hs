{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | The types that make up the Abstract Syntax Trees of the Language
module Test.Cardano.Ledger.Constrained.Ast where

import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Core (Era, hashScript)
import Cardano.Ledger.Hashes (DataHash, ScriptHash (..))
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Data.Char (toLower)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Universe as Univ (Any (..))
import Data.Void (Void)
import Data.Word (Word64)
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Classes (
  Adds (..),
  Count (..),
  OrdCond (..),
  ScriptF (..),
  Sizeable (..),
 )
import Test.Cardano.Ledger.Constrained.Env (
  Access (..),
  AnyF (..),
  Env (..),
  Field (..),
  Name (..),
  Payload (..),
  V (..),
  findVar,
  storeVar,
 )
import Test.Cardano.Ledger.Constrained.Monad (HasConstraint (With), Typed (..), failT, monadTyped)
import Test.Cardano.Ledger.Constrained.Size (Size (..), runSize, seps)
import Test.Cardano.Ledger.Constrained.TypeRep (
  Rep (..),
  format,
  hasEq,
  synopsis,
  testEql,
  (:~:) (Refl),
 )
import Test.Cardano.Ledger.Generic.PrettyCore (PDoc, ppRecord, ppString)
import Test.Cardano.Ledger.Generic.Proof (Reflect)
import Test.QuickCheck (Gen, oneof)
import Type.Reflection (TypeRep, Typeable, typeRep)

-- =========================================================
-- class FromList cannot be defined in Classes.hs because
-- (Rep era t) for method tsRep is not in scope

class (Eq a, Eq ts) => FromList ts a | ts -> a where
  makeFromList :: [a] -> ts
  getList :: ts -> [a]
  tsRep :: Rep era ts -> Rep era a

instance Eq a => FromList [a] a where
  makeFromList xs = xs
  getList xs = xs
  tsRep (ListR z) = z

instance Eq a => FromList (Maybe a) a where
  makeFromList [] = Nothing
  makeFromList (x : _) = Just x
  getList Nothing = []
  getList (Just x) = [x]
  tsRep (MaybeR z) = z

instance (Ord k, Eq a) => FromList (Map k a) (k, a) where
  makeFromList = Map.fromList
  getList = Map.toList
  tsRep (MapR k v) = PairR k v

-- | CAREFULL HERE, this instance may NOT be size preserving.
instance Ord a => FromList (Set a) a where
  makeFromList = Set.fromList
  getList = Set.toList
  tsRep (SetR x) = x

-- ================================================

type Direct a = Either a a

direct :: Direct a -> a
direct (Left a) = a
direct (Right a) = a

data Term era t where
  Lit :: Rep era t -> t -> Term era t
  Var :: V era t -> Term era t
  Dom :: Ord a => Term era (Map a b) -> Term era (Set a)
  Rng :: (Ord a, Ord b) => Term era (Map a b) -> Term era (Set b)
  Elems :: (Ord a, Eq b) => Term era (Map a b) -> Term era [b]
  ProjM :: Ord a => Lens' b t -> Rep era t -> Term era (Map a b) -> Term era (Map a t)
  ProjS :: (Ord b, Ord t) => Lens' b t -> Rep era t -> Term era (Set b) -> Term era (Set t)
  Proj :: Lens' b t -> Rep era t -> Term era b -> Term era t
  Delta :: Term era Coin -> Term era DeltaCoin
  Negate :: Term era DeltaCoin -> Term era DeltaCoin
  Restrict :: Ord a => Term era (Set a) -> Term era (Map a b) -> Term era (Map a b)
  HashD :: Era era => Term era (Data era) -> Term era DataHash
  HashS :: Reflect era => Term era (ScriptF era) -> Term era ScriptHash
  Pair :: Term era a -> Term era b -> Term era (a, b)

infix 4 :=:

infix 4 :<-:

data Pred era where
  MetaSize :: Size -> Term era Size -> Pred era
  Sized :: Sizeable t => Term era Size -> Term era t -> Pred era
  (:=:) :: Eq a => Term era a -> Term era a -> Pred era
  Subset :: Ord a => Term era (Set a) -> Term era (Set a) -> Pred era
  Disjoint :: Ord a => Term era (Set a) -> Term era (Set a) -> Pred era
  SumsTo :: (Era era, Adds c) => (Direct c) -> Term era c -> OrdCond -> [Sum era c] -> Pred era
  SumSplit :: (Era era, Adds c) => c -> Term era c -> OrdCond -> [Sum era c] -> Pred era
  Random :: Term era t -> Pred era
  Component :: Direct (Term era t) -> [AnyF era t] -> Pred era
  CanFollow :: Count n => Term era n -> Term era n -> Pred era
  Member :: Ord a => Direct (Term era a) -> Term era (Set a) -> Pred era
  NotMember :: Ord a => Term era a -> Term era (Set a) -> Pred era
  MapMember ::
    (Ord k, Eq v, Ord v) => Term era k -> Term era v -> (Direct (Term era (Map k v))) -> Pred era
  (:<-:) :: Term era t -> RootTarget era r t -> Pred era
  GenFrom :: Term era t -> RootTarget era r (Gen t) -> Pred era
  List :: FromList fs t => Term era fs -> [Term era t] -> Pred era
  Choose ::
    (Era era, Eq t) => Term era Size -> Term era [t] -> [(Int, Target era t, [Pred era])] -> Pred era
  ForEach ::
    (Era era, FromList fs t, Eq t) =>
    Term era Size ->
    Term era fs ->
    Pat era t ->
    [Pred era] ->
    Pred era
  Maybe ::
    forall r era t.
    (Era era, Typeable r) =>
    Term era (Maybe t) ->
    RootTarget era r t ->
    [Pred era] ->
    Pred era
  Oneof :: (Eq t, Era era) => Term era t -> [(Int, RootTarget era t t, [Pred era])] -> Pred era
  SubMap :: (Ord k, Eq v, Ord v) => Term era (Map k v) -> Term era (Map k v) -> Pred era
  If :: RootTarget era r Bool -> Pred era -> Pred era -> Pred era
  Before :: Term era a -> Term era b -> Pred era
  ListWhere ::
    (Era era, Eq t) => Term era Size -> Term era [t] -> RootTarget era t t -> [Pred era] -> Pred era

data Sum era c where
  SumMap :: Adds c => Term era (Map a c) -> Sum era c
  SumList :: Adds c => Term era [c] -> Sum era c
  One :: Term era c -> Sum era c
  ProjOne :: forall x c era. Lens' x c -> Rep era c -> Term era x -> Sum era c
  ProjMap :: forall x c a era. Adds c => Rep era c -> Lens' x c -> Term era (Map a x) -> Sum era c

-- ====================================================================
-- Special patterns for building literal Terms of type Size and Word64

infix 4 :⊆:

pattern (:⊆:) :: forall era. (forall a. Ord a => Term era (Set a) -> Term era (Set a) -> Pred era)
pattern x :⊆: y = Subset x y

pattern ExactSize :: Era era => Int -> Term era Size
pattern ExactSize x <- (sameRng -> Just x)
  where
    ExactSize x = Lit SizeR (SzRng x x)

pattern AtLeast :: Era era => Int -> Term era Size
pattern AtLeast n = Lit SizeR (SzLeast n)

pattern Size :: Era era => Size -> Term era Size
pattern Size n = Lit SizeR n

pattern AtMost :: Era era => Int -> Term era Size
pattern AtMost n = Lit SizeR (SzMost n)

pattern Range :: Era era => Int -> Int -> Term era Size
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

pattern Word64 :: Era era => Word64 -> Term era Word64
pattern Word64 x = Lit Word64R x

var :: Era era => String -> Rep era t -> Term era t
var s r = Var (V s r No)

unVar :: Term era t -> V era t
unVar (Var v) = v
unVar x = error ("Non Var in unVar: " ++ show x)

fieldToTerm :: Field era rec field -> Term era field
fieldToTerm (Field nm rep repx l) = Var (V nm rep (Yes repx l))
fieldToTerm (FConst rep t _ _) = Lit rep t

-- | (select small big lens), 'big' is evaluated to a value, 'small' is a Var, constrain 'small'
--   such that it is bound to the 'lens' component of 'big'. Computes a (:=:) Pred
select :: (Eq t, Era era) => Term era t -> Term era big -> Lens' big t -> Pred era
select small big lenz = Proj lenz (termRep small) big :=: small

-- | (select small big lens), 'big' is evaluated to a value, 'small' is a Var, constrain 'small'
--   such that it is bound to the 'lens' component of 'big'. Computes a 'Component' Pred
select2 :: Era era => Term era t -> Term era big -> Lens' big t -> Pred era
select2 (Var (V nm t _)) big lenz = Component (Left big) [AnyF (Field nm t (termRep big) lenz)]
select2 t1 t2 _ =
  error ("In (select " ++ show t1 ++ " " ++ show t2 ++ " lens)  " ++ show t1 ++ " is not a Var term.")

-- ====================================================================

infixl 0 :$

data RootTarget era root t where
  Simple :: Term era t -> RootTarget era Void t
  (:$) :: RootTarget era r (a -> b) -> RootTarget era r a -> RootTarget era r b
  Constr :: String -> (a -> b) -> RootTarget era Void (a -> b)
  Invert :: String -> TypeRep root -> (a -> b) -> RootTarget era root (a -> b)
  Lensed :: Term era t -> SimpleGetter root t -> RootTarget era root t
  Partial ::
    Term era t ->
    (root -> Maybe t) ->
    -- | Partial designed to be used in Pred 'Oneof' where the RootTarget getter may fail.
    --   So use 'Partial' instead of 'Lensed' in 'Oneof'
    RootTarget era root t
  Shift :: RootTarget era root2 a -> Lens' root1 root2 -> RootTarget era root1 a
  Mask :: RootTarget era r a -> RootTarget era Void a
  Virtual ::
    Term era t ->
    PDoc ->
    Lens' root t ->
    -- | Just like Lensed but uses the String to name the field (instead of the Term)
    RootTarget era root t

-- Treat a Target as if it's (RootTarget era Void t)

type Target era t = RootTarget era Void t

infixl 0 ^$

-- | Version of (:$) That takes a Term on the right, rather than a Target
(^$) :: Target era (a -> t) -> Term era a -> Target era t
(^$) f x = f :$ Simple x

constTarget :: t -> Target era t
constTarget t = Constr "constTarget" (const t) ^$ Lit UnitR ()

constRootTarget :: forall era t. Typeable t => t -> RootTarget era t t
constRootTarget t = Invert "constRootTarget" (typeRep @t) (\() -> t) :$ Lensed (Lit UnitR ()) (to (const ()))

emptyTarget :: Target era ()
emptyTarget = Simple (Lit UnitR ())

justTarget :: Term era t -> Target era (Maybe t)
justTarget x = Constr "Just" Just ^$ x

idTarget :: Term era t -> Target era t
idTarget x = Constr "id" id ^$ x

-- | Usefull when using the Pred 'FromGen'
--   E.g. (FromGen termMaybeT (maybeTarget ^$ termT))
maybeTarget :: Target era (t -> Gen (Maybe t))
maybeTarget = Constr "maybeTarget" genMaybe
  where
    genMaybe x = oneof [pure Nothing, pure (Just x)]

listToSetTarget :: Ord x => Term era [x] -> Target era (Set.Set x)
listToSetTarget x = Constr "FromList" Set.fromList ^$ x

setToListTarget :: Term era (Set x) -> Target era [x]
setToListTarget x = Constr "toList" Set.toList ^$ x

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
  show (Elems x) = "(Elems " ++ show x ++ ")"
  show (ProjM _ r t) = "(ProjM " ++ show r ++ " " ++ show t ++ ")"
  show (ProjS _ r t) = "(ProjS " ++ show r ++ " " ++ show t ++ ")"
  show (Proj _ r t) = "(Proj " ++ show r ++ " " ++ show t ++ ")"
  show (Delta x) = "(Delta " ++ show x ++ ")"
  show (Negate x) = "(Negate " ++ show x ++ ")"
  show (Restrict r t) = "(Restrict " ++ show r ++ " " ++ show t ++ ")"
  show (HashS r) = "(HashS " ++ show r ++ ")"
  show (HashD r) = "(HashD " ++ show r ++ ")"
  show (Pair r t) = "(Pair " ++ show r ++ " " ++ show t ++ ")"
  showList xs ans = unlines (ans : map show xs)

instance Show (Sum era c) where
  show (SumMap t) = "sum " ++ show t
  show (SumList t) = "sum " ++ show t
  show (One t) = show t
  show (ProjOne _ c t) = seps ["ProjOne", show c, show t]
  show (ProjMap crep _lens t) = "ProjMap " ++ show crep ++ " " ++ show t

instance Show (Pred era) where
  show (MetaSize n t) = "MetaSize " ++ show n ++ " " ++ show t
  show (Sized n t) = "Sized " ++ show n ++ " " ++ show t
  show (x :=: y) = show x ++ " :=: " ++ show y
  show (Subset x y) = show x ++ " ⊆  " ++ show y
  show (Disjoint x y) = "Disjoint " ++ show x ++ " " ++ show y
  show (SumsTo i c cond m) = "SumsTo (" ++ show i ++ ") " ++ show c ++ show cond ++ showL show " + " m
  show (SumSplit i c cond m) = "SumSplit (" ++ show i ++ ") " ++ show c ++ show cond ++ showL show " + " m
  show (Random x) = "Random " ++ show x
  show (Component t ws) = "Component (" ++ show t ++ ") " ++ show ws
  show (CanFollow x y) = "CanFollow " ++ show x ++ " " ++ show y
  show (Member x y) = "Member (" ++ show x ++ ") " ++ show y
  show (NotMember x y) = "NotMember " ++ show x ++ " " ++ show y
  show (MapMember k v m) = "MapMember " ++ show k ++ " " ++ show v ++ " (" ++ show m ++ ")"
  show (x :<-: y) = show x ++ " :<-: " ++ showT y
  show (GenFrom x y) = "GenFrom " ++ show x ++ " " ++ showT y
  show (List t xs) = "List " ++ show t ++ " [" ++ showL show ", " xs ++ "]"
  show (Choose s term xs) = unlines (("Choose " ++ show s ++ " " ++ show term) : map showchoices xs)
    where
      showchoices (i, target, ps) =
        "(" ++ show i ++ ", " ++ showAllTarget target ++ showT target ++ " | " ++ showL show ", " ps ++ ")"
  show (Oneof term xs) = unlines (("Oneof " ++ " " ++ show term) : (map showchoices xs))
    where
      showchoices (i, target, ps) =
        "(" ++ show i ++ ", " ++ showAllTarget target ++ showT target ++ " | " ++ showL show ", " ps ++ ")"
  show (ForEach s term pat ps) =
    unlines
      [ seps ["ForEach", show s, show term]
      , "forall (" ++ show pat ++ " | " ++ showL show ", " ps
      ]
  show (Maybe term target ps) = "Maybe " ++ show term ++ showAllTarget target ++ " | " ++ showL show ", " ps
  show (SubMap x y) = "SubMap " ++ show x ++ " " ++ show y
  show (If t x y) = "If (" ++ show t ++ ") (" ++ show x ++ ") (" ++ show y ++ ")"
  show (Before x y) = "Before " ++ show x ++ " " ++ show y
  show (ListWhere sz t tar ps) =
    "(ListWhere " ++ show sz ++ " " ++ show t ++ " " ++ show tar ++ " " ++ showL show ", " ps ++ ")"
  showList xs ans = unlines (ans : (map show xs))

showAllTarget :: RootTarget era r t -> [Char]
showAllTarget tar = "   forall " ++ showL show " " (HashSet.toList (varsOfTarget HashSet.empty tar)) ++ ". "

instance Show (RootTarget era r t) where
  show (Constr nm _f) = nm
  show (Simple x) = show x
  show (Lensed x _) = show x
  show (Partial x _) = show x
  show (f :$ x) = "(" ++ show f ++ " :$ " ++ showL pp " :$ " (args x) ++ ")"
    where
      pp :: Univ.Any (RootTarget era r) -> String
      pp (Univ.Any spec) = show spec
  show (Invert nm _ _) = nm
  show (Shift x _) = "(Shift " ++ show x ++ " lens)"
  show (Mask x) = show x
  show (Virtual x y _) = "(Virtual " ++ show x ++ " " ++ show y ++ ")"

-- | "Print a Target as nested applications"
showT :: forall era t r. RootTarget era r t -> String
showT (Constr nm _f) = nm
showT (Simple x) = show x
showT (Lensed x _) = show x
showT (Partial x _) = show x
showT (f :$ x) = "(" ++ showT f ++ " " ++ showL pp " " (args x) ++ ")"
  where
    pp :: Univ.Any (RootTarget era r) -> String
    pp (Univ.Any spec) = showT spec
showT (Invert nm _ _) = nm
showT (Shift x _) = "(Shift " ++ showT x ++ ")"
showT (Mask x) = showT x
showT (Virtual _ y _) = show y

args :: RootTarget era r t -> [Univ.Any (RootTarget era r)]
args (x :$ xs) = Univ.Any x : args xs
args other = [Univ.Any other]

-- | Print a Target as a record showing the struture and names of all
--   the variables involved. This documents what is in scope where
--   the Target value was defined.
ppTarget :: RootTarget era r t -> PDoc
ppTarget x = targetRecord x []

targetRecord :: RootTarget era r t -> [(Text, PDoc)] -> PDoc
targetRecord (Constr n _) xs = ppRecord (pack n) xs
targetRecord (ts :$ t) xs = targetRecord ts (targetPair t : xs)
targetRecord (Simple e) [] = ppString (show e)
targetRecord (Lensed e _) [] = ppString (show e)
targetRecord (Partial e _) [] = ppString (show e)
targetRecord (Invert n _ _) xs = ppRecord (pack n) xs
targetRecord (Shift x _) xs = targetRecord x xs
targetRecord (Mask x) xs = targetRecord x xs
targetRecord (Virtual _ e _) [] = e
targetRecord other xs = ppRecord (nameOf other) xs

nameOf :: RootTarget era r t -> Text
nameOf (Constr cs _) = pack (map toLower cs ++ "T")
nameOf (Simple (Var (V n _ _))) = pack n
nameOf (Lensed (Var (V n _ _)) _) = pack n
nameOf (Partial (Var (V n _ _)) _) = pack n
nameOf (Simple term) = pack (show term)
nameOf (Lensed term _) = pack (show term)
nameOf (Partial term _) = pack (show term)
nameOf (x :$ _) = nameOf x
nameOf (Invert cs _ _) = pack (map toLower cs ++ "T")
nameOf (Shift x _) = nameOf x
nameOf (Mask x) = nameOf x
nameOf (Virtual _ x _) = pack (show x)

targetPair :: RootTarget era r t -> (Text, PDoc)
targetPair (Simple (Var (V n rep _))) = (pack n, ppString (show rep))
targetPair (Lensed (Var (V n rep _)) _) = (pack n, ppString (show rep))
targetPair (Partial (Var (V n rep _)) _) = (pack n, ppString (show rep))
targetPair (Virtual (Var (V _ rep _)) newname _) = (pack (show newname), ppString (show rep))
targetPair x = (nameOf x, targetRecord x [])

-- ===================================================
-- Computing the variables (V era t) in a Term, Pred, RootTarget
-- Their are no binders in any of these, so this is not so difficult
-- But (V era t) may have different 't', so we hide 't' in 'Name'

varsOfTerm :: HashSet (Name era) -> Term era t -> HashSet (Name era)
varsOfTerm ans s = case s of
  Lit _ _ -> ans
  Var v@(V _ _ _) -> HashSet.insert (Name v) ans
  Dom x -> varsOfTerm ans x
  Rng x -> varsOfTerm ans x
  Elems x -> varsOfTerm ans x
  (ProjM _ _ x) -> varsOfTerm ans x
  (ProjS _ _ x) -> varsOfTerm ans x
  (Proj _ _ x) -> varsOfTerm ans x
  Delta x -> varsOfTerm ans x
  Negate x -> varsOfTerm ans x
  Restrict st mp -> varsOfTerm (varsOfTerm ans st) mp
  HashS st -> varsOfTerm ans st
  HashD st -> varsOfTerm ans st
  Pair a b -> varsOfTerm (varsOfTerm ans a) b

vars :: Term era t -> HashSet (Name era)
vars x = varsOfTerm HashSet.empty x

varsOfTarget :: HashSet (Name era) -> RootTarget era r t -> HashSet (Name era)
varsOfTarget ans s = case s of
  (a :$ b) -> varsOfTarget (varsOfTarget ans a) b
  (Simple x) -> varsOfTerm ans x
  (Lensed x _) -> varsOfTerm ans x
  (Partial x _) -> varsOfTerm ans x
  (Constr _ _) -> ans
  (Invert _ _ _) -> ans
  (Shift x _) -> varsOfTarget ans x
  (Mask x) -> varsOfTarget ans x
  (Virtual x _ _) -> varsOfTerm ans x

varsOfPred :: forall era. HashSet (Name era) -> Pred era -> HashSet (Name era)
varsOfPred ans s = case s of
  MetaSize _ term -> varsOfTerm ans term
  Sized a b -> varsOfTerm (varsOfTerm ans a) b
  a :=: b -> varsOfTerm (varsOfTerm ans a) b
  Subset a b -> varsOfTerm (varsOfTerm ans a) b
  Disjoint a b -> varsOfTerm (varsOfTerm ans a) b
  SumsTo _ x _ xs -> List.foldl' varsOfSum (varsOfTerm ans x) xs
  SumSplit _ x _ xs -> List.foldl' varsOfSum (varsOfTerm ans x) xs
  Random x -> varsOfTerm ans x
  Component t cs -> varsOfTerm (List.foldl' varsOfComponent ans cs) (direct t)
    where
      varsOfComponent :: forall z. HashSet (Name era) -> AnyF era z -> HashSet (Name era)
      varsOfComponent l (AnyF (Field n r rx l2)) = HashSet.insert (Name $ V n r (Yes rx l2)) l
      varsOfComponent l (AnyF (FConst _ _ _ _)) = l
  CanFollow a b -> varsOfTerm (varsOfTerm ans a) b
  Member a b -> varsOfTerm (varsOfTerm ans (direct a)) b
  NotMember a b -> varsOfTerm (varsOfTerm ans a) b
  MapMember k v m -> varsOfTerm (varsOfTerm (varsOfTerm ans k) v) (direct m)
  a :<-: b -> varsOfTarget (varsOfTerm ans a) b
  GenFrom a b -> varsOfTarget (varsOfTerm ans a) b
  List a bs -> List.foldl' varsOfTerm (varsOfTerm ans a) bs
  Choose sz term pairs -> varsOfTerm (varsOfTerm (varsOfTrips ans pairs) term) sz
  Oneof term pairs -> varsOfTerm (varsOfTrips ans pairs) term
  ForEach sz term pat ps -> varsOfTerm (varsOfTerm (varsOfPats ans [(pat, ps)]) term) sz
  Maybe term target ps -> varsOfTerm (varsOfPairs ans [(target, ps)]) term
  SubMap a b -> varsOfTerm (varsOfTerm ans a) b
  If t x y -> varsOfTarget (varsOfPred (varsOfPred ans x) y) t
  Before a b -> varsOfTerm (varsOfTerm ans a) b
  ListWhere sz t tar ps -> HashSet.union vs others
    where
      vs = varsOfTerm (varsOfTerm ans t) sz
      bound = varsOfTarget HashSet.empty tar
      pvs = List.foldl' varsOfPred HashSet.empty ps
      others = HashSet.difference pvs bound

varsOfTrips ::
  Era era => HashSet (Name era) -> [(Int, RootTarget era r t2, [Pred era])] -> HashSet (Name era)
varsOfTrips ans1 [] = ans1
varsOfTrips ans1 ((_, t, ps) : more) = varsOfTrips (act ans1 t ps) more
  where
    act ans2 tar preds =
      HashSet.union
        ( HashSet.difference
            (List.foldl' varsOfPred HashSet.empty preds)
            (varsOfTarget HashSet.empty tar)
        )
        ans2

varsOfPairs ::
  Era era => HashSet (Name era) -> [(RootTarget era r t2, [Pred era])] -> HashSet (Name era)
varsOfPairs ans1 [] = ans1
varsOfPairs ans1 ((t, ps) : more) = varsOfPairs (act ans1 t ps) more
  where
    act ans2 tar preds =
      HashSet.union
        ( HashSet.difference
            (List.foldl' varsOfPred HashSet.empty preds)
            (varsOfTarget HashSet.empty tar)
        )
        ans2

varsOfPats :: Era era => HashSet (Name era) -> [(Pat era t2, [Pred era])] -> HashSet (Name era)
varsOfPats ans1 [] = ans1
varsOfPats ans1 ((pat0, ps) : more) = varsOfPats (act ans1 pat0 ps) more
  where
    act ans2 pat preds =
      HashSet.union
        ( HashSet.difference
            (List.foldl' varsOfPred HashSet.empty preds)
            (varsOfPat HashSet.empty pat)
        )
        ans2

varsOfSum :: HashSet (Name era) -> Sum era r -> HashSet (Name era)
varsOfSum ans (SumMap y) = varsOfTerm ans y
varsOfSum ans (SumList y) = varsOfTerm ans y
varsOfSum ans (One y) = varsOfTerm ans y
varsOfSum ans (ProjOne _ _ y) = varsOfTerm ans y
varsOfSum ans (ProjMap _ _ x) = varsOfTerm ans x

-- =====================================================================

expandSum :: Sum era c -> [Int] -> [Sum era c]
expandSum (One (Var (V n r a))) ns = map (\i -> One (Var (V (n ++ "." ++ show i) r a))) ns
expandSum (ProjOne l rep (Var (V n r a))) ns = map (\i -> ProjOne l rep (Var (V (n ++ "." ++ show i) r a))) ns
expandSum x _ = error ("Bad Sum in expandSum: " ++ show x)

-- =====================================================================
-- Subst, the type of Substitutions

pad :: Int -> String -> String
pad n x = x ++ replicate (n - length x) ' '

data SubstElem era where
  SubstElem :: Rep era t -> Term era t -> Access era s t -> SubstElem era

instance Show (SubstElem era) where
  show (SubstElem rep t _) = show t ++ " :: " ++ show rep

newtype Subst era = Subst (Map String (SubstElem era))

extend :: V era t -> Term era t -> Subst era -> Subst era
extend (V nm rep access) term (Subst m) = Subst (Map.insert nm (SubstElem rep term access) m)

instance Show (Subst era) where
  show (Subst m) = unlines (map f (Map.toList m))
    where
      f (nm, SubstElem _ t _) = nm ++ " -> " ++ show t

emptySubst :: Subst era
emptySubst = Subst Map.empty

substToEnv :: Subst era -> Env era -> Typed (Env era)
substToEnv (Subst m) env = Map.foldlWithKey' accum (pure env) m
  where
    accum ansM key (SubstElem r (Lit _ v) access) = do
      Env ans <- ansM
      pure $ Env $ Map.insert key (Payload r v access) ans
    accum _ _ (SubstElem _ e _) = failT ["Not Literal expr in substToEnv: " ++ show e]

envToSubst :: Env era -> Subst era
envToSubst (Env env) = Subst (Map.map f env)
  where
    f (Payload rep t access) = SubstElem rep (Lit rep t) access

findV :: Subst era -> V era t -> Term era t
findV (Subst m) v@(V n1 rep1 _) = case Map.lookup n1 m of
  Nothing -> Var v -- If its not in the Subst, return the Var
  Just (SubstElem rep2 term _) -> case testEql rep1 rep2 of
    Just Refl -> term
    Nothing ->
      error
        ( "In findV, we found: "
            ++ n1
            ++ ", but the types did not match. "
            ++ show rep1
            ++ " =/= "
            ++ show rep2
        )

--  | Not really a composition, just adding 'sub1' to 'sub2', but if any thing
--    is in both 'sub1' and 'sub2', the 'sub1' binding overrides the 'sub2' binding
composeSubst :: Subst era -> Subst era -> Subst era
composeSubst (Subst sub1) (Subst sub2) = Subst (Map.foldrWithKey' Map.insert sub2 sub1)

singleSubst :: V era t -> Term era t -> Subst era
singleSubst (V n r access) expr = Subst (Map.insert n (SubstElem r expr access) Map.empty)

substFromNames :: forall era. HashSet (Name era) -> Subst era
substFromNames names = Subst (HashSet.foldl' accum Map.empty names)
  where
    accum :: Map String (SubstElem era) -> Name era -> Map String (SubstElem era)
    accum ans (Name v@(V n r access)) = Map.insert n (SubstElem r (Var v) access) ans

data SubItem era where
  SubItem :: V era t -> Term era t -> SubItem era

instance Show (SubItem era) where
  show (SubItem x y) = "(SubItem " ++ show x ++ " " ++ show y ++ ")"

itemsToSubst :: [SubItem era] -> Subst era
itemsToSubst ss = Subst (List.foldr accum Map.empty ss)
  where
    accum :: SubItem era -> Map String (SubstElem era) -> Map String (SubstElem era)
    accum (SubItem (V nm rep access) term) !ans = Map.insert nm (SubstElem rep term access) ans

-- =====================================================
-- Subtitution of (V era t) inside of (Spec era t)

substTerm :: Subst era -> Term era t -> Term era t
substTerm sub (Var v) = findV sub v
substTerm _ (Lit r k) = Lit r k
substTerm sub (Dom x) = Dom (substTerm sub x)
substTerm sub (Rng x) = Rng (substTerm sub x)
substTerm sub (Elems x) = Elems (substTerm sub x)
substTerm sub (ProjM l r x) = ProjM l r (substTerm sub x)
substTerm sub (ProjS l r x) = ProjS l r (substTerm sub x)
substTerm sub (Proj l r x) = Proj l r (substTerm sub x)
substTerm sub (Delta x) = Delta (substTerm sub x)
substTerm sub (Negate x) = Negate (substTerm sub x)
substTerm sub (Restrict s m) = Restrict (substTerm sub s) (substTerm sub m)
substTerm sub (HashS s) = HashS (substTerm sub s)
substTerm sub (HashD s) = HashD (substTerm sub s)
substTerm sub (Pair a b) = Pair (substTerm sub a) (substTerm sub b)

substPred :: Subst era -> Pred era -> Pred era
substPred sub (MetaSize a b) = MetaSize a (substTerm sub b)
substPred sub (Sized a b) = Sized (substTerm sub a) (substTerm sub b)
substPred sub (a :=: b) = substTerm sub a :=: substTerm sub b
substPred sub (a `Subset` b) = substTerm sub a `Subset` substTerm sub b
substPred sub (Disjoint a b) = Disjoint (substTerm sub a) (substTerm sub b)
substPred sub (SumsTo i a cond b) = SumsTo i (substTerm sub a) cond (map (substSum sub) b)
substPred sub (SumSplit i a cond b) = SumSplit i (substTerm sub a) cond (map (substSum sub) b)
substPred sub (Random x) = Random (substTerm sub x)
substPred sub (Component t cs) = case t of
  Left x -> Component (Left (substTerm sub x)) (substComp <$> cs)
  Right x -> Component (Right (substTerm sub x)) (substComp <$> cs)
  where
    substComp (AnyF w@(Field n r rx l)) = AnyF $ case findV sub (V n r (Yes rx l)) of
      (Lit rep x) -> FConst rep x rx l
      (Var (V n2 r2 _a2)) -> Field n2 r2 rx l
      _ -> w
    substComp x@(AnyF (FConst _ _ _ _)) = x
substPred sub (CanFollow a b) = CanFollow (substTerm sub a) (substTerm sub b)
substPred sub (Member dirA b) = case dirA of
  Left a -> Member (Left (substTerm sub a)) (substTerm sub b)
  Right a -> Member (Right (substTerm sub a)) (substTerm sub b)
substPred sub (NotMember a b) = NotMember (substTerm sub a) (substTerm sub b)
substPred sub (MapMember k v dirM) = case dirM of
  Left m -> MapMember (substTerm sub k) (substTerm sub v) (Left (substTerm sub m))
  Right m -> MapMember (substTerm sub k) (substTerm sub v) (Right (substTerm sub m))
substPred sub (a :<-: b) = substTerm sub a :<-: substTarget sub b
substPred sub (GenFrom a b) = GenFrom (substTerm sub a) (substTarget sub b)
substPred sub (List a b) = List (substTerm sub a) (map (substTerm sub) b)
substPred sub (Choose sz t pairs) = Choose (substTerm sub sz) (substTerm sub t) (map (subPair sub) pairs)
  where
    subPair sub0 (i, tar, ps) = (i, tar, map (substPred (composeSubst sub1 sub0)) ps)
      where
        sub1 = substFromTarget tar
substPred sub (Oneof t ps) = Oneof (substTerm sub t) (map (\(i, tr, p) -> (i, substTarget sub tr, map (substPred sub) p)) ps)
substPred sub (ForEach sz t pat ps) =
  ForEach
    (substTerm sub sz)
    (substTerm sub t)
    pat
    (map (substPred sub1) ps)
  where
    sub1 = composeSubst (substFromPat pat) sub
substPred sub (Maybe term target ps) = Maybe (substTerm sub term) target (map (substPred (composeSubst sub1 sub)) ps)
  where
    sub1 = substFromTarget target
substPred sub (SubMap a b) = SubMap (substTerm sub a) (substTerm sub b)
substPred sub (If t x y) = If (substTarget sub t) (substPred sub x) (substPred sub y)
substPred sub (Before a b) = Before (substTerm sub a) (substTerm sub b)
substPred sub (ListWhere sz t tar ps) =
  ListWhere (substTerm sub sz) (substTerm sub t) tar (map (substPred newsub) ps)
  where
    newsub = composeSubst (substFromTarget tar) sub

-- | Apply the Subst, and test if all variables are removed.
substPredWithVarTest :: Subst era -> Pred era -> Pred era
substPredWithVarTest sub oldpred =
  let newpred = substPred sub oldpred
      freevars = varsOfPred HashSet.empty newpred
   in case HashSet.null freevars of
        False -> newpred
        True ->
          error
            ( unlines
                [ "When solving: " ++ show oldpred ++ ","
                , "we applied the Subst from earlier stages, and we obtain the pred:"
                , "   " ++ show newpred
                , "with no free variables. This probably means an introductory pred like (Sized x)"
                , "or (Random x), appears both in an earlier stage, and the current stage."
                ]
            )
  where

substFromTarget :: RootTarget era r t -> Subst era
substFromTarget tar = substFromNames (varsOfTarget HashSet.empty tar)

substFromPat :: Pat era t -> Subst era
substFromPat pat = substFromNames (varsOfPat HashSet.empty pat)

substSum :: Subst era -> Sum era t -> Sum era t
substSum sub (SumMap x) = SumMap (substTerm sub x)
substSum sub (SumList x) = SumList (substTerm sub x)
substSum sub (One x) = One (substTerm sub x)
substSum sub (ProjOne l r x) = ProjOne l r (substTerm sub x)
substSum sub (ProjMap crep l x) = ProjMap crep l (substTerm sub x)

substTarget :: Subst era -> RootTarget era r t -> RootTarget era r t
substTarget sub (Simple e) = Simple (substTerm sub e)
substTarget sub (Lensed e l) = Lensed (substTerm sub e) l
substTarget sub (Partial e l) = Partial (substTerm sub e) l
substTarget sub (a :$ b) = substTarget sub a :$ substTarget sub b
substTarget _ (Constr n f) = Constr n f
substTarget _ (Invert x l f) = Invert x l f
substTarget sub (Shift x l) = Shift (substTarget sub x) l
substTarget sub (Mask x) = Mask (substTarget sub x)
substTarget sub (Virtual x y l) = Virtual (substTerm sub x) y l

-- ======================================================
-- Symbolic evaluators

-- | Simplify Terms that only contain Literals (or constant) sub-Terms
simplify :: Term era t -> Typed t
simplify (Lit _ x) = pure x
simplify (Dom (Lit _ x)) = pure (Map.keysSet x)
simplify (Dom (ProjM _ _ t)) = simplify (Dom t)
simplify (Dom x) = do
  m <- simplify x
  pure (Map.keysSet m)
simplify (Rng (Lit _ x)) = pure (Set.fromList (Map.elems x))
simplify (Rng (ProjM l _ (Lit _ m))) = pure (Set.fromList (Map.elems (Map.map (\x -> x ^. l) m)))
simplify (Rng x) = do
  m <- simplify x
  pure (Set.fromList (Map.elems m))
simplify (Elems (Lit _ x)) = pure (Map.elems x)
simplify (Elems (ProjM l _ (Lit _ m))) = pure (Map.elems (Map.map (\x -> x ^. l) m))
simplify (Elems x) = do
  m <- simplify x
  pure (Map.elems m)
simplify (ProjM l _ (Lit _ x)) = pure (Map.map (\z -> z ^. l) x)
simplify (ProjM l _ t) = do
  m <- simplify t
  pure (Map.map (\z -> z ^. l) m)
simplify (ProjS l _ (Lit _ x)) = pure (Set.map (\z -> z ^. l) x)
simplify (ProjS l _ t) = do
  s <- simplify t
  pure (Set.map (\z -> z ^. l) s)
simplify (Proj l _ (Lit _ x)) = pure (x ^. l)
simplify (Proj l _ t) = do
  s <- simplify t
  pure (s ^. l)
simplify (Delta (Lit CoinR (Coin n))) = pure (DeltaCoin n)
simplify (Negate (Lit DeltaCoinR (DeltaCoin n))) = pure (DeltaCoin (-n))
simplify (Restrict s m) = do
  sv <- simplify s
  mv <- simplify m
  pure (Map.restrictKeys mv sv)
simplify (HashS s) = do
  ScriptF _ sv <- simplify s
  pure (hashScript sv)
simplify (HashD s) = do
  sv <- simplify s
  pure (hashData sv)
simplify (Pair s m) = do
  sv <- simplify s
  mv <- simplify m
  pure (sv, mv)
simplify x = failT ["Can't simplify term: " ++ show x ++ ", to a value."]

-- | Simplify constant Sum's
simplifySum :: Sum era c -> Typed c
simplifySum (One (Lit _ x)) = pure x
simplifySum (One (Delta (Lit CoinR (Coin n)))) = pure (DeltaCoin n)
simplifySum (One (Negate (Lit DeltaCoinR (DeltaCoin n)))) = pure (DeltaCoin (-n))
simplifySum (ProjOne l _ (Lit _ x)) = pure (x ^. l)
simplifySum (SumMap (Lit _ m)) = pure (Map.foldl' add zero m)
simplifySum (SumList (Lit _ m)) = pure (List.foldl' add zero m)
simplifySum (ProjMap _ l (Lit _ m)) = pure (List.foldl' (\ans x -> add ans (x ^. l)) zero m)
simplifySum x = failT ["Can't simplify Sum: " ++ show x ++ ", to a value."]

simplifyTarget :: forall era t root. RootTarget era root t -> Typed t
simplifyTarget (Invert _ _ f) = pure f
simplifyTarget (Shift x _) = simplifyTarget x
simplifyTarget (Mask x) = simplifyTarget x
simplifyTarget (Virtual x _ _) = simplify x
simplifyTarget (Simple t) = simplify t
simplifyTarget (Lensed t _) = simplify t
simplifyTarget (Partial t _) = simplify t
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
runTerm env (Elems x) = Map.elems <$> runTerm env x
runTerm env (Var v) = findVar v env
runTerm env (ProjM l _ x) = do
  m <- runTerm env x
  pure (Map.map (\z -> z ^. l) m)
runTerm env (ProjS l _ x) = do
  m <- runTerm env x
  pure (Set.map (\z -> z ^. l) m)
runTerm env (Proj l _ x) = do
  m <- runTerm env x
  pure (m ^. l)
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
runTerm env (HashD x) = do
  s <- runTerm env x
  pure (hashData s)
runTerm env (HashS x) = do
  ScriptF _ s <- runTerm env x
  pure (hashScript s)
runTerm env (Pair s m) =
  do
    sv <- runTerm env s
    mv <- runTerm env m
    pure (sv, mv)

runTarget :: Env era -> RootTarget era x t -> Typed t
runTarget _ (Invert _ _ f) = pure f
runTarget env (Simple t) = runTerm env t
runTarget env (Lensed t _) = runTerm env t
runTarget env (Partial t _) = runTerm env t
runTarget env (Shift x _) = runTarget env x
runTarget env (Mask x) = runTarget env x
runTarget env (Virtual x _ _) = runTerm env x
runTarget _ (Constr _ f) = pure f
runTarget env (x :$ y) = do
  f <- runTarget env x
  z <- runTarget env y
  pure (f z)

-- | Overwrite the bindings for the Vars in the Env that appear in the RootTarget, by updating the Env with the values picked from the root
--   When the target has type (RootTarget era Void x), the function is the identity on the Env.
getTarget :: forall era root t. root -> RootTarget era root t -> Env era -> Env era
getTarget root (Lensed (Var v) l) env = storeVar v (root ^. l) env
getTarget _ (Lensed _ _) env = env
getTarget root (Partial (Var v) f) env =
  case f root of
    Just val -> storeVar v val env
    Nothing ->
      error
        ( "A Partial RootTarget returned Nothing: "
            ++ show v
            ++ "\n Maybe use 'targetMaybeEnv' instead of 'getTarget' "
        )
getTarget _ (Partial _ _) env = env
getTarget root (x :$ y) env = getTarget root x (getTarget root y env)
getTarget root (Shift x l) env = getTarget (root ^. l) x env
getTarget _ (Invert _ _ _) env = env
getTarget _ (Constr _ _) env = env
getTarget _ (Simple _) env = env
getTarget _ (Mask _) env = env
getTarget root (Virtual (Var v) _ l) env = storeVar v (root ^. l) env
getTarget _ (Virtual {}) env = env

targetMaybeEnv :: forall era root t. root -> RootTarget era root t -> Env era -> Maybe (Env era)
targetMaybeEnv root (Lensed (Var v) l) env = Just (storeVar v (root ^. l) env)
targetMaybeEnv root (Virtual (Var v) _ l) env = Just (storeVar v (root ^. l) env)
targetMaybeEnv root (Shift x l) env = targetMaybeEnv (root ^. l) x env
targetMaybeEnv root (Partial (Var v) f) env =
  case f root of
    Just val -> Just (storeVar v val env)
    Nothing -> Nothing
targetMaybeEnv root (x :$ y) env =
  targetMaybeEnv root y env >>= targetMaybeEnv root x
targetMaybeEnv _ _ _ = Nothing

runPred :: Env era -> Pred era -> Typed Bool
runPred env (MetaSize w x) = do
  sz <- runTerm env x
  case sz of
    SzExact n -> pure $ runSize n w
    _ -> pure $ False
runPred env (Sized szt tt) = do
  sz <- runTerm env szt
  t <- runTerm env tt
  pure $ runSize (getSize t) sz
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
  pure (runOrdCondition cond x2 y2)
runPred env (SumSplit _ x cond ys) = do
  x2 <- runTerm env x
  is <- mapM (runSum env) ys
  let y2 = List.foldl' add zero is
  pure (runOrdCondition cond x2 y2)
runPred _ (Random _) = pure True
runPred env (Component t cs) = do
  t' <- runTerm env (direct t)
  and <$> mapM (runComp env t') cs
runPred env (CanFollow x y) = do
  x2 <- runTerm env x
  y2 <- runTerm env y
  pure (canFollow x2 y2)
runPred env (Member x y) = do
  x2 <- runTerm env (direct x)
  y2 <- runTerm env y
  pure (Set.member x2 y2)
runPred env (NotMember x y) = do
  x2 <- runTerm env x
  y2 <- runTerm env y
  pure (Set.notMember x2 y2)
runPred env (MapMember k v m) = do
  k' <- runTerm env k
  v' <- runTerm env v
  m' <- runTerm env (direct m)
  pure $ Map.isSubmapOf (Map.singleton k' v') m'
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
-- choose should have been removed by the Rewrite phase.
runPred _ (Choose _ _ _) = pure True -- We can't really test this. failT ["Choose predicate in runPred", show p]
runPred env (ForEach _sz term pat ps) = do
  -- size <- runTerm env _sz
  ts <- getList <$> runTerm env term
  bs <- mapM (\t -> runPreds (bindPat t env pat) (filter (not . extendableSumsTo pat) ps)) ts
  pure (and bs)
runPred env (SubMap x y) = do
  x2 <- runTerm env x
  y2 <- runTerm env y
  pure (Map.isSubmapOf x2 y2)
runPred env (If t x y) = do
  b <- runTarget env t
  if b
    then runPred env x
    else runPred env y
runPred _ (Before _ _) = pure True
runPred env (Oneof name triples) = do
  root <- runTerm env name
  let firstMatching [] = pure False
      firstMatching ((_, tar, ps) : more) = case targetMaybeEnv root tar env of
        Nothing -> firstMatching more
        Just env2 -> do
          qs <- mapM (runPred env2) ps
          if and qs
            then pure True
            else firstMatching more
  firstMatching triples
runPred env (ListWhere sz name target ps) = do
  xs <- runTerm env name
  size <- runTerm env sz
  let sizeOK = runSize (length xs) size
  let p x = do
        let env2 = getTarget x target env
        bs <- mapM (runPred env2) ps
        pure (and bs)
  bs <- mapM p xs
  pure $ sizeOK && (and bs)

hashSetToSet :: Ord a => HashSet a -> Set a
hashSetToSet x = Set.fromList (HashSet.toList x)

hashSetDisjoint :: Hashable a => HashSet a -> HashSet a -> Bool
hashSetDisjoint x y = HashSet.null (HashSet.intersection x y)

-- | One type of Pred in ForEach is handled differently from others
--   if valCoin is amongst the free variables of `pat` and
--   'balanceCoin' is not amongst the free variables of `pat` then
--   SumsTo (Left (Coin 1)) balanceCoin EQL [One valCoin]  expands to
--   SumsTo (Left (Coin 1)) balanceCoin EQL [One valCoin.1,One valCoin.2,One valCoin.3]

--   SumSplit (Coin 1) balanceCoin EQL [One valCoin]  expands to
--   SumSplit (Coin 1) balanceCoin EQL [One valCoin.1,One valCoin.2,One valCoin.3]

--   other predicates P(x) would exapnd to 3 copies like P(x.1) P(x.2) P(x.3)
--   This is called Sum extension, is implemented by `extendSum`
extendableSumsTo :: Pat era t -> Pred era -> Bool
extendableSumsTo pat (SumsTo _ t _ [One s]) =
  HashSet.size boundS == 1 && HashSet.isSubsetOf boundS free && hashSetDisjoint boundT free
  where
    free = varsOfPat HashSet.empty pat
    boundS = varsOfTerm HashSet.empty s
    boundT = varsOfTerm HashSet.empty t
extendableSumsTo pat (SumSplit _ t _ [One s]) =
  HashSet.size boundS == 1 && HashSet.isSubsetOf boundS free && hashSetDisjoint boundT free
  where
    free = varsOfPat HashSet.empty pat
    boundS = varsOfTerm HashSet.empty s
    boundT = varsOfTerm HashSet.empty t
extendableSumsTo _ _ = False

-- | run a bunch of Preds, and and together the results
runPreds :: Env era -> [Pred era] -> Typed Bool
runPreds env ps = do
  bs <- mapM (runPred env) ps
  pure (and bs)

bind :: RootTarget era r t -> t -> Env era -> Env era
bind (Simple (Var v)) x env = storeVar v x env
bind (Lensed (Var v) _) x env = storeVar v x env
bind t _ _ = error ("Non simple Target in bind: " ++ show t)

runComp :: Env era -> s -> AnyF era s -> Typed Bool
runComp env t (AnyF (Field n rt rx l)) = do
  t' <- runTerm env $ Var (V n rt (Yes rx l))
  With _ <- hasEq rt rt
  pure $ t ^. l == t'
runComp _ t (AnyF (FConst r v _ l)) = do
  With _ <- hasEq r r
  pure $ t ^. l == v

termRep :: Era era => Term era t -> Rep era t
termRep (Lit r _) = r
termRep (Var (V _ r _)) = r
termRep (Dom (termRep -> MapR r _)) = SetR r
termRep (Rng (termRep -> MapR _ r)) = SetR r
termRep (Elems (termRep -> MapR _ r)) = ListR r
termRep (ProjM _ t (termRep -> MapR a _)) = MapR a t
termRep (ProjS _ t (termRep -> SetR _)) = SetR t
termRep (Proj _ t _) = t
termRep (Delta _) = DeltaCoinR
termRep (Negate _) = DeltaCoinR
termRep (Restrict _ m) = termRep m
termRep (HashD _) = DataHashR
termRep (HashS _) = ScriptHashR
termRep (Pair a b) = PairR (termRep a) (termRep b)

runSum :: Env era -> Sum era c -> Typed c
runSum env (SumMap t) = Map.foldl' add zero <$> runTerm env t
runSum env (SumList t) = List.foldl' add zero <$> runTerm env t
runSum env (One t) = runTerm env t
runSum env (ProjOne l _ t) = do
  x <- runTerm env t
  pure (x ^. l)
runSum env (ProjMap _ l t) = Map.foldl' accum zero <$> runTerm env t
  where
    accum ans x = add ans (x ^. l)

makeTest :: Env era -> Pred era -> Typed (String, Bool, Pred era)
makeTest env c = do
  b <- runPred env c
  pure (show c ++ " => " ++ show b, b, c)

displayTerm :: Era era => Env era -> Term era a -> IO ()
displayTerm env (Var v@(V nm rep _)) = do
  x <- monadTyped (findVar v env)
  putStrLn (nm ++ "\n" ++ format rep x)
displayTerm env term = do
  x <- monadTyped (runTerm env term)
  putStrLn (show term ++ "\n" ++ format (termRep term) x)

-- =======================================================================
-- Patterns are used to indicate bound variables in a ForEach constraint

patToAnyF :: Pat era t -> [AnyF era t]
patToAnyF (Pat rep as) = concat (map (argToAnyF rep) as)

argToAnyF :: Rep era t -> Arg era t -> [AnyF era t]
argToAnyF _ (Arg f) = [AnyF f]
argToAnyF rept (ArgPs f@(Field _ _ _ lensx) ps) = AnyF f : map (push rept lensx) (concat (map patToAnyF ps))
argToAnyF rept (ArgPs f@(FConst _ _ _ lensx) ps) = AnyF f : map (push rept lensx) (concat (map patToAnyF ps))

push :: Rep era t -> Lens' t s -> AnyF era s -> AnyF era t
push rept l1 (AnyF (Field nm rep _ l2)) = AnyF (Field nm rep rept (l1 . l2))
push rept l1 (AnyF (FConst rep x _ l2)) = AnyF (FConst rep x rept (l1 . l2))

data Pat era t where
  Pat :: !(Rep era t) -> ![Arg era t] -> Pat era t

data Arg era t where
  ArgPs :: !(Field era t s) -> ![Pat era s] -> Arg era t
  Arg :: !(Field era t s) -> Arg era t

-- | Succeds if 'term' is a variable with an embedded (Lens' t2 t1)
patt :: Rep era t1 -> Term era t2 -> Pat era t1
patt rep term = Pat rep [arg rep term]

instance Show (Pat era t) where
  show (Pat r xs) = "Pat " ++ show r ++ " " ++ showL show ", " xs

instance Show (Arg era t) where
  show (Arg (Field nm _ _ _)) = nm
  show (Arg f@(FConst _ _ _ _)) = show f
  show (ArgPs (Field nm _ _ _) qs) = nm ++ " [" ++ showL show ", " qs ++ "]"
  show (ArgPs f@(FConst _ _ _ _) qs) = show f ++ " [" ++ showL show ", " qs ++ "]"

varsOfField :: HashSet (Name era) -> Field era s t -> HashSet (Name era)
varsOfField l (Field n r rx l2) = HashSet.insert (Name $ V n r (Yes rx l2)) l
varsOfField l (FConst _ _ _ _) = l

varsOfPat :: HashSet (Name era) -> Pat era t -> HashSet (Name era)
varsOfPat ans (Pat _ qs) = List.foldl' varsOfArg ans qs

varsOfArg :: HashSet (Name era) -> Arg era t -> HashSet (Name era)
varsOfArg ans (ArgPs f qs) = List.foldl' varsOfPat (varsOfField ans f) qs
varsOfArg ans (Arg f) = varsOfField ans f

substField :: Subst era -> Field era rec fld -> Field era rec fld
substField _ w@(FConst _ _ _ _) = w
substField sub w@(Field n r rx l) = case findV sub (V n r (Yes rx l)) of
  (Lit rep x) -> FConst rep x rx l
  (Var (V n2 r2 _a2)) -> Field n2 r2 rx l
  _ -> w

substPat :: Subst era -> Pat era t -> Pat era t
substPat sub (Pat r as) = Pat r (map (substArg sub) as)

substArg :: Subst era -> Arg era t -> Arg era t
substArg sub (ArgPs r as) = ArgPs (substField sub r) (map (substPat sub) as)
substArg sub (Arg r) = Arg (substField sub r)

bindPat :: t -> Env era -> Pat era t -> Env era
bindPat t env (Pat _ as) = List.foldl' (bindArg t) env as

bindArg :: t -> Env era -> Arg era t -> Env era
bindArg _ env (Arg (FConst _ _ _ _)) = env
bindArg t env (Arg (Field n r rx l)) = storeVar (V n r (Yes rx l)) (t ^. l) env
bindArg t env (ArgPs (FConst _ _ _ l2) qs) = List.foldl' (bindPat (t ^. l2)) env qs
bindArg t env (ArgPs (Field n r rx l) qs) =
  List.foldl' (bindPat (t ^. l)) (storeVar (V n r (Yes rx l)) (t ^. l) env) qs

-- | Construct a Arg with sub-patterns from
--   1) Rep era s, telling what type of Arg
--   2) A variable (Term era s) with a Yes Access,
--   3) A list of sub-patterns.
--   Check that all the embedded Access have the right Lens'.
--   If not throw an error.
argP :: Rep era s -> Term era t -> [Pat era t] -> Arg era s
argP repS1 (Var (V name rept (Yes repS2 ll))) qs = case testEql repS1 repS2 of
  Just Refl -> ArgPs (Field name rept repS2 ll) qs
  Nothing ->
    error
      ( unlines
          [ "In 'argP' the given rep and lens target do not match: "
          , "rep: " ++ show repS1
          , "lens target: " ++ show repS2
          ]
      )
argP _ t@(Var _) _ = error ("argP applied to variable term with No access." ++ show t)
argP _ term _ = error ("argP can only be applied to variable terms: " ++ show term)

-- | Construct an Arg from a variable (Term era s) with a Yes Access.
--   Check that the Access has the right Lens'. If not throw an error.
arg :: Rep era s -> Term era t -> Arg era s
arg repS1 (Var (V name rept (Yes repS2 l))) = case testEql repS1 repS2 of
  Just Refl -> Arg (Field name rept repS2 l)
  Nothing ->
    error
      ( unlines
          [ "In 'arg' the given rep and lens target do not match: "
          , "rep: " ++ show repS1
          , "lens target: " ++ show repS2
          ]
      )
arg _ t@(Var (V _ _ No)) = error ("arg applied to variable term with No access." ++ show t)
arg _ term = error ("arg can only be applied to variable terms: " ++ show term)
