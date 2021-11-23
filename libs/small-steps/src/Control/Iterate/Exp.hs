{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module provides deep embeddings of three things
--   1) Exp is a deep embedding of expressions over Sets and Maps as a typed data structure.
--   2) Fun is a deep embedding of symbolic functions
--   3) Query is a deep embedding of queries over Sets and Maps. It can be thought of as
--      a low-level compiled form of Exp
module Control.Iterate.Exp where

import Control.Iterate.BaseTypes (BaseRep (..), Basic (..), Iter (..), List (..), Sett (..), Single (..), fromPairs)
import Control.Iterate.BiMap (BiMap, Bimap, biMapEmpty)
import Control.Iterate.Collect (Collect (..), front, hasElem, none, one)
import Data.Compact.SplitMap (Split, SplitMap)
import qualified Data.Compact.SplitMap as Split
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.PrettyPrint.ANSI.Leijen (Doc, align, parens, text, vsep, (<+>))
import Prelude hiding (lookup)

-- ================================================================================================
-- PART 1. Exp over sets and maps
-- ================================================================================================

-- | The self typed GADT: Exp, that encodes the shape of Set expressions. A deep embedding.
-- Exp is a typed Symbolic representation of queries we may ask. It allows us to introspect a query
-- The strategy is to
-- 1) Define Exp so all queries can be represented.
-- 2) Define smart constructors that "parse" the surface syntax, and build a typed Exp
-- 3) Write an evaluate function:  eval:: Exp t -> t
-- 4) "eval" can introspect the code and apply efficient domain and type specific translations
-- 5) Use the (Iter f) class to evaluate some Exp that can benefit from its efficient nature.
data Exp t where
  Base :: (Ord k, Basic f) => BaseRep f k v -> f k v -> Exp (f k v) -- Note the use of BaseRep to witness what Base type.
  Dom :: Ord k => Exp (f k v) -> Exp (Sett k ())
  Rng :: (Ord k, Ord v) => Exp (f k v) -> Exp (Sett v ())
  DRestrict :: (Ord k, Iter g) => Exp (g k ()) -> Exp (f k v) -> Exp (f k v)
  DExclude :: (Ord k, Iter g) => Exp (g k ()) -> Exp (f k v) -> Exp (f k v)
  RRestrict :: (Ord k, Iter g, Ord v) => Exp (f k v) -> Exp (g v ()) -> Exp (f k v)
  RExclude :: (Ord k, Iter g, Ord v) => Exp (f k v) -> Exp (g v ()) -> Exp (f k v)
  Elem :: (Ord k, Iter g, Show k) => k -> Exp (g k ()) -> Exp Bool
  NotElem :: (Ord k, Iter g, Show k) => k -> Exp (g k ()) -> Exp Bool
  Intersect :: (Ord k, Iter f, Iter g) => Exp (f k v) -> Exp (g k u) -> Exp (Sett k ())
  Subset :: (Ord k, Iter f, Iter g) => Exp (f k v) -> Exp (g k u) -> Exp Bool
  SetDiff :: (Ord k, Iter f, Iter g) => Exp (f k v) -> Exp (g k u) -> Exp (f k v)
  UnionOverrideLeft :: (Show k, Show v, Ord k) => Exp (f k v) -> Exp (g k v) -> Exp (f k v)
  -- The (Show k, Show v) supports logging errors if there are duplicate keys.
  UnionPlus :: (Ord k, Monoid n) => Exp (f k n) -> Exp (f k n) -> Exp (f k n)
  UnionOverrideRight :: Ord k => Exp (f k v) -> Exp (g k v) -> Exp (f k v)
  Singleton :: (Ord k) => k -> v -> Exp (Single k v)
  SetSingleton :: (Ord k) => k -> Exp (Single k ())
  KeyEqual :: (Ord k, Iter f, Iter g) => Exp (f k v) -> Exp (g k u) -> Exp Bool

instance Show (Exp t) where
  show (Base MapR _) = "Map?"
  show (Base SetR _) = "Set?"
  show (Base ListR _) = "List?"
  show (Base SplitR _) = "SplitMap?"
  show (Base SingleR (Single _ _)) = "Single(_ _)"
  show (Base SingleR (SetSingle _)) = "SetSingle(_)"
  show (Base rep _x) = show rep ++ "?"
  show (Dom x) = "(dom " ++ show x ++ ")"
  show (Rng x) = "(rng " ++ show x ++ ")"
  show (DRestrict x y) = "(" ++ show x ++ " ◁ " ++ show y ++ ")"
  show (DExclude x y) = "(" ++ show x ++ " ⋪ " ++ show y ++ ")"
  show (RRestrict x y) = "(" ++ show x ++ " ▷ " ++ show y ++ ")"
  show (RExclude x y) = "(" ++ show x ++ " ⋫ " ++ show y ++ ")"
  show (Elem k x) = "(" ++ show k ++ " ∈ " ++ show x ++ ")"
  show (NotElem k x) = "(" ++ show k ++ " ∉ " ++ show x ++ ")"
  show (Intersect x y) = "(" ++ show x ++ " ∩ " ++ show y ++ ")"
  show (SetDiff x y) = "(" ++ show x ++ " ➖ " ++ show y ++ ")"
  show (Subset x y) = "(" ++ show x ++ " ⊆ " ++ show y ++ ")"
  show (UnionOverrideLeft x y) = "(" ++ show x ++ " ∪ " ++ show y ++ ")"
  show (UnionPlus x y) = "(" ++ show x ++ " ∪+ " ++ show y ++ ")"
  show (UnionOverrideRight x y) = "(" ++ show x ++ " ⨃ " ++ show y ++ ")"
  show (Singleton _ _) = "(singleton _ _ )"
  show (SetSingleton _) = "(setSingleton _ )"
  show (KeyEqual x y) = "(" ++ show x ++ " ≍ " ++ show y ++ ")"

-- =================================================================

-- | Basic types are those that can be embedded into Exp.
-- The HasExp class, encodes how to lift a Basic type into an Exp.
-- The function 'toExp' will build a typed Exp for that Basic type.
-- This will be really usefull in the smart constructors.
class HasExp s t | s -> t where
  toExp :: s -> Exp t

-- | The simplest Base type is one that is already an Exp
instance HasExp (Exp t) t where
  toExp x = x

instance (Ord k) => HasExp (Map k v) (Map k v) where
  toExp x = Base MapR x

instance (Ord k) => HasExp (Set.Set k) (Sett k ()) where
  toExp x = Base SetR (Sett x)

instance (Ord k) => HasExp [(k, v)] (List k v) where
  toExp l = Base ListR (UnSafeList (sortBy (\x y -> compare (fst x) (fst y)) l))

instance (Ord k) => HasExp (Single k v) (Single k v) where
  toExp x = Base SingleR x

instance (Ord k, Ord v) => HasExp (Bimap k v) (Bimap k v) where
  toExp x = Base BiMapR x

instance (Ord k, Split k) => HasExp (SplitMap k v) (SplitMap k v) where
  toExp x = Base SplitR x

-- =======================================================================================================
-- When we build an Exp, we want to make sure all Sets with one element become (SetSingleton x)
-- so we use these 'smart' constructors.

dRestrict :: (Ord k, Iter g) => Exp (g k ()) -> Exp (f k v) -> Exp (f k v)
dRestrict (Base SetR (Sett x)) y | Set.size x == 1 = DRestrict (SetSingleton (Set.elemAt 0 x)) y
dRestrict x y = DRestrict x y

rRestrict :: (Ord k, Iter g, Ord v) => Exp (f k v) -> Exp (g v ()) -> Exp (f k v)
rRestrict y (Base SetR (Sett x)) | Set.size x == 1 = RRestrict y (SetSingleton (Set.elemAt 0 x))
rRestrict y x = RRestrict y x

dExclude :: (Ord k, Iter g) => Exp (g k ()) -> Exp (f k v) -> Exp (f k v)
dExclude (Base SetR (Sett x)) y | Set.size x == 1 = DExclude (SetSingleton (Set.elemAt 0 x)) y
dExclude x y = DExclude x y

rExclude :: (Ord k, Iter g, Ord v) => Exp (f k v) -> Exp (g v ()) -> Exp (f k v)
rExclude y (Base SetR (Sett x)) | Set.size x == 1 = RExclude y (SetSingleton (Set.elemAt 0 x))
rExclude y x = RExclude y x

-- ==========================================================================================
-- Smart constructors build typed Exp with real values at the leaves (the Base constuctor)

-- (⊆),
-- (∩),

dom :: (Ord k, HasExp s (f k v)) => s -> Exp (Sett k ())
dom x = Dom (toExp x)

rng :: (Ord k, Ord v) => HasExp s (f k v) => s -> Exp (Sett v ())
rng x = Rng (toExp x)

(◁), (<|), drestrict :: (Ord k, HasExp s1 (Sett k ()), HasExp s2 (f k v)) => s1 -> s2 -> Exp (f k v)
(◁) x y = dRestrict (toExp x) (toExp y)
drestrict = (◁)
(<|) = drestrict

(⋪), dexclude :: (Ord k, Iter g, HasExp s1 (g k ()), HasExp s2 (f k v)) => s1 -> s2 -> Exp (f k v)
(⋪) x y = dExclude (toExp x) (toExp y)
dexclude = (⋪)

(▷), (|>), rrestrict :: (Ord k, Iter g, Ord v, HasExp s1 (f k v), HasExp s2 (g v ())) => s1 -> s2 -> Exp (f k v)
(▷) x y = rRestrict (toExp x) (toExp y)
rrestrict = (▷)
(|>) = (▷)

(⋫), rexclude :: (Ord k, Iter g, Ord v, HasExp s1 (f k v), HasExp s2 (g v ())) => s1 -> s2 -> Exp (f k v)
(⋫) x y = rExclude (toExp x) (toExp y)
rexclude = (⋫)

(∈) :: (Show k, Ord k, Iter g, HasExp s (g k ())) => k -> s -> Exp Bool
(∈) x y = Elem x (toExp y)

(∉), notelem :: (Show k, Ord k, Iter g, HasExp s (g k ())) => k -> s -> Exp Bool
(∉) x y = NotElem x (toExp y)
notelem = (∉)

(∪), unionleft :: (Show k, Show v, Ord k, HasExp s1 (f k v), HasExp s2 (g k v)) => s1 -> s2 -> Exp (f k v)
(∪) x y = UnionOverrideLeft (toExp x) (toExp y)
unionleft = (∪)

(⨃), unionright :: (Ord k, HasExp s1 (f k v), HasExp s2 (g k v)) => s1 -> s2 -> Exp (f k v)
(⨃) x y = UnionOverrideRight (toExp x) (toExp y)
unionright = (⨃)

(∪+), unionplus :: (Ord k, Monoid n, HasExp s1 (f k n), HasExp s2 (f k n)) => s1 -> s2 -> Exp (f k n)
(∪+) x y = UnionPlus (toExp x) (toExp y)
unionplus = (∪+)

singleton :: (Ord k) => k -> v -> Exp (Single k v)
singleton k v = Singleton k v

setSingleton :: (Ord k) => k -> Exp (Single k ())
setSingleton k = SetSingleton k

(∩), intersect :: (Ord k, Iter f, Iter g, HasExp s1 (f k v), HasExp s2 (g k u)) => s1 -> s2 -> Exp (Sett k ())
(∩) x y = Intersect (toExp x) (toExp y)
intersect = (∩)

(⊆), subset :: (Ord k, Iter f, Iter g, HasExp s1 (f k v), HasExp s2 (g k u)) => s1 -> s2 -> Exp Bool
(⊆) x y = Subset (toExp x) (toExp y)
subset = (⊆)

(➖), setdiff :: (Ord k, Iter f, Iter g, HasExp s1 (f k v), HasExp s2 (g k u)) => s1 -> s2 -> Exp (f k v)
(➖) x y = SetDiff (toExp x) (toExp y)
setdiff = (➖)

(≍), keyeq :: (Ord k, Iter f, Iter g, HasExp s1 (f k v), HasExp s2 (g k u)) => s1 -> s2 -> Exp Bool
(≍) x y = KeyEqual (toExp x) (toExp y)
keyeq = (≍)

-- ==========================================================================================
-- Part 2. Symbolic functions
-- ===========================================================================================

-- | An symbolic function Fun has two parts, a Lam that can be analyzed, and real function that can be applied
data Fun t = Fun (Lam t) t

-- | We can observe a Fun by showing the Lam part.
instance Show (Fun t) where
  show (Fun lam _fun) = show lam

-- | Symbolc functions (Fun) are data, that can be pattern matched over. They
-- 1) Represent a wide class of binary functions that are used in translating the SetAlgebra
-- 2) Turned into a String so they can be printed
-- 3) Turned into the function they represent.
-- 4) Composed into bigger functions
-- 5) Symbolically symplified
-- Here  we implement Symbolic Binary functions with upto 4 variables, which is enough for this use
-- =================================================================================================
data Pat env t where
  P1 :: Pat (d, c, b, a) d
  P2 :: Pat (d, c, b, a) c
  P3 :: Pat (d, c, b, a) b
  P4 :: Pat (d, c, b, a) a
  PPair :: Pat (d, c, b, a) a -> Pat (d, c, b, a) b -> Pat (d, c, b, a) (a, b)

data Expr env t where
  X1 :: Expr (d, c, b, a) d
  X2 :: Expr (d, c, b, a) c
  X3 :: Expr (d, c, b, a) b
  X4 :: Expr (d, c, b, a) a
  HasKey :: (Iter f, Ord k) => Expr e k -> (f k v) -> Expr e Bool
  Neg :: Expr e Bool -> Expr e Bool
  Ap :: Lam (a -> b -> c) -> Expr e a -> Expr e b -> Expr e c
  EPair :: Expr e a -> Expr e b -> Expr e (a, b)
  FST :: Expr e (a, b) -> Expr e a
  SND :: Expr e (a, b) -> Expr e b
  Lit :: Show t => t -> Expr env t

-- Carefull no pattern P1, P2, P3, P4 should appear MORE THAN ONCE in a Lam.

data Lam t where
  Lam :: Pat (d, c, b, a) t -> Pat (d, c, b, a) s -> Expr (d, c, b, a) v -> Lam (t -> s -> v)
  Add :: Num n => Lam (n -> n -> n)
  Cat :: Monoid m => Lam (m -> m -> m)
  Eql :: Eq t => Lam (t -> t -> Bool)
  Both :: Lam (Bool -> Bool -> Bool)
  Lift :: (a -> b -> c) -> Lam (a -> b -> c) -- For use n the tests only!

-- ============= Printing in 𝜷-Normal Form =========================

type StringEnv = (String, String, String, String)

bindE :: Pat (a, b, c, d) t -> Expr (w, x, y, z) t -> StringEnv -> StringEnv
bindE P1 v (e@(_, c, b, a)) = (showE e v, c, b, a)
bindE P2 v (e@(d, _, b, a)) = (d, showE e v, b, a)
bindE P3 v (e@(d, c, _, a)) = (d, c, showE e v, a)
bindE P4 v (e@(d, c, b, _)) = (d, c, b, showE e v)
bindE (PPair p1 p2) (EPair e1 e2) env = bindE p1 e1 (bindE p2 e2 env)
bindE (PPair p1 p2) e env = bindE p2 (SND e) (bindE p1 (FST e) env)

showE :: StringEnv -> (Expr (a, b, c, d) t) -> String
showE (x, _, _, _) X1 = x
showE (_, y, _, _) X2 = y
showE (_, _, z, _) X3 = z
showE (_, _, _, w) X4 = w
showE e (EPair a b) = "(" ++ showE e a ++ "," ++ showE e b ++ ")"
showE e (Ap (Lam p1 p2 expr) x y) = showE (bindE p2 y (bindE p1 x e)) expr
showE e (FST f) = "(fst " ++ showE e f ++ ")"
showE e (SND f) = "(snd " ++ showE e f ++ ")"
showE e (Ap oper a b) = "(" ++ showE e a ++ showL e oper ++ showE e b ++ ")"
showE e (HasKey k _datum) = "(haskey " ++ showE e k ++ " ?)"
showE e (Neg x) = "(not " ++ showE e x ++ ")"
showE _ (Lit n) = show n

showL :: StringEnv -> Lam t -> String
showL e (Lam p1 p2 expr) = "\\ " ++ showP e p1 ++ " " ++ showP e p2 ++ " -> " ++ showE e expr
showL _e Add = " + "
showL _e Cat = " <> "
showL _e Eql = " == "
showL _e Both = " && "
showL _e (Lift _f) = "<lifted function>"

showP :: StringEnv -> (Pat any t) -> String
showP (x, _, _, _) P1 = x
showP (_, y, _, _) P2 = y
showP (_, _, z, _) P3 = z
showP (_, _, _, w) P4 = w
showP env (PPair p1 p2) = "(" ++ showP env p1 ++ "," ++ showP env p2 ++ ")"

instance Show (Expr (a, b, c, d) t) where
  show x = showE ("X1", "X2", "X3", "X4") x

instance Show (Lam t) where
  show x = showL ("X1", "X2", "X3", "X4") x

-- ======================================================================================
-- Operations we use to manipulate Fun. Some simple ones, and some ways to compose them.
-- The great thing is the types completely decide what the operations do.
-- ======================================================================================

-- Used in projectStep, chainStep, andPStep, orStep and guardStep
apply :: Fun t -> t
apply (Fun _e f) = f

-- Used in compile (UnionOverrideLeft case)
first :: Fun (v -> s -> v)
first = Fun (Lam P1 P2 X1) (\x _y -> x)

-- Used in compile (UnionOverrideRight case)
second :: Fun (v -> s -> s)
second = Fun (Lam P1 P2 X2) (\_x y -> y)

-- Used in compile (UnionPlus case)
plus :: Monoid t => Fun (t -> t -> t)
plus = (Fun Cat (<>))

eql :: Eq t => Fun (t -> t -> Bool)
eql = (Fun Eql (==))

constant :: Show c => c -> Fun (a -> b -> c)
constant c = Fun (Lam P1 P2 (Lit c)) (\_x _y -> c)

-- Used in compile (RExclude RRestrict cases)
rngElem :: (Ord rng, Iter f) => f rng v -> Fun (dom -> rng -> Bool)
rngElem realset = Fun (Lam P1 P2 (HasKey X2 realset)) (\_x y -> haskey y realset) -- x is ignored and realset is supplied

domElem :: (Ord dom, Iter f) => f dom v -> Fun (dom -> rng -> Bool)
domElem realset = Fun (Lam P1 P2 (HasKey X1 realset)) (\x _y -> haskey x realset) -- y is ignored and realset is supplied

rngFst :: Fun (x -> (a, b) -> a)
rngFst = Fun (Lam P1 (PPair P2 P3) X2) (\_x (a, _b) -> a)

rngSnd :: Fun (x -> (a, b) -> b)
rngSnd = Fun (Lam P1 (PPair P2 P3) X3) (\_x y -> snd y)

compose1 :: Fun (t1 -> t2 -> t3) -> Fun (t1 -> t4 -> t2) -> Fun (t1 -> t4 -> t3)
compose1 (Fun e1 f1) (Fun e2 f2) = Fun (Lam P1 P2 (Ap e1 X1 (Ap e2 X1 X2))) (\a b -> f1 a (f2 a b))

compSndL :: Fun (k -> (a, b) -> c) -> Fun (k -> d -> a) -> Fun (k -> (d, b) -> c)
compSndL (Fun m mf) (Fun g mg) = Fun (Lam P1 (PPair P2 P3) (Ap m X1 (EPair (Ap g X1 X2) X3))) (\x (a, b) -> mf x (mg x a, b))

compSndR :: Fun (k -> (a, b) -> c) -> Fun (k -> d -> b) -> Fun (k -> (a, d) -> c)
compSndR (Fun m mf) (Fun g mg) = (Fun (Lam P1 (PPair P2 P3) (Ap m X1 (EPair X2 (Ap g X1 X3)))) (\x (a, b) -> mf x (a, mg x b)))

compCurryR :: Fun (k -> (a, b) -> d) -> Fun (a -> c -> b) -> Fun (k -> (a, c) -> d)
compCurryR (Fun ef f) (Fun eg g) = Fun (Lam P1 (PPair P2 P3) (Ap ef X1 (EPair X2 (Ap eg X2 X3)))) (\x (a, b) -> f x (a, g a b))

nEgate :: Fun (k -> v -> Bool) -> Fun (k -> v -> Bool)
nEgate (Fun ef f) = Fun (Lam P1 P2 (Neg (Ap ef X1 X2))) (\x y -> not (f x y))

always :: Fun (a -> b -> Bool)
always = constant True

both :: Fun (a -> b -> Bool) -> Fun (a -> b -> Bool) -> Fun (a -> b -> Bool)
both (Fun ef e) (Fun ff f) = Fun (Lam P1 P2 (Ap Both (Ap ef X1 X2) (Ap ff X1 X2))) (\a b -> (e a b) && (f a b))

lift :: (a -> b -> c) -> Fun (a -> b -> c) -- This is used in the tests, not good to use it elsewhere.
lift f = Fun (Lift f) f

-- ==============================================================
-- Part 3. Queries, a compiled form of Exp
-- ==============================================================

-- =================================================================================
-- Query is a single datatype that describes a low-level language that can be
-- used to build compound iterators, from other iterators.
-- =================================================================================

data Query k v where
  BaseD :: (Iter f, Ord k) => BaseRep f k v -> f k v -> Query k v
  ProjectD :: Ord k => Query k v -> Fun (k -> v -> u) -> Query k u
  AndD :: Ord k => Query k v -> Query k w -> Query k (v, w)
  ChainD :: (Ord k, Ord v) => Query k v -> Query v w -> Fun (k -> (v, w) -> u) -> Query k u
  AndPD :: Ord k => Query k v -> Query k u -> Fun (k -> (v, u) -> w) -> Query k w
  OrD :: Ord k => Query k v -> Query k v -> Fun (v -> v -> v) -> Query k v
  GuardD :: Ord k => Query k v -> Fun (k -> v -> Bool) -> Query k v
  DiffD :: Ord k => Query k v -> Query k u -> Query k v

-- ======================================================================================
-- low-level smart constructors for Query. These apply semantic preserving rewrites when applicable
-- ======================================================================================

smart :: Bool
smart = True -- for debugging purposes, this can be set to False, in which case no rewrites occurr.

projD :: Ord k => Query k v -> Fun (k -> v -> u) -> Query k u
projD x y = case (x, y) of
  (ProjectD f p, q) | smart -> projD f (compose1 q p)
  (AndD f g, q) | smart -> andPD f g (compose1 q second)
  (AndPD f g p, q) | smart -> andPD f g (compose1 q p)
  (f, p) -> ProjectD f p

andD :: Ord k => Query k v1 -> Query k v2 -> Query k (v1, v2)
andD (ProjectD f p) g | smart = AndPD f g (compSndL second p)
andD f (ProjectD g p) | smart = AndPD f g (compSndR second p)
andD f g = AndD f g

andPD :: Ord k => Query k v1 -> Query k u -> Fun (k -> (v1, u) -> v) -> Query k v
andPD (ProjectD f p) g q | smart = andPD f g (compSndL q p)
andPD f g p = AndPD f g p

chainD :: (Ord k, Ord v) => Query k v -> Query v w -> Fun (k -> (v, w) -> u) -> Query k u
chainD f (ProjectD g p) q | smart = chainD f g (compCurryR q p)
chainD f g p = ChainD f g p

guardD :: Ord k => Query k v -> Fun (k -> v -> Bool) -> Query k v
guardD (GuardD q1 test1) test2 | smart = GuardD q1 (both test1 test2)
guardD qry test = GuardD qry test

-- =======================================================================================
-- Finally we make high-level smart constructors for Query, so we can lift un-embedded Base types
-- into Queries, so programmers don't need to know about List and Sett and other anomalies.
-- Note that these high-level smart constructors are in 1-to-1 correspondance with the low-level
-- ones, except the low-level ones take Querys, and the high-level ones just take data, lift the
-- data to Query, and then apply the low-level smart constructors.

projectQ :: (Ord k, HasQuery c k v) => c -> Fun (k -> v -> u) -> Query k u
projectQ q fun = ProjectD (query q) fun

andQ :: (Ord k, HasQuery concrete1 k v, HasQuery concrete2 k w) => concrete1 -> concrete2 -> Query k (v, w)
andQ x y = AndD (query x) (query y)

orQ ::
  (Ord k, HasQuery concrete1 k v, HasQuery concrete2 k v) =>
  concrete1 ->
  concrete2 ->
  Fun (v -> v -> v) ->
  Query k v
orQ x y comb = OrD (query x) (query y) comb

chainQ ::
  (Ord k, Ord v, HasQuery concrete1 k v, HasQuery concrete2 v w) =>
  concrete1 ->
  concrete2 ->
  Fun (k -> (v, w) -> u) ->
  Query k u
chainQ x y p = ChainD (query x) (query y) p

andPQ ::
  (Ord k, HasQuery concrete1 k v, HasQuery concrete2 k u) =>
  concrete1 ->
  concrete2 ->
  Fun (k -> (v, u) -> w) ->
  Query k w
andPQ x y p = AndPD (query x) (query y) p

guardQ ::
  (Ord k, HasQuery concrete k v) =>
  concrete ->
  Fun (k -> v -> Bool) ->
  Query k v
guardQ x p = GuardD (query x) p

-- Don't know why this won't type check
-- diffQ :: (Ord k, HasQuery concrete1 k v, HasQuery concrete2 k u) => concrete1 -> concrete2 -> Query k v
-- diffQ = \ x y -> DiffD (query x) (query y)
diffQ ::
  forall k v u concrete1 concrete2.
  (Ord k, HasQuery (concrete1 k v) k v, HasQuery (concrete2 k u) k u) =>
  (concrete1 k v) ->
  (concrete2 k u) ->
  Query k v
diffQ x y = DiffD (query x) (query @(concrete2 k u) @k @u y)

class HasQuery concrete k v where
  query :: concrete -> Query k v

instance HasQuery (Query k v) k v where
  query xs = xs

instance Ord k => HasQuery [(k, v)] k v where
  query xs = BaseD ListR (fromPairs (\l _r -> l) xs)

instance Ord k => HasQuery (Set.Set k) k () where
  query xs = BaseD SetR (Sett xs)

instance Ord k => HasQuery (Map.Map k v) k v where
  query xs = BaseD MapR xs

instance (Ord v, Ord k) => HasQuery (BiMap v k v) k v where
  query xs = BaseD BiMapR xs

instance Ord k => HasQuery (Single k v) k v where
  query xs = BaseD SingleR xs

instance (Ord k, Split k) => HasQuery (SplitMap k v) k v where
  query xs = BaseD SplitR xs

-- =================================================
-- Show Instance of Query

ppQuery :: Query k v -> Doc
ppQuery (BaseD rep _f) = parens $ text (show rep)
ppQuery (ProjectD f p) = parens $ text "Proj" <+> align (vsep [ppQuery f, text (show p)])
ppQuery (AndD f g) = parens $ text "And" <+> align (vsep [ppQuery f, ppQuery g])
ppQuery (ChainD f g p) = parens $ text "Chain" <+> align (vsep [ppQuery f, ppQuery g, text (show p)])
ppQuery (OrD f g p) = parens $ text "Or" <+> align (vsep [ppQuery f, ppQuery g, text (show p)])
ppQuery (GuardD f p) = parens $ text "Guard" <+> align (vsep [ppQuery f, text (show p)])
ppQuery (DiffD f g) = parens $ text "Diff" <+> align (vsep [ppQuery f, ppQuery g])
ppQuery (AndPD f g p) = parens $ text "AndP" <+> align (vsep [ppQuery f, ppQuery g, text (show p)])

instance Show (Query k v) where
  show x = show (ppQuery x)

-- =================================================
-- An instance of Iter can be made for Query.

nxtQuery :: Query a b -> Collect (a, b, Query a b)
nxtQuery (BaseD rep x) = do (k, v, x2) <- nxt x; one (k, v, BaseD rep x2)
nxtQuery (ProjectD x p) = projStep nxtQuery p x
nxtQuery (AndD f g) = do triple1 <- nxtQuery f; triple2 <- nxtQuery g; andStep triple1 triple2
nxtQuery (ChainD f g p) = do trip <- nxtQuery f; chainStep trip g p
nxtQuery (AndPD f g p) = do triple1 <- nxtQuery f; triple2 <- nxtQuery g; andPstep triple1 triple2 p
nxtQuery (OrD f g comb) = orStep nxtQuery f g comb
nxtQuery (GuardD f p) = guardStep nxtQuery p f
nxtQuery (DiffD f g) = do trip <- nxtQuery f; diffStep trip g

lubQuery :: Ord a => a -> Query a b -> Collect (a, b, Query a b)
lubQuery key (BaseD rep x) = do (k, v, x2) <- lub key x; one (k, v, BaseD rep x2)
lubQuery key (ProjectD x p) = projStep (lubQuery key) p x
lubQuery key (AndD f g) = do triple1 <- lubQuery key f; triple2 <- lubQuery key g; andStep triple1 triple2
lubQuery key (ChainD f g p) = do trip <- lubQuery key f; chainStep trip g p
lubQuery key (AndPD f g p) = do triple1 <- lubQuery key f; triple2 <- lubQuery key g; andPstep triple1 triple2 p
lubQuery key (OrD f g comb) = orStep (lubQuery key) f g comb
lubQuery key (GuardD f p) = guardStep (lubQuery key) p f
lubQuery key (DiffD f g) = do trip <- lubQuery key f; diffStep trip g

instance Iter Query where
  nxt = nxtQuery
  lub = lubQuery

-- ==============================================================================================
-- To make the Iter instance for Query, we need semantic operators for each of the constructors
-- of Query. These semantic operators are "step" functions for each constructor.
-- ==============================================================================================

-- ==== Project ====
projStep ::
  Ord k =>
  (t -> Collect (k, v, Query k v)) ->
  Fun (k -> v -> u) ->
  t ->
  Collect (k, u, Query k u)
projStep next p f = do (k, v, f') <- next f; one (k, apply p k v, ProjectD f' p)

-- ===== And = ====
andStep ::
  Ord a =>
  (a, b1, Query a b1) ->
  (a, b2, Query a b2) ->
  Collect (a, (b1, b2), Query a (b1, b2))
andStep (ftrip@(k1, v1, f1)) (gtrip@(k2, v2, g2)) =
  case compare k1 k2 of
    EQ -> one (k1, (v1, v2), AndD f1 g2)
    LT -> do ftrip' <- lubQuery k2 f1; andStep ftrip' gtrip
    GT -> do gtrip' <- lubQuery k1 g2; andStep ftrip gtrip'

-- ==== Chain ====
chainStep ::
  (Ord b, Ord a) =>
  (a, b, Query a b) ->
  Query b w ->
  Fun (a -> (b, w) -> u) ->
  Collect (a, u, Query a u)
chainStep (d, r1, f1) g comb =
  case lookup r1 g of -- recall that the values 'r1' from f, are not iterated in ascending order, only the keys 'd' are ascending
    Just w -> one (d, apply comb d (r1, w), ChainD f1 g comb)
    Nothing -> do trip <- nxtQuery f1; chainStep trip g comb

-- ==== And with Projection ====
andPstep ::
  Ord a =>
  (a, b1, Query a b1) ->
  (a, b2, Query a b2) ->
  Fun (a -> (b1, b2) -> w) ->
  Collect (a, w, Query a w)
andPstep (ftrip@(k1, v1, f1)) (gtrip@(k2, v2, g2)) p =
  case compare k1 k2 of
    EQ -> one (k1, (apply p k1 (v1, v2)), AndPD f1 g2 p)
    LT -> do ftrip' <- lubQuery k2 f1; andPstep ftrip' gtrip p
    GT -> do gtrip' <- lubQuery k1 g2; andPstep ftrip gtrip' p

-- ==== Or with combine ====
orStep ::
  (Ord k, Ord a) =>
  (Query k v -> Collect (a, v, Query k v)) ->
  Query k v ->
  Query k v ->
  Fun (v -> v -> v) ->
  Collect (a, v, Query k v)
orStep next f g comb =
  case (hasElem (next f), hasElem (next g)) of -- We have to be careful, because if only one has a nxt, there is still an answer
    (Nothing, Nothing) -> none
    (Just (k1, v1, f1), Nothing) -> one (k1, v1, OrD f1 g comb)
    (Nothing, Just (k1, v1, g1)) -> one (k1, v1, OrD f g1 comb)
    (Just (k1, v1, f1), Just (k2, v2, g2)) ->
      case compare k1 k2 of
        EQ -> one (k1, apply comb v1 v2, OrD f1 g2 comb)
        LT -> one (k1, v1, OrD f1 g comb)
        GT -> one (k2, v2, OrD f g2 comb)

-- ===== Guard =====
guardStep ::
  Ord a =>
  (Query a b -> Collect (a, b, Query a b)) ->
  Fun (a -> b -> Bool) ->
  Query a b ->
  Collect (a, b, Query a b)
guardStep next p f = do triple <- next f; loop triple
  where
    loop (k, v, f') = if (apply p k v) then one (k, v, GuardD f' p) else do triple <- nxtQuery f'; loop triple

-- ===== Difference by key =====
diffStep :: Ord k => (k, v, Query k v) -> Query k u -> Collect (k, v, Query k v)
diffStep (k1, u1, f1) g =
  case hasElem (lubQuery k1 g) of
    Nothing -> one (k1, u1, DiffD f1 g) -- g has nothing to subtract
    Just (k2, _u2, g2) -> case compare k1 k2 of
      EQ -> do tup <- nxtQuery f1; diffStep tup g2
      LT -> one (k1, u1, DiffD f1 g)
      GT -> one (k1, u1, DiffD f1 g) -- the hasLub guarantees k1 <= k2, so this case is dead code

-- ========== Rng ====================
rngStep :: Ord v => Query k v -> Sett v ()
rngStep dat = materialize SetR (loop dat)
  where
    loop x = do (_k, v, x2) <- nxt x; front (v, ()) (loop x2)

-- =======================================================================================

-- | Given a BaseRep we can materialize a (Collect k v) into the type witnessed by the BaseRep.
-- Recall a (Collect k v) has no intrinsic type (it is just an ABSTRACT sequence of tuples), so
-- the witness describes how to turn them into the chosen datatype. Note that materialize is meant
-- to be applied to a collection built by iterating over a Query. This produces the keys in
-- ascending order, with no duplicate keys. So we do not need to specify how to merge values.
materialize :: (Ord k) => BaseRep f k v -> Collect (k, v) -> f k v
materialize ListR x = fromPairs (\l _r -> l) (runCollect x [] (:))
materialize MapR x = runCollect x Map.empty (\(k, v) ans -> Map.insert k v ans)
materialize SetR x = Sett (runCollect x Set.empty (\(k, _) ans -> Set.insert k ans))
materialize BiMapR x = runCollect x biMapEmpty (\(k, v) ans -> addpair k v ans)
materialize SingleR x = runCollect x Fail (\(k, v) _ignore -> Single k v)
materialize SplitR x = runCollect x Split.empty (\(k, v) ans -> Split.insert k v ans)
