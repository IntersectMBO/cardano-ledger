{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Constrained.MinModel where

import qualified Constrained.Base as Base
import Constrained.Core (Evidence (..), Rename (rename), Var (..), eqVar, unionWithMaybe,freshen,Value(..),unValue)
import Constrained.Env
import Constrained.GenT
import Constrained.List hiding (ListCtx)
import Control.Applicative ((<|>))
import Control.Monad.Identity
import Control.Monad.Writer (Writer, tell, runWriter)
import Control.Monad(guard)
import qualified Data.Foldable as Foldable (fold, toList)
import Data.Foldable(fold)
import Data.Kind
import Data.List (intersect, nub, (\\))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, isJust, maybeToList,fromMaybe,listToMaybe,isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Typeable
import GHC.Stack
import Prettyprinter (Pretty (pretty))
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import qualified Data.Monoid as Monoid
import Data.Semigroup (Max (..), getMax,sconcat,Any(..))
import Test.QuickCheck hiding (forAll)



-- ===========================================
-- Terms
-- ===========================================

type AppRequires t dom rng =
  ( Logic t
  , TypeList dom
  , Eq (t dom rng)
  , Show (t dom rng)
  , Typeable dom
  , Typeable rng
  , All HasSpec dom
  , HasSpec rng
  )

data Term a where
  App ::
    forall t dom rng.
    AppRequires t dom rng =>
    t dom rng ->
    List Term dom ->
    Term rng
  Lit :: (Typeable a, Eq a, Show a) => a -> Term a
  V :: HasSpec a => Var a -> Term a

instance Show (Term a) where
  show (V x) = show x
  show (Lit x) = show x
  show (App f xs) = "(" ++ name f ++ " " ++ show xs ++ ")"

instance Eq (Term a) where
  V x == V x' = x == x'
  Lit a == Lit b = a == b
  App (w1 :: x1) (ts :: List Term dom1) == App (w2 :: x2) (ts' :: List Term dom2) =
    case (eqT @dom1 @dom2, eqT @x1 @x2) of
      (Just Refl, Just Refl) ->
        w1 == w2
          && sameTerms ts ts'
      _ -> False
  _ == _ = False

-- How to compare the args of two applications for equality
sameTerms :: All HasSpec as => List Term as -> List Term as -> Bool
sameTerms Nil Nil = True
sameTerms (x :> xs) (y :> ys) = x == y && sameTerms xs ys

-- | Syntactic operations are ones that have to do with the structure and appearence of the type.
class Syntax (t :: [Type] -> Type -> Type) where
  inFix :: forall dom rng. t dom rng -> Bool
  inFix _ = False
  name :: forall dom rng. t dom rng -> String

-- | Semantic operations are ones that give the function symbol, meaning as a function.
--   I.e. how to apply the function to a list of arguments and return a value.
class Syntax t => Semantics (t :: [Type] -> Type -> Type) where
  semantics :: forall d r. t d r -> FunTy d r -- e.g. FunTy '[a,Int] Bool == a -> Int -> Bool

-- | Logical operations are one that support reasoning about how a function symbol
--   relates to logical properties, that we call Spec's
class (Typeable t, Syntax t, Semantics t) => Logic (t :: [Type] -> Type -> Type) where
  {-# MINIMAL propagate | (propagateTypeSpec, propagateMemberSpec) #-}

  propagateTypeSpec ::
    (AppRequires t as b, HasSpec a) =>
    t as b -> ListCtx as a -> TypeSpec b -> [b] -> Spec a
  propagateTypeSpec f ctx ts cant = propagate f ctx (TypeSpec ts cant)

  propagateMemberSpec ::
    (AppRequires t as b, HasSpec a) =>
    t as b -> ListCtx as a -> NonEmpty b -> Spec a
  propagateMemberSpec f ctx xs = propagate f ctx (MemberSpec xs)

  propagate ::
    (AppRequires t as b, HasSpec a) =>
    t as b -> ListCtx as a -> Spec b ->Spec a
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec es) = ErrorSpec es
  propagate f ctx (SuspendedSpec v ps) = constrained $ \v' -> Let (App f (fromListCtx ctx v')) (v :-> ps)
  propagate f ctx (TypeSpec ts cant) = propagateTypeSpec f ctx ts cant
  propagate f ctx (MemberSpec xs) = propagateMemberSpec f ctx xs

-- =========================================
-- Contexts
-- =========================================

data HOLE a where HOLE :: HOLE a

data ListCtx (as :: [Type]) a where
  Unary :: Ctx a a -> ListCtx '[a] a
  (:-+) :: Ctx b a -> b -> ListCtx '[a, b] a
  (:+-) :: a -> Ctx a b -> ListCtx '[a, b] b

data Ctx var (hole :: Type) where
  CtxApp :: (AppRequires fn as b
    , HasSpec b
    , TypeList as
    , Typeable as
    , All HasSpec as
    , Logic fn) => fn as b -> ListCtx as v -> Ctx v b
  Hole :: HasSpec v => Ctx v v


ctxHasSpec :: Ctx v a -> Evidence (HasSpec a)
ctxHasSpec Hole = Evidence
ctxHasSpec CtxApp {} = Evidence

-- This is where the logical properties of a function symbol are applied to transform one spec into another
-- Note if there is a bunch of functions nested together, like (sizeOf_ (elems_ (snd_ x)))
-- we propagate each of those nested function symbols over the current spec, one at a time.
-- The result of this propagation is then made the current spec in the recusive calls to 'propagateSpec'
propagateSpec ::
  forall v a.
  HasSpec v =>
  Spec a ->
  Ctx v a ->
  Spec v
propagateSpec spec = \case
  Hole -> spec
  CtxApp f (Unary ctx) | Evidence <- ctxHasSpec ctx -> propagate f (Unary ctx) spec
  CtxApp f (ctx :-+ v) | Evidence <- ctxHasSpec ctx -> propagateSpec (propagate f (ctx :-+ v) spec) Hole
  CtxApp f (v :+- ctx) | Evidence <- ctxHasSpec ctx -> propagateSpec (propagate f (v :+- ctx) spec) Hole
  

toCtx :: Var a -> Term b -> GE (Ctx a b)
toCtx = undefined


-- | From a ListCtx, build a (List Term as), to which the function symbol can be applied.
--   Hole becomes 't', values become `Lit`
fromListCtx :: All HasSpec as => ListCtx as a -> Term a -> List Term as
fromListCtx (Unary c) t = t :> Nil
fromListCtx (Hole :-+ y) t = t :> Lit y :> Nil
fromListCtx (x :+- Hole) t = Lit x :> t :> Nil    

-- ===========================

data Binder a where
  (:->) :: HasSpec a => Var a -> Pred -> Binder a

deriving instance Show (Binder a)

data Pred where
  ElemPred :: forall a. HasSpec a => Bool -> Term a -> NonEmpty a -> Pred
  And :: [Pred] -> Pred
  ForAll :: (Forallable t a, HasSpec t, HasSpec a) => Term t -> Binder a -> Pred
  Assert :: Term Bool -> Pred
  TruePred :: Pred
  FalsePred :: NonEmpty String -> Pred
  -- Match :: Term (a,b) -> (Term a -> Term b -> Pred) -> Pred
  Case :: HasSpec (Either a b) => Term (Either a b) -> Binder a -> Binder b -> Pred
  Let :: Term a -> Binder a -> Pred
  Subst :: HasSpec a => Var a -> Term a -> Pred -> Pred

deriving instance Show Pred

-- ===============================

data Spec a where
  TrueSpec :: Spec a
  ErrorSpec :: NonEmpty String -> Spec a
  SuspendedSpec :: HasSpec a => Var a -> Pred -> Spec a -- Maybe we elide this at first
  MemberSpec :: NonEmpty a -> Spec a
  TypeSpec :: HasSpec a => TypeSpec a -> [a] -> Spec a

deriving instance Show a => Show (Spec a)

typeSpec :: HasSpec a => TypeSpec a -> Spec a
typeSpec ts = TypeSpec ts mempty

isErrorLike :: forall a. Spec a -> Bool
isErrorLike ErrorSpec {} = True
isErrorLike (TypeSpec x _) =
  case guardTypeSpec @a x of
    ErrorSpec msgs -> True
    _ -> False
isErrorLike _ = False

-- ========================================

class (Typeable a, Eq a, Show a, Show (TypeSpec a), Typeable (TypeSpec a)) => HasSpec a where
  -- | The `TypeSpec a` is the type-specific `Spec a`.
  type TypeSpec a

  -- `TypeSpec` behaves sort-of like a monoid with a neutral
  -- element `emptySpec` and a `combineSpec` for combining
  -- two `TypeSpec a`. However, in order to provide flexibilty
  -- `combineSpec` takes two `TypeSpec` and constucts a `Spec`. This
  -- avoids e.g. having to have a separate implementation of `ErrorSpec`
  -- and `MemberSpec` in `TypeSpec`.

  emptySpec :: TypeSpec a
  combineSpec :: TypeSpec a -> TypeSpec a -> Spec a

  -- | Generate a value that satisfies the `TypeSpec`.
  -- The key property for this generator is soundness:
  --  ∀ a ∈ genFromTypeSpec spec. a `conformsTo` spec
  genFromTypeSpec :: (HasCallStack, MonadGenError m) => TypeSpec a -> GenT m a

  -- | Check conformance to the spec.
  conformsTo :: HasCallStack => a -> TypeSpec a -> Bool

  -- | Convert a spec to predicates:
  -- The key property here is:
  --   ∀ a. a `conformsTo` spec == a `conformsTo` constrained (\t -> toPreds t spec)
  toPreds :: Term a -> TypeSpec a -> Pred

  -- | This is used to detect self inconsistencies in a (TypeSpec t)
  --   guardTypeSpec message ty --> ErrorSpec message, if ty is inconsistent
  guardTypeSpec :: TypeSpec a -> Spec a
  guardTypeSpec ty = typeSpec ty

-- ====================================================
-- Now some concrete examples
-- 1) Introduce the function symbols
-- 2) Give the Syntax, Semantics, and Logic instances
-- 3) Give the HasSpec instance
-- ====================================================

-- ======== Integer example ==============

data IntegerSym (dom :: [Type]) rng where
  PlusW :: IntegerSym '[Integer, Integer] Integer
  MinusW :: IntegerSym '[Integer, Integer] Integer
  LessOrEqW :: IntegerSym '[Integer, Integer] Bool
  GreaterOrEqW :: IntegerSym '[Integer, Integer] Bool
deriving instance Eq (IntegerSym dom rng)
instance Show (IntegerSym dom rng) where show = name

instance Syntax IntegerSym where
  name PlusW = "+."
  name MinusW = "-."
  name LessOrEqW = "<=."
  name GreaterOrEqW = ">=."
  inFix _ = True

instance Semantics IntegerSym where
  semantics PlusW = (+)
  semantics MinusW = (-)
  semantics LessOrEqW = (<=)
  semantics GreaterOrEqW = (>=)

instance Logic IntegerSym where
  propagateTypeSpec PlusW (Hole :-+ n) (Interval lo hi) bad =
    TypeSpec (Interval ((+ n) <$> lo) ((+ n) <$> hi)) (map (+ n) bad)
  propagateTypeSpec PlusW (n :+- Hole) (Interval lo hi) bad =
    TypeSpec (Interval ((+ n) <$> lo) ((+ n) <$> hi)) (map (+ n) bad)
  propagateTypeSpec MinusW (Hole :-+ n) (Interval lo hi) bad =
    TypeSpec (Interval (minus n <$> lo) (minus n <$> hi)) (map (minus n) bad)
  propagateTypeSpec MinusW (n :+- Hole) (Interval lo hi) bad =
    TypeSpec (Interval ((+ n) <$> lo) ((+ n) <$> hi)) (map (+ n) bad)
  propagateTypeSpec LessOrEqW (Hole :-+ n) boolspec bad = undefined
  propagateTypeSpec LessOrEqW (n :+- Hole) boolspec bad = undefined
  propagateTypeSpec GreaterOrEqW (Hole :-+ n) boolspec bad = undefined
  propagateTypeSpec GreaterOrEqW (n :+- Hole) boolspec bad = undefined

  propagateMemberSpec PlusW (Hole :-+ n) xs = MemberSpec (fmap (+ n) xs)
  propagateMemberSpec PlusW (n :+- Hole) xs = MemberSpec (fmap (+ n) xs)
  propagateMemberSpec MinusW (Hole :-+ n) xs = MemberSpec (fmap (minus n) xs)
  propagateMemberSpec MinusW (n :+- Hole) xs = MemberSpec (fmap (+ n) xs)
  propagateMemberSpec LessOrEqW (Hole :-+ n) boolList = undefined
  propagateMemberSpec LessOrEqW (n :+- Hole) boolList = undefined
  propagateMemberSpec GreaterOrEqW (Hole :-+ n) boolList = undefined
  propagateMemberSpec GreaterOrEqW (n :+- Hole) boolList = undefined


geqSpec :: Integer -> Spec Integer
geqSpec n = typeSpec (Interval (Just n) Nothing)

leqSpec :: Integer -> Spec Integer
leqSpec n = typeSpec (Interval Nothing (Just n))



minus :: Integer -> Integer -> Integer
minus n x = x - n  

(<=.) :: Term Integer -> Term Integer -> Term Bool
(<=.) = undefined

data Range = Interval (Maybe Integer) (Maybe Integer) deriving (Eq, Show)

instance Semigroup Range where
  Interval ml mu <> Interval ml' mu' =
    Interval
      (unionWithMaybe max ml ml')
      (unionWithMaybe min mu mu')

instance Monoid Range where
  mempty = Interval Nothing Nothing

instance HasSpec Integer where
  type TypeSpec Integer = Range

  emptySpec = Interval Nothing Nothing

  combineSpec s s' = guardTypeSpec (s <> s')

  guardTypeSpec r@(Interval (Just n) (Just m))
    | n > m = ErrorSpec (pure ("lower bound greater than upper bound\n" ++ show r))
    | otherwise = typeSpec r

  genFromTypeSpec (Interval ml mu) = do
    n <- sizeT
    chooseT =<< constrainInterval (ml <|> lowerBound) (mu <|> upperBound) (fromIntegral n)

  conformsTo i (Interval ml mu) = maybe True (<= i) ml && maybe True (i <=) mu

  toPreds v (Interval ml mu) =
    Foldable.fold $
      [Assert $ Lit l <=. v | l <- maybeToList ml]
        ++ [Assert $ v <=. Lit u | u <- maybeToList mu]

-- ========== Bool example ==================

data BoolSym (dom :: [Type]) rng where
  NotW :: BoolSym '[Bool] Bool

instance Syntax BoolSym where
  name NotW = "not_"
  inFix _ = False

instance Semantics BoolSym where
  semantics NotW = not

instance Logic BoolSym where
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate NotW (Unary Hole) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App NotW (v' :> Nil)) (v :-> ps)
  propagate NotW (Unary Hole) spec =
    caseBoolSpec spec (equalSpec . not)

instance HasSpec Bool where
  type TypeSpec Bool = Set Bool

  emptySpec = Set.fromList [True, False]

  combineSpec s s' = typeSpec (Set.union s s')

  genFromTypeSpec set
    | Set.null set = fatalError "genFromTypeSpec @Set where the typeSpec is empty"
    | otherwise = oneofT (map pure (Set.toList set))

  conformsTo i set = Set.member i set

  toPreds v set = case Set.toList set of
    [] -> FalsePred (pure "toPreds @Set where the typeSpec is empty")
    (x : xs) -> ElemPred True v (x :| xs)


-- ========== Set example =======================

data SetSym (dom :: [Type]) rng where
  MemberW :: Ord a => SetSym [a, Set a] Bool
  SizeW :: SetSym '[Set a] Integer
  SubsetW :: Ord a => SetSym [Set a, Set a] Bool

deriving instance Eq (SetSym dom rng)

instance Show (SetSym dom rng) where show = name

instance Syntax SetSym where
  name MemberW = "member_"
  name SizeW = "size_"
  name SubsetW = "subset_"
  inFix _ = False


instance Semantics SetSym where
  semantics MemberW = Set.member
  semantics SizeW = setSize
  semantics SubsetW = Set.isSubsetOf

instance Logic SetSym where
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec es) = ErrorSpec es
  propagate f ctx (SuspendedSpec v ps) = constrained $ \v' -> Let (App f (fromListCtx ctx v')) (v :-> ps)
  {-
  propagate SizeW (Unary Hole) spec = undefined
  propagate MemberW (Hole :-+ ss) spec = undefined
  propagate MemberW (x :+- Hole) spec = undefined
  propagate SubsetW (Hole :-+ ss) spec = undefined
  propagate SubsetW (x :+- Hole) spec = undefined
  -}


setSize :: Set a -> Integer
setSize = toInteger . Set.size

size_ :: (HasSpec s,Ord s) => Term (Set s) -> Term Integer
size_ s = App SizeW (s :> Nil)

subset_ :: (HasSpec s,Ord s) => Term (Set s) -> Term (Set s) ->Term Bool
subset_ s1 s2 = App SubsetW (s1 :> s2 :> Nil)

-- Helpers for the `HasSpec (Set s)` instance

instance Ord s => Forallable (Set s) s where
  fromForAllSpec e = typeSpec $ SetSpec mempty e TrueSpec
  forAllToList = Set.toList

data SetSpec a = SetSpec (Set a) (Spec a) (Spec Integer)
  deriving Show

guardSetSpec :: (HasSpec a, Ord a) => SetSpec a -> Spec (Set a)
guardSetSpec (SetSpec must elemS ((<> geqSpec 0) -> size))
  | Just u <- knownUpperBound size
  , u < 0 =
      ErrorSpec (("guardSetSpec: negative size " ++ show u) :| [])
  | not (all (`conformsToSpec` elemS) must) =
      ErrorSpec (("Some 'must' items do not conform to 'element' spec: " ++ show elemS) :| [])
  | isErrorLike size = ErrorSpec ("guardSetSpec: error in size" :| [])
  | isErrorLike (geqSpec (setSize must) <> size) =
      ErrorSpec $
        ("Must set size " ++ show (setSize must) ++ ", is inconsistent with SetSpec size" ++ show size) :| []
  | otherwise = typeSpec (SetSpec must elemS size)

knownUpperBound :: Spec Integer -> Maybe Integer
knownUpperBound TrueSpec = Nothing
knownUpperBound (MemberSpec as) = Just $ maximum as
knownUpperBound ErrorSpec {} = Nothing
knownUpperBound SuspendedSpec {} = Nothing
knownUpperBound (TypeSpec (Interval lo hi) cant) = upper (lo <|> lowerBound) (hi <|> upperBound)
  where
    upper _ Nothing = Nothing
    upper Nothing (Just b) = listToMaybe $ [b, b - 1 ..] \\ cant
    upper (Just a) (Just b)
      | a == b = a <$ guard (a `notElem` cant)
      | otherwise = listToMaybe $ [b, b - 1 .. a] \\ cant

instance (Ord a, HasSpec a) => Semigroup (SetSpec a) where
  SetSpec must es size <> SetSpec must' es' size' =
    SetSpec (must <> must') (es <> es') (size <> size')

instance (Ord a, HasSpec a) => Monoid (SetSpec a) where
  mempty = SetSpec mempty mempty TrueSpec


-- ===== The Set HasSpec instance

instance (Forallable (Set a) a, Ord a,HasSpec a) => HasSpec (Set a) where
  type TypeSpec (Set a) = SetSpec a

  emptySpec = SetSpec Set.empty TrueSpec TrueSpec

  combineSpec x y = guardSetSpec (x <> y)

  conformsTo s (SetSpec must es size) =
    and
      [ setSize s `conformsToSpec` size
      , must `Set.isSubsetOf` s
      , all (`conformsToSpec` es) s
      ]

  toPreds s (SetSpec m es size) =
    Foldable.fold $
      -- Don't include this if the must set is empty
      [ Assert $ subset_ (Lit m) s | not $ Set.null m ]
        ++ [ forAll s (\e -> satisfies e es)
           , satisfies (size_ s) size
           ]

  genFromTypeSpec (SetSpec must e _)
    | any (not . (`conformsToSpec` e)) must =
        genErrorNE
          ( NE.fromList
              [ "Failed to generate set"
              , "Some element in the must set does not conform to the elem specification"
              , "Unconforming elements from the must set:"
              , unlines (map (\x -> "  " ++ show x) (filter (not . (`conformsToSpec` e)) (Set.toList must)))
              , "Element Specifcation"
              , "  " ++ show e
              ]
          )
  -- Special case when elemS is a MemberSpec.
  -- Just union 'must' with enough elements of 'xs' to meet  'szSpec'
  genFromTypeSpec (SetSpec must elemS@(MemberSpec xs) szSpec) = do
    let szSpec' = szSpec <> geqSpec (setSize must)  -- <> maxSpec (cardinality elemS)
    choices <- pureGen $ shuffle (NE.toList xs \\ Set.toList must)
    size <- fromInteger <$> genFromSpecT szSpec'
    let additions = Set.fromList $ take (size - Set.size must) choices
    pure (Set.union must additions)
  genFromTypeSpec (SetSpec must elemS szSpec) = do
    let szSpec' = szSpec <> geqSpec (setSize must) -- <> maxSpec (cardinality elemS)
    count <-
      explain "Choose a size for the Set to be generated" $
        genFromSpecT szSpec'
    let targetSize = count - setSize must
    explainNE
      ( NE.fromList
          [ "Choose size count = " ++ show count
          , "szSpec' = " ++ show szSpec'
          , "Picking items not in must = " ++ show (Set.toList must)
          , "that also meet the element test: "
          , "  " ++ show elemS
          ]
      ) $ go 100 targetSize must
       
    where
      go _ n s | n <= 0 = pure s
      go tries n s = do
        e <-
          explainNE
            ( NE.fromList
                [ "Generate set member at type " ++ showType @a
                , "  number of items starting with  = " ++ show (Set.size must)
                , "  number of items left to pick   = " ++ show n
                , "  number of items already picked = " ++ show (Set.size s)
                ]
            )
            $ withMode Strict
            $ suchThatWithTryT tries (genFromSpecT elemS) (`Set.notMember` s)

        go tries (n - 1) (Set.insert e s)

-- ==========

data PairSym (dom :: [Type]) rng where
  FstW :: PairSym '[(a, b)] a
  SndW :: PairSym '[(a, b)] b
  PairW :: PairSym '[a, b] (a, b)

instance Syntax PairSym where
  name FstW = "fst_"
  name SndW = "snd_"
  name PairW = "pair_"
  inFix _ = False

instance Semantics PairSym where
  semantics FstW = fst
  semantics SndW = snd
  semantics PairW = (,)

-- ==========

data EitherSym (dom :: [Type]) rng where
  LeftW :: EitherSym '[a] (Either a b)
  RightW :: EitherSym '[b] (Either a b)

instance Syntax EitherSym where
  name LeftW = "left_"
  name RightW = "right_"
  inFix _ = False

instance Semantics EitherSym where
  semantics LeftW = Left
  semantics RightW = Right


-- ========== List example ===================

data ListSym (dom :: [Type]) rng where
  ElemW :: Eq a => ListSym [a, [a]] Bool
  LengthW :: ListSym '[[a]] Integer

instance Syntax ListSym where
  name ElemW = "elem_"
  name LengthW = "length_"
  inFix _ = False

instance Semantics ListSym where
  semantics ElemW = elem
  semantics LengthW = toInteger . length



-- =======================================
-- Tools for building Spec

forAll :: (Forallable t a, HasSpec t, HasSpec a) => Term t -> (Term a -> Pred) -> Pred
forAll tm = mkForAll tm . bind

mkForAll :: ( Forallable t a, HasSpec t, HasSpec a) =>
  Term t -> Binder a -> Pred
mkForAll (Lit (forAllToList -> [])) _ = TruePred
mkForAll _ (_ :-> TruePred) = TruePred
mkForAll tm binder = ForAll tm binder

constrained :: forall a. HasSpec a => (Term a -> Pred) -> Spec a
constrained body =
  let x :-> p = bind body
   in SuspendedSpec x p

equalSpec :: a -> Spec a
equalSpec = MemberSpec . pure

notMemberSpec :: forall a f. (HasSpec a, Foldable f) => f a -> Spec a
notMemberSpec x = TypeSpec (emptySpec @a) (Foldable.toList x)

caseBoolSpec :: HasSpec a => Spec Bool -> (Bool -> Spec a) -> Spec a
caseBoolSpec spec cont = case possibleValues spec of
  [] -> ErrorSpec (NE.fromList ["No possible values in caseBoolSpec"])
  [b] -> cont b
  _ -> mempty
  where
    possibleValues s = filter (flip conformsToSpec s) [True, False]

bind :: HasSpec a => (Term a -> Pred) -> Binder a
bind bodyf = newv :-> bodyPred
  where
    bodyPred = {- toPred -} body
    newv = Var (nextVar bodyPred) "v"
    body = bodyf (V newv)

    nextVar q = 1 + bound q

    boundBinder :: Binder a -> Int
    boundBinder (x :-> p) = max (nameOf x) (bound p)

    bound (ElemPred _ _ _) = -1
    bound (Subst x _ p) = max (nameOf x) (bound p)
    bound (And ps) = maximum $ (-1) : map bound ps -- (-1) as the default to get 0 as `nextVar p`
    bound (Let _ b) = boundBinder b
    bound (ForAll _ b) = boundBinder b
    bound (Case _ ba bb) = max (boundBinder ba) (boundBinder bb)
    bound Assert {} = -1
    bound TruePred = -1
    bound FalsePred {} = -1


-- ======================================
-- Operations on Range

class MaybeBounded a where
  lowerBound :: Maybe a
  upperBound :: Maybe a

instance MaybeBounded Integer where
  lowerBound = Nothing
  upperBound = Nothing

constrainInterval ::
  (MonadGenError m, Ord a, Num a, Show a) => Maybe a -> Maybe a -> Integer -> m (a, a)
constrainInterval ml mu r =
  case (ml, mu) of
    (Nothing, Nothing) -> pure (-r', r')
    (Just l, Nothing)
      | l < 0 -> pure (max l (negate r'), r')
      | otherwise -> pure (l, l + 2 * r')
    (Nothing, Just u)
      | u > 0 -> pure (negate r', min u r')
      | otherwise -> pure (u - r' - r', u)
    (Just l, Just u)
      | l > u -> genError ("bad interval: " ++ show l ++ " " ++ show u)
      | u < 0 -> pure (safeSub l (safeSub l u r') r', u)
      | l >= 0 -> pure (l, safeAdd u (safeAdd u l r') r')
      -- TODO: this is a bit suspect if the bounds are lopsided
      | otherwise -> pure (max l (-r'), min u r')
  where
    r' = abs $ fromInteger r
    safeSub l a b
      | a - b > a = l
      | otherwise = max l (a - b)
    safeAdd u a b
      | a + b < a = u
      | otherwise = min u (a + b)

-- =========================================================================
-- Conformance and Monoid Spec

showType :: forall t. Typeable t => String
showType = show (typeRep (Proxy @t))

-- | Add the explanations, if it's an ErrorSpec, else drop them
addToErrorSpec :: NE.NonEmpty String -> Spec a -> Spec a
addToErrorSpec es (ErrorSpec es') = ErrorSpec (es <> es')
addToErrorSpec _ s = s

-- | return a MemberSpec or ans ErrorSpec depending on if 'xs' the null list or not
memberSpecList :: [a] -> NE.NonEmpty String -> Spec a
memberSpecList xs messages =
  case NE.nonEmpty xs of
    Nothing -> ErrorSpec messages
    Just ys -> MemberSpec ys

satisfies :: forall a. HasSpec a => Term a -> Spec a -> Pred
satisfies _ TrueSpec = TruePred
satisfies e (MemberSpec nonempty) = ElemPred True e nonempty
satisfies t (SuspendedSpec x p) = Subst x t p
satisfies e (TypeSpec s cant) = case cant of
  [] -> toPreds e s
  (c : cs) -> ElemPred False e (c :| cs) <> toPreds e s
satisfies _ (ErrorSpec e) = FalsePred e

-- ====================

instance Semigroup Pred where
  FalsePred xs <> FalsePred ys = FalsePred (xs <> ys)
  FalsePred es <> _ = FalsePred es
  _ <> FalsePred es = FalsePred es
  TruePred <> p = p
  p <> TruePred = p
  p <> p' = And (unpackPred p ++ unpackPred p')
    where
      unpackPred (And ps) = ps
      unpackPred x = [x]

instance Monoid Pred where
  mempty = TruePred

class Forallable t e | t -> e where
  fromForAllSpec :: (HasSpec t, HasSpec e) => Spec e -> Spec t
  forAllToList :: t -> [e]
  

-- =====================

instance HasSpec a => Semigroup (Spec a) where
  TrueSpec <> s = s
  s <> TrueSpec = s
  ErrorSpec e <> ErrorSpec e' =
    ErrorSpec
      ( e
          <> pure ("------ spec <> spec ------ @" ++ showType @a)
          <> e'
      )
  ErrorSpec e <> _ = ErrorSpec e
  _ <> ErrorSpec e = ErrorSpec e
  MemberSpec as <> MemberSpec as' =
    addToErrorSpec
      ( NE.fromList
          ["Intersecting: ", "  MemberSpec " ++ show (NE.toList as), "  MemberSpec " ++ show (NE.toList as')]
      )
      ( memberSpecList
          (nub $ intersect (NE.toList as) (NE.toList as'))
          (pure "Empty intersection")
      )
  ms@(MemberSpec as) <> ts@TypeSpec {} =
    memberSpecList
      (nub $ NE.filter (`conformsToSpec` ts) as)
      ( NE.fromList
          [ "The two " ++ showType @a ++ " Specs are inconsistent."
          , "  " ++ show ms
          , "  " ++ show ts
          ]
      )
  TypeSpec s cant <> MemberSpec as = MemberSpec as <> TypeSpec s cant
  SuspendedSpec v p <> SuspendedSpec v' p' = SuspendedSpec v (p <> rename v' v p')
  SuspendedSpec v ps <> s = SuspendedSpec v (ps <> satisfies (V v) s)
  s <> SuspendedSpec v ps = SuspendedSpec v (ps <> satisfies (V v) s)
  TypeSpec s cant <> TypeSpec s' cant' = case combineSpec s s' of
    -- NOTE: This might look like an unnecessary case, but doing
    -- it like this avoids looping.
    TypeSpec s'' cant'' -> TypeSpec s'' (cant <> cant' <> cant'')
    s'' -> s'' <> notMemberSpec (cant <> cant')

instance HasSpec a => Monoid (Spec a) where
  mempty = TrueSpec

runTermE :: forall a. Env -> Term a -> Either (NE.NonEmpty String) a
runTermE env = \case
  Lit a -> Right a
  V v -> case lookupEnv env v of
    Just a -> Right a
    Nothing -> Left (pure ("Couldn't find " ++ show v ++ " in " ++ show env))
  App f (ts :: List Term dom) -> do
    vs <- mapMList (fmap Identity . runTermE env) ts
    pure $ uncurryList_ runIdentity (semantics f) vs

conformsToSpec :: forall a. HasSpec a => a -> Spec a -> Bool
conformsToSpec _ TrueSpec = True
conformsToSpec a (MemberSpec as) = elem a as
conformsToSpec a spec@(TypeSpec s cant) = notElem a cant && conformsTo a s
conformsToSpec a (SuspendedSpec v ps) = case checkPredE (singletonEnv v a) (pure "checkPredE") ps of
  Nothing -> True
  Just _ -> False
conformsToSpec _ (ErrorSpec es) = False

checkPredE :: Env -> NonEmpty String -> Pred -> Maybe (NonEmpty String)
checkPredE env msgs = \case
  p@(ElemPred bool t xs) ->
    case runTermE env t of
      Left message -> Just (msgs <> message)
      Right v -> case (elem v xs, bool) of
        (True, True) -> Nothing
        (True, False) -> Just ("notElemPred reduces to True" :| [show p])
        (False, True) -> Just ("elemPred reduces to False" :| [show p])
        (False, False) -> Nothing
  Subst x t p -> checkPredE env msgs $ substitutePred x t p
  Assert t -> case runTermE env t of
    Right True -> Nothing
    Right False ->
      Just
        (msgs <> pure ("Assert " ++ show t ++ " returns False") <> pure ("\nenv=\n" ++ show (pretty env)))
    Left es -> Just (msgs <> es)
  ForAll t (x :-> p) -> case runTermE env t of
      Left es -> Just $ (msgs <> NE.fromList ["checkPredE: ForAll fails to run."] <> es)
      Right set ->
        let answers =
              catMaybes
                [ checkPredE env' (pure "Some items in ForAll fail") p
                | v <- forAllToList set
                , let env' = extendEnv x v env
                ]
         in case answers of
              [] -> Nothing
              (y : ys) -> Just (NE.nub (sconcat (y NE.:| ys)))
  Case t a b  -> case runTermE env t of
      Right v -> runCaseOn v a b (\x val ps -> checkPredE (extendEnv x val env) msgs ps)
      Left es -> Just (msgs <> pure "checkPredE: Case fails" <> es)      
  Let t (x :-> p) -> case runTermE env t of
      Right val -> checkPredE (extendEnv x val env) msgs p
      Left es -> Just (msgs <> pure "checkPredE: Let fails" <> es)
  TruePred -> Nothing
  FalsePred es -> Just (msgs <> pure "checkPredE: FalsePred" <> es)
  And ps ->
    case catMaybes (fmap (checkPredE env (pure "Some items in And  fail")) ps) of
      [] -> Nothing
      (x : xs) -> Just (msgs <> NE.nub (sconcat (x NE.:| xs)))

runCaseOn ::
  Either a b -> Binder a -> Binder b -> (forall x. HasSpec x => Var x -> x -> Pred -> r) -> r
runCaseOn (Left a) (x :-> xps) (_ :-> _) f = f x a xps
runCaseOn (Right b) (_ :-> _) (y :-> yps) f = f y b yps 
      

-- ==================================================
-- Renaming

-- Name

data Name where
  Name :: HasSpec a => Var a -> Name

deriving instance Show Name

instance Eq Name where
  Name v == Name v' = isJust $ eqVar v v'

-- Instances

instance Pretty (Var a) where
  pretty = fromString . show

instance Pretty Name where
  pretty (Name v) = pretty v

instance Ord Name where
  compare (Name v) (Name v') = compare (nameOf v, typeOf v) (nameOf v', typeOf v')

instance Rename Name where
  rename v v' (Name v'') = Name $ rename v v' v''

instance Rename (Term a) where
  rename v v'
    | v == v' = id
    | otherwise = \case
        Lit l -> Lit l
        V v'' -> V (rename v v' v'')
        App f a -> App f (rename v v' a)

instance Rename Pred where
  rename v v'
    | v == v' = id
    | otherwise = \case
        ElemPred bool t xs -> ElemPred bool (rename v v' t) xs
        -- nSubst x t p -> rename v v' $ substitutePred x t p
        And ps -> And (rename v v' ps)
        -- Let t b -> Let (rename v v' t) (rename v v' b)
        Assert t -> Assert (rename v v' t)
        ForAll set b -> ForAll (rename v v' set) (rename v v' b)
        Case t a b -> Case (rename v v' t) (rename v v' a) (rename v v' b)
        TruePred -> TruePred
        FalsePred es -> FalsePred es

instance Rename (Binder a) where
  rename v v' (va :-> psa) = va' :-> rename v v' psa'
    where
      (va', psa') = freshen va psa (Set.fromList [nameOf v, nameOf v'] <> Set.delete (nameOf va) (freeVarNames psa))

-- ============================================================
-- 1) Free variables and variable names
-- ============================================================

freeVarNames :: forall t. HasVariables t => t -> Set Int
freeVarNames = Set.mapMonotonic (\(Name v) -> nameOf v) . freeVarSet

newtype FreeVars = FreeVars {unFreeVars :: Map Name Int}
  deriving (Show)

restrictedTo :: FreeVars -> Set Name -> FreeVars
restrictedTo (FreeVars m) nms = FreeVars $ Map.restrictKeys m nms

memberOf :: Name -> FreeVars -> Bool
memberOf n (FreeVars m) = Map.member n m

count :: Name -> FreeVars -> Int
count n (FreeVars m) = fromMaybe 0 $ Map.lookup n m

instance Semigroup FreeVars where
  FreeVars fv <> FreeVars fv' = FreeVars $ Map.unionWith (+) fv fv'

instance Monoid FreeVars where
  mempty = FreeVars mempty

freeVar :: Name -> FreeVars
freeVar n = singleton n 1

singleton :: Name -> Int -> FreeVars
singleton n k = FreeVars $ Map.singleton n k

without :: Foldable t => FreeVars -> t Name -> FreeVars
without (FreeVars m) remove = FreeVars $ foldr Map.delete m (Foldable.toList remove)

class HasVariables a where
  freeVars :: a -> FreeVars
  freeVarSet :: a -> Set Name
  freeVarSet = Map.keysSet . unFreeVars . freeVars
  countOf :: Name -> a -> Int
  countOf n = count n . freeVars
  appearsIn :: Name -> a -> Bool
  appearsIn n = (> 0) . count n . freeVars

instance (HasVariables a, HasVariables b) => HasVariables (a, b) where
  freeVars (a, b) = freeVars a <> freeVars b
  freeVarSet (a, b) = freeVarSet a <> freeVarSet b
  countOf n (a, b) = countOf n a + countOf n b
  appearsIn n (a, b) = appearsIn n a || appearsIn n b

instance HasVariables (List Term as) where
  freeVars Nil = mempty
  freeVars (x :> xs) = freeVars x <> freeVars xs
  freeVarSet Nil = mempty
  freeVarSet (x :> xs) = freeVarSet x <> freeVarSet xs
  countOf _ Nil = 0
  countOf n (x :> xs) = countOf n x + countOf n xs
  appearsIn _ Nil = False
  appearsIn n (x :> xs) = appearsIn n x || appearsIn n xs

instance HasVariables Name where
  freeVars = freeVar
  freeVarSet = Set.singleton
  countOf n n'
    | n == n' = 1
    | otherwise = 0
  appearsIn n n' = n == n'

instance HasVariables (Term a) where
  freeVars = \case
    Lit {} -> mempty
    V x -> freeVar (Name x)
    App _ ts -> freeVars ts
  freeVarSet = \case
    Lit {} -> mempty
    V x -> freeVarSet (Name x)
    App _ ts -> freeVarSet ts
  countOf n = \case
    Lit {} -> 0
    V x -> countOf n (Name x)
    App _ ts -> countOf n ts
  appearsIn n = \case
    Lit {} -> False
    V x -> appearsIn n (Name x)
    App _ ts -> appearsIn n ts

instance HasVariables Pred where
  freeVars = \case
    ElemPred _ t _ -> freeVars t
    -- GenHint _ t -> freeVars t
    Subst x t p -> freeVars t <> freeVars p `without` [Name x]
    And ps -> foldMap freeVars ps
    Let t b -> freeVars t <> freeVars b
    -- Exists _ b -> freeVars b
    Assert t -> freeVars t
    -- Reifies t' t _ -> freeVars t' <> freeVars t
    -- DependsOn x y -> freeVars x <> freeVars y
    ForAll set b -> freeVars set <> freeVars b
    Case t as bs -> freeVars t <> freeVars as <> freeVars bs
    -- When b p -> freeVars b <> freeVars p
    TruePred -> mempty
    FalsePred _ -> mempty
    -- Monitor {} -> mempty
    -- Explain _ p -> freeVars p
  freeVarSet = \case
    ElemPred _ t _ -> freeVarSet t
    -- GenHint _ t -> freeVarSet t
    Subst x t p -> freeVarSet t <> Set.delete (Name x) (freeVarSet p)
    And ps -> foldMap freeVarSet ps
    Let t b -> freeVarSet t <> freeVarSet b
    -- Exists _ b -> freeVarSet b
    Assert t -> freeVarSet t
    -- Reifies t' t _ -> freeVarSet t' <> freeVarSet t
    -- DependsOn x y -> freeVarSet x <> freeVarSet y
    ForAll set b -> freeVarSet set <> freeVarSet b
    Case t a b  -> freeVarSet t <> freeVarSet a <> freeVarSet b
    -- When b p -> freeVarSet b <> freeVarSet p
    -- Explain _ p -> freeVarSet p
    TruePred -> mempty
    FalsePred _ -> mempty
    -- Monitor {} -> mempty
  countOf n = \case
    ElemPred _ t _ -> countOf n t
    -- GenHint _ t -> countOf n t
    Subst x t p
      | n == Name x -> countOf n t
      | otherwise -> countOf n t + countOf n p
    And ps -> sum $ map (countOf n) ps
    Let t b -> countOf n t + countOf n b
    -- Exists _ b -> countOf n b
    Assert t -> countOf n t
    -- Reifies t' t _ -> countOf n t' + countOf n t
    -- DependsOn x y -> countOf n x + countOf n y
    ForAll set b -> countOf n set + countOf n b
    Case t a b -> countOf n t + countOf n a + countOf n b
    -- When b p -> countOf n b + countOf n p
    -- Explain _ p -> countOf n p
    TruePred -> 0
    FalsePred _ -> 0
    -- Monitor {} -> 0
  appearsIn n = \case
    ElemPred _ t _ -> appearsIn n t
    -- GenHint _ t -> appearsIn n t
    Subst x t p
      | n == Name x -> appearsIn n t
      | otherwise -> appearsIn n t || appearsIn n p
    And ps -> any (appearsIn n) ps
    Let t b -> appearsIn n t || appearsIn n b
    -- Exists _ b -> appearsIn n b
    Assert t -> appearsIn n t
    -- Reifies t' t _ -> appearsIn n t' || appearsIn n t
    -- DependsOn x y -> appearsIn n x || appearsIn n y
    ForAll set b -> appearsIn n set || appearsIn n b
    Case t a b -> appearsIn n t || appearsIn n a || appearsIn n b
    -- When b p -> appearsIn n b || appearsIn n p
    -- Explain _ p -> appearsIn n p
    TruePred -> False
    FalsePred _ -> False
    -- Monitor {} -> False

instance HasVariables (Binder a) where
  freeVars (x :-> p) = freeVars p `without` [Name x]
  freeVarSet (x :-> p) = Set.delete (Name x) (freeVarSet p)
  countOf n (x :-> p)
    | Name x == n = 0
    | otherwise = countOf n p
  appearsIn n (x :-> p)
    | Name x == n = False
    | otherwise = appearsIn n p

{-
instance HasVariables (f a) => HasVariables (Weighted f a) where
  freeVars = freeVars . thing
  freeVarSet = freeVarSet . thing
  countOf n = countOf n . thing
  appearsIn n = appearsIn n . thing

instance HasVariables (List (Weighted Binder) as) where
  freeVars Nil = mempty
  freeVars (a :> as) = freeVars a <> freeVars as
  freeVarSet Nil = mempty
  freeVarSet (a :> as) = freeVarSet a <> freeVarSet as
  countOf _ Nil = 0
  countOf n (x :> xs) = countOf n x + countOf n xs
  appearsIn _ Nil = False
  appearsIn n (x :> xs) = appearsIn n x || appearsIn n xs
-}

instance {-# OVERLAPPABLE #-} (Foldable t, HasVariables a) => HasVariables (t a) where
  freeVars = foldMap freeVars
  freeVarSet = foldMap freeVarSet
  countOf n = Monoid.getSum . foldMap (Monoid.Sum . countOf n)
  appearsIn n = any (appearsIn n)

instance HasVariables a => HasVariables (Set a) where
  freeVars = foldMap freeVars
  freeVarSet = foldMap freeVarSet
  countOf n = sum . Set.map (countOf n)
  appearsIn n = any (appearsIn n)

-- =========================================================
-- Helpers

fromLits :: List Term as -> Maybe (List Value as)
fromLits = mapMList fromLit

fromLit :: Term a -> Maybe (Value a)
fromLit (Lit l) = pure $ Value l
-- fromLit (To x) = (Value . toSimpleRep . unValue) <$> fromLit x -- MAYBE we don't want to do this?
-- fromLit (From x) = (Value . fromSimpleRep . unValue) <$> fromLit x -- Why not apply unary functions to Lit ?
fromLit _ = Nothing

isLit :: Term a -> Bool
isLit = isJust . fromLit

-- =================================================================
-- 2) Substitutions
-- ============================================================

type Subst = [SubstEntry]

data SubstEntry where
  (:=) :: HasSpec a => Var a -> Term a -> SubstEntry

backwardsSubstitution :: forall a. HasSpec a => Subst -> Term a -> Term a
backwardsSubstitution sub0 t =
  case findMatch sub0 t of
    -- TODO: what about multiple matches??
    Just x -> V x
    Nothing -> case t of
      Lit a -> Lit a
      V x -> V x
      App f ts -> App f (mapListC @HasSpec (backwardsSubstitution sub0) ts)
  where
    findMatch :: Subst -> Term a -> Maybe (Var a)
    findMatch [] _ = Nothing
    findMatch (x := t' : sub1) t1
      | fastInequality t1 t' = findMatch sub1 t1
      | Just (x', t'') <- cast (x, t')
      , t == t'' =
          Just x'
      | otherwise = findMatch sub1 t1

-- | Sound but not complete inequality on terms
fastInequality :: Term a -> Term b -> Bool
fastInequality (V (Var i _)) (V (Var j _)) = i /= j
fastInequality Lit {} Lit {} = False
fastInequality (App _ as) (App _ bs) = go as bs
  where
    go :: List Term as -> List Term bs -> Bool
    go Nil Nil = False
    go (a :> as') (b :> bs') = fastInequality a b || go as' bs'
    go _ _ = True
fastInequality _ _ = True

-- ===================================================================

substituteTerm :: forall a. Subst -> Term a -> Term a
substituteTerm sub = \case
  Lit a -> Lit a
  V x -> substVar sub x
  App f (mapList (substituteTerm sub) -> (ts :: List Term dom)) ->
    case fromLits ts of
      Just vs -> Lit (uncurryList_ unValue (semantics f) vs)
      _ -> App f ts
  where
    substVar :: HasSpec a => Subst -> Var a -> Term a
    substVar [] x = V x
    substVar (y := t : sub1) x
      | Just Refl <- eqVar x y = t
      | otherwise = substVar sub1 x

substituteTerm' :: forall a. Subst -> Term a -> Writer Any (Term a)
substituteTerm' sub = \case
  Lit a -> pure $ Lit a
  V x -> substVar sub x
  App f ts ->
    App f <$> mapMList (substituteTerm' sub) ts
  where
    substVar :: HasSpec a => Subst -> Var a -> Writer Any (Term a)
    substVar [] x = pure $ V x
    substVar (y := t : sub1) x
      | Just Refl <- eqVar x y = t <$ tell (Any True)
      | otherwise = substVar sub1 x

substituteBinder :: HasSpec a => Var a -> Term a -> Binder b -> Binder b
substituteBinder x tm (y :-> p) = y' :-> substitutePred x tm p'
  where
    (y', p') =
      freshen y p (Set.singleton (nameOf x) <> freeVarNames tm <> Set.delete (nameOf y) (freeVarNames p))

substitutePred :: HasSpec a => Var a -> Term a -> Pred -> Pred
substitutePred x tm = \case
  ElemPred bool t xs -> ElemPred bool (substituteTerm [x := tm] t) xs
  -- GenHint h t -> GenHint h (substituteTerm [x := tm] t)
  Subst x' t p -> substitutePred x tm $ substitutePred x' t p
  Assert t -> Assert (substituteTerm [x := tm] t)
  And ps -> Foldable.fold (substitutePred x tm <$> ps)
  -- Exists k b -> Exists (\eval -> k (eval . substituteTerm [x := tm])) (substituteBinder x tm b)
  Let t b -> Let (substituteTerm [x := tm] t) (substituteBinder x tm b)
  ForAll t b -> ForAll (substituteTerm [x := tm] t) (substituteBinder x tm b)
  Case t as bs -> Case (substituteTerm [x := tm] t) (substituteBinder x tm as) (substituteBinder x tm bs)
  -- When b p -> When (substituteTerm [x := tm] b) (substitutePred x tm p)
  -- Reifies t' t f -> Reifies (substituteTerm [x := tm] t') (substituteTerm [x := tm] t) f
  -- DependsOn t t' -> DependsOn (substituteTerm [x := tm] t) (substituteTerm [x := tm] t')
  TruePred -> TruePred
  FalsePred es -> FalsePred es
  -- Monitor m -> Monitor (\eval -> m (eval . substituteTerm [x := tm]))
  -- Explain es p -> Explain es $ substitutePred x tm p


-- =====================================================
-- Substituion under an Env, rather than a single Var
-- It takes Values in the Env, and makes them Literals in the Term.

substTerm :: Env -> Term a -> Term a
substTerm env = \case
  Lit a -> Lit a
  V v
    | Just a <- lookupEnv env v -> Lit a
    | otherwise -> V v
  App f (mapList (substTerm env) -> ts) ->
    case fromLits ts of
      Just vs -> Lit (uncurryList_ unValue (semantics f) vs)
      _ -> App f ts

substBinder :: Env -> Binder a -> Binder a
substBinder env (x :-> p) = x :-> substPred (removeVar x env) p

substPred :: Env -> Pred -> Pred
substPred env = \case
  ElemPred bool t xs -> ElemPred bool (substTerm env t) xs
  -- GenHint h t -> GenHint h (substTerm env t)
  Subst x t p -> substPred env $ substitutePred x t p
  Assert t -> Assert (substTerm env t)
  -- Reifies t' t f -> Reifies (substTerm env t') (substTerm env t) f
  ForAll set b -> ForAll (substTerm env set) (substBinder env b)
  Case t as bs -> Case (substTerm env t)  (substBinder env as)  (substBinder env bs)
  -- When b p -> When (substTerm env b) (substPred env p)
  -- DependsOn x y -> DependsOn (substTerm env x) (substTerm env y)
  TruePred -> TruePred
  FalsePred es -> FalsePred es
  And ps -> Foldable.fold (substPred env <$> ps)
  -- Exists k b -> Exists (\eval -> k $ eval . substTerm env) (substBinder env b)
  Let t b -> Let (substTerm env t) (substBinder env b)
  -- Monitor m -> Monitor m
  -- Explain es p -> Explain es $ substPred env p

unBind :: a -> Binder a -> Pred
unBind a (x :-> p) = substPred (singletonEnv x a) p

-- ====================================================================


-- | Generalize `genFromTypeSpec` from `TypeSpec t` to `Spec t`
--  Generate a value that satisfies the spec. This function can fail if the
-- spec is inconsistent, there is a dependency error, or if the underlying
-- generators are not flexible enough.
genFromSpecT ::
  forall a m. (HasCallStack, HasSpec a, MonadGenError m) => Spec a -> GenT m a
genFromSpecT (simplifySpec -> spec) = case spec of
  MemberSpec as -> explain ("genFromSpecT on spec" ++ show spec) $ pureGen (elements (NE.toList as))
  TrueSpec -> genFromSpecT (typeSpec $ emptySpec @a)
  SuspendedSpec x p
    -- NOTE: If `x` isn't free in `p` we still have to try to generate things
    -- from `p` to make sure `p` is sat and then we can throw it away. A better
    -- approach would be to only do this in the case where we don't know if `p`
    -- is sat. The proper way to implement such a sat check is to remove
    -- sat-but-unnecessary variables in the optimiser.
    | not $ Name x `appearsIn` p -> do
        !_ <- genFromPreds mempty p
        genFromSpecT TrueSpec
    | otherwise -> do
        env <- genFromPreds mempty p
        findEnv env x
  TypeSpec s cant -> do
    mode <- getMode
    explainNE
      ( NE.fromList
          [ "genFromSpecT on (TypeSpec tspec cant) at type " ++ showType @a
          , "tspec = "
          , show s
          , "cant = " ++ show cant
          , "with mode " ++ show mode
          ]
      )
      $
      -- TODO: we could consider giving `cant` as an argument to `genFromTypeSpec` if this
      -- starts giving us trouble.
      genFromTypeSpec s `suchThatT` (`notElem` cant)
  ErrorSpec e -> genErrorNE e    



simplifySpec :: HasSpec a => Spec a -> Spec a
simplifySpec x = x
{-
simplifySpec spec = case regularizeNames spec of
  SuspendedSpec x p ->
    let optP = optimisePred p
     in fromGESpec $
          explain
            ("\nWhile calling simplifySpec on var " ++ show x ++ "\noptP=\n" ++ show optP ++ "\n")
            (computeSpecSimplified x optP)
  MemberSpec xs -> MemberSpec xs
  ErrorSpec es -> ErrorSpec es
  TypeSpec ts cant -> TypeSpec ts cant
  TrueSpec -> TrueSpec
  ExplainSpec es s -> explainSpecOpt es (simplifySpec s)
-}  


-- | Generate a satisfying `Env` for a `p : Pred fn`. The `Env` contains values for
-- all the free variables in `flattenPred p`.
genFromPreds :: forall m. MonadGenError m => Env -> Pred -> GenT m Env
genFromPreds env0 preds = error "Not yet genFromPreds"
{-
-- TODO: remove this once optimisePred does a proper fixpoint computation
genFromPreds env0 (optimisePred . optimisePred -> preds) =
  {- explain1 (show $ "genFromPreds fails\nPreds are:" /> pretty preds) -} do
    -- NOTE: this is just lazy enough that the work of flattening,
    -- computing dependencies, and linearizing is memoized in
    -- properties that use `genFromPreds`.
    plan <- runGE $ prepareLinearization preds
    go env0 plan
  where
    go :: Env -> SolverPlan -> GenT m Env
    go env plan | isEmptyPlan plan = pure env
    go env plan = do
      (env', plan') <-
        explain (show $ "Stepping the plan:" /> vsep [pretty plan, pretty env]) $ stepPlan env plan
      go env' plan'
-}      


------- Stages of simplifying -------------------------------

-- TODO: it might be necessary to run aggressiveInlining again after the let floating etc.
optimisePred :: Pred -> Pred
optimisePred p =
  simplifyPred
    . letSubexpressionElimination
    . letFloating
    . aggressiveInlining
    . simplifyPred
    $ p

aggressiveInlining :: Pred -> Pred
aggressiveInlining pred
  | inlined = aggressiveInlining pInlined
  | otherwise = pred
  where
    (pInlined, Any inlined) = runWriter $ go (freeVars pred) [] pred

    underBinder fvs x p = fvs `without` [Name x] <> singleton (Name x) (countOf (Name x) p)

    underBinderSub sub x =
      [ x' := t
      | x' := t <- sub
      , isNothing $ eqVar x x'
      ]

    -- NOTE: this is safe because we only use the `Subst` when it results in a literal so there
    -- is no risk of variable capture.
    goBinder :: FreeVars -> Subst -> Binder a -> Writer Any (Binder a)
    goBinder fvs sub (x :-> p) = (x :->) <$> go (underBinder fvs x p) (underBinderSub sub x) p

    -- Check that the name `n` is only ever used as the only variable
    -- in the expressions where it appears. This ensures that it doesn't
    -- interact with anything.
    onlyUsedUniquely n p = case p of
      Assert t
        | n `appearsIn` t -> Set.size (freeVarSet t) == 1
        | otherwise -> True
      And ps -> all (onlyUsedUniquely n) ps
      -- TODO: we can (and should) probably add a bunch of cases to this.
      _ -> False

    go fvs sub pred2 = case pred2 of
      ElemPred bool t xs
        | not (isLit t)
        , Lit a <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ ElemPred bool (Lit a) xs
        | otherwise -> pure $ ElemPred bool t xs
      Subst x t p -> go fvs sub (substitutePred x t p)
      {-
      Reifies t' t f
        | not (isLit t)
        , Lit a <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ Reifies t' (Lit a) f
        | otherwise -> pure $ Reifies t' t f -}
      ForAll set b
        | not (isLit set)
        , Lit a <- substituteAndSimplifyTerm sub set -> do
            tell $ Any True
            pure $ foldMap (`unBind` b) (forAllToList a)
        | otherwise -> ForAll set <$> goBinder fvs sub b
      
      Case t as bs
        | not (isLit t)
        , Lit a <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ runCaseOn a as bs $ \x v p -> substPred (singletonEnv x v) p
        | otherwise -> Case t <$> (goBinder fvs sub as) <*> (goBinder fvs sub bs)

      {- When b tp
        | not (isLit b)
        , Lit a <- substituteAndSimplifyTerm sub b -> do
            tell $ Any True
            pure $ if a then tp else TruePred
        | otherwise -> whenTrue b <$> go fvs sub tp
      -}
      Let t (x :-> p)
        | all (\n -> count n fvs <= 1) (freeVarSet t) -> do
            tell $ Any True
            pure $ substitutePred x t p
        | onlyUsedUniquely (Name x) p -> do
            tell $ Any True
            pure $ substitutePred x t p
        | not $ Name x `appearsIn` p -> do
            tell $ Any True
            pure p
        | not (isLit t)
        , Lit a <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ unBind a (x :-> p)
        | otherwise -> Let t . (x :->) <$> go (underBinder fvs x p) (x := t : sub) p
      -- Exists k b -> Exists k <$> goBinder fvs sub b
      And ps -> Foldable.fold <$> mapM (go fvs sub) ps
      Assert t
        | not (isLit t)
        , Lit b <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ toPred b
        | otherwise -> pure pred2
      {- -- If the term turns into a literal, there is no more generation to do here
      -- so we can ignore the `GenHint`
      GenHint _ t
        | not (isLit t)
        , Lit {} <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure TruePred
        | otherwise -> pure pred2
      DependsOn t t'
        | not (isLit t)
        , Lit {} <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ TruePred
        | not (isLit t')
        , Lit {} <- substituteAndSimplifyTerm sub t' -> do
            tell $ Any True
            pure $ TruePred
        | otherwise -> pure pred2
      -}
      TruePred -> pure pred2
      FalsePred {} -> pure pred2
      -- Monitor {} -> pure pred2
      -- Explain es p -> Explain es <$> go fvs sub p

-- | Apply a substitution and simplify the resulting term if the
-- substitution changed the term.
substituteAndSimplifyTerm :: Subst -> Term a -> Term a
substituteAndSimplifyTerm sub t =
  case runWriter $ substituteTerm' sub t of
    (t', Any b)
      | b -> simplifyTerm t'
      | otherwise -> t'

-- | Simplify a Term, if the Term is an 'App', apply the rewrite rules
--   chosen by the (Logic sym t bs a) instance attached
--   to the function witness 'f'
simplifyTerm :: forall a. Term a -> Term a
simplifyTerm = \case
  V v -> V v
  Lit l -> Lit l
  App (f :: t bs a) (mapList simplifyTerm -> ts)
    | Just vs <- fromLits ts -> Lit $ uncurryList_ unValue (semantics f) vs
    -- | Just t <- rewriteRules f ts (Evidence @(AppRequires t bs a)) -> simplifyTerm t
    -- TODO This might matter
    | otherwise -> App f ts

simplifyPred :: Pred -> Pred
simplifyPred = \case
  -- If the term simplifies away to a literal, that means there is no
  -- more generation to do so we can get rid of `GenHint`
  {- GenHint h t -> case simplifyTerm t of
    Lit {} -> TruePred
    t' -> GenHint h t'
  -}
  p@(ElemPred bool t xs) -> case simplifyTerm t of
    Lit x -> case (elem x xs, bool) of
      (True, True) -> TruePred
      (True, False) -> FalsePred ("notElemPred reduces to True" :| [show p])
      (False, True) -> FalsePred ("elemPred reduces to False" :| [show p])
      (False, False) -> TruePred
    t' -> ElemPred bool t' xs
  Subst x t p -> simplifyPred $ substitutePred x t p
  Assert t -> Assert $ simplifyTerm t
  {- Reifies t' t f -> case simplifyTerm t of
    Lit a ->
      -- Assert $ simplifyTerm t' ==. Lit (f a)
      ElemPred True (simplifyTerm t') (pure (f a))
    t'' -> Reifies (simplifyTerm t') t'' f
  -}
  ForAll (ts :: Term t) (b :: Binder a) -> case simplifyTerm ts of
    Lit as -> foldMap (`unBind` b) (forAllToList as)
    -- (App (extractW (UnionW @t) -> Just Refl) xs) -> error "MADE IT"
    {- Has to wait until we have HasSpec(Set a) instance
    UnionPat (xs :: Term (Set a)) ys ->
       let b' = simplifyBinder b
       in mkForAll xs b' <> mkForAll ys b' -}
    set' -> case simplifyBinder b of
      _ :-> TruePred -> TruePred
      b' -> ForAll set' b'
  -- DependsOn _ Lit {} -> TruePred
  -- DependsOn Lit {} _ -> TruePred
  -- DependsOn x y -> DependsOn x y
  -- Here is where we need the SumSpec instance
  Case t as@(_ :-> _) bs@(_ :-> _) -> mkCase (simplifyTerm t) (simplifyBinder as) (simplifyBinder bs)
  -- When b p -> whenTrue (simplifyTerm b) (simplifyPred p)
  TruePred -> TruePred
  FalsePred es -> FalsePred es
  And ps -> Foldable.fold (simplifyPreds ps)
  Let t b -> case simplifyTerm t of
    t'@App {} -> Let t' (simplifyBinder b)
    -- Variable or literal
    t' | x :-> p <- b -> simplifyPred $ substitutePred x t' p
  {- Exists k b -> case simplifyBinder b of
    _ :-> TruePred -> TruePred
    -- This is to get rid of exisentials like:
    -- `constrained $ \ x -> exists $ \ y -> [x ==. y, y + 2 <. 10]`
    x :-> p | Just t <- pinnedBy x p -> simplifyPred $ substitutePred x t p
    b' -> Exists k b'
  Monitor {} -> TruePred
  -- TODO: This is a bit questionable. On the one hand we could get rid of `Explain` here
  -- and just return `simplifyPred p` but doing so risks missing explanations when things
  -- do go wrong.
  Explain es p -> explanation es $ simplifyPred p
  -}

mkCase ::
  HasSpec (Either a b) => Term (Either a b) -> Binder a -> Binder b -> Pred
mkCase tm as bs
  -- TODO: all equal maybe?
  | isTrueBinder as && isTrueBinder bs = TruePred
  | isFalseBinder as && isFalseBinder bs = FalsePred (pure "mkCase on all False")
  | Lit a <- tm = runCaseOn a as bs (\x val p -> substPred (singletonEnv x val) p)
  | otherwise = Case tm as bs
  where
    isTrueBinder (_ :-> TruePred) = True
    isTrueBinder (_ :-> _) = False

    isFalseBinder (_ :-> FalsePred {}) = True
    isFalseBinder (_ :-> _) = False


simplifyPreds :: [Pred] -> [Pred]
simplifyPreds = go [] . map simplifyPred
  where
    go acc [] = reverse acc
    go _ (FalsePred err : _) = [FalsePred err]
    go acc (TruePred : ps) = go acc ps
    go acc (p : ps) = go (p : acc) ps

simplifyBinder :: Binder a -> Binder a
simplifyBinder (x :-> p) = x :-> simplifyPred p

toPred True = TruePred
toPred False = FalsePred (pure "toPred False")



-- TODO: this can probably be cleaned up and generalized along with generalizing
-- to make sure we float lets in some missing cases.
letFloating :: Pred -> Pred
letFloating = Foldable.fold . go []
  where
    goBlock ctx ps = goBlock' (freeVarNames ctx <> freeVarNames ps) ctx ps

    goBlock' _ ctx [] = ctx
    goBlock' fvs ctx (Let t (x :-> p) : ps) =
      -- We can do `goBlock'` here because we've already done let floating
      -- on the inner `p`
      [Let t (x' :-> Foldable.fold (goBlock' (Set.insert (nameOf x') fvs) ctx (p' : ps)))]
      where
        (x', p') = freshen x p fvs
    goBlock' fvs ctx (And ps : ps') = goBlock' fvs ctx (ps ++ ps')
    goBlock' fvs ctx (p : ps) = goBlock' fvs (p : ctx) ps

    goExists ::
      HasSpec a =>
      [Pred] ->
      (Binder a -> Pred) ->
      Var a ->
      Pred ->
      [Pred]
    goExists ctx ex x (Let t (y :-> p))
      | not $ Name x `appearsIn` t =
          let (y', p') = freshen y p (Set.insert (nameOf x) $ freeVarNames p <> freeVarNames t)
           in go ctx (Let t (y' :-> ex (x :-> p')))
    goExists ctx ex x p = ex (x :-> p) : ctx

    
    {-
    pushExplain es (Let t (x :-> p)) = Let t (x :-> pushExplain es p)
    pushExplain es (And ps) = And (pushExplain es <$> ps)
    pushExplain es (Exists k (x :-> p)) =
      Exists (explainSemantics k) (x :-> pushExplain es p)
      where
        -- TODO: Unfortunately this is necessary on ghc 8.10.7
        explainSemantics ::
          forall a.
          ((forall b. Term b -> b) -> GE a) ->
          (forall b. Term b -> b) ->
          GE a
        explainSemantics k2 env = explainNE es $ k2 env
    -- TODO: possibly one wants to have a `Term` level explanation in case
    -- the `b` propagates to ErrorSpec for some reason?
    -- pushExplain es (When b p) = When b (pushExplain es p)
    pushExplain es p = explanation es p
    -}

    go ctx = \case
      ElemPred bool t xs -> ElemPred bool t xs : ctx
      And ps0 -> goBlock ctx (map letFloating ps0)
      Let t (x :-> p) -> goBlock ctx [Let t (x :-> letFloating p)]
      -- Exists k (x :-> p) -> goExists ctx (Exists k) x (letFloating p)
      Subst x t p -> go ctx (substitutePred x t p)
      -- Reifies t' t f -> Reifies t' t f : ctx
      -- Explain es p -> pushExplain es p : ctx
      -- TODO: float let through forall if possible
      ForAll t (x :-> p) -> ForAll t (x :-> letFloating p) : ctx
      -- TODO: float let through the cases if possible
      Case t (x :-> px) (y :-> py) -> Case t (x :-> letFloating px) (y :-> letFloating py) : ctx
      -- TODO: float let through if possible
      -- When b p -> When b (letFloating p) : ctx
      -- Boring cases
      Assert t -> Assert t : ctx
      -- GenHint h t -> GenHint h t : ctx
      -- DependsOn t t' -> DependsOn t t' : ctx
      TruePred -> TruePred : ctx
      FalsePred es -> FalsePred es : ctx
      -- Monitor m -> Monitor m : ctx


-- Common subexpression elimination but only on terms that are already let-bound.
letSubexpressionElimination :: HasSpec Bool => Pred -> Pred
letSubexpressionElimination = go []
  where
    adjustSub x sub =
      [ x' := t
      | x' := t <- sub
      , isNothing $ eqVar x x'
      , -- TODO: possibly freshen the binder where
      -- `x` appears instead?
      not $ Name x `appearsIn` t
      ]

    goBinder :: Subst -> Binder a -> Binder a
    goBinder sub (x :-> p) = x :-> go (adjustSub x sub) p

    go sub = \case
      ElemPred bool t xs -> ElemPred bool (backwardsSubstitution sub t) xs
      -- GenHint h t -> GenHint h (backwardsSubstitution sub t)
      And ps -> And (go sub <$> ps)
      Let t (x :-> p) -> Let t' (x :-> go (x := t' : sub') p)
        where
          t' = backwardsSubstitution sub t
          sub' = adjustSub x sub
      -- Exists k b -> Exists k (goBinder sub b)
      Subst x t p -> go sub (substitutePred x t p)
      Assert t -> Assert (backwardsSubstitution sub t)
      -- Reifies t' t f -> Reifies (backwardsSubstitution sub t') (backwardsSubstitution sub t) f
      -- NOTE: this is a tricky case. One possible thing to do here is to keep the old `DependsOn t t'`
      -- and have the new DependsOn if `backwardsSubstitution` changed something. With this semantics you
      -- risk running into unintuitive behaviour if you have something like:
      -- ```
      -- let x = y + z in
      --  {y + z `dependsOn` w
      --   assert $ w <. y + 2
      --   ...}
      -- ```
      -- This will be rewritten as:
      -- ```
      -- let x = y + z in
      --  {z `dependsOn` w
      --   assert $ w <. y + 2
      --   ...}
      -- ```
      -- which changes the dependency order of `w` and `y`. However, fixing
      -- this behaviour in turn makes it more difficult to detect when
      -- variables are no longer used after being substituted away - which
      -- blocks some other optimizations. As we strongly encourage users not to
      -- use `letBind` in their own code most users will never encounter this issue
      -- so the tradeoff is probably worth it.
      -- DependsOn t t' -> DependsOn (backwardsSubstitution sub t) (backwardsSubstitution sub t')
      ForAll t b -> ForAll (backwardsSubstitution sub t) (goBinder sub b)
      Case t as bs -> Case (backwardsSubstitution sub t) (goBinder sub as) (goBinder sub bs)
      -- When b p -> When (backwardsSubstitution sub b) (go sub p)
      TruePred -> TruePred
      FalsePred es -> FalsePred es
      -- Monitor m -> Monitor m
      -- Explain es p -> Explain es $ go sub p




-- | Precondition: the `Pred` defines the `Var a`
-- Runs in `GE` in order for us to have detailed context on failure.
computeSpecSimplified ::
  forall a. (HasSpec a, HasCallStack) => Var a -> Pred -> GE (Spec a)
computeSpecSimplified x pred3 = localGESpec $ case simplifyPred pred3 of
  ElemPred True t xs -> propagateSpec (MemberSpec xs) <$> toCtx x t
  ElemPred False (t :: Term b) xs -> propagateSpec (TypeSpec @b (emptySpec @b) (NE.toList xs)) <$> toCtx x t
  -- Monitor {} -> pure mempty
  -- GenHint h t -> propagateSpec (giveHint h) <$> toCtx x t
  Subst x' t p' -> computeSpec x (substitutePred x' t p') -- NOTE: this is impossible as it should have gone away already
  TruePred -> pure mempty
  FalsePred es -> genErrorNE es
  And ps -> do
    spec <- fold <$> mapM (computeSpecSimplified x) ps
    case spec of
      SuspendedSpec y ps' -> pure $ SuspendedSpec y $ simplifyPred ps'
      s -> pure s
  Let t b -> pure $ SuspendedSpec x (Let t b)
  -- Exists k b -> pure $ SuspendedSpec x (Exists k b)
  Assert (Lit True) -> pure mempty
  Assert (Lit False) -> genError (show pred3)
  -- Elem was a pattern
  -- Assert (Elem _ (Lit [])) -> pure (ErrorSpec (NE.fromList ["Empty list in ElemPat", show pred3]))
  -- Assert (Elem t (Lit (y : ys))) -> propagateSpec (MemberSpec (y :| ys)) <$> toCtx x t
  Assert t -> propagateSpec (equalSpec True) <$> toCtx x t
  ForAll (Lit s) b -> fold <$> mapM (\val -> computeSpec x $ unBind val b) (forAllToList s)
  ForAll t b -> do
    bSpec <- computeSpecBinderSimplified b
    propagateSpec (fromForAllSpec bSpec) <$> toCtx x t
  Case (Lit val) as bs -> runCaseOn val as bs $ \va vaVal psa -> computeSpec x (substPred (singletonEnv va vaVal) psa)
  {-
  Case t branches -> do
    branchSpecs <- mapMList (traverseWeighted computeSpecBinderSimplified) branches
    propagateSpec (caseSpec (Just (showType @a)) branchSpecs) <$> toCtx x t
    -}
  -- When (Lit b) tp -> if b then computeSpecSimplified x tp else pure TrueSpec
  -- This shouldn't happen a lot of the time because when the body is trivial we mostly get rid of the `When` entirely
  -- When {} -> pure $ SuspendedSpec x pred3
  {- Reifies (Lit a) (Lit val) f
    | f val == a -> pure TrueSpec
    | otherwise ->
        pure $
          ErrorSpec (NE.fromList ["Value does not reify to literal: " ++ show val ++ " -/> " ++ show a])
  Reifies t' (Lit val) f ->
    propagateSpec (equalSpec (f val)) <$> toCtx x t'
  Reifies Lit {} _ _ ->
    fatalErrorNE $ NE.fromList ["Dependency error in computeSpec: Reifies", "  " ++ show pred3]
  Explain es p -> do
    -- In case things crash in here we want the explanation
    s <- pushGE (NE.toList es) (computeSpecSimplified x p)
    -- This is because while we do want to propagate `explanation`s into `SuspendedSpec`
    -- we probably don't want to propagate the full "currently simplifying xyz" explanation.
    case s of
      SuspendedSpec x2 p2 -> pure $ SuspendedSpec x2 (explanation es p2)
      _ -> pure $ addToErrorSpec es s
  -- Impossible cases that should be ruled out by the dependency analysis and linearizer
  DependsOn {} ->
    fatalErrorNE $
      NE.fromList
        [ "The impossible happened in computeSpec: DependsOn"
        , "  " ++ show x
        , show $ indent 2 (pretty pred3)
        ]
  Reifies {} ->
    fatalErrorNE $
      NE.fromList
        ["The impossible happened in computeSpec: Reifies", "  " ++ show x, show $ indent 2 (pretty pred3)]
  -}
  where
    -- We want `genError` to turn into `ErrorSpec` and we want `FatalError` to turn into `FatalError`
    localGESpec ge = case ge of
      (GenError xs) -> Result $ ErrorSpec (catMessageList xs)
      (FatalError es) -> FatalError es
      (Result v) -> Result v


-- | Precondition: the `Pred fn` defines the `Var a`.
--   Runs in `GE` in order for us to have detailed context on failure.
computeSpec ::
  forall a. (HasSpec a, HasCallStack) => Var a -> Pred -> GE (Spec a)
computeSpec x p = computeSpecSimplified x (simplifyPred p)

computeSpecBinder :: Binder a -> GE (Spec a)
computeSpecBinder (x :-> p) = computeSpec x p

computeSpecBinderSimplified :: Binder a -> GE (Spec a)
computeSpecBinderSimplified (x :-> p) = computeSpecSimplified x p


{-
toCtxList ::
  forall m v as.
  (Show v, Typeable v, MonadGenError m, HasCallStack) =>
  Var v ->
  List Term as ->
  m (ListCtx Value as (Ctx v))
toCtxList v xs = prefix xs
  where
    prefix :: forall as'. HasCallStack => List Term as' -> m (ListCtx Value as' (Ctx v))
    prefix Nil = fatalError ("toCtxList without hole, for variable " ++ show v)
    prefix (Lit l :> ts) = do
      ctx <- prefix ts
      pure $ l :-+ ctx
    prefix (t :> ts) = do
      hole <- toCtx v t
      suf <- suffix ts
      pure $ hole :? suf

    suffix :: forall as'. List Term as' -> m (List Value as')
    suffix Nil = pure Nil
    suffix (Lit l :> ts) = (Value l :>) <$> suffix ts
    suffix (_ :> _) = fatalErrorNE $ NE.fromList ["toCtxList with too many holes, for variable " ++ show v]
-}    