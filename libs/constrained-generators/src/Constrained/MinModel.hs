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

module Constrained.MinModel where

import qualified Constrained.Base as Base
import Constrained.Core (Evidence (..), Rename (rename), Var (..), eqVar, unionWithMaybe)
import Constrained.Env
import Constrained.GenT
import Constrained.List hiding (ListCtx)
import Control.Applicative ((<|>))
import Control.Monad.Identity
import qualified Data.Foldable as Foldable (fold, toList)
import Data.Kind
import Data.List (intersect, nub)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, isJust, maybeToList)
import Data.Semigroup (sconcat)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Typeable
import GHC.Stack
import Prettyprinter (Pretty (pretty))

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
--   relates to logical properties, that we call Specification's
class (Typeable t, Syntax t, Semantics t) => Logic (t :: [Type] -> Type -> Type) where
  {-# MINIMAL propagate | (propagateTypeSpec, propagateMemberSpec) #-}

  propagateTypeSpec ::
    (AppRequires t as b, HasSpec a) =>
    t as b ->
    ListCtx as a ->
    -- Ctx as a ->
    TypeSpec b ->
    [b] ->
    Spec a
  propagateTypeSpec f ctx ts cant = propagate f ctx (TypeSpec ts cant)

  propagateMemberSpec ::
    (AppRequires t as b, HasSpec a) =>
    t as b ->
    ListCtx as a ->
    -- Ctx as a ->
    NonEmpty b ->
    Spec a
  propagateMemberSpec f ctx xs = propagate f ctx (MemberSpec xs)

  propagate ::
    (AppRequires t as b, HasSpec a) =>
    t as b ->
    ListCtx as a ->
    -- Ctx as a ->
    Spec b ->
    Spec a
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec es) = ErrorSpec es
  -- propagate f ctx (SuspendedSpec v ps) = constrained $ \v' -> Let (App f (fromListCtx ctx v')) (v :-> ps)
  propagate f ctx (TypeSpec ts cant) = propagateTypeSpec f ctx ts cant
  propagate f ctx (MemberSpec xs) = propagateMemberSpec f ctx xs

{- data ListCtx f (as :: [Type]) c where
  (:?) :: c a -> List f as -> ListCtx f (a : as) c
  (:!) :: f a -> ListCtx f as c -> ListCtx f (a : as) c

--  ListCtx Value as (HOLE a)

data Ctx v a where
  -- | A single hole of type `v`. Note ctxHOLE is a nullary constructor, where the `a` type index is the same as the `v` type index.
  CtxHOLE ::
    HasSpec v =>
    Ctx v v
  -- | The application `f vs Ctx vs'`
  CtxApp ::
    ( AppRequires fn as b
    , HasSpec b
    , TypeList as
    , Typeable as
    , All HasSpec as
    , Logic fn
    ) =>
    fn as b ->
    -- This is basically a `List` where
    -- everything is `Value` except for
    -- one entry which is `Ctx fn v`.
    ListCtx Value as (Ctx v) ->
    Ctx v b
-}

data HOLE a where HOLE :: HOLE a

data ListCtx (as :: [Type]) a where
  Unary :: Ctx '[a] a -> ListCtx '[a] a
  (:-+) :: Ctx '[a, b] a -> b -> ListCtx '[a, b] a
  (:+-) :: a -> Ctx '[a, b] b -> ListCtx '[a, b] b

data Ctx as (a :: Type) where
  CtxApp :: Logic fn => fn as b -> ListCtx as v -> Ctx as b
  Hole :: HasSpec v => Ctx as v

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

  propagateMemberSpec PlusW (Hole :-+ n) xs = MemberSpec (fmap (+ n) xs)
  propagateMemberSpec PlusW (n :+- Hole) xs = MemberSpec (fmap (+ n) xs)
  propagateMemberSpec MinusW (Hole :-+ n) xs = MemberSpec (fmap (minus n) xs)
  propagateMemberSpec MinusW (n :+- Hole) xs = MemberSpec (fmap (+ n) xs)
  propagateMemberSpec LessOrEqW (Hole :-+ n) boolList = undefined
  propagateMemberSpec LessOrEqW (n :+- Hole) boolList = undefined

minus :: Integer -> Integer -> Integer
minus n x = x - n

-- ===========================

data Binder a where
  (:->) :: HasSpec a => Var a -> Pred -> Binder a

deriving instance Show (Binder a)

data Pred where
  ElemPred :: forall a. HasSpec a => Bool -> Term a -> NonEmpty a -> Pred
  And :: [Pred] -> Pred
  -- ForAll :: Term a -> (Term a -> Pred) -> Pred
  Assert :: Term Bool -> Pred
  TruePred :: Pred
  FalsePred :: NonEmpty String -> Pred
  -- Match :: Term (a,b) -> (Term a -> Term b -> Pred) -> Pred
  -- Case :: Term (Either a b) -> (Term a -> Pred) -> (Term b -> Pred) -> Pred
  Let :: Term a -> Binder a -> Pred
  Subst :: Var a -> Term a -> Pred -> Pred

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

-- ========================================

class (Typeable a, Eq a, Show a, Show (TypeSpec a), Typeable (TypeSpec a)) => HasSpec a where
  -- | The `TypeSpec a` is the type-specific `Spec a`.
  type TypeSpec a

  -- `TypeSpec` behaves sort-of like a monoid with a neutral
  -- element `emptySpec` and a `combineSpec` for combining
  -- two `TypeSpec a`. However, in order to provide flexibilty
  -- `combineSpec` takes two `TypeSpec` and constucts a `Specification`. This
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

-- ============================================
-- Now some concrete examples

data IntegerSym (dom :: [Type]) rng where
  PlusW :: IntegerSym '[Integer, Integer] Integer
  MinusW :: IntegerSym '[Integer, Integer] Integer
  LessOrEqW :: IntegerSym '[Integer, Integer] Bool

deriving instance Eq (IntegerSym dom rng)
instance Show (IntegerSym dom rng) where show = name

instance Syntax IntegerSym where
  name PlusW = "+."
  name MinusW = "-."
  name LessOrEqW = "<=."
  inFix _ = True

instance Semantics IntegerSym where
  semantics PlusW = (+)
  semantics MinusW = (-)
  semantics LessOrEqW = (<)

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

-- =================================================

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

-- =======================================
-- Tools for building Spec

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
    -- bound (ForAll _ b) = boundBinder b
    -- bound (Case _ cs) = getMax $ foldMapList (Max . boundBinder . thing) cs
    bound Assert {} = -1
    bound TruePred = -1
    bound FalsePred {} = -1

-- ==========

data SetSym (dom :: [Type]) rng where
  MemberW :: Ord a => SetSym [a, Set a] Bool
  SizeW :: SetSym '[Set a] Integer

instance Syntax SetSym where
  name MemberW = "member_"
  name SizeW = "size_"
  inFix _ = False

instance Semantics SetSym where
  semantics MemberW = Set.member
  semantics SizeW = toInteger . Set.size

-- ==========

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
          [ "The two " ++ showType @a ++ " Specifications are inconsistent."
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
  -- Subst x t p -> checkPredE env msgs $ substitutePred x t p
  Assert t -> case runTermE env t of
    Right True -> Nothing
    Right False ->
      Just
        (msgs <> pure ("Assert " ++ show t ++ " returns False") <> pure ("\nenv=\n" ++ show (pretty env)))
    Left es -> Just (msgs <> es)
  {-
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
    Case t bs -> case runTermE env t of
      Right v -> runCaseOn v (mapList thing bs) (\x val ps -> checkPredE (extendEnv x val env) msgs ps)
      Left es -> Just (msgs <> pure "checkPredE: Case fails" <> es)
    Let t (x :-> p) -> case runTermE env t of
      Right val -> checkPredE (extendEnv x val env) msgs p
      Left es -> Just (msgs <> pure "checkPredE: Let fails" <> es)
  -}
  TruePred -> Nothing
  FalsePred es -> Just (msgs <> pure "checkPredE: FalsePred" <> es)
  And ps ->
    case catMaybes (fmap (checkPredE env (pure "Some items in And  fail")) ps) of
      [] -> Nothing
      (x : xs) -> Just (msgs <> NE.nub (sconcat (x NE.:| xs)))

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
        -- ForAll set b -> ForAll (rename v v' set) (rename v v' b)
        -- Case t bs -> Case (rename v v' t) (rename v v' bs)
        TruePred -> TruePred
        FalsePred es -> FalsePred es

{-
instance Rename (Binder a) where
  rename v v' (va :-> psa) = va' :-> rename v v' psa'
    where
      (va', psa') = freshen va psa (Set.fromList [nameOf v, nameOf v'] <> Set.delete (nameOf va) (freeVarNames psa))
-}
