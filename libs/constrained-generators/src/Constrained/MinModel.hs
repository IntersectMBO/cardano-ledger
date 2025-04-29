{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.MinModel where

import qualified Constrained.Base as Base
import Constrained.Core (
  Evidence (..),
  Rename (rename),
  Value (..),
  Var (..),
  eqVar,
  freshen,
  unValue,
  unionWithMaybe,
 )
import Constrained.Env
import Constrained.GenT
import Constrained.Graph
import Constrained.List hiding (ListCtx)
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.Identity
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Foldable (fold)
import qualified Data.Foldable as Foldable (fold, toList)
import Data.Kind
import Data.List (intersect, nub, (\\))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

-- (Pretty (pretty),Doc)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, maybeToList)
import qualified Data.Monoid as Monoid
import Data.Semigroup (Any (..), Max (..), getMax, sconcat)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Typeable
import GHC.Stack
import MinBase
import MinSyntax
import Prettyprinter
import Test.QuickCheck hiding (forAll)

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
    | Set.null set = fatalError "genFromTypeSpec @Set where the typeSpec is Set.empty"
    | otherwise = oneofT (map pure (Set.toList set))

  guardTypeSpec s
    | Set.null s = ErrorSpec $ pure "guardTypeSpec @Set where the typeSpec is Set.empty"
    | otherwise = TypeSpec s []

  conformsTo i set = Set.member i set

  toPreds v set = case Set.toList set of
    [] -> FalsePred (pure "toPreds @Set where the typeSpec is Set.empty")
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

size_ :: (HasSpec s, Ord s) => Term (Set s) -> Term Integer
size_ s = App SizeW (s :> Nil)

subset_ :: (HasSpec s, Ord s) => Term (Set s) -> Term (Set s) -> Term Bool
subset_ s1 s2 = App SubsetW (s1 :> s2 :> Nil)

-- Helpers for the `HasSpec (Set s)` instance

instance Ord s => Forallable (Set s) s where
  fromForAllSpec e = typeSpec $ SetSpec mempty e TrueSpec
  forAllToList = Set.toList

data SetSpec a = SetSpec (Set a) (Spec a) (Spec Integer)
  deriving (Show)

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
        ("Must set size " ++ show (setSize must) ++ ", is inconsistent with SetSpec size" ++ show size)
          :| []
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

instance (Forallable (Set a) a, Ord a, HasSpec a) => HasSpec (Set a) where
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
      [Assert $ subset_ (Lit m) s | not $ Set.null m]
        ++ [ forAll s (\e -> satisfies e es)
           , satisfies (size_ s) size
           ]

  guardTypeSpec = guardSetSpec

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
    let szSpec' = szSpec <> geqSpec (setSize must) -- <> maxSpec (cardinality elemS)
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
      )
      $ go 100 targetSize must
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

-- ========== The Pair (a,b) HasSpec instance

data PairSym (dom :: [Type]) rng where
  FstW :: PairSym '[(a, b)] a
  SndW :: PairSym '[(a, b)] b
  PairW :: PairSym '[a, b] (a, b)

deriving instance Eq (PairSym dom rng)

instance Show (PairSym dom rng) where show = name

instance Syntax PairSym where
  name FstW = "fst_"
  name SndW = "snd_"
  name PairW = "pair_"
  inFix _ = False

instance Semantics PairSym where
  semantics FstW = fst
  semantics SndW = snd
  semantics PairW = (,)

instance Logic PairSym where
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec es) = ErrorSpec es
  propagate f ctx (SuspendedSpec v ps) = constrained $ \v' -> Let (App f (fromListCtx ctx v')) (v :-> ps)
  propagate FstW (Unary Hole) spec = undefined
  propagate SndW (Unary Hole) spec = undefined
  propagate PairW (x :+- Hole) spec = undefined
  propagate PairW (Hole :-+ ss) spec = undefined

fst_ :: (HasSpec a, HasSpec b) => Term (a, b) -> Term a
fst_ x = App FstW (x :> Nil)

snd_ :: (HasSpec a, HasSpec b) => Term (a, b) -> Term b
snd_ x = App SndW (x :> Nil)

pair_ :: (HasSpec a, HasSpec b) => Term a -> Term b -> Term (a, b)
pair_ a b = App PairW (a :> b :> Nil)

data PairSpec a b = Cartesian (Spec a) (Spec b)

instance (HasSpec a, HasSpec b) => Show (PairSpec a b) where
  show pair@(Cartesian l r) = "(Cartesian " ++ "(" ++ show l ++ ") (" ++ show r ++ "))"

{-
instance (Arbitrary (Spec a), Arbitrary (Spec b)) => Arbitrary (PairSpec a b) where
  arbitrary = Cartesian <$> arbitrary <*> arbitrary
  shrink (Cartesian a b) = uncurry Cartesian <$> shrink (a, b) -}

-- pairView :: forall a b. (HasSpec a, HasSpec b) => Term (a,b) -> Maybe (Term a, Term b)
-- pairView (App (sameFunSym $ PairW @a @b -> Just (_, Refl, Refl, Refl)) (x :> y :> Nil)) = Just (x, y)
-- pairView _ = Nothing

guardPair :: forall a b. (HasSpec a, HasSpec b) => Spec a -> Spec b -> Spec (a, b)
guardPair (ErrorSpec es) (ErrorSpec fs) = ErrorSpec (es <> fs)
guardPair (ErrorSpec es) _ = ErrorSpec (NE.cons "pair error on left" es)
guardPair _ (ErrorSpec es) = ErrorSpec (NE.cons "pair error on right" es)
guardPair s s' = typeSpec $ Cartesian s s'

instance (HasSpec a, HasSpec b) => HasSpec (a, b) where
  type TypeSpec (a, b) = PairSpec a b

  emptySpec = Cartesian mempty mempty

  combineSpec (Cartesian a b) (Cartesian a' b') = guardPair (a <> a') (b <> b')

  conformsTo (a, b) (Cartesian sa sb) = conformsToSpec a sa && conformsToSpec b sb

  guardTypeSpec (Cartesian x y) = guardPair x y

  genFromTypeSpec (Cartesian sa sb) = (,) <$> genFromSpecT sa <*> genFromSpecT sb

  toPreds x (Cartesian sf ss) =
    satisfies (fst_ x) sf
      <> satisfies (snd_ x) ss

-- ========== The Either HasSpec instance

data EitherSym (dom :: [Type]) rng where
  LeftW :: EitherSym '[a] (Either a b)
  RightW :: EitherSym '[b] (Either a b)

deriving instance Eq (EitherSym dom rng)
instance Show (EitherSym dom rng) where show = name

instance Syntax EitherSym where
  name LeftW = "left_"
  name RightW = "right_"
  inFix _ = False

instance Semantics EitherSym where
  semantics LeftW = Left
  semantics RightW = Right

instance Logic EitherSym where
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec es) = ErrorSpec es
  propagate f ctx (SuspendedSpec v ps) = constrained $ \v' -> Let (App f (fromListCtx ctx v')) (v :-> ps)
  propagate LeftW (Unary Hole) spec = undefined
  propagate RightW (Unary Hole) spec = undefined

left_ :: (HasSpec a, HasSpec b) => Term a -> Term (Either a b)
left_ x = App LeftW (x :> Nil)

right_ :: (HasSpec a, HasSpec b) => Term b -> Term (Either a b)
right_ x = App RightW (x :> Nil)

data SumSpec a b = SumSpec (Spec a) (Spec b)
  deriving (Show)

deriving instance (Eq (Spec a), Eq (Spec b)) => Eq (SumSpec a b)

guardSum :: forall a b. (HasSpec a, HasSpec b) => Spec a -> Spec b -> Spec (Either a b)
guardSum (ErrorSpec es) (ErrorSpec fs) = ErrorSpec (es <> fs)
guardSum (ErrorSpec es) _ = ErrorSpec (NE.cons "sum error on left" es)
guardSum _ (ErrorSpec es) = ErrorSpec (NE.cons "sum error on right" es)
guardSum s s' = typeSpec $ SumSpec s s'

instance (HasSpec a, HasSpec b) => HasSpec (Either a b) where
  type TypeSpec (Either a b) = SumSpec a b

  emptySpec = SumSpec mempty mempty

  combineSpec (SumSpec a b) (SumSpec c d) = guardSum (a <> c) (b <> d)

  conformsTo (Left a) (SumSpec sa _) = conformsToSpec a sa
  conformsTo (Right b) (SumSpec _ sb) = conformsToSpec b sb

  toPreds x (SumSpec a b) = Case x (bind $ \x -> satisfies x a) (bind $ \y -> satisfies y b)

  genFromTypeSpec (SumSpec (simplifySpec -> sa) (simplifySpec -> sb))
    | emptyA, emptyB = genError "genFromTypeSpec @SumSpec: empty"
    | emptyA = Right <$> genFromSpecT sb
    | emptyB = Left <$> genFromSpecT sa
    | otherwise = oneofT [Left <$> genFromSpecT sa, Right <$> genFromSpecT sb]
    where
      emptyA = isErrorLike sa
      emptyB = isErrorLike sb

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

-- ====================================================================

-- | Generalize `genFromTypeSpec` from `TypeSpec t` to `Spec t`
--   Generate a value that satisfies the spec. This function can fail if the
--   spec is inconsistent, there is a dependency error, or if the underlying
--   generators are not flexible enough.
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

-- | Turn 'GenError' into 'ErrorSpec', and FatalError into 'error'
fromGESpec :: HasCallStack => GE (Spec a) -> Spec a
fromGESpec ge = case ge of
  Result s -> s
  GenError xs -> ErrorSpec (catMessageList xs)
  FatalError es -> error $ catMessages es

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

-- ==================================================================================

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
  where
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
