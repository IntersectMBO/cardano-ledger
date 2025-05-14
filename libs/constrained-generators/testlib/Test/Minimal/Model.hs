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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- HasSpec instances for known types Integer, Bool, Set , (,)
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Minimal.Model where

import Constrained.Core (
  Evidence (..),
  Var (..),
  eqVar,
  freshen,
  unionWithMaybe,
 )
import Constrained.Env
import Constrained.GenT
import qualified Constrained.Graph as Graph
import Constrained.List hiding (ListCtx)
import Control.Monad (guard)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Foldable (fold)
import qualified Data.Foldable as Foldable (fold)
import Data.Kind
import Data.List (nub, partition, (\\))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isNothing, listToMaybe, maybeToList)
import Data.Semigroup (Any (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import GHC.Stack
import Prettyprinter
import Test.Minimal.Base
import Test.Minimal.Syntax
import Test.QuickCheck hiding (forAll)

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
  NegateW :: IntegerSym '[Integer] Integer
  LessOrEqW :: IntegerSym '[Integer, Integer] Bool
  GreaterOrEqW :: IntegerSym '[Integer, Integer] Bool

deriving instance Eq (IntegerSym dom rng)

instance Show (IntegerSym dom rng) where show = name

instance Syntax IntegerSym where
  name PlusW = "+."
  name MinusW = "-."
  name NegateW = "negate_"
  name LessOrEqW = "<=."
  name GreaterOrEqW = ">=."
  inFix NegateW = False
  inFix _ = True

instance Semantics IntegerSym where
  semantics PlusW = (+)
  semantics MinusW = (-)
  semantics NegateW = negate
  semantics LessOrEqW = (<=)
  semantics GreaterOrEqW = (>=)

instance Logic IntegerSym where
  propagate tag ctx spec = case (tag, ctx, spec) of
    (_, _, TrueSpec) -> TrueSpec
    (_, _, ErrorSpec xs) -> ErrorSpec xs
    (f, context, SuspendedSpec v ps) ->
      constrained $ \v' -> Let (App f (fromListCtx context v')) (v :-> ps)
    (LessOrEqW, HOLE :<| l, bspec) ->
      caseBoolSpec bspec $ \case True -> leqSpec l; False -> gtSpec l
    (LessOrEqW, l :|> HOLE, bspec) ->
      caseBoolSpec bspec $ \case True -> geqSpec l; False -> ltSpec l
    (GreaterOrEqW, HOLE :<| x, spec1) ->
      propagate LessOrEqW (x :|> HOLE) spec1
    (GreaterOrEqW, x :|> HOLE, spec2) ->
      propagate LessOrEqW (HOLE :<| x) spec2
    (NegateW, Unary HOLE, TypeSpec interval cant) -> typeSpec (negateRange interval) <> notMemberSpec (map negate cant)
    (NegateW, Unary HOLE, MemberSpec xs) -> MemberSpec $ NE.nub $ fmap negate xs
    (PlusW, HOLE :<| n, TypeSpec (Interval lo hi) bad) ->
      TypeSpec (Interval ((minus n) <$> lo) ((minus n) <$> hi)) (map (minus n) bad)
    (PlusW, HOLE :<| n, MemberSpec xs) ->
      MemberSpec (fmap (minus n) xs)
    (PlusW, n :|> HOLE, TypeSpec (Interval lo hi) bad) ->
      TypeSpec (Interval ((minus n) <$> lo) ((minus n) <$> hi)) (map (minus n) bad)
    (PlusW, n :|> HOLE, MemberSpec xs) -> MemberSpec (fmap (minus n) xs)
    (MinusW, HOLE :<| n, TypeSpec (Interval lo hi) bad) ->
      TypeSpec (Interval ((+ n) <$> lo) ((+ n) <$> hi)) (map (+ n) bad)
    (MinusW, HOLE :<| n, MemberSpec xs) ->
      MemberSpec (fmap (+ n) xs)
    (MinusW, n :|> HOLE, TypeSpec (Interval lo hi) bad) ->
      TypeSpec (negateRange (Interval ((minus n) <$> lo) ((minus n) <$> hi))) (map (minus n) bad)
    (MinusW, n :|> HOLE, MemberSpec xs) ->
      MemberSpec (fmap (minus n) xs)

negateRange :: Range -> Range
negateRange (Interval ml mu) = Interval (negate <$> mu) (negate <$> ml)

minus :: Integer -> Integer -> Integer
minus n x = n - x

geqSpec :: Integer -> Spec Integer
geqSpec n = typeSpec (Interval (Just n) Nothing)

leqSpec :: Integer -> Spec Integer
leqSpec n = typeSpec (Interval Nothing (Just n))

gtSpec :: Integer -> Spec Integer
gtSpec n = typeSpec (Interval (Just (n + 1)) Nothing)

ltSpec :: Integer -> Spec Integer
ltSpec n = typeSpec (Interval Nothing (Just (n - 1)))

(<=.) :: Term Integer -> Term Integer -> Term Bool
(<=.) x y = App LessOrEqW (x :> y :> Nil)

(>=.) :: Term Integer -> Term Integer -> Term Bool
(>=.) x y = App GreaterOrEqW (x :> y :> Nil)

(+.) :: Term Integer -> Term Integer -> Term Integer
(+.) x y = App PlusW (x :> y :> Nil)

(-.) :: Term Integer -> Term Integer -> Term Integer
(-.) x y = App MinusW (x :> y :> Nil)

negate_ :: Term Integer -> Term Integer
negate_ x = App NegateW (x :> Nil)

-- =========================
-- HasSpec Integer instance

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

  -- \| From -∞ to +∞
  anySpec = Interval Nothing Nothing

  -- \| Catch inconsistencies after using Monoid operation of the two Ranges.
  combineSpec s s' = guardTypeSpec (s <> s')

  -- \| In Interval where the lo bound is greater than the hi bound is inconsistent
  guardTypeSpec r@(Interval (Just n) (Just m))
    | n > m = ErrorSpec (pure ("lower bound greater than upper bound\n" ++ show r))
    | otherwise = typeSpec r
  guardTypeSpec range = typeSpec range

  genFromTypeSpec (Interval ml mu) = do
    n <- sizeT
    chooseT =<< constrainInterval ml mu (fromIntegral n)

  conformsTo i (Interval ml mu) = maybe True (<= i) ml && maybe True (i <=) mu

  toPreds v (Interval ml mu) =
    Foldable.fold $
      [Assert $ Lit l <=. v | l <- maybeToList ml]
        ++ [Assert $ v <=. Lit u | u <- maybeToList mu]

constrainInterval ::
  MonadGenError m => Maybe Integer -> Maybe Integer -> Integer -> m (Integer, Integer)
constrainInterval ml mu qcSize =
  case (ml, mu) of
    (Nothing, Nothing) -> pure (-qcSize', qcSize')
    (Just l, Nothing)
      | l < 0 -> pure (max l (negate qcSize'), qcSize')
      | otherwise -> pure (l, l + 2 * qcSize')
    (Nothing, Just u)
      | u > 0 -> pure (negate qcSize', min u qcSize')
      | otherwise -> pure (u - qcSize' - qcSize', u)
    (Just l, Just u)
      | l > u -> genError ("bad interval: " ++ show l ++ " " ++ show u)
      | u < 0 -> pure (safeSub l (safeSub l u qcSize') qcSize', u)
      | l >= 0 -> pure (l, safeAdd u (safeAdd u l qcSize') qcSize')
      -- TODO: this is a bit suspect if the bounds are lopsided
      | otherwise -> pure (max l (-qcSize'), min u qcSize')
  where
    qcSize' = abs $ fromInteger qcSize
    -- FIX THIS TO WORK just on Integer, Should be much simpler, as Integer has no undeflow or overflow.
    safeSub l a b
      | a - b > a = l
      | otherwise = max l (a - b)
    safeAdd u a b
      | a + b < a = u
      | otherwise = min u (a + b)

-- ========== Bool example ==================

data BoolSym (dom :: [Type]) rng where
  NotW :: BoolSym '[Bool] Bool

deriving instance Eq (BoolSym dom rng)

instance Show (BoolSym dom rng) where show = name

instance Syntax BoolSym where
  name NotW = "not_"
  inFix _ = False

instance Semantics BoolSym where
  semantics NotW = not

instance Logic BoolSym where
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate NotW (Unary HOLE) (SuspendedSpec v ps) =
    constrained $ \v' -> Let (App NotW (v' :> Nil)) (v :-> ps)
  propagate NotW (Unary HOLE) spec =
    caseBoolSpec spec (equalSpec . not)

not_ :: Term Bool -> Term Bool
not_ x = App NotW (x :> Nil)

-- =========================
-- HasSpec Bool instance

instance HasSpec Bool where
  type TypeSpec Bool = Set Bool

  anySpec = Set.fromList [False, True]

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
  MemberW :: (HasSpec a, Ord a) => SetSym [a, Set a] Bool
  SizeW :: (HasSpec a, Ord a) => SetSym '[Set a] Integer
  SubsetW :: (HasSpec a, Ord a) => SetSym [Set a, Set a] Bool

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

  rewriteRules SubsetW (Lit s :> _ :> Nil) Evidence | null s = Just $ Lit True
  rewriteRules SubsetW (x :> Lit s :> Nil) Evidence | null s = Just $ x ==. Lit Set.empty
  rewriteRules MemberW (t :> Lit s :> Nil) Evidence
    | null s = Just $ Lit False
    | [a] <- Set.toList s = Just $ t ==. Lit a
  rewriteRules t l Evidence = Lit <$> (applyFunSym @SetSym (semantics t) l)

instance Logic SetSym where
  propagate tag ctx spec = case (tag, ctx, spec) of
    (_, _, TrueSpec) -> TrueSpec
    (_, _, ErrorSpec es) -> ErrorSpec es
    (f, context, SuspendedSpec v ps) -> constrained $ \v' -> Let (App f (fromListCtx context v')) (v :-> ps)
    (MemberW, HOLE :<| (s :: Set a), spec1) ->
      caseBoolSpec spec1 $ \case
        True -> memberSpecList (Set.toList s) (pure "propagateSpecFun on (Member x s) where s is Set.empty")
        False -> notMemberSpec s
    (MemberW, e :|> HOLE, spec2) ->
      caseBoolSpec spec2 $ \case
        True -> typeSpec $ SetSpec (Set.singleton e) mempty mempty
        False -> typeSpec $ SetSpec mempty (notEqualSpec e) mempty
    (SizeW, Unary HOLE, spec3) -> typeSpec (SetSpec mempty mempty spec3)
    (SubsetW, HOLE :<| big, spec4) -> caseBoolSpec spec4 $ \case
      True -> constrained $ \small ->
        And
          [ Assert $ size_ small <=. Lit (setSize big)
          , forAll small $ \x -> Assert $ member_ x (Lit big)
          ]
      False -> constrained $ \small ->
        exists (\eval -> headGE $ Set.difference big (eval small)) $ \e ->
          And
            [ -- set `DependsOn` e,
              Assert $ not_ $ member_ e (Lit big)
            , Assert $ member_ e small
            ]
    (SubsetW, small :|> HOLE, spec5) -> caseBoolSpec spec5 $ \case
      True -> typeSpec $ SetSpec small TrueSpec mempty
      False -> constrained $ \big ->
        exists (\eval -> headGE $ Set.difference (eval big) small) $ \e ->
          And
            [ -- set `DependsOn` e,
              Assert $ member_ e (Lit small)
            , Assert $ not_ $ member_ e big
            ]

setSize :: Set a -> Integer
setSize = toInteger . Set.size

size_ :: (HasSpec s, Ord s) => Term (Set s) -> Term Integer
size_ s = App SizeW (s :> Nil)

subset_ :: (HasSpec s, Ord s) => Term (Set s) -> Term (Set s) -> Term Bool
subset_ s1 s2 = App SubsetW (s1 :> s2 :> Nil)

member_ :: (Ord a, HasSpec a) => Term a -> Term (Set a) -> Term Bool
member_ x y = App MemberW (x :> y :> Nil)

-- Helpers for the `HasSpec (Set s)` instance

instance Ord s => Container (Set s) s where
  fromForAllSpec e = typeSpec $ SetSpec mempty e TrueSpec
  forAllToList = Set.toList

data SetSpec a = SetSpec {setMust :: Set a, setAll :: Spec a, setCount :: Spec Integer}
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
knownUpperBound (TypeSpec (Interval lo hi) cant) = upper lo hi
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

-- =========================
-- HasSpec Set instance

instance (Container (Set a) a, Ord a, HasSpec a) => HasSpec (Set a) where
  type TypeSpec (Set a) = SetSpec a

  anySpec = SetSpec Set.empty TrueSpec TrueSpec

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
  genFromTypeSpec (SetSpec must (MemberSpec xs) szSpec) = do
    let szSpec' = szSpec <> geqSpec (setSize must) -- <> maxSpec (cardinality elemS)
    choices <- pureGen $ shuffle (NE.toList xs \\ Set.toList must)
    size <- fromInteger <$> genFromSpecT szSpec'
    let additions = Set.fromList $ take (size - Set.size must) choices
    pure (Set.union must additions)
  genFromTypeSpec (SetSpec must elemS szSpec) = do
    let szSpec' = szSpec <> geqSpec (setSize must) -- <> maxSpec (cardinality elemS)
    sizecount <-
      explain "Choose a size for the Set to be generated" $
        genFromSpecT szSpec'
    let targetSize = sizecount - setSize must
    explainNE
      ( NE.fromList
          [ "Choose size count = " ++ show sizecount
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

-- ========== Pairs example =======================

pattern Pair ::
  forall c. () => forall a b. (c ~ (a, b), HasSpec a, HasSpec b) => Term a -> Term b -> Term c
pattern Pair x y <- App (getWitness -> Just PairW) (x :> y :> Nil)

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
  rewriteRules FstW (Pair x _ :> Nil) Evidence = Just x
  rewriteRules SndW (Pair _ y :> Nil) Evidence = Just y
  rewriteRules t l Evidence = Lit <$> applyFunSym @PairSym (semantics t) l

instance Logic PairSym where
  propagateTypeSpec FstW (Unary HOLE) ts cant = typeSpec $ Cartesian (TypeSpec ts cant) TrueSpec
  propagateTypeSpec SndW (Unary HOLE) ts cant = typeSpec $ Cartesian TrueSpec (TypeSpec ts cant)
  propagateTypeSpec PairW (a :|> HOLE) sc@(Cartesian sa sb) cant
    | a `conformsToSpec` sa = sb <> foldMap notEqualSpec (sameFst a cant)
    | otherwise =
        ErrorSpec
          ( NE.fromList
              ["propagate (pair_ " ++ show a ++ " HOLE) has conformance failure on a", show (TypeSpec sc cant)]
          )
  propagateTypeSpec PairW (HOLE :<| b) sc@(Cartesian sa sb) cant
    | b `conformsToSpec` sb = sa <> foldMap notEqualSpec (sameSnd b cant)
    | otherwise =
        ErrorSpec
          ( NE.fromList
              ["propagate (pair_ HOLE " ++ show b ++ ") has conformance failure on b", show (TypeSpec sc cant)]
          )

  propagateMemberSpec FstW (Unary HOLE) es = typeSpec $ Cartesian (MemberSpec es) TrueSpec
  propagateMemberSpec SndW (Unary HOLE) es = typeSpec $ Cartesian TrueSpec (MemberSpec es)
  propagateMemberSpec PairW (a :|> HOLE) es =
    case (nub (sameFst a (NE.toList es))) of
      (w : ws) -> MemberSpec (w :| ws)
      [] ->
        ErrorSpec $
          NE.fromList
            [ "propagate (pair_ HOLE " ++ show a ++ ") on (MemberSpec " ++ show (NE.toList es)
            , "Where " ++ show a ++ " does not appear as the fst component of anything in the MemberSpec."
            ]
  propagateMemberSpec PairW (HOLE :<| b) es =
    case (nub (sameSnd b (NE.toList es))) of
      (w : ws) -> MemberSpec (w :| ws)
      [] ->
        ErrorSpec $
          NE.fromList
            [ "propagate (pair_ HOLE " ++ show b ++ ") on (MemberSpec " ++ show (NE.toList es)
            , "Where " ++ show b ++ " does not appear as the snd component of anything in the MemberSpec."
            ]

sameFst :: Eq a1 => a1 -> [(a1, a2)] -> [a2]
sameFst a ps = [b | (a', b) <- ps, a == a']

sameSnd :: Eq a1 => a1 -> [(a2, a1)] -> [a2]
sameSnd b ps = [a | (a, b') <- ps, b == b']

fst_ :: (HasSpec a, HasSpec b) => Term (a, b) -> Term a
fst_ x = App FstW (x :> Nil)

snd_ :: (HasSpec a, HasSpec b) => Term (a, b) -> Term b
snd_ x = App SndW (x :> Nil)

pair_ :: (HasSpec a, HasSpec b) => Term a -> Term b -> Term (a, b)
pair_ a b = App PairW (a :> b :> Nil)

-- ========== The Pair (a,b) HasSpec instance

data PairSpec a b = Cartesian (Spec a) (Spec b)

instance (HasSpec a, HasSpec b) => Show (PairSpec a b) where
  show (Cartesian l r) = "(Cartesian " ++ "(" ++ show l ++ ") (" ++ show r ++ "))"

instance (HasSpec a, HasSpec b) => Semigroup (PairSpec a b) where
  (Cartesian x y) <> (Cartesian a b) = Cartesian (x <> a) (y <> b)

instance (HasSpec a, HasSpec b) => Monoid (PairSpec a b) where mempty = Cartesian mempty mempty

guardPair :: forall a b. (HasSpec a, HasSpec b) => Spec a -> Spec b -> Spec (a, b)
guardPair specA specB = handleErrors specA specB (\s t -> typeSpec (Cartesian s t))

instance (HasSpec a, HasSpec b) => HasSpec (a, b) where
  type TypeSpec (a, b) = PairSpec a b

  anySpec = Cartesian mempty mempty

  combineSpec (Cartesian a b) (Cartesian a' b') = guardPair (a <> a') (b <> b')

  conformsTo (a, b) (Cartesian sa sb) = conformsToSpec a sa && conformsToSpec b sb

  guardTypeSpec (Cartesian x y) = guardPair x y

  genFromTypeSpec (Cartesian sa sb) = (,) <$> genFromSpecT sa <*> genFromSpecT sb

  toPreds x (Cartesian sf ss) =
    satisfies (fst_ x) sf
      <> satisfies (snd_ x) ss

-- ========== Either example =======================

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
  propagateTypeSpec LeftW (Unary HOLE) (SumSpec sl _) cant = sl <> foldMap notEqualSpec [a | Left a <- cant]
  propagateTypeSpec RightW (Unary HOLE) (SumSpec _ sr) cant = sr <> foldMap notEqualSpec [a | Right a <- cant]

  propagateMemberSpec LeftW (Unary HOLE) es =
    case [a | Left a <- NE.toList es] of
      (x : xs) -> MemberSpec (x :| xs)
      [] ->
        ErrorSpec $
          pure $
            "propMemberSpec (left_ HOLE) on (MemberSpec es) with no Left in es: " ++ show (NE.toList es)
  propagateMemberSpec RightW (Unary HOLE) es =
    case [a | Right a <- NE.toList es] of
      (x : xs) -> MemberSpec (x :| xs)
      [] ->
        ErrorSpec $
          pure $
            "propagate (Right HOLE) on (MemberSpec es) with no Right in es: " ++ show (NE.toList es)

left_ :: (HasSpec a, HasSpec b) => Term a -> Term (Either a b)
left_ x = App LeftW (x :> Nil)

right_ :: (HasSpec a, HasSpec b) => Term b -> Term (Either a b)
right_ x = App RightW (x :> Nil)

-- ========== The Either HasSpec instance

data SumSpec a b = SumSpec a b

deriving instance (Eq a, Eq b) => Eq (SumSpec a b)

deriving instance (Show a, Show b) => Show (SumSpec a b)

guardSum :: forall a b. (HasSpec a, HasSpec b) => Spec a -> Spec b -> Spec (Either a b)
guardSum (ErrorSpec es) (ErrorSpec fs) = ErrorSpec (es <> fs)
guardSum (ErrorSpec es) _ = ErrorSpec (NE.cons "sum error on left" es)
guardSum _ (ErrorSpec es) = ErrorSpec (NE.cons "sum error on right" es)
guardSum s s' = typeSpec $ SumSpec s s'

instance (HasSpec a, HasSpec b) => HasSpec (Either a b) where
  type TypeSpec (Either a b) = SumSpec (Spec a) (Spec b)

  anySpec = SumSpec mempty mempty

  combineSpec (SumSpec a b) (SumSpec c d) = guardSum (a <> c) (b <> d)

  conformsTo (Left a) (SumSpec sa _) = conformsToSpec a sa
  conformsTo (Right b) (SumSpec _ sb) = conformsToSpec b sb

  toPreds x (SumSpec a b) = Case x (bind $ \y -> satisfies y a) (bind $ \y -> satisfies y b)

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

-- =========================================================================
-- User Facing functions
-- ====================================================================

-- | Generalize `genFromTypeSpec` from `TypeSpec t` to `Spec t`
--   Generate a value that satisfies the spec. This function can fail if the
--   spec is inconsistent, there is a dependency error, or if the underlying
--   generators are not flexible enough.
genFromSpecT ::
  forall a m. (HasCallStack, HasSpec a, MonadGenError m) => Spec a -> GenT m a
genFromSpecT (simplifySpec -> spec) = case spec of
  MemberSpec as -> explain ("genFromSpecT on spec" ++ show spec) $ pureGen (elements (NE.toList as))
  TrueSpec -> genFromSpecT (typeSpec $ anySpec @a)
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

-- | A version of `genFromSpecT` that simply errors if the generator fails
genFromSpec :: forall a. (HasCallStack, HasSpec a) => Spec a -> Gen a
genFromSpec spec = do
  res <- catchGen $ genFromSpecT @a @GE spec
  either (error . ('\n' :) . catMessages) pure res

-- | A version of `genFromSpecT` that runs in the IO monad. Good for debugging.
debugSpec :: forall a. HasSpec a => Spec a -> IO ()
debugSpec spec = do
  ans <- generate $ genFromGenT $ inspect (genFromSpecT spec)
  let f x = putStrLn (unlines (NE.toList x))
      ok x =
        if conformsToSpec x spec
          then putStrLn "True"
          else putStrLn "False, perhaps there is an unsafeExists in the spec?"
  case ans of
    FatalError xs -> mapM_ f xs
    GenError xs -> mapM_ f xs
    Result x -> print spec >> print (simplifySpec spec) >> print x >> ok x

-- | Generate a satisfying `Env` for a `p : Pred fn`. The `Env` contains values for
-- all the free variables in `flattenPred p`.
genFromPreds :: forall m. MonadGenError m => Env -> Pred -> GenT m Env
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
    go env plan = explain (show $ "Stepping the plan:" /> vsep [pretty env, pretty (substPlan env plan)]) $ do
      (env', plan') <- stepPlan env plan
      go env' plan'

-- =============================================================
-- Simplifcation
-- =============================================================

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
aggressiveInlining pred0
  | inlined = aggressiveInlining pInlined
  | otherwise = pred0
  where
    (pInlined, Any inlined) = runWriter $ go (freeVars pred0) [] pred0

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
      Exists k b -> Exists k <$> goBinder fvs sub b
      And ps -> Foldable.fold <$> mapM (go fvs sub) ps
      Assert t
        | not (isLit t)
        , Lit b <- substituteAndSimplifyTerm sub t -> do
            tell $ Any True
            pure $ toPred b
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
      TruePred -> pure pred2
      FalsePred {} -> pure pred2

-- ==================================================================================

-- | Lifts 'propagateSpec' to take a Monadic 'Ctx'
propagateSpecM ::
  forall v a m.
  (Monad m, HasSpec v) =>
  Spec a ->
  m (Ctx v a) ->
  m (Spec v)
propagateSpecM spec ctxM = do ctx <- ctxM; pure $ propagateSpec ctx spec

-- | Precondition: the `Pred` defines the `Var a`
-- Runs in `GE` in order for us to have detailed context on failure.
computeSpecSimplified ::
  forall a. (HasSpec a, HasCallStack) => Var a -> Pred -> GE (Spec a)
computeSpecSimplified x pred3 = localGESpec $ case simplifyPred pred3 of
  And ps -> do
    spec <- fold <$> mapM (computeSpecSimplified x) ps
    case spec of
      SuspendedSpec y ps' -> pure $ SuspendedSpec y $ simplifyPred ps'
      s -> pure s
  ElemPred True t xs -> propagateSpecM (MemberSpec xs) (toCtx x t)
  ElemPred False (t :: Term b) xs -> propagateSpecM (TypeSpec @b (anySpec @b) (NE.toList xs)) (toCtx x t)
  Subst x' t p' -> computeSpec x (substitutePred x' t p') -- NOTE: this is impossible as it should have gone away already
  TruePred -> pure mempty
  FalsePred es -> genErrorNE es
  Let t b -> pure $ SuspendedSpec x (Let t b)
  Exists k b -> pure $ SuspendedSpec x (Exists k b)
  Assert (Lit True) -> pure mempty
  Assert (Lit False) -> genError (show pred3)
  Assert t -> propagateSpecM (equalSpec True) (toCtx x t)
  ForAll (Lit s) b -> fold <$> mapM (\val -> computeSpec x $ unBind val b) (forAllToList s)
  ForAll t b -> do
    bSpec <- computeSpecBinderSimplified b
    propagateSpecM (fromForAllSpec bSpec) (toCtx x t)
  Case (Lit val) as bs -> runCaseOn val as bs $ \va vaVal psa -> computeSpec x (substPred (singletonEnv va vaVal) psa)
  Case t as bs -> do
    simpAs <- computeSpecBinderSimplified as
    simpBs <- computeSpecBinderSimplified bs
    propagateSpecM (typeSpec (SumSpec simpAs simpBs)) (toCtx x t)
  -- Impossible cases that should be ruled out by the dependency analysis and linearizer
  DependsOn {} ->
    fatalErrorNE $
      NE.fromList
        [ "The impossible happened in computeSpec: DependsOn"
        , "  " ++ show x
        , show $ indent 2 (pretty pred3)
        ]
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

-- ---------------------- Building a plan -----------------------------------

substStage :: Env -> SolverStage -> SolverStage
substStage env (SolverStage y ps spec) = normalizeSolverStage $ SolverStage y (substPred env <$> ps) spec

normalizeSolverStage :: SolverStage -> SolverStage
normalizeSolverStage (SolverStage x ps spec) = SolverStage x ps'' (spec <> spec')
  where
    (ps', ps'') = partition ((1 ==) . Set.size . freeVarSet) ps
    spec' = fromGESpec $ computeSpec x (And ps')

type Hints = DependGraph

type DependGraph = Graph.Graph Name

dependency :: HasVariables t => Name -> t -> DependGraph
dependency x (freeVarSet -> xs) = Graph.dependency x xs

irreflexiveDependencyOn ::
  forall t t'. (HasVariables t, HasVariables t') => t -> t' -> DependGraph
irreflexiveDependencyOn (freeVarSet -> xs) (freeVarSet -> ys) = Graph.irreflexiveDependencyOn xs ys

noDependencies :: HasVariables t => t -> DependGraph
noDependencies (freeVarSet -> xs) = Graph.noDependencies xs

respecting :: Hints -> DependGraph -> DependGraph
respecting hints g = g `Graph.subtractGraph` Graph.opGraph hints

solvableFrom :: Name -> Set Name -> DependGraph -> Bool
solvableFrom x s g =
  let less = Graph.dependencies x g
   in s `Set.isSubsetOf` less && not (x `Set.member` less)

-- TODO: here we can compute both the explicit hints (i.e. constraints that
-- define the order of two variables) and any whole-program smarts.
computeHints :: [Pred] -> Hints
computeHints ps =
  Graph.transitiveClosure $ fold [x `irreflexiveDependencyOn` y | DependsOn x y <- ps]

saturatePred :: Pred -> [Pred]
saturatePred p = [p]

-- | Linearize a predicate, turning it into a list of variables to solve and
-- their defining constraints such that each variable can be solved independently.
prepareLinearization :: Pred -> GE SolverPlan
prepareLinearization p = do
  let preds = concatMap saturatePred $ flattenPred p
      hints = computeHints preds
      graph = Graph.transitiveClosure $ hints <> respecting hints (foldMap computeDependencies preds)
  plan <-
    explainNE
      ( NE.fromList
          [ "Linearizing"
          , show $ "  preds: " <> pretty preds
          , show $ "  graph: " <> pretty graph
          ]
      )
      $ linearize preds graph
  pure $ backPropagation $ SolverPlan plan graph

-- | Flatten nested `Let`, `Exists`, and `And` in a `Pred fn`. `Let` and
-- `Exists` bound variables become free in the result.
flattenPred :: Pred -> [Pred]
flattenPred pIn = go (freeVarNames pIn) [pIn]
  where
    go _ [] = []
    go fvs (p : ps) = case p of
      And ps' -> go fvs (ps' ++ ps)
      -- NOTE: the order of the arguments to `==.` here are important.
      -- The whole point of `Let` is that it allows us to solve all of `t`
      -- before we solve the variables in `t`.
      Let t b -> goBinder fvs b ps (\x -> (Assert (t ==. (V x)) :))
      Exists _ b -> goBinder fvs b ps (const id)
      _ -> p : go fvs ps

    goBinder ::
      Set Int ->
      Binder a ->
      [Pred] ->
      (HasSpec a => Var a -> [Pred] -> [Pred]) ->
      [Pred]
    goBinder fvs (x :-> p) ps k = k x' $ go (Set.insert (nameOf x') fvs) (p' : ps)
      where
        (x', p') = freshen x p fvs

-- Consider: A + B = C + D
-- We want to fail if A and B are independent.
-- Consider: A + B = A + C, A <- B
-- Here we want to consider this constraint defining for A
linearize ::
  MonadGenError m => [Pred] -> DependGraph -> m [SolverStage]
linearize preds graph = do
  sorted <- case Graph.topsort graph of
    Left cycleX ->
      fatalError
        ( show $
            "linearize: Dependency cycle in graph:"
              /> vsep'
                [ "cycle:" /> pretty cycleX
                , "graph:" /> pretty graph
                ]
        )
    Right sorted -> pure sorted
  go sorted [(freeVarSet ps, ps) | ps <- filter isRelevantPred preds]
  where
    isRelevantPred TruePred = False
    isRelevantPred DependsOn {} = False
    isRelevantPred (Assert (Lit True)) = False
    isRelevantPred _ = True

    go [] [] = pure []
    go [] ps
      | null $ foldMap fst ps =
          case checkPredsE (pure "Linearizing fails") mempty (map snd ps) of
            Nothing -> pure []
            Just msgs -> genErrorNE msgs
      | otherwise =
          fatalErrorNE $
            NE.fromList
              [ "Dependency error in `linearize`: "
              , show $ indent 2 $ "graph: " /> pretty graph
              , show $
                  indent 2 $
                    "the following left-over constraints are not defining constraints for a unique variable:"
                      /> vsep' (map (pretty . snd) ps)
              ]
    go (n@(Name x) : ns) ps = do
      let (nps, ops) = partition (isLastVariable n . fst) ps
      (normalizeSolverStage (SolverStage x (map snd nps) mempty) :) <$> go ns ops

    isLastVariable n set = n `Set.member` set && solvableFrom n (Set.delete n set) graph

-- =================================
-- Operations on Stages and Plans

-- | Does nothing if the variable is not in the plan already.
mergeSolverStage :: SolverStage -> [SolverStage] -> [SolverStage]
mergeSolverStage (SolverStage x ps spec) plan =
  [ case eqVar x y of
      Just Refl ->
        SolverStage
          y
          (ps ++ ps')
          ( addToErrorSpec
              ( NE.fromList
                  ( [ "Solving var " ++ show x ++ " fails."
                    , "Merging the Specs"
                    , "   1. " ++ show spec
                    , "   2. " ++ show spec'
                    ]
                  )
              )
              (spec <> spec')
          )
      Nothing -> stage
  | stage@(SolverStage y ps' spec') <- plan
  ]

prettyPlan :: HasSpec a => Spec a -> Doc ann
prettyPlan (simplifySpec -> spec)
  | SuspendedSpec _ p <- spec
  , Result plan <- prepareLinearization p =
      vsep'
        [ "Simplified spec:" /> pretty spec
        , pretty plan
        ]
  | otherwise = "Simplfied spec:" /> pretty spec

printPlan :: HasSpec a => Spec a -> IO ()
printPlan = print . prettyPlan

isEmptyPlan :: SolverPlan -> Bool
isEmptyPlan (SolverPlan plan _) = null plan

stepPlan :: MonadGenError m => Env -> SolverPlan -> GenT m (Env, SolverPlan)
stepPlan env plan@(SolverPlan [] _) = pure (env, plan)
stepPlan env p@(SolverPlan (SolverStage x ps spec : pl) gr) = do
  (spec', specs) <- runGE
    $ explain
      (show (pretty env) ++ "\nStep " ++ show x ++ show (pretty p))
    $ do
      ispecs <- mapM (computeSpec x) ps
      pure $ (fold ispecs, ispecs)
  val <-
    genFromSpecT
      ( addToErrorSpec
          ( NE.fromList
              ( ( "\nStepPlan for variable: "
                    ++ show x
                    ++ " fails to produce Specification, probably overconstrained."
                    ++ "PS = "
                    ++ unlines (map show ps)
                )
                  : ("Original spec " ++ show spec)
                  : "Predicates"
                  : zipWith
                    (\pred1 specx -> "  pred " ++ show pred1 ++ " -> " ++ show specx)
                    ps
                    specs
              )
          )
          (spec <> spec')
      )
  let env1 = extendEnv x val env
  pure (env1, backPropagation $ SolverPlan (substStage env1 <$> pl) (Graph.deleteNode (Name x) gr))

computeDependencies :: Pred -> DependGraph
computeDependencies = \case
  ElemPred _bool term _xs -> computeTermDependencies term
  Subst x t p -> computeDependencies (substitutePred x t p)
  Assert t -> computeTermDependencies t
  ForAll set b ->
    let innerG = computeBinderDependencies b
     in innerG <> set `irreflexiveDependencyOn` Graph.nodes innerG
  DependsOn x y -> x `irreflexiveDependencyOn` y
  Case t as bs -> noDependencies t <> computeBinderDependencies as <> computeBinderDependencies bs
  TruePred -> mempty
  FalsePred {} -> mempty
  And ps -> foldMap computeDependencies ps
  Exists _ b -> computeBinderDependencies b
  Let t b -> noDependencies t <> computeBinderDependencies b

computeBinderDependencies :: Binder a -> DependGraph
computeBinderDependencies (x :-> p) =
  Graph.deleteNode (Name x) $ computeDependencies p

computeTermDependencies :: Term a -> DependGraph
computeTermDependencies = fst . computeTermDependencies'

computeTermDependencies' :: Term a -> (DependGraph, Set Name)
computeTermDependencies' = \case
  (App _ args) -> go args
  Lit {} -> (mempty, mempty)
  (V x) -> (noDependencies (Name x), Set.singleton (Name x))
  where
    go :: List Term as -> (DependGraph, Set Name)
    go Nil = (mempty, mempty)
    go (t :> ts) =
      let (gr, ngr) = go ts
          (tgr, ntgr) = computeTermDependencies' t
       in (ntgr `irreflexiveDependencyOn` ngr <> tgr <> gr, ngr <> ntgr)

-- | Push as much information we can backwards through the plan.
backPropagation :: SolverPlan -> SolverPlan
-- backPropagation (SolverPlan _plan _graph) =
backPropagation (SolverPlan initplan graph) = SolverPlan (go [] (reverse initplan)) graph
  where
    go acc [] = acc
    go acc (s@(SolverStage (x :: Var a) ps spec) : plan) = go (s : acc) plan'
      where
        newStages = concatMap (newStage spec) ps
        plan' = foldr mergeSolverStage plan newStages
        -- Note use of the Term Pattern Equal
        newStage specl (Assert (Equal (V x') t)) =
          termVarEqCases specl x' t
        newStage specr (Assert (Equal t (V x'))) =
          termVarEqCases specr x' t
        newStage _ _ = []

        termVarEqCases :: HasSpec b => Spec a -> Var b -> Term b -> [SolverStage]
        termVarEqCases (MemberSpec vs) x' t
          | Set.singleton (Name x) == freeVarSet t =
              [SolverStage x' [] $ MemberSpec (NE.nub (fmap (\v -> errorGE $ runTerm (singletonEnv x v) t) vs))]
        termVarEqCases specx x' t
          | Just Refl <- eqVar x x'
          , [Name y] <- Set.toList $ freeVarSet t
          , Result ctx <- toCtx y t =
              [SolverStage y [] (propagateSpec ctx specx)]
        termVarEqCases _ _ _ = []

spec9 :: Spec (Set Integer)
spec9 = constrained $ \x -> Assert $ (size_ x +. Lit 3) <=. Lit 12
