{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | `HasSpec` instance for `Set`s and functions for writing
-- constraints about sets
module Constrained.Spec.Set (
  SetSpec (..),
  SetW (..),
  singleton_,
  subset_,
  member_,
  union_,
  disjoint_,
  fromList_,
) where

import Constrained.AbstractSyntax
import Constrained.Base
import Constrained.Conformance
import Constrained.Core
import Constrained.FunctionSymbol
import Constrained.GenT
import Constrained.Generation
import Constrained.List
import Constrained.NumOrd
import Constrained.PrettyUtils
import Constrained.Spec.List
import Constrained.SumList
import Constrained.Syntax
import Constrained.TheKnot
import Data.Foldable
import Data.Kind
import Data.List ((\\))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set
import Prettyprinter hiding (cat)
import Test.QuickCheck (shrinkList, shuffle)

------------------------------------------------------------------------
-- HasSpec instance for Set
------------------------------------------------------------------------

-- | `TypeSpec` for `Set`
data SetSpec a
  = SetSpec
      -- | Required elements
      (Set a)
      -- | Specification for elements
      (Specification a)
      -- | Specification for size
      (Specification Integer)

instance Ord a => Sized (Set.Set a) where
  sizeOf = toInteger . Set.size
  liftSizeSpec spec cant = typeSpec (SetSpec mempty TrueSpec (TypeSpec spec cant))
  liftMemberSpec xs = case NE.nonEmpty xs of
    Nothing -> ErrorSpec (pure "In liftMemberSpec for the (Sized Set) instance, xs is the empty list")
    Just zs -> typeSpec (SetSpec mempty TrueSpec (MemberSpec zs))
  sizeOfTypeSpec (SetSpec must _ sz) = sz <> geqSpec (sizeOf must)

instance (Ord a, HasSpec a) => Semigroup (SetSpec a) where
  SetSpec must es size <> SetSpec must' es' size' =
    SetSpec (must <> must') (es <> es') (size <> size')

instance (Ord a, HasSpec a) => Monoid (SetSpec a) where
  mempty = SetSpec mempty mempty TrueSpec

instance Ord a => Forallable (Set a) a where
  fromForAllSpec (e :: Specification a)
    | Evidence <- prerequisites @(Set a) = typeSpec $ SetSpec mempty e TrueSpec
  forAllToList = Set.toList

prettySetSpec :: HasSpec a => SetSpec a -> Doc ann
prettySetSpec (SetSpec must elemS size) =
  parens
    ( "SetSpec"
        /> sep ["must=" <> short (Set.toList must), "elem=" <> pretty elemS, "size=" <> pretty size]
    )

instance HasSpec a => Show (SetSpec a) where
  show x = show (prettySetSpec x)

guardSetSpec :: (HasSpec a, Ord a) => [String] -> SetSpec a -> Specification (Set a)
guardSetSpec es (SetSpec must elemS ((<> geqSpec 0) -> size))
  | Just u <- knownUpperBound size
  , u < 0 =
      ErrorSpec (("guardSetSpec: negative size " ++ show u) :| es)
  | not (all (`conformsToSpec` elemS) must) =
      ErrorSpec (("Some 'must' items do not conform to 'element' spec: " ++ show elemS) :| es)
  | isErrorLike size = ErrorSpec ("guardSetSpec: error in size" :| es)
  | isErrorLike (geqSpec (sizeOf must) <> size) =
      ErrorSpec $
        ("Must set size " ++ show (sizeOf must) ++ ", is inconsistent with SetSpec size" ++ show size) :| es
  | isErrorLike (maxSpec (cardinality elemS) <> size) =
      ErrorSpec $
        NE.fromList $
          [ "Cardinality of SetSpec elemSpec (" ++ show elemS ++ ") = " ++ show (maxSpec (cardinality elemS))
          , "   This is inconsistent with SetSpec size (" ++ show size ++ ")"
          ]
            ++ es
  | otherwise = typeSpec (SetSpec must elemS size)

instance (Ord a, HasSpec a) => HasSpec (Set a) where
  type TypeSpec (Set a) = SetSpec a

  type Prerequisites (Set a) = HasSpec a

  emptySpec = mempty

  combineSpec s s' = guardSetSpec ["While combining 2 SetSpecs", "  " ++ show s, "  " ++ show s'] (s <> s')

  conformsTo s (SetSpec must es size) =
    and
      [ sizeOf s `conformsToSpec` size
      , must `Set.isSubsetOf` s
      , all (`conformsToSpec` es) s
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
  genFromTypeSpec (SetSpec must (ExplainSpec [] elemspec) szSpec) =
    genFromTypeSpec (SetSpec must elemspec szSpec)
  genFromTypeSpec (SetSpec must (ExplainSpec (e : es) elemspec) szSpec) =
    explainNE (e :| es) $ genFromTypeSpec (SetSpec must elemspec szSpec)
  genFromTypeSpec (SetSpec must elemS@(MemberSpec xs) szSpec) = do
    let szSpec' = szSpec <> geqSpec (sizeOf must) <> maxSpec (cardinality elemS)
    choices <- pureGen $ shuffle (NE.toList xs \\ Set.toList must)
    size <- fromInteger <$> genFromSpecT szSpec'
    let additions = Set.fromList $ take (size - Set.size must) choices
    pure (Set.union must additions)
  genFromTypeSpec (SetSpec must elemS szSpec) = do
    let szSpec' = szSpec <> geqSpec (sizeOf must) <> maxSpec (cardinality elemS)
    chosenSize <-
      explain "Choose a size for the Set to be generated" $
        genFromSpecT szSpec'
    let targetSize = chosenSize - sizeOf must
    explainNE
      ( NE.fromList
          [ "Choose size = " ++ show chosenSize
          , "szSpec' = " ++ show szSpec'
          , "Picking items not in must = " ++ show (short (Set.toList must))
          , "that also meet the element test: "
          , "  " ++ show elemS
          ]
      )
      $ case theMostWeCanExpect of
        -- 0 means TrueSpec or SuspendedSpec so we can't rule anything out
        0 -> go 100 targetSize must
        n -> case compare n targetSize of
          LT -> fatalError "The number of things that meet the element test is too small."
          GT -> go 100 targetSize must
          EQ -> go 100 targetSize must
    where
      theMostWeCanExpect = maxFromSpec 0 (cardinality (simplifySpec elemS))
      go _ n s | n <= 0 = pure s
      go tries n s = do
        e <-
          explainNE
            ( NE.fromList
                [ "Generate set member at type " ++ showType @a
                , "  number of items starting with  = " ++ show (Set.size must)
                , "  number of items left to pick   = " ++ show n
                , "  number of items already picked = " ++ show (Set.size s)
                , "  the most items we can expect is " ++ show theMostWeCanExpect ++ " (a SuspendedSpec)"
                ]
            )
            $ withMode Strict
            $ suchThatWithTryT tries (genFromSpecT elemS) (`Set.notMember` s)

        go tries (n - 1) (Set.insert e s)

  cardinalTypeSpec (SetSpec _ es _)
    | Just ub <- knownUpperBound (cardinality es) = leqSpec (2 ^ ub)
  cardinalTypeSpec _ = TrueSpec

  cardinalTrueSpec
    | Just ub <- knownUpperBound $ cardinalTrueSpec @a = leqSpec (2 ^ ub)
    | otherwise = TrueSpec

  shrinkWithTypeSpec (SetSpec _ es _) as = map Set.fromList $ shrinkList (shrinkWithSpec es) (Set.toList as)

  toPreds s (SetSpec m es size) =
    fold $
      -- Don't include this if the must set is empty
      [ Explain (pure (show m ++ " is a subset of the set.")) $ Assert $ subset_ (Lit m) s
      | not $ Set.null m
      ]
        ++ [ forAll s (\e -> satisfies e es)
           , satisfies (sizeOf_ s) size
           ]

  guardTypeSpec = guardSetSpec

------------------------------------------------------------------------
-- Functions that deal with sets
------------------------------------------------------------------------

-- | Symbols for working on sets
data SetW (d :: [Type]) (r :: Type) where
  SingletonW :: (HasSpec a, Ord a) => SetW '[a] (Set a)
  UnionW :: (HasSpec a, Ord a) => SetW '[Set a, Set a] (Set a)
  SubsetW :: (HasSpec a, Ord a, HasSpec a) => SetW '[Set a, Set a] Bool
  MemberW :: (HasSpec a, Ord a) => SetW '[a, Set a] Bool
  DisjointW :: (HasSpec a, Ord a) => SetW '[Set a, Set a] Bool
  FromListW :: (HasSpec a, Ord a) => SetW '[[a]] (Set a)

deriving instance Eq (SetW dom rng)

instance Show (SetW ds r) where
  show SingletonW = "singleton_"
  show UnionW = "union_"
  show SubsetW = "subset_"
  show MemberW = "member_"
  show DisjointW = "disjoint_"
  show FromListW = "fromList_"

setSem :: SetW ds r -> FunTy ds r
setSem SingletonW = Set.singleton
setSem UnionW = Set.union
setSem SubsetW = Set.isSubsetOf
setSem MemberW = Set.member
setSem DisjointW = Set.disjoint
setSem FromListW = Set.fromList

instance Semantics SetW where
  semantics = setSem

instance Syntax SetW where
  prettySymbol SubsetW (Lit n :> y :> Nil) p = Just $ parensIf (p > 10) $ "subset_" <+> short (Set.toList n) <+> prettyPrec 10 y
  prettySymbol SubsetW (y :> Lit n :> Nil) p = Just $ parensIf (p > 10) $ "subset_" <+> prettyPrec 10 y <+> short (Set.toList n)
  prettySymbol DisjointW (Lit n :> y :> Nil) p = Just $ parensIf (p > 10) $ "disjoint_" <+> short (Set.toList n) <+> prettyPrec 10 y
  prettySymbol DisjointW (y :> Lit n :> Nil) p = Just $ parensIf (p > 10) $ "disjoint_" <+> prettyPrec 10 y <+> short (Set.toList n)
  prettySymbol UnionW (Lit n :> y :> Nil) p = Just $ parensIf (p > 10) $ "union_" <+> short (Set.toList n) <+> prettyPrec 10 y
  prettySymbol UnionW (y :> Lit n :> Nil) p = Just $ parensIf (p > 10) $ "union_" <+> prettyPrec 10 y <+> short (Set.toList n)
  prettySymbol MemberW (y :> Lit n :> Nil) p = Just $ parensIf (p > 10) $ "member_" <+> prettyPrec 10 y <+> short (Set.toList n)
  prettySymbol _ _ _ = Nothing

instance (Ord a, HasSpec a, HasSpec (Set a)) => Semigroup (Term (Set a)) where
  (<>) = union_

instance (Ord a, HasSpec a, HasSpec (Set a)) => Monoid (Term (Set a)) where
  mempty = Lit mempty

-- Logic instance for SetW ------------------------------------------------

singletons :: [Set a] -> [Set a] -- Every Set in the filterd output has size 1 (if there are any)
singletons = filter ((1 ==) . Set.size)

instance Logic SetW where
  propagate f ctxt (ExplainSpec es s) = explainSpec es $ propagate f ctxt s
  propagate _ _ TrueSpec = TrueSpec
  propagate _ _ (ErrorSpec msgs) = ErrorSpec msgs
  propagate f ctx (SuspendedSpec v ps) = constrained $ \v' -> Let (App f (fromListCtx ctx v')) (v :-> ps)
  propagate SingletonW (Unary HOLE) (TypeSpec (SetSpec must es size) cant)
    | not $ 1 `conformsToSpec` size =
        ErrorSpec (pure "propagateSpecFun Singleton with spec that doesn't accept 1 size set")
    | [a] <- Set.toList must
    , a `conformsToSpec` es
    , Set.singleton a `notElem` cant =
        equalSpec a
    | null must = es <> notMemberSpec (Set.toList $ fold $ singletons cant)
    | otherwise = ErrorSpec (pure "propagateSpecFun Singleton with `must` of size > 1")
  propagate SingletonW (Unary HOLE) (MemberSpec es) =
    case Set.toList $ fold $ singletons (NE.toList es) of
      [] -> ErrorSpec $ pure "In propagateSpecFun Singleton, the sets of size 1, in MemberSpec is empty"
      (x : xs) -> MemberSpec (x :| xs)
  propagate UnionW ctx spec
    | (Value s :! Unary HOLE) <- ctx =
        propagate UnionW (HOLE :? Value s :> Nil) spec
    | (HOLE :? Value (s :: Set a) :> Nil) <- ctx
    , Evidence <- prerequisites @(Set a) =
        case spec of
          _ | null s -> spec
          TypeSpec (SetSpec must es size) cant
            | not $ all (`conformsToSpec` es) s ->
                ErrorSpec $
                  NE.fromList
                    [ "Elements in union argument does not conform to elem spec"
                    , "  spec: " ++ show es
                    , "  elems: " ++ show (filter (not . (`conformsToSpec` es)) (Set.toList s))
                    ]
            | not $ null cant -> ErrorSpec (pure "propagateSpecFun Union TypeSpec, not (null cant)")
            | TrueSpec <- size -> typeSpec $ SetSpec (Set.difference must s) es TrueSpec
            | TypeSpec (NumSpecInterval mlb Nothing) [] <- size
            , maybe True (<= sizeOf s) mlb ->
                typeSpec $ SetSpec (Set.difference must s) es TrueSpec
            | otherwise -> constrained $ \x ->
                exists (\eval -> pure $ Set.intersection (eval x) s) $ \overlap ->
                  exists (\eval -> pure $ Set.difference (eval x) s) $ \disjoint ->
                    [ Assert $ overlap `subset_` Lit s
                    , Assert $ disjoint `disjoint_` Lit s
                    , satisfies (sizeOf_ disjoint + Lit (sizeOf s)) size
                    , Assert $ x ==. (overlap <> disjoint) -- depends on Semigroup (Term (Set a))
                    , forAll disjoint $ \e -> e `satisfies` es
                    , Assert $ Lit (must Set.\\ s) `subset_` disjoint
                    ]
          -- We only do singleton MemberSpec to avoid really bad blowup
          MemberSpec (e :| [])
            | s `Set.isSubsetOf` e ->
                typeSpec
                  ( SetSpec
                      (Set.difference e s)
                      ( memberSpec
                          (Set.toList e)
                          (pure "propagateSpec (union_ s HOLE) on (MemberSpec [e]) where e is the empty set")
                      )
                      mempty
                  )
          -- TODO: improve this error message
          _ ->
            ErrorSpec
              ( NE.fromList
                  [ "propagateSpecFun (union_ s HOLE) with spec"
                  , "s = " ++ show s
                  , "spec = " ++ show spec
                  ]
              )
  propagate SubsetW ctx spec
    | (HOLE :? Value (s :: Set a) :> Nil) <- ctx
    , Evidence <- prerequisites @(Set a) = caseBoolSpec spec $ \case
        True ->
          case NE.nonEmpty (Set.toList s) of
            Nothing -> MemberSpec (pure Set.empty)
            Just slist -> typeSpec $ SetSpec mempty (MemberSpec slist) mempty
        False -> constrained $ \set ->
          exists (\eval -> headGE $ Set.difference (eval set) s) $ \e ->
            [ set `DependsOn` e
            , Assert $ not_ $ member_ e (Lit s)
            , Assert $ member_ e set
            ]
    | (Value (s :: Set a) :! Unary HOLE) <- ctx
    , Evidence <- prerequisites @(Set a) = caseBoolSpec spec $ \case
        True -> typeSpec $ SetSpec s TrueSpec mempty
        False -> constrained $ \set ->
          exists (\eval -> headGE $ Set.difference (eval set) s) $ \e ->
            [ set `DependsOn` e
            , Assert $ member_ e (Lit s)
            , Assert $ not_ $ member_ e set
            ]
  propagate MemberW ctx spec
    | (HOLE :? Value s :> Nil) <- ctx = caseBoolSpec spec $ \case
        True -> memberSpec (Set.toList s) (pure "propagateSpecFun on (Member x s) where s is Set.empty")
        False -> notMemberSpec s
    | (Value e :! Unary HOLE) <- ctx = caseBoolSpec spec $ \case
        True -> typeSpec $ SetSpec (Set.singleton e) mempty mempty
        False -> typeSpec $ SetSpec mempty (notEqualSpec e) mempty
  propagate DisjointW ctx spec
    | (HOLE :? Value (s :: Set a) :> Nil) <- ctx =
        propagate DisjointW (Value s :! Unary HOLE) spec
    | (Value (s :: Set a) :! Unary HOLE) <- ctx
    , Evidence <- prerequisites @(Set a) = caseBoolSpec spec $ \case
        True -> typeSpec $ SetSpec mempty (notMemberSpec s) mempty
        False -> constrained $ \set ->
          exists (\eval -> headGE (Set.intersection (eval set) s)) $ \e ->
            [ set `DependsOn` e
            , Assert $ member_ e (Lit s)
            , Assert $ member_ e set
            ]
  propagate FromListW (Unary HOLE) spec =
    case spec of
      MemberSpec (xs :| []) ->
        typeSpec $
          ListSpec
            Nothing
            (Set.toList xs)
            TrueSpec
            ( memberSpec
                (Set.toList xs)
                (pure "propagateSpec (fromList_ HOLE) on (MemberSpec xs) where the set 'xs' is empty")
            )
            NoFold
      TypeSpec (SetSpec must elemSpec sizeSpec) []
        | TrueSpec <- sizeSpec -> typeSpec $ ListSpec Nothing (Set.toList must) TrueSpec elemSpec NoFold
        | TypeSpec (NumSpecInterval (Just l) Nothing) cantSize <- sizeSpec
        , l <= sizeOf must
        , all (< sizeOf must) cantSize ->
            typeSpec $ ListSpec Nothing (Set.toList must) TrueSpec elemSpec NoFold
      _ ->
        -- Here we simply defer to basically generating the universe that we can
        -- draw from according to `spec` first and then fold that into the spec for the list.
        -- The tricky thing about this is that it may not play super nicely with other constraints
        -- on the list. For this reason it's important to try to find as many possible work-arounds
        -- in the above cases as possible.
        constrained $ \xs ->
          exists (\eval -> pure $ Set.fromList (eval xs)) $ \s ->
            [ s `satisfies` spec
            , xs `DependsOn` s
            , forAll xs $ \e -> e `member_` s
            , forAll s $ \e -> e `elem_` xs
            ]

  mapTypeSpec FromListW ts =
    constrained $ \x ->
      unsafeExists $ \x' -> Assert (x ==. fromList_ x') <> toPreds x' ts
  mapTypeSpec SingletonW ts =
    constrained $ \x ->
      unsafeExists $ \x' ->
        Assert (x ==. singleton_ x') <> toPreds x' ts

  rewriteRules SubsetW (Lit s :> _ :> Nil) Evidence | null s = Just $ Lit True
  rewriteRules SubsetW (x :> Lit s :> Nil) Evidence | null s = Just $ x ==. Lit Set.empty
  rewriteRules UnionW (x :> Lit s :> Nil) Evidence | null s = Just x
  rewriteRules UnionW (Lit s :> x :> Nil) Evidence | null s = Just x
  rewriteRules MemberW (t :> Lit s :> Nil) Evidence
    | null s = Just $ Lit False
    | [a] <- Set.toList s = Just $ t ==. Lit a
  rewriteRules DisjointW (Lit s :> _ :> Nil) Evidence | null s = Just $ Lit True
  rewriteRules DisjointW (_ :> Lit s :> Nil) Evidence | null s = Just $ Lit True
  rewriteRules _ _ _ = Nothing

-- Functions for writing constraints on sets ------------------------------

-- | Create a set with a single element
singleton_ :: (Ord a, HasSpec a) => Term a -> Term (Set a)
singleton_ = appTerm SingletonW

-- | Check if the first argument is a subset of the second
subset_ :: (Ord a, HasSpec a) => Term (Set a) -> Term (Set a) -> Term Bool
subset_ = appTerm SubsetW

-- | Check if an element is a member of the set
member_ :: (Ord a, HasSpec a) => Term a -> Term (Set a) -> Term Bool
member_ = appTerm MemberW

-- | Take the union of two sets
union_ :: (Ord a, HasSpec a) => Term (Set a) -> Term (Set a) -> Term (Set a)
union_ = appTerm UnionW

-- | Check if two sets have no elements in common
disjoint_ :: (Ord a, HasSpec a) => Term (Set a) -> Term (Set a) -> Term Bool
disjoint_ = appTerm DisjointW

-- | Convert a list to a set
fromList_ :: forall a. (Ord a, HasSpec a) => Term [a] -> Term (Set a)
fromList_ = appTerm FromListW
