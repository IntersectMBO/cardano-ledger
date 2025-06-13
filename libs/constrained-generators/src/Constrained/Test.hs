{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Useful properties for debugging HasSpec instances and this library itself
module Constrained.Test where

import Constrained.API
import Constrained.Base (
  AppRequires,
  BaseW (..),
  HOLE (..),
  appTerm,
  memberSpecList,
  typeSpec,
 )
import Constrained.Conformance (
  monitorSpec,
 )
import Constrained.Core (
  Value (..),
  Var (..),
  unValue,
 )
import Constrained.FunctionSymbol (getWitness)
import Constrained.GenT (
  GE (..),
  fromGEDiscard,
  fromGEProp,
  strictGen,
 )
import Constrained.Generation
import Constrained.List (
  All,
  FunTy,
  List (..),
  ListCtx (..),
  TypeList,
  fillListCtx,
  lengthList,
  listShape,
  mapListCtxC,
  mapMListC,
  uncurryList,
  uncurryList_,
 )
import Constrained.NumOrd (
  IntW (..),
  OrdW (..),
 )
import Constrained.PrettyUtils
import Constrained.Properties
import Constrained.Spec.Map (
  MapW (..),
 )
import Constrained.Spec.SumProd ()
import Constrained.TheKnot
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Set (Set)
import Data.Typeable (Typeable, typeOf)
import Prettyprinter
import Test.QuickCheck hiding (Fun)
import qualified Test.QuickCheck as QC

prop_sound ::
  HasSpec a =>
  Specification a ->
  QC.Property
prop_sound spec =
  QC.forAllBlind (strictGen $ genFromSpecT spec) $ \ma ->
    case ma of
      Result a ->
        QC.cover 80 True "successful" $
          QC.counterexample (show a) $
            monitorSpec spec a $
              conformsToSpecProp a spec
      _ -> QC.cover 80 False "successful" True

prop_constrained_satisfies_sound :: HasSpec a => Specification a -> QC.Property
prop_constrained_satisfies_sound spec = prop_sound (constrained $ \a -> satisfies a spec)

prop_constrained_explained :: HasSpec a => Specification a -> QC.Property
prop_constrained_explained spec =
  QC.forAll QC.arbitrary $ \es ->
    prop_sound $ constrained $ \x -> Explain es $ x `satisfies` spec

-- | `prop_complete ps` assumes that `ps` is satisfiable
prop_complete :: HasSpec a => Specification a -> QC.Property
prop_complete s =
  QC.forAllBlind (strictGen $ genFromSpecT s) $ \ma -> fromGEProp $ do
    a <- ma
    -- Force the value to make sure we don't crash with `error` somewhere
    -- or fall into an inifinite loop
    pure $ length (show a) > 0

prop_constrained_satisfies_complete :: HasSpec a => Specification a -> QC.Property
prop_constrained_satisfies_complete spec = prop_complete (constrained $ \a -> satisfies a spec)

prop_shrink_sound :: HasSpec a => Specification a -> QC.Property
prop_shrink_sound s =
  QC.forAll (strictGen $ genFromSpecT s) $ \ma -> fromGEDiscard $ do
    a <- ma
    let shrinks = shrinkWithSpec s a
    pure $
      QC.cover 40 (not $ null shrinks) "non-null shrinks" $
        if null shrinks
          then QC.property True
          else QC.forAll (QC.elements shrinks) $ \a' ->
            conformsToSpecProp a' s

prop_conformEmpty ::
  forall a.
  HasSpec a =>
  a ->
  QC.Property
prop_conformEmpty a = QC.property $ conformsTo a (emptySpec @a)

prop_univSound :: TestableFn -> QC.Property
prop_univSound (TestableFn (fn :: t as b)) =
  QC.label (show fn) $
    QC.forAllShrinkBlind @QC.Property (QC.arbitrary @(TestableCtx as)) QC.shrink $ \tc@(TestableCtx ctx) ->
      QC.forAllShrinkBlind QC.arbitrary QC.shrink $ \spec ->
        QC.counterexample ("\nfn ctx = " ++ showCtxWith fn tc) $
          QC.counterexample (show $ "\nspec =" <+> pretty spec) $
            let sspec = simplifySpec (propagate fn ctx spec)
             in QC.counterexample ("\n" ++ show ("propagate ctx spec =" /> pretty sspec)) $
                  QC.counterexample ("\n" ++ show (prettyPlan sspec)) $
                    QC.within 20_000_000 $
                      QC.forAllBlind (strictGen $ genFromSpecT sspec) $ \ge ->
                        fromGEDiscard $ do
                          a <- ge
                          let res = uncurryList_ unValue (semantics fn) $ fillListCtx ctx $ \HOLE -> Value a
                          pure $
                            QC.counterexample ("\ngenerated value: a = " ++ show a) $
                              QC.counterexample ("\nfn ctx[a] = " ++ show res) $
                                conformsToSpecProp res spec

main :: IO ()
main = QC.quickCheck (QC.withMaxSuccess 10000 prop_univSound)

prop_gen_sound :: forall a. HasSpec a => Specification a -> QC.Property
prop_gen_sound spec =
  let sspec = simplifySpec spec
   in QC.tabulate "specType spec" [specType spec] $
        QC.tabulate "specType (simplifySpec spec)" [specType sspec] $
          QC.counterexample ("\n" ++ show (prettyPlan sspec)) $
            QC.forAllBlind (strictGen $ genFromSpecT @a @GE sspec) $ \ge ->
              fromGEDiscard $ do
                a <- ge
                pure $
                  QC.counterexample ("\ngenerated value: a = " ++ show a) $
                    conformsToSpecProp a spec

specType :: Specification a -> String
specType (ExplainSpec [] s) = specType s
specType (ExplainSpec _ s) = "(ExplainSpec " ++ specType s ++ ")"
specType SuspendedSpec {} = "SuspendedSpec"
specType ErrorSpec {} = "ErrorSpec"
specType MemberSpec {} = "MemberSpec"
specType TypeSpec {} = "TypeSpec"
specType TrueSpec {} = "TrueSpec"

-- ============================================================
-- An abstraction that hides everything about a function symbol
-- But includes inside in the constraints, everything needed to
-- use the function symbol

showCtxWith ::
  forall fn as b.
  AppRequires fn as b =>
  fn as b ->
  TestableCtx as ->
  String
showCtxWith fn (TestableCtx ctx) = show tm
  where
    tm :: Term b
    tm =
      uncurryList (appTerm fn) $
        fillListCtx (mapListCtxC @HasSpec @_ @Value @Term (lit @_ . unValue) ctx) (\HOLE -> V $ Var 0 "v")

data TestableFn where
  TestableFn ::
    ( QC.Arbitrary (Specification b)
    , Typeable (FunTy as b)
    , AppRequires t as b
    ) =>
    t as b ->
    TestableFn

instance Show TestableFn where
  show (TestableFn (fn :: t as b)) =
    show fn ++ " :: " ++ show (typeOf (undefined :: FunTy as b))

prop_mapSpec ::
  ( HasSpec a
  , AppRequires t '[a] b
  ) =>
  t '[a] b ->
  Specification a ->
  QC.Property
prop_mapSpec funsym spec =
  QC.forAll (strictGen $ genFromSpecT spec) $ \ma -> fromGEDiscard $ do
    a <- ma
    pure $ conformsToSpec (semantics funsym a) (mapSpec funsym spec)

prop_propagateSpecSound ::
  ( HasSpec a
  , AppRequires t '[a] b
  ) =>
  t '[a] b ->
  b ->
  QC.Property
prop_propagateSpecSound funsym b =
  QC.forAll (strictGen $ genInverse (Fun funsym) TrueSpec b) $ \ma -> fromGEDiscard $ do
    a <- ma
    pure $ semantics funsym a == b

------------------------------------------------------------------------
-- Arbitrary instances for Specifications
------------------------------------------------------------------------

instance (Arbitrary (Specification a), Arbitrary (Specification b)) => Arbitrary (SumSpec a b) where
  arbitrary =
    SumSpec
      <$> frequency
        [ (3, pure Nothing)
        , (10, Just <$> ((,) <$> choose (0, 100) <*> choose (0, 100)))
        , (1, arbitrary)
        ]
      <*> arbitrary
      <*> arbitrary
  shrink (SumSpec h a b) = [SumSpec h' a' b' | (h', a', b') <- shrink (h, a, b)]

instance (Arbitrary (Specification a), Arbitrary (Specification b)) => Arbitrary (PairSpec a b) where
  arbitrary = Cartesian <$> arbitrary <*> arbitrary
  shrink (Cartesian a b) = uncurry Cartesian <$> shrink (a, b)

-- TODO: consider making this more interesting to get fewer discarded tests
-- in `prop_gen_sound`
instance
  ( Arbitrary k
  , Arbitrary v
  , Arbitrary (TypeSpec k)
  , Arbitrary (TypeSpec v)
  , Ord k
  , HasSpec k
  , Foldy v
  ) =>
  Arbitrary (MapSpec k v)
  where
  arbitrary =
    MapSpec
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> frequency [(1, pure NoFold), (1, arbitrary)]
  shrink = genericShrink

instance Arbitrary (FoldSpec (Map k v)) where
  arbitrary = pure NoFold

instance (HasSpec a, Arbitrary (TypeSpec a)) => Arbitrary (Specification a) where
  arbitrary = do
    baseSpec <-
      frequency
        [ (1, pure TrueSpec)
        ,
          ( 7
          , do
              zs <- nub <$> listOf1 (genFromSpec TrueSpec)
              pure
                ( memberSpecList
                    zs
                    ( NE.fromList
                        [ "In (Arbitrary Specification) this should never happen"
                        , "listOf1 generates empty list."
                        ]
                    )
                )
          )
        , (10, typeSpec <$> arbitrary)
        ,
          ( 1
          , do
              len <- choose (1, 5)
              TypeSpec <$> arbitrary <*> vectorOf len (genFromSpec TrueSpec)
          )
        , (1, ErrorSpec <$> arbitrary)
        , -- Recurse to make sure we apply the tricks for generating suspended specs multiple times
          (1, arbitrary)
        ]
    -- TODO: we probably want smarter ways of generating constraints
    frequency
      [ (1, pure $ constrained $ \x -> x `satisfies` baseSpec)
      , (1, ExplainSpec ["Arbitrary"] <$> arbitrary)
      ,
        ( 1
        , pure $ constrained $ \x -> exists (\eval -> pure $ eval x) $ \y ->
            [ assert $ x ==. y
            , y `satisfies` baseSpec
            ]
        )
      , (1, pure $ constrained $ \x -> letBind x $ \y -> y `satisfies` baseSpec)
      ,
        ( 1
        , pure $ constrained $ \x -> exists (\_ -> pure True) $ \b ->
            ifElse b (x `satisfies` baseSpec) (x `satisfies` baseSpec)
        )
      ,
        ( 1
        , pure $ constrained $ \x -> exists (\_ -> pure True) $ \b ->
            [ ifElse b True (x `satisfies` baseSpec)
            , x `satisfies` baseSpec
            ]
        )
      ,
        ( 1
        , pure $ constrained $ \x -> exists (\_ -> pure False) $ \b ->
            [ ifElse b (x `satisfies` baseSpec) True
            , x `satisfies` baseSpec
            ]
        )
      ,
        ( 1
        , pure $ constrained $ \x -> explanation (pure "its very subtle, you won't get it.") $ x `satisfies` baseSpec
        )
      , (10, pure baseSpec)
      ]

instance
  ( Arbitrary a
  , Arbitrary (FoldSpec a)
  , Arbitrary (TypeSpec a)
  , HasSpec a
  ) =>
  Arbitrary (ListSpec a)
  where
  arbitrary = ListSpec <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (ListSpec a b c d e) = [ListSpec a' b' c' d' e' | (a', b', c', d', e') <- shrink (a, b, c, d, e)]

instance {-# OVERLAPPABLE #-} (Arbitrary (Specification a), Foldy a) => Arbitrary (FoldSpec a) where
  arbitrary = oneof [FoldSpec (Fun IdW) <$> arbitrary, pure NoFold]
  shrink NoFold = []
  shrink (FoldSpec (Fun (getWitness -> Just IdW)) spec) = FoldSpec (Fun IdW) <$> shrink spec
  shrink FoldSpec {} = [NoFold]

instance (Ord a, Arbitrary (Specification a), Arbitrary a) => Arbitrary (SetSpec a) where
  arbitrary = SetSpec <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (SetSpec a b c) = [SetSpec a' b' c' | (a', b', c') <- shrink (a, b, c)]

-- TODO: consider improving this
instance Arbitrary (FoldSpec (Set a)) where
  arbitrary = pure NoFold

------------------------------------------------------------------------
-- Random contexts
------------------------------------------------------------------------

data TestableCtx as where
  TestableCtx ::
    HasSpec a =>
    ListCtx Value as (HOLE a) ->
    TestableCtx as

instance forall as. (All HasSpec as, TypeList as) => QC.Arbitrary (TestableCtx as) where
  arbitrary = do
    let shape = listShape @as
    idx <- QC.choose (0, lengthList shape - 1)
    go idx shape
    where
      go :: forall f as'. All HasSpec as' => Int -> List f as' -> QC.Gen (TestableCtx as')
      go 0 (_ :> as) =
        TestableCtx . (HOLE :?) <$> mapMListC @HasSpec (\_ -> Value <$> genFromSpec TrueSpec) as
      go n (_ :> as) = do
        TestableCtx ctx <- go (n - 1) as
        TestableCtx . (:! ctx) . Value <$> genFromSpec TrueSpec
      go _ _ = error "The impossible happened in Arbitrary for TestableCtx"

  shrink (TestableCtx ctx) = TestableCtx <$> shrinkCtx ctx
    where
      shrinkCtx :: forall c as'. All HasSpec as' => ListCtx Value as' c -> [ListCtx Value as' c]
      shrinkCtx (c :? as) = (c :?) <$> go as
      shrinkCtx (Value a :! ctx') = map ((:! ctx') . Value) (shrinkWithSpec TrueSpec a) ++ map (Value a :!) (shrinkCtx ctx')

      go :: forall as'. All HasSpec as' => List Value as' -> [List Value as']
      go Nil = []
      go (Value a :> as) = map ((:> as) . Value) (shrinkWithSpec TrueSpec a) ++ map (Value a :>) (go as)

genTestableFn :: QC.Gen TestableFn
genTestableFn = QC.arbitrary

instance QC.Arbitrary TestableFn where
  arbitrary =
    QC.elements
      [ -- data IntW
        TestableFn $ AddW @Int
      , TestableFn $ NegateW @Int
      , TestableFn $ SizeOfW @(Map Int Int)
      , -- data BaseW
        TestableFn $ EqualW @Int
      , TestableFn $ ProdFstW @Int @Int
      , TestableFn $ ProdSndW @Int @Int
      , TestableFn $ ProdW @Int @Int
      , TestableFn $ InjRightW @Int @Int
      , TestableFn $ InjLeftW @Int @Int
      , TestableFn $ ElemW @Int
      , TestableFn $ FromGenericW @(Either Int Bool)
      , TestableFn $ ToGenericW @(Either Int Bool)
      , -- data SetW
        TestableFn $ SingletonW @Int
      , TestableFn $ UnionW @Int
      , TestableFn $ SubsetW @Int
      , TestableFn $ MemberW @Int
      , TestableFn $ DisjointW @Int
      , TestableFn $ FromListW @Int
      , -- data BoolW
        TestableFn $ NotW
      , TestableFn $ OrW
      , -- data OrdW
        TestableFn $ LessW @Int
      , TestableFn $ LessOrEqualW @Int
      , -- data MapW
        TestableFn $ RngW @Int @Int
      , TestableFn $ DomW @Int @Int
      , TestableFn $ LookupW @Int @Int
      , -- data ListW
        TestableFn $ FoldMapW @Int (Fun IdW)
      , TestableFn $ SingletonListW @Int
      , TestableFn $ AppendW @Int
      ]
  shrink _ = []
