{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Useful properties for debugging custom @HasSpec@ instances.
module Constrained.Experiment.Properties where

import Constrained.Core (Evidence (..), Value (..), Var (..), unValue)
import Constrained.Experiment.API
import Constrained.Experiment.Base (
  AppRequires,
  CList (..),
  Ctx (Ctx, HOLE),
  Fun (..),
  Literal,
  Mode (..),
  appTerm,
 )
import Constrained.Experiment.Conformance
import Constrained.Experiment.Specs.ListFoldy (genInverse)
import Constrained.Experiment.Syntax (PolyCList (..))
import Constrained.Experiment.TheKnot
import Constrained.GenT
import Constrained.List
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Set (Set)
import Data.Typeable (Typeable, typeOf)
import Prettyprinter
import Test.QuickCheck (Arbitrary (..), oneof)
import Test.QuickCheck qualified as QC

conformsToSpecProp :: forall a. HasSpec a => a -> Specification a -> QC.Property
conformsToSpecProp a s = case conformsToSpecE a (simplifySpec s) (pure "call to conformsToSpecProp") of
  Nothing -> QC.property True
  Just msgs -> QC.counterexample (unlines (NE.toList msgs)) False

forAllSpecShow ::
  (HasSpec a, QC.Testable p) => Specification a -> (a -> String) -> (a -> p) -> QC.Property
forAllSpecShow spec pp prop =
  let sspec = simplifySpec spec
   in QC.forAllShrinkShow (genFromSpec sspec) (shrinkWithSpec sspec) pp $ \a ->
        monitorSpec spec a $ prop a

forAllSpec :: (HasSpec a, QC.Testable p) => Specification a -> (a -> p) -> QC.Property
forAllSpec spec prop = forAllSpecShow spec show prop

forAllSpecDiscard :: (HasSpec a, QC.Testable p) => Specification a -> (a -> p) -> QC.Property
forAllSpecDiscard spec prop =
  let sspec = simplifySpec spec
   in QC.forAllShrinkBlind
        (strictGen $ genFromSpecT @_ @GE sspec)
        (map pure . shrinkWithSpec sspec . errorGE)
        $ \ge ->
          fromGEDiscard $ do
            a <- ge
            pure $ QC.counterexample (show a) $ prop a

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

{-
prop_constrained_explained :: HasSpec a => Specification a -> QC.Property
prop_constrained_explained spec =
  QC.forAll QC.arbitrary $ \es ->
    prop_sound $ constrained $ \x -> Explain es $ x `satisfies` spec
-}

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

{-
prop_univSound :: forall as b.  TestableFn -> QC.Property
prop_univSound (TestableFn fn) =
  QC.label (show fn) $
    QC.forAllShrinkBlind QC.arbitrary QC.shrink $ \ tc@(TestableCtx clist :: TestableCtx as) ->
      QC.forAllShrinkBlind QC.arbitrary QC.shrink $ \(spec :: Specification a) ->
        QC.counterexample ("\nfn ctx = " ++ showCtxWith fn tc) $
          QC.counterexample (show $ "\nspec =" <+> pretty spec) $
            let sspec = simplifySpec (propagate (Context Evidence fn clist) spec)
             in QC.counterexample ("\n" ++ show ("propagate ctx spec =" /> pretty sspec)) $
                  QC.counterexample ("\n" ++ show (prettyPlan sspec)) $
                    QC.within 20_000_000 $
                      QC.forAllBlind (strictGen $ genFromSpecT @_ @_ sspec) $ \ge ->
                        fromGEDiscard $ do
                          a <- ge
                          let res = runOnCtx clist fn a
                                    -- uncurryList_ (semantics fn) $ fillCList (\HOLE -> Value a) Value clist
                          pure $
                            QC.counterexample ("\ngenerated value: a = " ++ show a) $
                              QC.counterexample ("\nfn ctx[a] = " ++ show res) $
                                conformsToSpecProp res spec
-}

prop_gen_sound :: forall a. HasSpec a => Specification a -> QC.Property
prop_gen_sound spec =
  let sspec = simplifySpec spec
   in QC.tabulate "specType spec" [specType spec] $
        QC.tabulate "specType (simplifySpec spec)" [specType sspec] $
          QC.counterexample ("\n" ++ show (prettyPlan sspec)) $
            QC.forAllBlind (strictGen $ genFromSpecT @_ @GE sspec) $ \ge ->
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

-- ========================================================================
-- Combining function symbols and compatible contexts in interesting ways.
-- ========================================================================

-- | Builds a Term from A FunSym and an appropriate (List Term) of args
--   computed from the TestableCtx, using fillCList, which fills the HOLE with variable 'v0'
--  (uncurryList $ appTerm ProdW) (Lit True :> Lit False :> Nil)  ===> prod_ True False
showCtxWith ::
  forall c s t as b.
  (AppRequires c s t as b, Fill as) =>
  t c s as b ->
  TestableCtx as ->
  String
showCtxWith fn (TestableCtx ctx) = show $ pretty tm
  where
    tm :: Term b
    tm =
      uncurryList (appTerm fn) $
        fillCList
          (\case HOLE -> V $ Var 0 "v"; Ctx {} -> error ("Impossible, only HOLE has type (Ctx hole hole)"))
          Lit
          ctx

-- | Takes a CList, replaces the hole with 'x', and turns it into a (List Value as), then uses the
--   semantics of the function symbol 'fn' applying it to each item in the List in turn,
--   to get a resulting answer of type 'b'
runOnCtx ::
  forall c s t as b x.
  (AppRequires c s t as b, Fill as) =>
  CList Pre as x x ->
  t c s as b ->
  x ->
  b
runOnCtx clist fn x = uncurryList_ unValue (semantics fn) (fillCList fillWithX Value clist)
  where
    fillWithX HOLE = Value x
    fillWithX _ = error ("Impossible, only HOLE has type (Ctx hole hole)")

-- | Fill in the HOLE in a CList, and then turn it into a regular List
class Fill as where
  fillCList ::
    forall f hole m.
    (Ctx hole hole -> f hole) -> (forall a. Literal a => a -> f a) -> CList m as hole hole -> List f as

instance Fill '[] where
  fillCList _ _ End = Nil

instance Fill as => Fill (a : as) where
  fillCList f g (x :|> xs) = g x :> fillCList @as f g xs
  fillCList f g (x :<| xs) = g x :> fillCList @as f g xs
  fillCList f g (c :<> xs) = f c :> fillCList @as f g xs

-- ============================================================
-- An abstraction that hides everything about a function symbol
-- But includes inside in the constraints, everything needed to
-- use the function symbol

data TestableFn where
  TestableFn ::
    ( Fill as
    , QC.Arbitrary (Specification b)
    , Typeable (FunTy as b)
    , AppRequires c s t as b
    ) =>
    t c s as b ->
    TestableFn

ex1 :: String
ex1 = showCtxWith EqualW (TestableCtx (HOLE :<> True :<| End))

ex2 :: Prod (Value Bool) (Value Bool)
ex2 = (uncurryList (semantics ProdW)) (Value True :> Value False :> Nil)

instance Show TestableFn where
  show (TestableFn (fn :: t c s as b)) =
    show fn ++ " :: " ++ show (typeOf (undefined :: FunTy as b))

-- ===========================================================
-- Random contexts
-- ===========================================================

data TestableCtx as where
  TestableCtx ::
    ( HasSpec a
    , All HasSpec as
    ) =>
    CList Pre as a a -> -- This is a Clist with only HOLE Ctx inside, the final 'a a' ensure this.
    TestableCtx as

instance forall as. (All HasSpec as, TypeList as) => QC.Arbitrary (TestableCtx as) where
  arbitrary = do
    let shape = listShape @as
    idx <- QC.choose (0, lengthList shape - 1)
    genPre idx shape
  shrink (TestableCtx clist) = map TestableCtx (shrinkCList clist)

genPre :: forall f as. All HasSpec as => Int -> List f as -> QC.Gen (TestableCtx as)
genPre _ Nil = error ("The impossible happened, unless we chose 'idx' in the Arbitrary instance out of bounds")
genPre n (_ :> xs) | n <= 0 = do Poly ys <- genPost xs; pure $ TestableCtx (HOLE :<> ys)
genPre n ((_ :: f a) :> xs) =
  do
    TestableCtx ys <- genPre (n - 1) xs
    x <- genFromSpec (TrueSpec @a)
    pure $ (TestableCtx (x :|> ys))

{- An unfolding of genPre at type '[a,b] might look something like this

instance (HasSpec b,HasSpec a) => Arbitrary (TestableCtx '[a,b]) where
  arbitrary = oneof [ do l <- genFromSpec @a TrueSpec
                         pure $ TestableCtx  (l :|> HOLE :<> End)
                    , do r <- genFromSpec @b TrueSpec
                         pure $ TestableCtx  (HOLE :<> r :<| End)
                    ]
  shrink (TestableCtx cl) = TestableCtx <$> shrinkCList cl
-}

genPost :: forall f as. All HasSpec as => List f as -> QC.Gen (PolyCList as)
genPost Nil = pure (Poly End)
genPost ((_ :: f a) :> xs) =
  do
    Poly ys <- genPost xs
    n <- genFromSpec (TrueSpec @a)
    pure (Poly (n :<| ys))

shrinkCList :: All HasSpec as => CList m as x y -> [CList m as x y]
shrinkCList (HOLE :<> End) = []
shrinkCList (x :|> HOLE :<> End) = [x' :|> HOLE :<> End | x' <- shrinkWithSpec TrueSpec x]
shrinkCList (HOLE :<> x :<| End) = [HOLE :<> x' :<| End | x' <- shrinkWithSpec TrueSpec x]
shrinkCList (a :|> b :|> HOLE :<> End) =
  [ (a' :|> b' :|> HOLE :<> End)
  | a' <- shrinkWithSpec TrueSpec a
  , b' <- shrinkWithSpec TrueSpec b
  ]
shrinkCList (a :|> HOLE :<> c :<| End) =
  [ (a' :|> HOLE :<> c' :<| End)
  | a' <- shrinkWithSpec TrueSpec a
  , c' <- shrinkWithSpec TrueSpec c
  ]
shrinkCList (HOLE :<> b :<| c :<| End) =
  [ (HOLE :<> b' :<| c' :<| End)
  | b' <- shrinkWithSpec TrueSpec b
  , c' <- shrinkWithSpec TrueSpec c
  ]
shrinkCList (a :|> b :|> c :|> HOLE :<> End) =
  [ (a' :|> b' :|> c' :|> HOLE :<> End)
  | a' <- shrinkWithSpec TrueSpec a
  , b' <- shrinkWithSpec TrueSpec b
  , c' <- shrinkWithSpec TrueSpec c
  ]
shrinkCList (a :|> b :|> HOLE :<> d :<| End) =
  [ (a' :|> b' :|> HOLE :<> d' :<| End)
  | a' <- shrinkWithSpec TrueSpec a
  , b' <- shrinkWithSpec TrueSpec b
  , d' <- shrinkWithSpec TrueSpec d
  ]
shrinkCList (a :|> HOLE :<> c :<| d :<| End) =
  [ (a' :|> HOLE :<> c' :<| d' :<| End)
  | a' <- shrinkWithSpec TrueSpec a
  , c' <- shrinkWithSpec TrueSpec c
  , d' <- shrinkWithSpec TrueSpec d
  ]
shrinkCList (HOLE :<> b :<| c :<| d :<| End) =
  [ (HOLE :<> b' :<| c' :<| d' :<| End)
  | b' <- shrinkWithSpec TrueSpec b
  , c' <- shrinkWithSpec TrueSpec c
  , d' <- shrinkWithSpec TrueSpec d
  ]
shrinkCList clist = error ("CList length greater than 4\n" ++ show clist)

-- =========================================================================
-- Pairs a function symbol and a random, but appropriate Context

data SoundCase where
  SoundCase ::
    forall as b c s t.
    (Fill as, QC.Arbitrary (Specification b), Typeable (FunTy as b), AppRequires c s t as b) =>
    t c s as b -> TestableCtx as -> SoundCase

instance Arbitrary SoundCase where
  arbitrary =
    oneof
      [ SoundCase (AddW @Int) <$> arbitrary
      , SoundCase (NegateW @Int) <$> arbitrary
      , SoundCase (LessW @Int) <$> arbitrary
      , SoundCase (LessOrEqualW @Int) <$> arbitrary
      , SoundCase (GreaterW @Int) <$> arbitrary
      , SoundCase (GreaterOrEqualW @Int) <$> arbitrary
      , SoundCase (SizeOfW @[Int]) <$> arbitrary
      , SoundCase (SizeOfW @(Set Int)) <$> arbitrary
      , SoundCase (SizeOfW @(Map Int Int)) <$> arbitrary
      , SoundCase (InjLeftW @Int @Integer) <$> arbitrary
      , SoundCase (InjRightW @Int @Int) <$> arbitrary
      , SoundCase (ProdW @Int @Bool) <$> arbitrary
      , SoundCase (ProdFstW @Int @Int) <$> arbitrary
      , SoundCase (ProdSndW @Int @Int) <$> arbitrary
      , SoundCase (EqualW @Bool) <$> arbitrary
      , SoundCase (EqualW @Int) <$> arbitrary
      , SoundCase (ToGenericW @(Int, Int)) <$> arbitrary
      , SoundCase (FromGenericW @(Int, Int)) <$> arbitrary
      , SoundCase (ElemW @Int) <$> arbitrary
      , SoundCase (SingletonW @Int) <$> arbitrary
      , SoundCase (UnionW @Int) <$> arbitrary
      , SoundCase (SubsetW @Int) <$> arbitrary
      , SoundCase (MemberW @Int) <$> arbitrary
      , SoundCase (DisjointW @Int) <$> arbitrary
      , SoundCase (FromListW @Int) <$> arbitrary
      , SoundCase NotW <$> arbitrary
      , SoundCase OrW <$> arbitrary
      , SoundCase (FoldMapW @Int @Int (Fun Evidence IdW)) <$> arbitrary
      , SoundCase (SingletonListW @Int) <$> arbitrary
      , SoundCase (AppendW @Int) <$> arbitrary
      , SoundCase (DomW @Int @Int) <$> arbitrary
      , SoundCase (RngW @Int @Int) <$> arbitrary
      , SoundCase (LookupW @Int @Int) <$> arbitrary
      , SoundCase (IdW @Int) <$> arbitrary
      -- , SoundCase (ComposeW (ProdFstW @Int @Int) ToGenericW) <$> arbitrary
      -- , SoundCase (ComposeW (ProdSndW @Bool @Integer) ToGenericW) <$> arbitrary
      -- , SoundCase (FlipW (ProdW @Int @Int)) <$> arbitrary
      -- , SoundCase (FlipW (LessW @Int)) <$> arbitrary
      -- , SoundCase (ComposeW FromGenericW (ProdFstW @Int @Int)) <$> arbitrary
      ]

instance QC.Arbitrary TestableFn where
  arbitrary =
    QC.elements
      [ TestableFn $ AddW @Int
      , TestableFn $ NegateW @Int
      , TestableFn $ SizeOfW @(Map Int Int)
      , TestableFn $ MemberW @Int
      , TestableFn $ NotW
      , TestableFn $ OrW
      , TestableFn $ DisjointW @Int
      , TestableFn $ SubsetW @Int
      , TestableFn $ ElemW @Int
      , TestableFn $ LessW @Int
      , TestableFn $ LessOrEqualW @Int
      , TestableFn $ EqualW @Int
      , TestableFn $ ProdFstW @Int @Int
      , TestableFn $ ProdSndW @Int @Int
      , TestableFn $ ProdW @Int @Int
      , TestableFn $ InjRightW @Int @Int
      , TestableFn $ InjLeftW @Int @Int
      , TestableFn $ SingletonW @Int
      , TestableFn $ UnionW @Int
      , TestableFn $ FoldMapW @Int (Fun Evidence IdW)
      , TestableFn $ RngW @Int @Int
      , TestableFn $ DomW @Int @Int
      , TestableFn $ FromListW @Int
      , TestableFn $ LookupW @Int @Int
      , TestableFn $ SingletonListW @Int
      , TestableFn $ AppendW @Int
      ]

prop_mapSpec ::
  ( HasSpec a
  , AppRequires c s t '[a] b
  ) =>
  t c s '[a] b ->
  Specification a ->
  QC.Property
prop_mapSpec funsym spec =
  QC.forAll (strictGen $ genFromSpecT spec) $ \ma -> fromGEDiscard $ do
    a <- ma
    pure $ conformsToSpec (semantics funsym a) (mapSpec funsym spec)

prop_propagateSpecSound ::
  ( HasSpec a
  , AppRequires c s t '[a] b
  ) =>
  t c s '[a] b ->
  b ->
  QC.Property
prop_propagateSpecSound funsym b =
  QC.forAll (strictGen $ genInverse (Fun Evidence funsym) TrueSpec b) $ \ma -> fromGEDiscard $ do
    a <- ma
    pure $ semantics funsym a == b
