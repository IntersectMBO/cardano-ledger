{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Useful properties for debugging custom @HasSpec@ instances.
module Constrained.Properties where

import Constrained.API
import Constrained.Base (
  AppRequires,
  CList (..),
  Context (..),
  Ctx (Ctx, HOLE),
  Mode (..),
  appTerm,
  (/>),
 )
import Constrained.Conformance (monitorSpec)
import Constrained.Core (Value (..), Var (..), unValue)
import Constrained.GenT (GE (..), errorGE, fromGEDiscard, fromGEProp, strictGen)
import Constrained.List (
  All,
  FunTy,
  List (..),
  TypeList,
  lengthList,
  listShape,
  uncurryList,
  uncurryList_,
 )
import Constrained.Spec.ListFoldy (genInverse)
import Constrained.Syntax (PolyCList (..))
import Constrained.TheKnot
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Typeable (Typeable, typeOf)
import Prettyprinter
import qualified Test.QuickCheck as QC

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
prop_univSound (TestableFn fn) =
  QC.label (show fn) $
    QC.forAllShrinkBlind @QC.Property (genCtxForFn fn) QC.shrink $ \tc@(TestableCtx clist) ->
      QC.forAllShrinkBlind QC.arbitrary QC.shrink $ \spec ->
        QC.counterexample ("\nfn ctx = " ++ showCtxWith fn tc) $
          QC.counterexample (show $ "\nspec =" <+> pretty spec) $
            let sspec = simplifySpec (propagate (Context fn clist) spec)
             in QC.counterexample ("\n" ++ show ("propagate ctx spec =" /> pretty sspec)) $
                  QC.counterexample ("\n" ++ show (prettyPlan sspec)) $
                    QC.within 20_000_000 $
                      QC.forAllBlind (strictGen $ genFromSpecT sspec) $ \ge ->
                        fromGEDiscard $ do
                          a <- ge
                          let res = runOnCtx clist fn a
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

-- ========================================================================
-- Combining function symbols and compatible contexts in interesting ways.
-- ========================================================================

-- | Builds a Term from A FunSym and an appropriate (List Term) of args
--   computed from the TestableCtx, using fillCList, which fills the HOLE with variable 'v0'
--  (uncurryList $ appTerm ProdW) (Lit True :> Lit False :> Nil)  ===> prod_ True False
showCtxWith ::
  forall s t as b.
  (AppRequires s t as b, Fill as) =>
  t s as b ->
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
  forall s t as b x.
  (AppRequires s t as b, Fill as) =>
  CList 'Pre as x x ->
  t s as b ->
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
    , AppRequires s t as b
    ) =>
    t s as b ->
    TestableFn

genCtxForFn :: forall s t as b. AppRequires s t as b => t s as b -> QC.Gen (TestableCtx as)
genCtxForFn _ = QC.arbitrary @(TestableCtx as)

ex2 :: Prod (Value Bool) (Value Bool)
ex2 = (uncurryList (semantics ProdW)) (Value True :> Value False :> Nil)

instance Show TestableFn where
  show (TestableFn (fn :: t s as b)) =
    show fn ++ " :: " ++ show (typeOf (undefined :: FunTy as b))

-- ===========================================================
-- Random contexts
-- ===========================================================

data TestableCtx as where
  TestableCtx ::
    ( HasSpec a
    , All HasSpec as
    ) =>
    CList 'Pre as a a -> -- This is a Clist with only HOLE Ctx inside, the final 'a a' ensure this.
    TestableCtx as

-- ===========================================================
-- QC.Arbitrary instances
-- ===========================================================

genTestableCtx :: (All HasSpec as, TypeList as) => QC.Gen (TestableCtx as)
genTestableCtx = QC.arbitrary

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
      , TestableFn $ FromGenericW @Bool -- These require GenericC constraints
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
      , -- data NumOrdW
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

-- ==========================================================
-- More Properties
-- ==========================================================

prop_mapSpec ::
  ( HasSpec a
  , AppRequires s t '[a] b
  ) =>
  t s '[a] b ->
  Specification a ->
  QC.Property
prop_mapSpec funsym spec =
  QC.forAll (strictGen $ genFromSpecT spec) $ \ma -> fromGEDiscard $ do
    a <- ma
    pure $ conformsToSpec (semantics funsym a) (mapSpec funsym spec)

prop_propagateSpecSound ::
  ( HasSpec a
  , AppRequires s t '[a] b
  ) =>
  t s '[a] b ->
  b ->
  QC.Property
prop_propagateSpecSound funsym b =
  QC.forAll (strictGen $ genInverse (Fun funsym) TrueSpec b) $ \ma -> fromGEDiscard $ do
    a <- ma
    pure $ semantics funsym a == b
