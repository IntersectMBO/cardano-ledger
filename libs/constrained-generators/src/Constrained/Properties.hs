{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Useful properties for debugging custom @HasSpec@ instances.
module Constrained.Properties where

import Constrained.Base
import Constrained.Internals
import Constrained.List
import Data.Map (Map)
import Data.Typeable
import Prettyprinter
import Test.QuickCheck qualified as QC

forAllSpecShow ::
  (HasSpec fn a, QC.Testable p) => Specification fn a -> (a -> String) -> (a -> p) -> QC.Property
forAllSpecShow spec pp prop =
  let sspec = simplifySpec spec
   in QC.forAllShrinkShow (genFromSpec_ sspec) (shrinkWithSpec sspec) pp $ \a ->
        monitorSpec spec a $ prop a

forAllSpec :: (HasSpec fn a, QC.Testable p) => Specification fn a -> (a -> p) -> QC.Property
forAllSpec spec prop = forAllSpecShow spec show prop

forAllSpecDiscard :: (HasSpec fn a, QC.Testable p) => Specification fn a -> (a -> p) -> QC.Property
forAllSpecDiscard spec prop =
  let sspec = simplifySpec spec
   in QC.forAllShrinkBlind
        (strictGen $ genFromSpec @_ @_ @GE sspec)
        (map pure . shrinkWithSpec sspec . errorGE)
        $ \ge ->
          fromGEDiscard $ do
            a <- ge
            pure $ QC.counterexample (show a) $ prop a

prop_sound ::
  HasSpec fn a =>
  Specification fn a ->
  QC.Property
prop_sound spec =
  QC.forAllBlind (strictGen $ genFromSpec spec) $ \ma ->
    case ma of
      Result _ a ->
        QC.cover 80 True "successful" $
          QC.counterexample (show a) $
            monitorSpec spec a $
              conformsToSpecProp a spec
      _ -> QC.cover 80 False "successful" True

prop_constrained_satisfies_sound :: HasSpec fn a => Specification fn a -> QC.Property
prop_constrained_satisfies_sound spec = prop_sound (constrained $ \a -> satisfies a spec)

-- | `prop_complete ps` assumes that `ps` is satisfiable
prop_complete :: HasSpec fn a => Specification fn a -> QC.Property
prop_complete s =
  QC.forAllBlind (strictGen $ genFromSpec s) $ \ma -> fromGEProp $ do
    a <- ma
    -- Force the value to make sure we don't crash with `error` somewhere
    -- or fall into an inifinite loop
    pure $ length (show a) > 0

prop_constrained_satisfies_complete :: HasSpec fn a => Specification fn a -> QC.Property
prop_constrained_satisfies_complete spec = prop_complete (constrained $ \a -> satisfies a spec)

prop_shrink_sound :: HasSpec fn a => Specification fn a -> QC.Property
prop_shrink_sound s =
  QC.forAll (strictGen $ genFromSpec s) $ \ma -> fromGEDiscard $ do
    a <- ma
    let shrinks = shrinkWithSpec s a
    pure $
      QC.cover 40 (not $ null shrinks) "non-null shrinks" $
        if null shrinks
          then QC.property True
          else QC.forAll (QC.elements shrinks) $ \a' ->
            conformsToSpecProp a' s

prop_conformEmpty ::
  forall fn a.
  HasSpec fn a =>
  a ->
  QC.Property
prop_conformEmpty a = QC.property $ conformsTo @fn a (emptySpec @fn @a)

prop_univSound :: forall fn. TestableFn fn -> QC.Property
prop_univSound (TestableFn fn) =
  QC.label (show fn) $
    QC.forAllShrinkBlind QC.arbitrary QC.shrink $ \(TestableCtx ctx :: TestableCtx fn as) ->
      QC.forAllShrinkBlind QC.arbitrary QC.shrink $ \(spec :: Specification fn a) ->
        QC.counterexample ("\nfn ctx = " ++ showCtxWith fn (TestableCtx ctx)) $
          QC.counterexample (show $ "\nspec =" <+> pretty spec) $
            let sspec = simplifySpec (propagateSpecFun fn ctx spec)
             in QC.counterexample ("\n" ++ show ("propagateSpecFun fn ctx spec =" /> pretty sspec)) $
                  QC.counterexample ("\n" ++ show (prettyPlan sspec)) $
                    QC.within 10_000_000 $
                      QC.forAllBlind (strictGen $ genFromSpec @_ @_ @GE sspec) $ \ge ->
                        fromGEDiscard $ do
                          a <- ge
                          let res = uncurryList_ unValue (sem fn) $ fillListCtx ctx $ \HOLE -> Value a
                          pure $
                            QC.counterexample ("\ngenerated value: a = " ++ show a) $
                              QC.counterexample ("\nfn ctx[a] = " ++ show res) $
                                conformsToSpecProp res spec

prop_gen_sound :: forall fn a. HasSpec fn a => Specification fn a -> QC.Property
prop_gen_sound spec =
  let sspec = simplifySpec spec
   in QC.tabulate "specType spec" [specType spec] $
        QC.tabulate "specType (simplifySpec spec)" [specType sspec] $
          QC.counterexample ("\n" ++ show ("simplifySpec spec =" /> pretty sspec)) $
            QC.within 10_000_000 $
              QC.forAllBlind (strictGen $ genFromSpec @_ @_ @GE sspec) $ \ge ->
                fromGEDiscard $ do
                  a <- ge
                  pure $
                    QC.counterexample ("\ngenerated value: a = " ++ show a) $
                      conformsToSpecProp a spec

specType :: Specification fn a -> String
specType TrueSpec {} = "TrueSpec"
specType SuspendedSpec {} = "SuspendedSpec"
specType ErrorSpec {} = "ErrorSpec"
specType MemberSpec {} = "MemberSpec"
specType TypeSpec {} = "TypeSpec"

showCtxWith ::
  forall fn as b.
  (Typeable as, TypeList as, All (HasSpec fn) as, HasSpec fn b) =>
  fn as b ->
  TestableCtx fn as ->
  String
showCtxWith fn (TestableCtx ctx) = show $ pretty tm
  where
    tm :: Term fn b
    tm =
      uncurryList (app fn) $
        fillListCtx (mapListCtxC @(HasSpec fn) (lit @_ @fn . unValue) ctx) (\HOLE -> V $ Var 0)

data TestableFn fn where
  TestableFn ::
    ( All (HasSpec fn) as
    , TypeList as
    , HasSpec fn b
    , Typeable as
    , QC.Arbitrary (Specification fn b)
    , Typeable (FunTy as b)
    ) =>
    fn as b ->
    TestableFn fn

instance BaseUniverse fn => Show (TestableFn fn) where
  show (TestableFn (fn :: fn as b)) =
    show fn ++ " :: " ++ show (typeOf (undefined :: FunTy as b))

data TestableCtx fn as where
  TestableCtx ::
    HasSpec fn a =>
    ListCtx Value as (HOLE a) ->
    TestableCtx fn as

instance forall fn as. (All (HasSpec fn) as, TypeList as) => QC.Arbitrary (TestableCtx fn as) where
  arbitrary = do
    let shape = listShape @as
    idx <- QC.choose (0, lengthList shape - 1)
    go idx shape
    where
      go :: forall f as'. All (HasSpec fn) as' => Int -> List f as' -> QC.Gen (TestableCtx fn as')
      go 0 (_ :> as) =
        TestableCtx . (HOLE :?) <$> mapMListC @(HasSpec fn) (\_ -> Value <$> genFromSpec_ @fn TrueSpec) as
      go n (_ :> as) = do
        TestableCtx ctx <- go (n - 1) as
        TestableCtx . (:! ctx) . Value <$> genFromSpec_ @fn TrueSpec
      go _ _ = error "The impossible happened in Arbitrary for TestableCtx"

  shrink (TestableCtx ctx) = TestableCtx <$> shrinkCtx ctx
    where
      shrinkCtx :: forall c as'. All (HasSpec fn) as' => ListCtx Value as' c -> [ListCtx Value as' c]
      shrinkCtx (c :? as) = (c :?) <$> go as
      shrinkCtx (Value a :! ctx') = map ((:! ctx') . Value) (shrinkWithSpec @fn TrueSpec a) ++ map (Value a :!) (shrinkCtx ctx')

      go :: forall as'. All (HasSpec fn) as' => List Value as' -> [List Value as']
      go Nil = []
      go (Value a :> as) = map ((:> as) . Value) (shrinkWithSpec @fn TrueSpec a) ++ map (Value a :>) (go as)

-- TODO: we should improve these
instance fn ~ BaseFn => QC.Arbitrary (TestableFn fn) where
  arbitrary =
    QC.elements
      [ TestableFn $ addFn @fn @Int
      , TestableFn $ negateFn @fn @Int
      , TestableFn $ sizeOfFn @fn @(Map Int Int)
      , TestableFn $ memberFn @fn @Int
      , TestableFn $ notFn @fn
      , TestableFn $ disjointFn @fn @Int
      , TestableFn $ subsetFn @fn @Int
      , TestableFn $ subsetFn @fn @Int
      , TestableFn $ elemFn @fn @Int
      , TestableFn $ orFn @fn
      , TestableFn $ lessFn @fn @Int
      , TestableFn $ lessOrEqualFn @fn @Int
      , TestableFn $ equalFn @fn @Int
      , TestableFn $ fstFn @fn @Int @Int
      , TestableFn $ sndFn @fn @Int @Int
      , TestableFn $ pairFn @fn @Int @Int
      , TestableFn $ injRightFn @fn @Int @Int
      , TestableFn $ singletonFn @fn @Int
      , TestableFn $ unionFn @fn @Int
      , TestableFn $ foldMapFn @fn @Int idFn
      , TestableFn $ rngFn @fn @Int @Int
      , TestableFn $ domFn @fn @Int @Int
      , TestableFn $ fromListFn @fn @Int
      ]

prop_mapSpec ::
  ( HasSpec fn a
  , HasSpec fn b
  ) =>
  fn '[a] b ->
  Specification fn a ->
  QC.Property
prop_mapSpec fn spec =
  QC.forAll (strictGen $ genFromSpec spec) $ \ma -> fromGEDiscard $ do
    a <- ma
    pure $ conformsToSpec (sem fn a) (mapSpec fn spec)

prop_propagateSpecSound ::
  ( HasSpec fn a
  , HasSpec fn b
  ) =>
  fn '[a] b ->
  b ->
  QC.Property
prop_propagateSpecSound fn b =
  QC.forAll (strictGen $ genInverse fn TrueSpec b) $ \ma -> fromGEDiscard $ do
    a <- ma
    pure $ sem fn a == b
