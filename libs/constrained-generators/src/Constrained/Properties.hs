{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Useful properties for debugging custom @HasSpec@ instances.
module Constrained.Properties where

import Constrained.Internals
import Test.QuickCheck qualified as QC

prop_sound ::
  HasSpec fn a =>
  Specification fn a ->
  QC.Property
prop_sound spec =
  QC.forAllBlind (strictGen $ genFromSpec spec) $ \ma ->
    case ma of
      Result _ a -> QC.cover 80 True "successful" $ QC.counterexample (show a) $ conformsToSpecProp a spec
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

prop_propagateSpec ::
  (HasSpec fn a, HasSpec fn b) =>
  Var b ->
  Term fn a ->
  Specification fn a ->
  QC.Property
prop_propagateSpec var tm spec =
  let spec' = errorGE $ do
        ctx <- toCtx var tm
        pure $ propagateSpec spec ctx
   in QC.forAll (strictGen $ genFromSpec spec') $ \geval -> fromGE (\err -> QC.counterexample (unlines err) False) $ do
        val <- geval
        res <- runTerm (singletonEnv var val) tm
        pure $ QC.property $ conformsToSpec res spec

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
