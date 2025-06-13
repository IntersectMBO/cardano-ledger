{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}

-- | Useful of helpers for writing properties with constrained generators
module Constrained.Properties where

import Constrained.API
import Constrained.Base ()
import Constrained.Conformance (
  monitorSpec,
 )
import Constrained.GenT (
  GE (..),
  errorGE,
  fromGEDiscard,
  strictGen,
 )
import Constrained.Generation
import Constrained.NumOrd ()
import Constrained.Spec.Set ()
import Constrained.Spec.SumProd ()
import qualified Data.List.NonEmpty as NE
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
