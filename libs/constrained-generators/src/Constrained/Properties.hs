{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}

-- | Useful of helpers for writing properties with constrained generators
module Constrained.Properties (
  conformsToSpecProp,
  forAllSpec,
  forAllSpecShow,
  forAllSpecDiscard,
) where

import Constrained.Base
import Constrained.Conformance
import Constrained.GenT
import Constrained.Generation
import qualified Data.List.NonEmpty as NE
import qualified Test.QuickCheck as QC

-- | Like @Constrained.Conformance.conformsToSpec@ but in @Test.QuickCheck.Property@ form.
conformsToSpecProp :: forall a. HasSpec a => a -> Specification a -> QC.Property
conformsToSpecProp a s = case conformsToSpecE a (simplifySpec s) (pure "call to conformsToSpecProp") of
  Nothing -> QC.property True
  Just msgs -> QC.counterexample (unlines (NE.toList msgs)) False

-- | Quanitfy over a @Constrained.Base.Specification@.
forAllSpec :: (HasSpec a, QC.Testable p) => Specification a -> (a -> p) -> QC.Property
forAllSpec spec prop = forAllSpecShow spec show prop

-- | Like `forAllSpec` with a custom way of printing values
forAllSpecShow ::
  (HasSpec a, QC.Testable p) => Specification a -> (a -> String) -> (a -> p) -> QC.Property
forAllSpecShow spec pp prop =
  let sspec = simplifySpec spec
   in QC.forAllShrinkShow (genFromSpec sspec) (shrinkWithSpec sspec) pp $ \a ->
        monitorSpec spec a $ prop a

-- | Quanitfy over a @Constrained.Base.Specification@ and discard any test where generation fails.
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
