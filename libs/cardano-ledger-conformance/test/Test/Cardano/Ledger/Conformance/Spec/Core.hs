{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Spec.Core (
  genFromBundle_,
  genFromBundle,
  conformsToImplConstrained,
  conformsToImplConstrained_,
) where

import Cardano.Ledger.Dijkstra.Core (EraRule)
import Constrained.Base (HasSpec)
import Constrained.Generation (genFromSpec)
import Control.State.Transition.Extended (STS (..), TRC (..))
import Test.Cardano.Ledger.Common (Arbitrary (..), Gen, Property)
import Test.Cardano.Ledger.Conformance (ExecSpecRule (..))
import Test.Cardano.Ledger.Conformance.Core (conformsToImpl)
import Test.Cardano.Ledger.Constrained.Conway.MiniTrace (ConstrainedGeneratorBundle (..))
import Test.Cardano.Ledger.Conway.ImpTest (ImpTestM)
import Test.Cardano.Ledger.Imp.Common (MonadGen (..))

genFromBundle_ ::
  ( HasSpec (Environment (EraRule rule era))
  , HasSpec (State (EraRule rule era))
  , Arbitrary (ExecContext rule era)
  ) =>
  ConstrainedGeneratorBundle ctx rule era ->
  ImpTestM era (ExecContext rule era, TRC (EraRule rule era))
genFromBundle_ x = liftGen . genFromBundle x $ \_ _ _ _ -> liftGen arbitrary

genFromBundle ::
  ( HasSpec (Environment (EraRule rule era))
  , HasSpec (State (EraRule rule era))
  ) =>
  ConstrainedGeneratorBundle ctx rule era ->
  ( ctx ->
    Environment (EraRule rule era) ->
    State (EraRule rule era) ->
    Signal (EraRule rule era) ->
    Gen (ExecContext rule era)
  ) ->
  Gen (ExecContext rule era, TRC (EraRule rule era))
genFromBundle ConstrainedGeneratorBundle {..} genExecContext = do
  ctx <- cgbContextGen
  env <- genFromSpec $ cgbEnvironmentSpec ctx
  st <- genFromSpec $ cgbStateSpec ctx env
  sig <- cgbSignalGen ctx env st
  (,TRC (env, st, sig)) <$> genExecContext ctx env st sig

conformsToImplConstrained ::
  forall ctx rule era.
  ( ExecSpecRule rule era
  , HasSpec (Environment (EraRule rule era))
  , HasSpec (State (EraRule rule era))
  ) =>
  ConstrainedGeneratorBundle ctx rule era ->
  ( ctx ->
    Environment (EraRule rule era) ->
    State (EraRule rule era) ->
    Signal (EraRule rule era) ->
    Gen (ExecContext rule era)
  ) ->
  Property
conformsToImplConstrained bundle genExecContext =
  conformsToImpl @rule @era . liftGen $ genFromBundle bundle genExecContext

conformsToImplConstrained_ ::
  ( ExecSpecRule rule era
  , HasSpec (Environment (EraRule rule era))
  , HasSpec (State (EraRule rule era))
  , Arbitrary (ExecContext rule era)
  ) =>
  ConstrainedGeneratorBundle ctx rule era ->
  Property
conformsToImplConstrained_ bundle = conformsToImplConstrained bundle $ \_ _ _ _ -> liftGen arbitrary
