{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Spec.Core where

import Cardano.Ledger.Core (EraRule)
import Constrained.API
import Control.Monad.Cont (ContT (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.State.Transition.Extended (STS (..), TRC (..))
import Test.Cardano.Ledger.Conformance (ExecSpecRule (..), ForAllExecTypes, testConformance)
import Test.Cardano.Ledger.Constrained.Conway.MiniTrace (
  ConstrainedGeneratorBundle (..),
 )
import Test.Cardano.Ledger.Conway.ImpTest (ImpTestM, impAnn)
import Test.Cardano.Ledger.Imp.Common
import UnliftIO (MonadIO (..), evaluateDeep)

conformsToImpl ::
  forall rule era.
  ExecSpecRule rule era =>
  Gen (ExecContext rule era, TRC (EraRule rule era)) ->
  Property
conformsToImpl genInputs = property @(ImpTestM era Property) . (`runContT` pure) $ do
  let
    deepEvalAnn s = "Deep evaluating " <> s
    deepEval x s = do
      _ <- lift $ impAnn (deepEvalAnn s) (liftIO (evaluateDeep x))
      pure ()
  (ctx, trc@(TRC (env, st, sig))) <- lift $ liftGen genInputs
  deepEval ctx "context"
  deepEval env "environment"
  deepEval st "state"
  deepEval sig "signal"
  pure $ testConformance @rule @era ctx trc

genFromBundle_ ::
  ( HasSpec (Environment (EraRule rule era))
  , HasSpec (State (EraRule rule era))
  , HasSpec (Signal (EraRule rule era))
  , Arbitrary (ExecContext rule era)
  ) =>
  ConstrainedGeneratorBundle ctx rule era ->
  Gen (ExecContext rule era, TRC (EraRule rule era))
genFromBundle_ x = genFromBundle x $ \_ _ _ _ -> arbitrary

genFromBundle ::
  ForAllExecTypes HasSpec rule era =>
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
  sig <- genFromSpec $ cgbSignalSpec ctx env st
  (,TRC (env, st, sig)) <$> genExecContext ctx env st sig

conformsToImplConstrained ::
  forall ctx rule era.
  ( ExecSpecRule rule era
  , ForAllExecTypes HasSpec rule era
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
  conformsToImpl @rule @era $ genFromBundle bundle genExecContext

conformsToImplConstrained_ ::
  ( ExecSpecRule rule era
  , ForAllExecTypes HasSpec rule era
  , Arbitrary (ExecContext rule era)
  ) =>
  ConstrainedGeneratorBundle ctx rule era ->
  Property
conformsToImplConstrained_ bundle = conformsToImplConstrained bundle $ \_ _ _ _ -> arbitrary
