{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Core where

import Cardano.Ledger.Core (EraRule)
import Control.Monad.Cont (ContT (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.State.Transition.Extended (TRC (..))
import Test.Cardano.Ledger.Conformance (ExecSpecRule (..), testConformance)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest (ImpTestM, impAnn)
import UnliftIO (MonadIO (..), evaluateDeep)

conformsToImpl ::
  forall rule era.
  ExecSpecRule rule era =>
  ImpTestM era (ExecContext rule era, TRC (EraRule rule era)) ->
  Property
conformsToImpl genInputs = property @(ImpTestM era Property) . (`runContT` pure) $ do
  let
    deepEvalAnn s = "Deep evaluating " <> s
    deepEval x s = do
      _ <- lift $ impAnn (deepEvalAnn s) (liftIO (evaluateDeep x))
      pure ()
  (ctx, trc@(TRC (env, st, sig))) <- lift genInputs
  deepEval ctx "context"
  deepEval env "environment"
  deepEval st "state"
  deepEval sig "signal"
  pure $ testConformance @rule @era ctx trc
