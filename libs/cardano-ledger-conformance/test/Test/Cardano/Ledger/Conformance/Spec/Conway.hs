{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Spec.Conway (spec) where

import Cardano.Ledger.Core (EraRule)
import Constrained.API
import Control.Monad.Cont (ContT (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.State.Transition.Extended (TRC (..))
import Test.Cardano.Ledger.Conformance (ExecSpecRule (..), ForAllExecTypes, testConformance)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway ()
import Test.Cardano.Ledger.Conformance.Imp qualified as Imp (spec)
import Test.Cardano.Ledger.Conformance.Imp.Ratify qualified as RatifyImp
import Test.Cardano.Ledger.Constrained.Conway.MiniTrace (
  ConstrainedGeneratorBundle (..),
  constrainedCert,
  constrainedCerts,
  constrainedDeleg,
  constrainedEnact,
  constrainedEpoch,
  constrainedGov,
  constrainedGovCert,
  constrainedPool,
  constrainedRatify,
  constrainedUtxo,
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

genFromBundle ::
  ForAllExecTypes HasSpec rule era =>
  ConstrainedGeneratorBundle ctx rule era ->
  Gen (ExecContext rule era, TRC (EraRule rule era))
genFromBundle ConstrainedGeneratorBundle {..} = do
  ctx <- cgbContextGen
  env <- genFromSpec $ cgbEnvironmentSpec ctx
  st <- genFromSpec $ cgbStateSpec ctx env
  sig <- genFromSpec $ cgbSignalSpec ctx env st
  pure (undefined, TRC (env, st, sig))

conformsToImplConstrained ::
  forall ctx rule era.
  ( ExecSpecRule rule era
  , ForAllExecTypes HasSpec rule era
  ) =>
  ConstrainedGeneratorBundle ctx rule era -> Property
conformsToImplConstrained = conformsToImpl @rule @era . genFromBundle

spec :: Spec
spec = do
  describe "Conformance with constrained generators" $ do
    describe "Ticks transition graph" $ do
      prop "ENACT" $ conformsToImplConstrained constrainedEnact
      prop "RATIFY" $ conformsToImplConstrained constrainedRatify
      xprop "EPOCH" $ conformsToImplConstrained constrainedEpoch
      xprop "NEWEPOCH" $ conformsToImplConstrained constrainedEpoch
    describe "Blocks transition graph" $ do
      prop "DELEG" $ conformsToImplConstrained constrainedDeleg
      prop "GOVCERT" $ conformsToImplConstrained constrainedGovCert
      prop "POOL" $ conformsToImplConstrained constrainedPool
      prop "CERT" $ conformsToImplConstrained constrainedCert
      prop "CERTS" $ conformsToImplConstrained constrainedCerts
      prop "GOV" $ conformsToImplConstrained constrainedGov
      -- UTXO is disabled due to: https://github.com/IntersectMBO/cardano-ledger/issues/4876
      xprop "UTXO" $ conformsToImplConstrained constrainedUtxo
      xprop "UTXOW" $ conformsToImplConstrained constrainedUtxo
      xprop "LEDGER" $ conformsToImplConstrained constrainedUtxo
      xprop "LEDGERS" $ conformsToImplConstrained constrainedUtxo
    describe "ImpTests" $ do
      RatifyImp.spec
      Imp.spec
