{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Spec.Conway (
  spec,
  genFromBundle,
  genFromBundle_,
) where

import Cardano.Ledger.Core (EraRule)
import Constrained.API
import Control.Monad.Cont (ContT (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.State.Transition.Extended (STS (..), TRC (..))
import Data.Map.Strict qualified as Map
import Test.Cardano.Ledger.Conformance (ExecSpecRule (..), ForAllExecTypes, testConformance)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway (ConwayCertExecContext (..))
import Test.Cardano.Ledger.Conformance.Imp qualified as Imp (spec)
import Test.Cardano.Ledger.Conformance.Imp.Ratify qualified as RatifyImp
import Test.Cardano.Ledger.Constrained.Conway (genUtxoExecContext)
import Test.Cardano.Ledger.Constrained.Conway.MiniTrace (
  ConstrainedGeneratorBundle (..),
  ConwayCertGenContext (..),
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

spec :: Spec
spec = do
  describe "Conformance with constrained generators" $ do
    describe "Ticks transition graph" $ do
      prop "ENACT" $
        conformsToImplConstrained constrainedEnact $
          \curEpoch _ _ _ -> pure curEpoch
      prop "RATIFY" $ conformsToImplConstrained_ constrainedRatify
      xprop "EPOCH" $ conformsToImplConstrained_ constrainedEpoch
      xprop "NEWEPOCH" $ conformsToImplConstrained_ constrainedEpoch
    describe "Blocks transition graph" $ do
      prop "DELEG" $
        conformsToImplConstrained constrainedDeleg $
          \(_, ConwayCertGenContext {..}) _ _ _ -> pure $ Map.keysSet ccccDelegatees
      prop "GOVCERT" $ conformsToImplConstrained_ constrainedGovCert
      prop "POOL" $ conformsToImplConstrained_ constrainedPool
      prop "CERT" $ conformsToImplConstrained_ constrainedCert
      prop "CERTS" $
        conformsToImplConstrained constrainedCerts $
          \(_, ConwayCertGenContext {..}) _ _ _ ->
            pure $
              ConwayCertExecContext
                { ccecVotes = ccccVotes
                , ccecWithdrawals = ccccWithdrawals
                }
      prop "GOV" $ conformsToImplConstrained_ constrainedGov
      -- UTXO is disabled due to: https://github.com/IntersectMBO/cardano-ledger/issues/4876
      xprop "UTXO" $ conformsToImplConstrained constrainedUtxo $ \_ _ _ _ -> genUtxoExecContext
      xprop "UTXOW" $ conformsToImplConstrained constrainedUtxo $ \_ _ _ _ -> genUtxoExecContext
      xprop "LEDGER" $ conformsToImplConstrained constrainedUtxo $ \_ _ _ _ -> genUtxoExecContext
      xprop "LEDGERS" $ conformsToImplConstrained constrainedUtxo $ \_ _ _ _ -> genUtxoExecContext
    describe "ImpTests" $ do
      RatifyImp.spec
      Imp.spec
