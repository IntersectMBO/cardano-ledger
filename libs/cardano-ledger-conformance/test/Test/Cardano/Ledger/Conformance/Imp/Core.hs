{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Cardano.Ledger.Conformance.Imp.Core where

import Cardano.Ledger.BaseTypes (Globals)
import Cardano.Ledger.Core (EraRule)
import Control.State.Transition
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Proxy (..))
import Data.List.NonEmpty
import Data.Text qualified as T
import GHC.TypeLits (symbolVal)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (
  ExecSpecRule (..),
  ExecSpecTopLevelRule (..),
  SpecTRC (..),
  diffConformance,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecNormalize (..),
  runSpecTransM,
 )
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common hiding (Args)
import UnliftIO (evaluateDeep)

conformanceHook ::
  forall rule era t.
  ( ExecSpecRule rule era
  , ToExpr (Event (EraRule rule era))
  ) =>
  Globals ->
  TRC (EraRule rule era) ->
  ExecContext rule era ->
  Either
    (NonEmpty (PredicateFailure (EraRule rule era)))
    (State (EraRule rule era), [Event (EraRule rule era)]) ->
  ImpM t ()
conformanceHook globals trc@(TRC (env, state, signal)) ctx impRuleResult =
  impAnn ("Conformance hook (" <> symbolVal (Proxy @rule) <> ")") $ do
    -- translate inputs
    specTRC@(SpecTRC specEnv specState specSignal) <-
      impAnn "Translating inputs" . expectRightDeepExpr $ runSpecTransM ctx $ translateInputs trc
    -- get agda response
    agdaResponse' <-
      fmap (second $ first specNormalize) . evaluateDeep $ runAgdaRuleWithDebug @rule @era specTRC
    -- translate imp response
    let
      agdaResponse = fmap fst agdaResponse'
      agdaDebug = either (const "") snd agdaResponse'
      impRuleResult' = bimap (T.pack . show) fst impRuleResult
      impResponse = first (T.pack . show) . runSpecTransM @era ctx . translateOutput trc =<< impRuleResult'

    logString "implEnv"
    logToExpr env
    logString "implState"
    logToExpr state
    logString "implSignal"
    logToExpr signal
    logString "implStateOut"
    logToExpr impRuleResult
    logString "specEnv"
    logToExpr specEnv
    logString "specState"
    logToExpr specState
    logString "specSignal"
    logToExpr specSignal
    logString "specDebug"
    logToExpr agdaDebug
    logString "Extra info:"
    logDoc $
      extraInfo @rule @era
        globals
        ctx
        (TRC (env, state, signal))
        (first (T.pack . show) impRuleResult)
    logString "diffConformance:"
    logDoc $ diffConformance impResponse agdaResponse
    case (impResponse, agdaResponse) of
      (Right impRes, Right agdaRes)
        | impRes == agdaRes -> pure ()
      (Left _, Left _) -> pure ()
      _ -> assertFailure "Conformance failure"

submitTxConformanceHook ::
  forall era t.
  (HasCallStack, ExecSpecTopLevelRule "LEDGER" era) =>
  Globals ->
  TRC (EraRule "LEDGER" era) ->
  Either
    (NonEmpty (PredicateFailure (EraRule "LEDGER" era)))
    (State (EraRule "LEDGER" era), [Event (EraRule "LEDGER" era)]) ->
  ImpM t ()
submitTxConformanceHook globals trc =
  conformanceHook globals trc (mkRuleExecContext globals trc)

epochBoundaryConformanceHook ::
  forall era t.
  ( ExecSpecTopLevelRule "NEWEPOCH" era
  , ToExpr (Event (EraRule "NEWEPOCH" era))
  ) =>
  Globals ->
  TRC (EraRule "NEWEPOCH" era) ->
  State (EraRule "NEWEPOCH" era) ->
  ImpM t ()
epochBoundaryConformanceHook globals trc res =
  conformanceHook @"NEWEPOCH" @era globals trc (mkRuleExecContext globals trc) $ Right (res, [])
