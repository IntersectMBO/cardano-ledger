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

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..), ledgerSlotNoL)
import Cardano.Ledger.TxIn (TxId)
import Control.State.Transition
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Proxy (..))
import Data.List.NonEmpty
import Data.Text qualified as T
import GHC.TypeLits (symbolVal)
import Lens.Micro
import MAlonzo.Code.Ledger.Foreign.API qualified as Agda
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway (ConwayLedgerExecContext (..))
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common hiding (Args)
import UnliftIO (evaluateDeep)

conformanceHook ::
  forall rule era t.
  ( ShelleyEraImp era
  , ExecSpecRule rule era
  , ToExpr (Event (EraRule rule era))
  ) =>
  Globals ->
  ExecContext rule era ->
  TRC (EraRule rule era) ->
  Either
    (NonEmpty (PredicateFailure (EraRule rule era)))
    (State (EraRule rule era), [Event (EraRule rule era)]) ->
  ImpM t ()
conformanceHook globals ctx trc@(TRC (env, state, signal)) impRuleResult =
  impAnn ("Conformance hook (" <> symbolVal (Proxy @rule) <> ")") $ do
    -- translate inputs
    specTRC@(SpecTRC specEnv specState specSignal) <-
      impAnn "Translating inputs" . expectRightDeepExpr $ translateInputs ctx trc
    -- get agda response
    agdaResponse' <-
      fmap (second $ first specNormalize) . evaluateDeep $ runAgdaRuleWithDebug @rule @era specTRC
    -- translate imp response
    let
      agdaResponse = fmap fst agdaResponse'
      agdaDebug = either (const "") snd agdaResponse'
      impRuleResult' = bimap (T.pack . show) fst impRuleResult
      impResponse = first (T.pack . show) . translateOutput @rule @era ctx trc =<< impRuleResult'

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
  ( ConwayEraImp era
  , ExecSpecRule "LEDGER" era
  , ExecContext "LEDGER" era ~ ConwayLedgerExecContext era
  , SpecTranslate (ExecContext "LEDGER" era) (TxWits era)
  , HasCallStack
  , SpecRep (TxWits era) ~ Agda.TxWitnesses
  , SpecRep (TxBody TopTx era) ~ Agda.TxBody
  , SpecTranslate TxId (TxBody TopTx era)
  , SpecTranslate (ConwayLedgerExecContext era) (Tx TopTx era)
  , ToExpr (SpecRep (Tx TopTx era))
  , SpecNormalize (SpecState "LEDGER" era)
  , Eq (SpecState "LEDGER" era)
  ) =>
  Globals ->
  TRC (EraRule "LEDGER" era) ->
  Either
    (NonEmpty (PredicateFailure (EraRule "LEDGER" era)))
    (State (EraRule "LEDGER" era), [Event (EraRule "LEDGER" era)]) ->
  ImpM t ()
submitTxConformanceHook globals trc@(TRC (env, state, signal)) =
  conformanceHook globals ctx trc
  where
    ctx =
      ConwayLedgerExecContext
        { clecPolicyHash =
            state ^. lsUTxOStateL . utxosGovStateL . constitutionGovStateL . constitutionScriptL
        , clecEnactState = mkEnactState $ state ^. lsUTxOStateL . utxosGovStateL
        , clecUtxoExecContext =
            UtxoExecContext
              { uecTx = signal
              , uecUTxO = state ^. utxoL
              , uecUtxoEnv =
                  UtxoEnv
                    { ueSlot = env ^. ledgerSlotNoL
                    , uePParams = state ^. lsUTxOStateL . utxosGovStateL . curPParamsGovStateL
                    , ueCertState = state ^. lsCertStateL
                    }
              }
        }

epochBoundaryConformanceHook ::
  forall era t.
  ( ShelleyEraImp era
  , ExecSpecRule "NEWEPOCH" era
  , ExecContext "NEWEPOCH" era ~ ()
  , ToExpr (Event (EraRule "NEWEPOCH" era))
  ) =>
  Globals ->
  TRC (EraRule "NEWEPOCH" era) ->
  State (EraRule "NEWEPOCH" era) ->
  ImpM t ()
epochBoundaryConformanceHook globals trc res =
  conformanceHook @"NEWEPOCH" @era globals () trc $ Right (res, [])

