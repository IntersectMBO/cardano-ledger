{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conformance.Imp (spec) where

import Cardano.Ledger.Alonzo.Tx (AlonzoTx)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (LedgerEnv, UtxoEnv (..), ledgerSlotNoL)
import Control.State.Transition
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (bimapM)
import Data.List.NonEmpty
import Lens.Micro
import Lib qualified as Agda
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway (ConwayLedgerExecContext (..))
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway (ConwayTxBodyTransContext)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Conway.Imp qualified as ConwayImp (conwaySpec)
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common hiding (Args)
import UnliftIO (evaluateDeep)

testImpConformance ::
  forall era.
  ( ConwayEraImp era
  , ExecSpecRule ConwayFn "LEDGER" era
  , ExecContext ConwayFn "LEDGER" era ~ ConwayLedgerExecContext era
  , ExecSignal ConwayFn "LEDGER" era ~ AlonzoTx era
  , ExecState ConwayFn "LEDGER" era ~ LedgerState era
  , SpecTranslate (ExecContext ConwayFn "LEDGER" era) (ExecState ConwayFn "LEDGER" era)
  , SpecTranslate (ExecContext ConwayFn "LEDGER" era) (ExecEnvironment ConwayFn "LEDGER" era)
  , SpecTranslate (ExecContext ConwayFn "LEDGER" era) (TxWits era)
  , HasCallStack
  , SpecRep (TxWits era) ~ Agda.TxWitnesses
  , SpecRep (TxBody era) ~ Agda.TxBody
  , ExecEnvironment ConwayFn "LEDGER" era ~ LedgerEnv era
  , Tx era ~ AlonzoTx era
  , SpecTranslate (ConwayTxBodyTransContext (EraCrypto era)) (TxBody era)
  ) =>
  Globals ->
  Either
    (NonEmpty (PredicateFailure (EraRule "LEDGER" era)))
    (State (EraRule "LEDGER" era), [Event (EraRule "LEDGER" era)]) ->
  ExecEnvironment ConwayFn "LEDGER" era ->
  ExecState ConwayFn "LEDGER" era ->
  ExecSignal ConwayFn "LEDGER" era ->
  Expectation
testImpConformance _ impRuleResult env state signal = do
  let ctx =
        ConwayLedgerExecContext
          { clecPolicyHash =
              state ^. lsUTxOStateL . utxosGovStateL . constitutionGovStateL . constitutionScriptL
          , clecEnactState = mkEnactState $ state ^. lsUTxOStateL . utxosGovStateL
          , clecUtxoExecContext =
              UtxoExecContext
                { uecTx = signal
                , uecUTxO = state ^. lsUTxOStateL . utxosUtxoL
                , uecUtxoEnv =
                    UtxoEnv
                      { ueSlot = env ^. ledgerSlotNoL
                      , uePParams = state ^. lsUTxOStateL . utxosGovStateL . curPParamsGovStateL
                      , ueCertState = state ^. lsCertStateL
                      }
                }
          }
  -- translate inputs
  (specEnv, specState, specSignal) <-
    (,,)
      <$> expectRight (runSpecTransM ctx $ toSpecRep env)
      <*> expectRight (runSpecTransM ctx $ toSpecRep state)
      <*> expectRight (runSpecTransM ctx $ toSpecRep signal)
  -- get agda response
  agdaResponse <-
    fmap (second fixup) $
      evaluateDeep $
        runAgdaRule @ConwayFn @"LEDGER" @era specEnv specState specSignal
  -- translate imp response
  impResponse <-
    expectRightExpr $
      runSpecTransM ctx $
        bimapM
          (pure . showOpaqueErrorString)
          (toTestRep . inject @_ @(ExecState ConwayFn "LEDGER" era) . fst)
          impRuleResult

  when (impResponse /= agdaResponse) $ do
    assertFailure "Conformance failure"

spec :: Spec
spec =
  withImpInit @(LedgerSpec Conway) $
    modifyImpInitProtVer @Conway (natVersion @10) $
      modifyImpInitExpectLedgerRuleConformance testImpConformance $ do
        describe "Basic imp conformance" $ do
          it "Submit constitution" $ do
            _ <- submitConstitution @Conway SNothing
            passNEpochs 2
        xdescribe "Conway Imp conformance" $ ConwayImp.conwaySpec @Conway
