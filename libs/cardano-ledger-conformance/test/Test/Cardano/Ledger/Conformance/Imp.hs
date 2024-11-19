{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conformance.Imp (spec) where

import Cardano.Ledger.Alonzo.Tx (AlonzoTx)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (EncCBOR)
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Governance (
  ConwayGovState,
  GovActionState,
  cgsProposalsL,
  proposalsActions,
 )
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..))
import Control.State.Transition
import Data.Bifunctor (bimap)
import Data.Bitraversable (bimapM)
import Data.Default (def)
import Data.Foldable (Foldable (..))
import Data.List.NonEmpty (NonEmpty)
import Lens.Micro
import Lens.Micro.Mtl (use)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway (ConwayLedgerExecContext (..))
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base ()
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Conway.Imp qualified as ConwayImp (conwaySpec)
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common hiding (Args)
import UnliftIO (MonadIO (..), evaluateDeep)

testImpConformance ::
  forall era.
  ( ConwayEraImp era
  , ExecSpecRule ConwayFn "LEDGER" era
  , ExecContext ConwayFn "LEDGER" era ~ ConwayLedgerExecContext era
  , ExecSignal ConwayFn "LEDGER" era ~ AlonzoTx era
  , ExecState ConwayFn "LEDGER" era ~ LedgerState era
  , SpecTranslate (ExecContext ConwayFn "LEDGER" era) (ExecState ConwayFn "LEDGER" era)
  , ForAllExecSpecRep NFData ConwayFn "LEDGER" era
  , ForAllExecSpecRep ToExpr ConwayFn "LEDGER" era
  , NFData (SpecRep (PredicateFailure (EraRule "LEDGER" era)))
  , ToExpr (SpecRep (PredicateFailure (EraRule "LEDGER" era)))
  , Eq (SpecRep (PredicateFailure (EraRule "LEDGER" era)))
  , FixupSpecRep (SpecRep (PredicateFailure (EraRule "LEDGER" era)))
  , EncCBOR (ExecContext ConwayFn "LEDGER" era)
  , EncCBOR (Environment (EraRule "LEDGER" era))
  , EncCBOR (State (EraRule "LEDGER" era))
  , ToExpr (ExecContext ConwayFn "LEDGER" era)
  , HasCallStack
  ) =>
  Either
    (NonEmpty (PredicateFailure (EraRule "LEDGER" era)))
    (State (EraRule "LEDGER" era), [Event (EraRule "LEDGER" era)]) ->
  ExecEnvironment ConwayFn "LEDGER" era ->
  ExecState ConwayFn "LEDGER" era ->
  ExecSignal ConwayFn "LEDGER" era ->
  ImpM (LedgerSpec era) ()
testImpConformance impRuleResult env state signal = do
  logDoc $
    mconcat
      [ "\n----- LedgerEnv:\n"
      , ansiExpr env
      , "\n----- LedgerState:\n"
      , ansiExpr state
      , "\n----- Transaction:\n"
      , ansiExpr signal
      ]
  slot <- use impLastTickG
  let ctx :: ConwayLedgerExecContext era =
        ConwayLedgerExecContext
          { clecPolicyHash = SNothing
          , clecEnactState = def
          , clecUtxoExecContext =
              UtxoExecContext
                { uecTx = signal
                , uecUTxO = state ^. lsUTxOStateL . utxosUtxoL
                , uecUtxoEnv =
                    UtxoEnv
                      { ueSlot = slot
                      , uePParams = state ^. lsUTxOStateL . utxosGovStateL . curPParamsGovStateL
                      , ueCertState = state ^. lsCertStateL
                      }
                }
          }
  logDoc $ "\n----- ConwayLedgerExecContext:\n" <> ansiExpr ctx

  (specEnv, specState, specSignal) <-
    impAnn "Translating the inputs" $
      translateInputs @ConwayFn @"LEDGER" @era env state signal ctx
  logDoc $
    mconcat
      [ "\n----- spec LedgerEnv:\n"
      , ansiExpr specEnv
      , "\n----- spec LedgerState:\n"
      , ansiExpr specState
      , "\n----- spec Transaction:\n"
      , ansiExpr specSignal
      ]

  agdaResponse <-
    fmap (bimap (fixup <$>) fixup) $
      impAnn "Deep evaluating Agda output" $
        evaluateDeep $
          runAgdaRule @ConwayFn @"LEDGER" @era specEnv specState specSignal

  impResponse <-
    impAnn "Translating implementation values to SpecRep" $
      expectRightExpr $
        runSpecTransM ctx $
          bimapM
            (traverse toTestRep)
            (toTestRep . inject @_ @(ExecState ConwayFn "LEDGER" era) . fst)
            impRuleResult

  checkConformance @"LEDGER" @era @ConwayFn
    ctx
    (inject env)
    (inject state)
    (inject signal)
    impResponse
    agdaResponse

testImpConformanceNewEpoch ::
  forall era fn.
  ( ShelleyEraImp era
  , ExecSpecRule fn "NEWEPOCH" era
  , NFData (SpecRep (PredicateFailure (EraRule "NEWEPOCH" era)))
  , Eq (SpecRep (PredicateFailure (EraRule "NEWEPOCH" era)))
  , Inject (State (EraRule "NEWEPOCH" era)) (ExecState fn "NEWEPOCH" era)
  , SpecTranslate (ExecContext fn "NEWEPOCH" era) (ExecState fn "NEWEPOCH" era)
  , SpecTranslate (ExecContext fn "NEWEPOCH" era) (State (EraRule "NEWEPOCH" era))
  , FixupSpecRep (SpecRep (PredicateFailure (EraRule "NEWEPOCH" era)))
  , EncCBOR (ExecContext fn "NEWEPOCH" era)
  , EncCBOR (Environment (EraRule "NEWEPOCH" era))
  , EncCBOR (State (EraRule "NEWEPOCH" era))
  , EncCBOR (Signal (EraRule "NEWEPOCH" era))
  , ToExpr (ExecContext fn "NEWEPOCH" era)
  , ToExpr (SpecRep (PredicateFailure (EraRule "NEWEPOCH" era)))
  , GovState era ~ ConwayGovState era
  , ExecContext fn "NEWEPOCH" era ~ [GovActionState era]
  , ExecState fn "NEWEPOCH" era ~ NewEpochState era
  , ExecEnvironment fn "NEWEPOCH" era ~ EpochExecEnv era
  , ExecSignal fn "NEWEPOCH" era ~ EpochNo
  ) =>
  ImpTestM era ()
testImpConformanceNewEpoch = do
  ctx <-
    getsNES $
      nesEsL
        . epochStateGovStateL
        . cgsProposalsL
        . to (toList . proposalsActions)
  st <- getsNES id
  eNo <- getsNES nesELL
  let
    env = EpochExecEnv stakeDistr
    stakeDistr = mempty
    args =
      stdArgs
        { maxSuccess = 1
        , chatty = False
        }
  liftIO . quickCheckWith args $
    testConformance @fn @"NEWEPOCH" @era ctx env st eNo

spec :: Spec
spec =
  withImpInit @(LedgerSpec Conway) $ do
    describe "Tx conformance"
      . modifyImpInitProtVer @Conway (natVersion @10)
      . modifyImpInitHook testImpConformance
      . modifyImpInitPassTickHook (testImpConformanceNewEpoch @Conway @ConwayFn)
      $ do
        xit "Tx conformance" $ do
          _ <- submitConstitution @Conway SNothing
          passNEpochs 2
        it "passEpoch conformance" $ do
          passNEpochs 10
    describe "Test.Cardano.Ledger.Conway.Imp conformance"
      . modifyImpInitHook testImpConformance
      . modifyImpInitPassTickHook (testImpConformanceNewEpoch @Conway @ConwayFn)
      $ ConwayImp.conwaySpec @Conway
