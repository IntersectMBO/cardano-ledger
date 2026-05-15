{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Ratify () where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  EnactState (..),
  GovActionState (..),
  RatifyEnv (..),
  RatifySignal (..),
  RatifyState (..),
  ensProtVerL,
  gasAction,
  rsEnactStateL,
 )
import qualified Cardano.Ledger.Conway.Rules as Conway
import Control.State.Transition.Extended (TRC (..))
import Data.Foldable (Foldable (..))
import Data.Ratio (denominator, numerator)
import Lens.Micro ((^.))
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import qualified Prettyprinter as PP
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (
  ExecSpecRule (..),
  SpecTRC (..),
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTranslate (..),
  runSpecTransM,
  unComputationResult_,
  withCtxSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Conway.ImpTest ()
import Test.Cardano.Ledger.Conway.TreeDiff (ansiExpr, tableDoc)

instance ExecSpecRule "RATIFY" ConwayEra where
  runAgdaRule (SpecTRC env st sig) = unComputationResult_ $ Agda.ratifyStep env st sig

  translateInputs (TRC (env, st@RatifyState {..}, sig@(RatifySignal actions))) =
    do
      specEnv <- withCtxSpecTransM (ensTreasury rsEnactState) (toSpecRep env)
      specSt <- withCtxSpecTransM (toList actions) (toSpecRep st)
      specSig <- withCtxSpecTransM () (toSpecRep sig)
      pure $ SpecTRC specEnv specSt specSig

  translateOutput (TRC (_, _, RatifySignal actions)) =
    withCtxSpecTransM (toList actions) . toSpecRep

  extraInfo _ _ trc@(TRC (env@RatifyEnv {..}, st@RatifyState {..}, RatifySignal actions)) _ =
    PP.vsep $ specExtraInfo : (actionAcceptedRatio <$> toList actions)
    where
      members = foldMap' (committeeMembers @ConwayEra) $ ensCommittee rsEnactState
      showAccepted True = PP.brackets "✓"
      showAccepted False = PP.brackets "×"
      showRatio r = PP.viaShow (numerator r) <> "/" <> PP.viaShow (denominator r)
      specExtraInfo =
        PP.vsep
          [ "Spec extra info:"
          , either PP.viaShow ansiExpr $ runSpecTransM () $ translateInputs @_ @ConwayEra trc
          ]
      pv = st ^. rsEnactStateL . ensProtVerL
      actionAcceptedRatio gas@GovActionState {..} =
        tableDoc
          (Just "GovActionState")
          [
            ( "GovActionId:"
            , PP.line <> PP.indent 2 (ansiExpr gasId)
            )
          ,
            ( "SPO:"
            , showAccepted (Conway.spoAccepted env st gas)
                PP.<+> showRatio (Conway.spoAcceptedRatio env gas pv)
            )
          ,
            ( "DRep:"
            , showAccepted (Conway.dRepAccepted env st gas)
                PP.<+> showRatio (Conway.dRepAcceptedRatio env gasDRepVotes (gasAction gas))
            )
          ,
            ( "CC:"
            , showAccepted (Conway.committeeAccepted env st gas)
                PP.<+> showRatio (Conway.committeeAcceptedRatio members gasCommitteeVotes reCommitteeState reCurrentEpoch)
            )
          ]
