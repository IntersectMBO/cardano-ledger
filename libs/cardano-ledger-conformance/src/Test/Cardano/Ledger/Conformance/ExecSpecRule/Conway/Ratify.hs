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
import Cardano.Ledger.Conway.Rules (
  committeeAccepted,
  committeeAcceptedRatio,
  dRepAccepted,
  dRepAcceptedRatio,
  spoAccepted,
  spoAcceptedRatio,
 )
import Control.State.Transition.Extended (TRC (..))
import Data.Foldable (Foldable (..))
import Data.Ratio (denominator, numerator)
import Lens.Micro ((^.))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import qualified Prettyprinter as PP
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  SpecTRC (..),
  SpecTranslate (..),
  runSpecTransM,
  unComputationResult_,
  withCtx,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Conway.ImpTest ()
import Test.Cardano.Ledger.Conway.TreeDiff (ansiExpr, tableDoc)

instance ExecSpecRule "RATIFY" ConwayEra where
  runAgdaRule (SpecTRC env st sig) = unComputationResult_ $ Agda.ratifyStep env st sig

  translateInputs _ (TRC (env, st@RatifyState {..}, sig@(RatifySignal actions))) =
    runSpecTransM () $ do
      let treasury = ensTreasury rsEnactState
      specEnv <- withCtx treasury (toSpecRep env)
      specSt <- withCtx (toList actions) (toSpecRep st)
      specSig <- toSpecRep sig
      pure $ SpecTRC specEnv specSt specSig

  translateOutput _ (TRC (_, _, RatifySignal actions)) out =
    runSpecTransM () . withCtx (toList actions) $ toSpecRep out

  extraInfo _ ctx trc@(TRC (env@RatifyEnv {..}, st@RatifyState {..}, RatifySignal actions)) _ =
    PP.vsep $ specExtraInfo : (actionAcceptedRatio <$> toList actions)
    where
      members = foldMap' (committeeMembers @ConwayEra) $ ensCommittee rsEnactState
      showAccepted True = PP.brackets "✓"
      showAccepted False = PP.brackets "×"
      showRatio r = PP.viaShow (numerator r) <> "/" <> PP.viaShow (denominator r)
      specExtraInfo =
        PP.vsep
          [ "Spec extra info:"
          , either PP.viaShow ansiExpr $ translateInputs @_ @ConwayEra ctx trc
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
            , showAccepted (spoAccepted env st gas)
                PP.<+> showRatio (spoAcceptedRatio env gas pv)
            )
          ,
            ( "DRep:"
            , showAccepted (dRepAccepted env st gas)
                PP.<+> showRatio (dRepAcceptedRatio env gasDRepVotes (gasAction gas))
            )
          ,
            ( "CC:"
            , showAccepted (committeeAccepted env st gas)
                PP.<+> showRatio (committeeAcceptedRatio members gasCommitteeVotes reCommitteeState reCurrentEpoch)
            )
          ]
