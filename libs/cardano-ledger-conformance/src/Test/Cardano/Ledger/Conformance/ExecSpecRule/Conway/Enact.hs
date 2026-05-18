{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Enact () where

import Cardano.Ledger.BaseTypes (EpochNo)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (EnactState (..))
import qualified Cardano.Ledger.Conway.Rules as Conway
import Control.State.Transition.Extended (TRC (..))
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (
  ExecSpecRule (..),
  SpecTRC (..),
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTranslate (..),
  askSpecTransM,
  unComputationResult,
  withCtxSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Conway.ImpTest ()

instance ExecSpecRule "ENACT" ConwayEra where
  type ExecContext "ENACT" ConwayEra = EpochNo
  type SpecEnvironment "ENACT" ConwayEra = Agda.EnactEnv

  translateInputs (TRC (_, st@EnactState {..}, sig@Conway.EnactSignal {..})) = do
    epochNo <- askSpecTransM
    withCtxSpecTransM () $ do
      agdaSt <- toSpecRep st
      agdaSig <- toSpecRep sig
      treasury <- toSpecRep ensTreasury
      gaId <- toSpecRep esGovActionId
      curEpoch <- toSpecRep epochNo
      pure $
        SpecTRC
          (Agda.MkEnactEnv gaId treasury curEpoch)
          agdaSt
          agdaSig

  runAgdaRule (SpecTRC env st sig) = unComputationResult $ Agda.enactStep env st sig
