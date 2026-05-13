{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Enact () where

import Cardano.Ledger.BaseTypes (EpochNo)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (EnactState (..))
import qualified Cardano.Ledger.Conway.Rules as Conway
import Control.State.Transition.Extended (TRC (..))
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  SpecTRC (..),
  SpecTranslate (..),
  runSpecTransM,
  unComputationResult,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Conway.ImpTest ()

instance ExecSpecRule "ENACT" ConwayEra where
  type ExecContext "ENACT" ConwayEra = EpochNo
  type SpecEnvironment "ENACT" ConwayEra = Agda.EnactEnv

  translateInputs ctx (TRC (_, st@EnactState {..}, sig@Conway.EnactSignal {..})) =
    runSpecTransM () $ do
      agdaSt <- toSpecRep @ConwayEra st
      agdaSig <- toSpecRep @ConwayEra sig
      treasury <- toSpecRep @ConwayEra ensTreasury
      gaId <- toSpecRep @ConwayEra esGovActionId
      curEpoch <- toSpecRep @ConwayEra ctx
      pure $
        SpecTRC
          (Agda.MkEnactEnv gaId treasury curEpoch)
          agdaSt
          agdaSig

  runAgdaRule (SpecTRC env st sig) = unComputationResult $ Agda.enactStep env st sig
