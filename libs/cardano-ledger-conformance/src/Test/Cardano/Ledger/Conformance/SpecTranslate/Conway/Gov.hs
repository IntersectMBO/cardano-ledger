{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Gov () where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Data.Functor.Identity (Identity)
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core

instance
  ( SpecTranslate ctx (PParamsHKD Identity era)
  , Inject ctx (EnactState era)
  , EraPParams era
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  ) =>
  SpecTranslate ctx (GovEnv era)
  where
  type SpecRep (GovEnv era) = Agda.GovEnv

  toSpecRep GovEnv {..} = do
    enactState <- askCtx @(EnactState era)
    Agda.MkGovEnv
      <$> toSpecRep geTxId
      <*> toSpecRep geEpoch
      <*> toSpecRep gePParams
      <*> toSpecRep gePPolicy
      <*> toSpecRep enactState

instance
  ( EraPParams era
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate ctx (GovSignal era)
  where
  type SpecRep (GovSignal era) = [Agda.GovSignal]

  toSpecRep GovSignal {gsVotingProcedures, gsProposalProcedures} = do
    votingProcedures <- toSpecRep gsVotingProcedures
    proposalProcedures <- toSpecRep gsProposalProcedures
    pure $
      mconcat
        [ Agda.GovSignalVote <$> votingProcedures
        , Agda.GovSignalProposal <$> proposalProcedures
        ]
