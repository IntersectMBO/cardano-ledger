{-# LANGUAGE DataKinds #-}
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
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.State
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()

instance
  ( SpecTranslate ctx ConwayEra (PParamsHKD Identity era)
  , Inject ctx (EnactState era)
  , EraPParams era
  , SpecRep ConwayEra (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx ConwayEra (CertState era)
  , SpecRep ConwayEra (CertState era) ~ Agda.CertState
  , EraCertState era
  ) =>
  SpecTranslate ctx ConwayEra (GovEnv era)
  where
  type SpecRep ConwayEra (GovEnv era) = Agda.GovEnv

  toSpecRep GovEnv {..} = do
    enactState <- askCtx @(EnactState era)
    let rewardAccounts = Map.keysSet $ geCertState ^. certDStateL . accountsL . accountsMapL
    Agda.MkGovEnv
      <$> toSpecRep @_ @ConwayEra geTxId
      <*> toSpecRep @_ @ConwayEra geEpoch
      <*> toSpecRep @_ @ConwayEra gePParams
      <*> toSpecRep @_ @ConwayEra geGuardrailsScriptHash
      <*> toSpecRep @_ @ConwayEra enactState
      <*> toSpecRep @_ @ConwayEra geCertState
      <*> toSpecRep @_ @ConwayEra rewardAccounts

instance
  ( EraPParams era
  , SpecTranslate ctx ConwayEra (PParamsHKD StrictMaybe era)
  , SpecRep ConwayEra (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate ctx ConwayEra (GovSignal era)
  where
  type SpecRep ConwayEra (GovSignal era) = [Either Agda.GovVote Agda.GovProposal]

  toSpecRep GovSignal {gsVotingProcedures, gsProposalProcedures} = do
    votingProcedures <- toSpecRep @_ @ConwayEra gsVotingProcedures
    proposalProcedures <- toSpecRep @_ @ConwayEra gsProposalProcedures
    pure $
      mconcat
        [ Left <$> votingProcedures
        , Right <$> proposalProcedures
        ]
