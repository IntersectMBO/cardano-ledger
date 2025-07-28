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
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.State
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base

instance
  ( SpecTranslate ctx (PParamsHKD Identity era)
  , Inject ctx (EnactState era)
  , EraPParams era
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx (CertState era)
  , SpecRep (CertState era) ~ Agda.CertState
  , EraCertState era
  ) =>
  SpecTranslate ctx (GovEnv era)
  where
  type SpecRep (GovEnv era) = Agda.GovEnv

  toSpecRep GovEnv {..} = do
    enactState <- askCtx @(EnactState era)
    let rewardAccounts = Map.keysSet $ geCertState ^. certDStateL . accountsL . accountsMapL
    Agda.MkGovEnv
      <$> toSpecRep geTxId
      <*> toSpecRep geEpoch
      <*> toSpecRep gePParams
      <*> toSpecRep gePPolicy
      <*> toSpecRep enactState
      <*> toSpecRep geCertState
      <*> toSpecRep rewardAccounts

instance
  ( EraPParams era
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate ctx (GovSignal era)
  where
  type SpecRep (GovSignal era) = [Either Agda.GovVote Agda.GovProposal]

  toSpecRep GovSignal {gsVotingProcedures, gsProposalProcedures} = do
    votingProcedures <- toSpecRep gsVotingProcedures
    proposalProcedures <- toSpecRep gsProposalProcedures
    pure $
      mconcat
        [ Left <$> votingProcedures
        , Right <$> proposalProcedures
        ]
