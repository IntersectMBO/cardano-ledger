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
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.State
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()

instance
  ( SpecTranslate (PParamsHKD Identity era)
  , SpecContext (PParamsHKD Identity era) ~ ()
  , EraPParams era
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate (CertState era)
  , SpecContext (CertState era) ~ ()
  , SpecRep (CertState era) ~ Agda.CertState
  , EraCertState era
  ) =>
  SpecTranslate (Conway.GovEnv era)
  where
  type SpecRep (Conway.GovEnv era) = Agda.GovEnv
  type SpecContext (Conway.GovEnv era) = EnactState era

  toSpecRep Conway.GovEnv {..} = do
    enactState <- askSpecTransM
    let rewardAccounts = Map.keysSet $ geCertState ^. certDStateL . accountsL . accountsMapL
    withCtxSpecTransM () $
      Agda.MkGovEnv
        <$> toSpecRep geTxId
        <*> toSpecRep geEpoch
        <*> toSpecRep gePParams
        <*> toSpecRep geGuardrailsScriptHash
        <*> toSpecRep enactState
        <*> toSpecRep geCertState
        <*> toSpecRep rewardAccounts

instance
  ( EraPParams era
  , SpecTranslate (PParamsHKD StrictMaybe era)
  , SpecContext (PParamsHKD StrictMaybe era) ~ ()
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate (Conway.GovSignal era)
  where
  type SpecRep (Conway.GovSignal era) = [Either Agda.GovVote Agda.GovProposal]

  toSpecRep Conway.GovSignal {gsVotingProcedures, gsProposalProcedures} = do
    votingProcedures <- toSpecRep gsVotingProcedures
    proposalProcedures <- toSpecRep gsProposalProcedures
    pure $
      mconcat
        [ Left <$> votingProcedures
        , Right <$> proposalProcedures
        ]
