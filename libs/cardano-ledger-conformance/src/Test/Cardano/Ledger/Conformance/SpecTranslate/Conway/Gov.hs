{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Gov () where

import Cardano.Ledger.Conway (ConwayEra)
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.State
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()

instance SpecTranslate ConwayEra (Conway.GovEnv ConwayEra) where
  type SpecRep ConwayEra (Conway.GovEnv ConwayEra) = Agda.GovEnv
  type SpecContext ConwayEra (Conway.GovEnv ConwayEra) = Conway.EnactState ConwayEra

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

instance SpecTranslate ConwayEra (Conway.GovSignal ConwayEra) where
  type SpecRep ConwayEra (Conway.GovSignal ConwayEra) = [Either Agda.GovVote Agda.GovProposal]

  toSpecRep Conway.GovSignal {gsVotingProcedures, gsProposalProcedures} = do
    votingProcedures <- toSpecRep gsVotingProcedures
    proposalProcedures <- toSpecRep gsProposalProcedures
    pure $
      mconcat
        [ Left <$> votingProcedures
        , Right <$> proposalProcedures
        ]
