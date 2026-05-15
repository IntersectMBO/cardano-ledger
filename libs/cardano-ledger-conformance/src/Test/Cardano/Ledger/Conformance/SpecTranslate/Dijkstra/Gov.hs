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

module Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Gov () where

import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.State
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Cert ()

instance SpecTranslate DijkstraEra (Conway.GovEnv DijkstraEra) where
  type SpecRep DijkstraEra (Conway.GovEnv DijkstraEra) = Agda.GovEnv
  type SpecContext DijkstraEra (Conway.GovEnv DijkstraEra) = Conway.EnactState DijkstraEra

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

instance SpecTranslate DijkstraEra (Conway.GovSignal DijkstraEra) where
  type SpecRep DijkstraEra (Conway.GovSignal DijkstraEra) = [Either Agda.GovVote Agda.GovProposal]

  toSpecRep Conway.GovSignal {gsVotingProcedures, gsProposalProcedures} = do
    votingProcedures <- toSpecRep gsVotingProcedures
    proposalProcedures <- toSpecRep gsProposalProcedures
    pure $
      mconcat
        [ Left <$> votingProcedures
        , Right <$> proposalProcedures
        ]
