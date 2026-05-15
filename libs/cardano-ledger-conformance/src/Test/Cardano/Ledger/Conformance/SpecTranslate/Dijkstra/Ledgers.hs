{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Ledgers () where

import Cardano.Ledger.Conway.Governance (Constitution (..), EnactState (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.State (ChainAccountState (..))
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTranslate (..),
  askSpecTransM,
  withCtxSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Base ()

instance SpecTranslate DijkstraEra (Shelley.ShelleyLedgersEnv DijkstraEra) where
  type SpecRep DijkstraEra (Shelley.ShelleyLedgersEnv DijkstraEra) = Agda.LedgerEnv
  type SpecContext DijkstraEra (Shelley.ShelleyLedgersEnv DijkstraEra) = EnactState DijkstraEra

  toSpecRep Shelley.LedgersEnv {..} = do
    enactState <- askSpecTransM
    let
      guardrailsScriptHash = constitutionGuardrailsScriptHash $ ensConstitution enactState
    withCtxSpecTransM () $
      Agda.MkLedgerEnv
        <$> toSpecRep ledgersSlotNo
        <*> toSpecRep guardrailsScriptHash
        <*> toSpecRep ledgersPp
        <*> toSpecRep enactState
        <*> toSpecRep (casTreasury ledgersAccount)
