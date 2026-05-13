{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Ledgers () where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (Constitution (..), EnactState (..))
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.State (ChainAccountState (..))
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  SpecTranslate (..),
  askSpecTransM,
  withCtxSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()

instance SpecTranslate ConwayEra (Shelley.ShelleyLedgersEnv ConwayEra) where
  type SpecRep ConwayEra (Shelley.ShelleyLedgersEnv ConwayEra) = Agda.LEnv
  type SpecContext ConwayEra (Shelley.ShelleyLedgersEnv ConwayEra) = EnactState ConwayEra

  toSpecRep Shelley.LedgersEnv {..} = do
    enactState <- askSpecTransM
    let
      guardrailsScriptHash = constitutionGuardrailsScriptHash $ ensConstitution enactState
    withCtxSpecTransM () $
      Agda.MkLEnv
        <$> toSpecRep ledgersSlotNo
        <*> toSpecRep guardrailsScriptHash
        <*> toSpecRep ledgersPp
        <*> toSpecRep enactState
        <*> toSpecRep (casTreasury ledgersAccount)
