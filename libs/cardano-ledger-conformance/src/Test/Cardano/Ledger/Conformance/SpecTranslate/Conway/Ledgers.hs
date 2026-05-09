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

import Cardano.Ledger.Conway.Core (EraPParams (..))
import Cardano.Ledger.Conway.Governance (Constitution (..), EnactState (..))
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.State (ChainAccountState (..))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  SpecTranslate (..),
  askSpecTransM,
  withCtxSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()

instance
  ( EraPParams era
  , SpecTranslate (PParamsHKD Shelley.Identity era)
  , SpecContext (PParamsHKD Shelley.Identity era) ~ ()
  , SpecRep (PParamsHKD Shelley.Identity era) ~ Agda.PParams
  ) =>
  SpecTranslate (Shelley.ShelleyLedgersEnv era)
  where
  type SpecRep (Shelley.ShelleyLedgersEnv era) = Agda.LEnv
  type SpecContext (Shelley.ShelleyLedgersEnv era) = EnactState era

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
