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

import Cardano.Ledger.BaseTypes (Inject)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (EraPParams (..))
import Cardano.Ledger.Conway.Governance (Constitution (..), EnactState (..))
import Cardano.Ledger.Shelley.Rules (Identity, ShelleyLedgersEnv (..))
import Cardano.Ledger.Shelley.State (ChainAccountState (..))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  SpecTranslate (..),
  askCtx,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()

instance
  ( EraPParams era
  , SpecTranslate ctx ConwayEra (PParamsHKD Identity era)
  , Inject ctx (EnactState era)
  , SpecRep ConwayEra (PParamsHKD Identity era) ~ Agda.PParams
  ) =>
  SpecTranslate ctx ConwayEra (ShelleyLedgersEnv era)
  where
  type SpecRep ConwayEra (ShelleyLedgersEnv era) = Agda.LEnv

  toSpecRep LedgersEnv {..} = do
    enactState <- askCtx @(EnactState era)
    let
      guardrailsScriptHash = constitutionGuardrailsScriptHash $ ensConstitution enactState
    Agda.MkLEnv
      <$> toSpecRep @_ @ConwayEra ledgersSlotNo
      <*> toSpecRep @_ @ConwayEra guardrailsScriptHash
      <*> toSpecRep @_ @ConwayEra ledgersPp
      <*> toSpecRep @_ @ConwayEra enactState
      <*> toSpecRep @_ @ConwayEra (casTreasury ledgersAccount)
