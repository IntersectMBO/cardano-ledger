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
import Cardano.Ledger.Shelley.Rules (Identity, ShelleyLedgersEnv (..))
import Cardano.Ledger.Shelley.State (ChainAccountState (..))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  SpecTranslate (..),
  askSpecTransM,
  withSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()

instance
  ( EraPParams era
  , SpecTranslate (PParamsHKD Identity era)
  , SpecContext (PParamsHKD Identity era) ~ ()
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  ) =>
  SpecTranslate (ShelleyLedgersEnv era)
  where
  type SpecRep (ShelleyLedgersEnv era) = Agda.LEnv
  type SpecContext (ShelleyLedgersEnv era) = EnactState era

  toSpecRep LedgersEnv {..} = do
    enactState <- askSpecTransM
    let
      guardrailsScriptHash = constitutionGuardrailsScriptHash $ ensConstitution enactState
    withSpecTransM (const ()) $
      Agda.MkLEnv
        <$> toSpecRep ledgersSlotNo
        <*> toSpecRep guardrailsScriptHash
        <*> toSpecRep ledgersPp
        <*> toSpecRep enactState
        <*> toSpecRep (casTreasury ledgersAccount)
