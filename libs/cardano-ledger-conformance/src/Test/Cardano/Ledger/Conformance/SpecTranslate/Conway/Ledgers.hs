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
import Cardano.Ledger.Conway.Core (EraPParams (..), EraRule)
import Cardano.Ledger.Conway.Governance (Constitution (..), EnactState (..))
import Cardano.Ledger.Shelley.Rules (Identity, ShelleyLedgersEnv (..), ShelleyLedgersPredFailure)
import Cardano.Ledger.Shelley.State (ChainAccountState (..))
import Control.State.Transition.Extended (STS (..))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  OpaqueErrorString (..),
  SpecTranslate (..),
  askCtx,
  showOpaqueErrorString,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr)

instance
  ( EraPParams era
  , SpecTranslate ctx (PParamsHKD Identity era)
  , Inject ctx (EnactState era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  ) =>
  SpecTranslate ctx (ShelleyLedgersEnv era)
  where
  type SpecRep (ShelleyLedgersEnv era) = Agda.LEnv

  toSpecRep LedgersEnv {..} = do
    enactState <- askCtx @(EnactState era)
    let
      pPolicy = constitutionScript $ ensConstitution enactState
    Agda.MkLEnv
      <$> toSpecRep ledgersSlotNo
      <*> toSpecRep pPolicy
      <*> toSpecRep ledgersPp
      <*> toSpecRep enactState
      <*> toSpecRep (casTreasury ledgersAccount)

instance
  ToExpr (PredicateFailure (EraRule "LEDGER" era)) =>
  SpecTranslate ctx (ShelleyLedgersPredFailure era)
  where
  type SpecRep (ShelleyLedgersPredFailure era) = OpaqueErrorString

  toSpecRep = pure . showOpaqueErrorString
