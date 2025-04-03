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

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Ledger () where

import Cardano.Ledger.BaseTypes (Inject)
import Cardano.Ledger.Conway.Core (EraPParams (..), EraRule, ScriptHash)
import Cardano.Ledger.Conway.Rules (ConwayLedgerPredFailure, EnactState)
import Cardano.Ledger.Shelley.LedgerState (ChainAccountState (..))
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..))
import Control.State.Transition.Extended (STS (..))
import Data.Functor.Identity (Identity)
import Data.Maybe.Strict (StrictMaybe)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (OpaqueErrorString (..), askCtx, showOpaqueErrorString)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (SpecTranslate (..))
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr (..))

instance
  ( EraPParams era
  , SpecTranslate ctx (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , Inject ctx (StrictMaybe ScriptHash)
  , Inject ctx (EnactState era)
  ) =>
  SpecTranslate ctx (LedgerEnv era)
  where
  type SpecRep (LedgerEnv era) = Agda.LEnv

  toSpecRep LedgerEnv {..} = do
    policyHash <- askCtx @(StrictMaybe ScriptHash)
    enactState <- askCtx @(EnactState era)
    Agda.MkLEnv
      <$> toSpecRep ledgerSlotNo
      <*> toSpecRep policyHash
      <*> toSpecRep ledgerPp
      <*> toSpecRep enactState
      <*> toSpecRep (casTreasury ledgerAccount)

instance
  ( ToExpr (PredicateFailure (EraRule "GOV" era))
  , ToExpr (PredicateFailure (EraRule "CERTS" era))
  , ToExpr (PredicateFailure (EraRule "UTXOW" era))
  ) =>
  SpecTranslate ctx (ConwayLedgerPredFailure era)
  where
  type SpecRep (ConwayLedgerPredFailure era) = OpaqueErrorString

  toSpecRep = pure . showOpaqueErrorString
