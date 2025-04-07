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

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Certs () where

import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Data.Functor.Identity (Identity)
import Data.Map (keysSet)
import Data.Map.Strict (Map)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool ()
import Test.Cardano.Ledger.Conway.TreeDiff

instance ToExpr (PredicateFailure (EraRule "CERT" era)) => SpecTranslate ctx (ConwayCertsPredFailure era) where
  type SpecRep (ConwayCertsPredFailure era) = OpaqueErrorString
  toSpecRep = pure . showOpaqueErrorString

instance
  ( SpecTranslate ctx (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , Inject ctx (VotingProcedures era)
  , Inject ctx (Map RewardAccount Coin)
  ) =>
  SpecTranslate ctx (CertsEnv era)
  where
  type SpecRep (CertsEnv era) = Agda.CertEnv
  toSpecRep CertsEnv {..} = do
    votes <- askCtx @(VotingProcedures era)
    withdrawals <- askCtx @(Map RewardAccount Coin)
    let ccColdCreds = foldMap (keysSet . committeeMembers) certsCurrentCommittee
    Agda.MkCertEnv
      <$> toSpecRep certsCurrentEpoch
      <*> toSpecRep certsPParams
      <*> toSpecRep votes
      <*> toSpecRep withdrawals
      <*> toSpecRep ccColdCreds
