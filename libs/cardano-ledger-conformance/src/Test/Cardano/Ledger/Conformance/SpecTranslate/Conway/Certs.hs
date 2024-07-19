{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Certs () where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (emptyDeposits)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool ()
import Test.Cardano.Ledger.Conway.TreeDiff

instance ToExpr (PredicateFailure (EraRule "CERT" era)) => SpecTranslate ctx (ConwayCertsPredFailure era) where
  type SpecRep (ConwayCertsPredFailure era) = OpaqueErrorString
  toSpecRep = pure . OpaqueErrorString . show . toExpr

instance
  ( SpecTranslate ctx (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , Inject ctx (VotingProcedures era)
  , Inject ctx (Map (Network, Credential 'Staking (EraCrypto era)) Coin)
  ) =>
  SpecTranslate ctx (CertsEnv era)
  where
  type SpecRep (CertsEnv era) = Agda.CertEnv
  toSpecRep CertsEnv {..} = do
    votes <- askCtx @(VotingProcedures era)
    withdrawals <- askCtx @(Map (Network, Credential 'Staking (EraCrypto era)) Coin)
    Agda.MkCertEnv
      <$> toSpecRep certsCurrentEpoch
      <*> toSpecRep certsPParams
      <*> toSpecRep votes
      <*> toSpecRep withdrawals
      -- TODO: replace with actual deposits map
      <*> pure emptyDeposits
