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

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway (ConwayEra)
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

instance
  ( SpecTranslate ctx ConwayEra (PParamsHKD Identity era)
  , SpecRep ConwayEra (PParamsHKD Identity era) ~ Agda.PParams
  , Inject ctx (VotingProcedures era)
  , Inject ctx (Map AccountAddress Coin)
  ) =>
  SpecTranslate ctx ConwayEra (CertsEnv era)
  where
  type SpecRep ConwayEra (CertsEnv era) = Agda.CertEnv
  toSpecRep CertsEnv {..} = do
    votes <- askCtx @(VotingProcedures era)
    withdrawals <- askCtx @(Map AccountAddress Coin)
    let ccColdCreds = foldMap (keysSet . committeeMembers) certsCurrentCommittee
    Agda.MkCertEnv
      <$> toSpecRep @_ @ConwayEra certsCurrentEpoch
      <*> toSpecRep @_ @ConwayEra certsPParams
      <*> toSpecRep @_ @ConwayEra votes
      <*> toSpecRep @_ @ConwayEra withdrawals
      <*> toSpecRep @_ @ConwayEra ccColdCreds
