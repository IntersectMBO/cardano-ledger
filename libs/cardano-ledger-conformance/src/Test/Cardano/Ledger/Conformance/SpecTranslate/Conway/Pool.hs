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

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool where

import Cardano.Ledger.BaseTypes (Network (Testnet))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.State
import qualified Data.Map.Strict as Map
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()

instance
  ( SpecRep ConwayEra (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx ConwayEra (PParamsHKD Identity era)
  ) =>
  SpecTranslate ctx ConwayEra (PoolEnv era)
  where
  type SpecRep ConwayEra (PoolEnv era) = Agda.PParams

  toSpecRep (PoolEnv _ pp) = toSpecRep @_ @ConwayEra pp

instance SpecTranslate ctx ConwayEra (PState era) where
  type SpecRep ConwayEra (PState era) = Agda.PState

  toSpecRep PState {..} =
    Agda.MkPState
      <$> toSpecRep @_ @ConwayEra (Map.mapWithKey (stakePoolStateToStakePoolParams Testnet) psStakePools)
      <*> toSpecRep @_ @ConwayEra psFutureStakePoolParams
      <*> toSpecRep @_ @ConwayEra psRetiring

instance SpecTranslate ctx ConwayEra PoolCert where
  type SpecRep ConwayEra PoolCert = Agda.DCert

  toSpecRep (RegPool p@StakePoolParams {sppId = KeyHash ppHash}) =
    Agda.Regpool
      <$> toSpecRep @_ @ConwayEra ppHash
      <*> toSpecRep @_ @ConwayEra p
  toSpecRep (RetirePool (KeyHash ppHash) e) =
    Agda.Retirepool
      <$> toSpecRep @_ @ConwayEra ppHash
      <*> toSpecRep @_ @ConwayEra e
