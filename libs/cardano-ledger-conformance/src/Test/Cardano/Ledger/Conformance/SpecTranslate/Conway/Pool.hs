{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool where

import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.State
import qualified Data.Map.Strict as Map
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()

instance
  ( SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD Identity era)
  ) =>
  SpecTranslate ctx (PoolEnv era)
  where
  type SpecRep (PoolEnv era) = Agda.PParams

  toSpecRep (PoolEnv _ pp) = toSpecRep pp

instance SpecTranslate ctx (PState era) where
  type SpecRep (PState era) = Agda.PState

  toSpecRep PState {..} =
    Agda.MkPState
      <$> toSpecRep (Map.mapWithKey stakePoolStateToPoolParams psStakePools)
      <*> toSpecRep (Map.mapWithKey stakePoolStateToPoolParams psFutureStakePools)
      <*> toSpecRep psRetiring

instance SpecTranslate ctx PoolCert where
  type SpecRep PoolCert = Agda.DCert

  toSpecRep (RegPool p@PoolParams {ppId = KeyHash ppHash}) =
    Agda.Regpool
      <$> toSpecRep ppHash
      <*> toSpecRep p
  toSpecRep (RetirePool (KeyHash ppHash) e) =
    Agda.Retirepool
      <$> toSpecRep ppHash
      <*> toSpecRep e
