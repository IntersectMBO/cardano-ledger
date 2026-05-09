{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool where

import Cardano.Ledger.BaseTypes (Network (Testnet))
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State
import qualified Data.Map.Strict as Map
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()

instance
  ( SpecRep (PParamsHKD Shelley.Identity era) ~ Agda.PParams
  , SpecTranslate (PParamsHKD Shelley.Identity era)
  , SpecContext (PParamsHKD Shelley.Identity era) ~ ()
  ) =>
  SpecTranslate (Shelley.PoolEnv era)
  where
  type SpecRep (Shelley.PoolEnv era) = Agda.PParams

  toSpecRep (Shelley.PoolEnv _ pp) = toSpecRep pp

instance SpecTranslate (PState era) where
  type SpecRep (PState era) = Agda.PState

  toSpecRep PState {..} =
    Agda.MkPState
      <$> toSpecRepMap (Map.mapWithKey (stakePoolStateToStakePoolParams Testnet) psStakePools)
      <*> toSpecRepMap psFutureStakePoolParams
      <*> toSpecRepMap psRetiring

instance SpecTranslate PoolCert where
  type SpecRep PoolCert = Agda.DCert

  toSpecRep (RegPool p@StakePoolParams {sppId = KeyHash ppHash}) =
    Agda.Regpool
      <$> toSpecRep ppHash
      <*> toSpecRep p
  toSpecRep (RetirePool (KeyHash ppHash) e) =
    Agda.Retirepool
      <$> toSpecRep ppHash
      <*> toSpecRep e
