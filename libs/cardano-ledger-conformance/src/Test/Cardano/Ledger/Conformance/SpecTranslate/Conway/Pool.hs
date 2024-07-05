{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool where

import Cardano.Ledger.CertState
import Cardano.Ledger.Core
import Cardano.Ledger.Keys
import Cardano.Ledger.PoolParams
import Cardano.Ledger.Shelley.Rules
import Data.Map.Strict (mapKeys)
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conway.TreeDiff

instance
  ( SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD Identity era)
  ) =>
  SpecTranslate ctx (PoolEnv era)
  where
  type SpecRep (PoolEnv era) = Agda.PParams

  toSpecRep (PoolEnv _ pp) = toSpecRep pp

instance SpecTranslate ctx (ShelleyPoolPredFailure era) where
  type SpecRep (ShelleyPoolPredFailure era) = OpaqueErrorString

  toSpecRep e = pure . OpaqueErrorString . show $ toExpr e

instance SpecTranslate ctx (PState era) where
  type SpecRep (PState era) = Agda.PState

  toSpecRep PState {..} =
    Agda.MkPState
      <$> toSpecRep (mapKeys (hashToInteger . unKeyHash) psStakePoolParams)
      <*> toSpecRep (mapKeys (hashToInteger . unKeyHash) psRetiring)

instance SpecTranslate ctx (PoolCert c) where
  type SpecRep (PoolCert c) = Agda.TxCert

  toSpecRep (RegPool p@PoolParams {ppId = KeyHash ppHash}) =
    Agda.RegPool
      <$> toSpecRep ppHash
      <*> toSpecRep p
  toSpecRep (RetirePool (KeyHash ppHash) e) =
    Agda.RetirePool
      <$> toSpecRep ppHash
      <*> toSpecRep e
