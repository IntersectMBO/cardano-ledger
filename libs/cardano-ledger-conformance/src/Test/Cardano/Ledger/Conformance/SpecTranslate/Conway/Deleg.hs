{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg () where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Rules (
  ConwayDelegEnv (..),
  ConwayDelegPredFailure,
 )
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert (..),
  getStakePoolDelegatee,
  getVoteDelegatee,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Shelley.LedgerState (DState (..))
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.UMap (dRepMap, rewardMap, sPoolMap)
import qualified Data.Map.Strict as Map
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance (
  hashToInteger,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.GovCert ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core
import Test.Cardano.Ledger.Constrained.Conway.Deleg
import Test.Cardano.Ledger.Conway.TreeDiff

instance
  ( SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD Identity era)
  ) =>
  SpecTranslate ctx (DelegExecEnv era)
  where
  type SpecRep (DelegExecEnv era) = Agda.DelegEnv
  toSpecRep DelegExecEnv {..} = do
    Agda.MkDelegEnv
      <$> toSpecRep (cdePParams deeDelegEnv)
      <*> toSpecRep (Map.mapKeys (hashToInteger . unKeyHash) (cdePools deeDelegEnv))
      <*> toSpecRep deeDeposits

instance SpecTranslate ctx (ConwayDelegCert c) where
  type SpecRep (ConwayDelegCert c) = Agda.TxCert

  toSpecRep (ConwayRegCert c d) =
    Agda.Delegate
      <$> toSpecRep c
      <*> pure Nothing
      <*> pure Nothing
      <*> strictMaybe (pure 0) toSpecRep d
  toSpecRep (ConwayUnRegCert c d) =
    Agda.Dereg
      <$> toSpecRep c
      <*> strictMaybe (pure 0) toSpecRep d
  toSpecRep (ConwayDelegCert c d) =
    Agda.Delegate
      <$> toSpecRep c
      <*> toSpecRep (getVoteDelegatee d)
      <*> toSpecRep (hashToInteger . unKeyHash <$> getStakePoolDelegatee d)
      <*> pure 0
  toSpecRep (ConwayRegDelegCert s d c) =
    Agda.Delegate
      <$> toSpecRep s
      <*> toSpecRep (getVoteDelegatee d)
      <*> toSpecRep (hashToInteger . unKeyHash <$> getStakePoolDelegatee d)
      <*> toSpecRep c

instance SpecTranslate ctx (ConwayDelegPredFailure era) where
  type SpecRep (ConwayDelegPredFailure era) = OpaqueErrorString

  toSpecRep e = pure . OpaqueErrorString . show $ toExpr e

instance SpecTranslate ctx (DState era) where
  type SpecRep (DState era) = Agda.DState

  toSpecRep DState {..} =
    Agda.MkDState
      <$> toSpecRep (dRepMap dsUnified)
      <*> toSpecRep (sPoolMap dsUnified)
      <*> toSpecRep (rewardMap dsUnified)
