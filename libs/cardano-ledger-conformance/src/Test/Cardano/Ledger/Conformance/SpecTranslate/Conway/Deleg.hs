{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core
import Test.Cardano.Ledger.Conway.TreeDiff

instance
  ( SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD Identity era)
  ) =>
  SpecTranslate ctx (ConwayDelegEnv era)
  where
  type SpecRep (ConwayDelegEnv era) = Agda.DelegEnv

  toSpecRep ConwayDelegEnv {..} =
    Agda.MkDelegEnv
      <$> toSpecRep cdePParams
      <*> toSpecRep (Map.mapKeys (hashToInteger . unKeyHash) cdePools)

instance SpecTranslate ctx (ConwayDelegCert c) where
  type SpecRep (ConwayDelegCert c) = Agda.TxCert

  toSpecRep (ConwayRegCert _ _) = throwError "RegCert not supported" -- TODO Investigate why!
  toSpecRep (ConwayUnRegCert c _) =
    Agda.Dereg <$> toSpecRep c
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
