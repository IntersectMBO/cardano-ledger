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
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Shelley.LedgerState (DState (..))
import Cardano.Ledger.Shelley.Rules
import qualified Cardano.Ledger.UMap as UMap
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  hashToInteger,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core

instance
  ( SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD Identity era)
  , Inject ctx (Set (Credential 'DRepRole))
  ) =>
  SpecTranslate ctx (ConwayDelegEnv era)
  where
  type SpecRep (ConwayDelegEnv era) = Agda.DelegEnv

  toSpecRep ConwayDelegEnv {..} = do
    delegatees <- askCtx @(Set (Credential 'DRepRole))
    Agda.MkDelegEnv
      <$> toSpecRep cdePParams
      <*> toSpecRep (Map.mapKeys (hashToInteger . unKeyHash) cdePools)
      <*> toSpecRep delegatees

instance SpecTranslate ctx ConwayDelegCert where
  type SpecRep ConwayDelegCert = Agda.DCert

  toSpecRep (ConwayRegCert c d) =
    Agda.Reg
      <$> toSpecRep c
      <*> strictMaybe (pure 0) toSpecRep d
  toSpecRep (ConwayUnRegCert c d) =
    Agda.Dereg
      <$> toSpecRep c
      <*> toSpecRep d
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

  toSpecRep = pure . showOpaqueErrorString

instance SpecTranslate ctx (DState era) where
  type SpecRep (DState era) = Agda.DState

  toSpecRep DState {..} =
    Agda.MkDState
      <$> toSpecRep (UMap.dRepMap dsUnified)
      <*> toSpecRep (UMap.sPoolMap dsUnified)
      <*> toSpecRep (UMap.rewardMap dsUnified)
      <*> toSpecRep deposits
    where
      deposits =
        Map.mapKeys CredentialDeposit (UMap.depositMap dsUnified)
