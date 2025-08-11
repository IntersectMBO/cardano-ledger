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
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Rules (
  ConwayDelegEnv (..),
 )
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert (..),
  getDRepDelegatee,
  getStakePoolDelegatee,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Shelley.Rules
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Lens.Micro
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
      <*> toSpecRep
        (Map.mapKeys (hashToInteger . unKeyHash) $ Map.mapWithKey stakePoolStateToPoolParams cdePools)
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
      <*> toSpecRep (getDRepDelegatee d)
      <*> toSpecRep (hashToInteger . unKeyHash <$> getStakePoolDelegatee d)
      <*> pure 0
  toSpecRep (ConwayRegDelegCert s d c) =
    Agda.Delegate
      <$> toSpecRep s
      <*> toSpecRep (getDRepDelegatee d)
      <*> toSpecRep (hashToInteger . unKeyHash <$> getStakePoolDelegatee d)
      <*> toSpecRep c

instance ConwayEraAccounts era => SpecTranslate ctx (DState era) where
  type SpecRep (DState era) = Agda.DState

  toSpecRep dState =
    Agda.MkDState
      <$> toSpecRep (Map.mapMaybe (^. dRepDelegationAccountStateL) accountsMap)
      <*> toSpecRep (Map.mapMaybe (^. stakePoolDelegationAccountStateL) accountsMap)
      <*> toSpecRep (Map.map (fromCompact . (^. balanceAccountStateL)) accountsMap)
      <*> deposits
    where
      accountsMap = dState ^. accountsL . accountsMapL
      deposits = do
        let
          m = Map.map (fromCompact . (^. depositAccountStateL)) accountsMap
          transEntry (cred, val) =
            (,)
              <$> (Agda.CredentialDeposit <$> toSpecRep cred)
              <*> toSpecRep val
        Agda.MkHSMap <$> traverse transEntry (Map.toList m)
