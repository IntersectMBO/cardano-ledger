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
import Cardano.Ledger.Conway (ConwayEra)
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
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()

instance
  ( SpecRep ConwayEra (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx ConwayEra (PParamsHKD Identity era)
  , Inject ctx (Set (Credential DRepRole))
  ) =>
  SpecTranslate ctx ConwayEra (ConwayDelegEnv era)
  where
  type SpecRep ConwayEra (ConwayDelegEnv era) = Agda.DelegEnv

  toSpecRep ConwayDelegEnv {..} = do
    delegatees <- askCtx @(Set (Credential DRepRole))
    Agda.MkDelegEnv
      <$> toSpecRep @_ @ConwayEra cdePParams
      <*> toSpecRep @_ @ConwayEra
        ( Map.mapKeys (hashToInteger . unKeyHash) $
            Map.mapWithKey (stakePoolStateToStakePoolParams Testnet) cdePools
        )
      <*> toSpecRep @_ @ConwayEra delegatees

instance SpecTranslate ctx ConwayEra ConwayDelegCert where
  type SpecRep ConwayEra ConwayDelegCert = Agda.DCert

  toSpecRep (ConwayRegCert c d) =
    Agda.Reg
      <$> toSpecRep @_ @ConwayEra c
      <*> strictMaybe (pure 0) (toSpecRep @_ @ConwayEra) d
  toSpecRep (ConwayUnRegCert c d) =
    Agda.Dereg
      <$> toSpecRep @_ @ConwayEra c
      <*> toSpecRep @_ @ConwayEra d
  toSpecRep (ConwayDelegCert c d) =
    Agda.Delegate
      <$> toSpecRep @_ @ConwayEra c
      <*> toSpecRep @_ @ConwayEra (getDRepDelegatee d)
      <*> toSpecRep @_ @ConwayEra (hashToInteger . unKeyHash <$> getStakePoolDelegatee d)
      <*> pure 0
  toSpecRep (ConwayRegDelegCert s d c) =
    Agda.Delegate
      <$> toSpecRep @_ @ConwayEra s
      <*> toSpecRep @_ @ConwayEra (getDRepDelegatee d)
      <*> toSpecRep @_ @ConwayEra (hashToInteger . unKeyHash <$> getStakePoolDelegatee d)
      <*> toSpecRep @_ @ConwayEra c

instance ConwayEraAccounts era => SpecTranslate ctx ConwayEra (DState era) where
  type SpecRep ConwayEra (DState era) = Agda.DState

  toSpecRep dState =
    Agda.MkDState
      <$> toSpecRep @_ @ConwayEra (Map.mapMaybe (^. dRepDelegationAccountStateL) accountsMap)
      <*> toSpecRep @_ @ConwayEra (Map.mapMaybe (^. stakePoolDelegationAccountStateL) accountsMap)
      <*> toSpecRep @_ @ConwayEra (Map.map (fromCompact . (^. balanceAccountStateL)) accountsMap)
      <*> deposits
    where
      accountsMap = dState ^. accountsL . accountsMapL
      deposits = do
        let
          m = Map.map (fromCompact . (^. depositAccountStateL)) accountsMap
          transEntry (cred, val) =
            (,)
              <$> (Agda.CredentialDeposit <$> toSpecRep @_ @ConwayEra cred)
              <*> toSpecRep @_ @ConwayEra val
        Agda.MkHSMap <$> traverse transEntry (Map.toList m)
