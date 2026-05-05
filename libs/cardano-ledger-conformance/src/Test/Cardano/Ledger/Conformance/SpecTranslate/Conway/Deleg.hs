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
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()

instance
  ( SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate (PParamsHKD Identity era)
  , SpecContext (PParamsHKD Identity era) ~ ()
  ) =>
  SpecTranslate (ConwayDelegEnv era)
  where
  type SpecRep (ConwayDelegEnv era) = Agda.DelegEnv
  type SpecContext (ConwayDelegEnv era) = Set (Credential DRepRole)

  toSpecRep ConwayDelegEnv {..} = do
    delegatees <- askSpecTransM
    withSpecTransM (const ()) $
      Agda.MkDelegEnv
        <$> toSpecRep cdePParams
        <*> withSpecTransM (const ((), ())) (toSpecRep
          ( Map.mapKeys (hashToInteger . unKeyHash) $
              Map.mapWithKey (stakePoolStateToStakePoolParams Testnet) cdePools
          ))
        <*> toSpecRep delegatees

instance SpecTranslate ConwayDelegCert where
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

instance ConwayEraAccounts era => SpecTranslate (DState era) where
  type SpecRep (DState era) = Agda.DState

  toSpecRep dState =
    Agda.MkDState
      <$> withSpecTransM dup (toSpecRep (Map.mapMaybe (^. dRepDelegationAccountStateL) accountsMap))
      <*> withSpecTransM dup (toSpecRep (Map.mapMaybe (^. stakePoolDelegationAccountStateL) accountsMap))
      <*> withSpecTransM dup (toSpecRep (Map.map (fromCompact . (^. balanceAccountStateL)) accountsMap))
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
