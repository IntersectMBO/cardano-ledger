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
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert (..),
  getDRepDelegatee,
  getStakePoolDelegatee,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  hashToInteger,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()

instance SpecTranslate ConwayEra (Conway.ConwayDelegEnv ConwayEra) where
  type SpecRep ConwayEra (Conway.ConwayDelegEnv ConwayEra) = Agda.DelegEnv
  type SpecContext ConwayEra (Conway.ConwayDelegEnv ConwayEra) = Set (Credential DRepRole)

  toSpecRep Conway.ConwayDelegEnv {..} = do
    delegatees <- askSpecTransM
    withCtxSpecTransM () $
      Agda.MkDelegEnv
        <$> toSpecRep cdePParams
        <*> ( toSpecRepMap
                ( Map.mapKeys (hashToInteger . unKeyHash) $
                    Map.mapWithKey (stakePoolStateToStakePoolParams Testnet) cdePools
                )
            )
        <*> toSpecRep delegatees

instance SpecTranslate ConwayEra ConwayDelegCert where
  type SpecRep ConwayEra ConwayDelegCert = Agda.DCert

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

instance SpecTranslate ConwayEra (DState ConwayEra) where
  type SpecRep ConwayEra (DState ConwayEra) = Agda.DState

  toSpecRep dState =
    Agda.MkDState
      <$> toSpecRepMap (Map.mapMaybe (view dRepDelegationAccountStateL) accountsMap)
      <*> toSpecRepMap (Map.mapMaybe (view stakePoolDelegationAccountStateL) accountsMap)
      <*> toSpecRepMap (Map.map (fromCompact . (view balanceAccountStateL)) accountsMap)
      <*> deposits
    where
      accountsMap = dState ^. accountsL . accountsMapL
      deposits = do
        let
          m = Map.map (fromCompact . (view depositAccountStateL)) accountsMap
          transEntry (cred, val) =
            (,)
              <$> (Agda.CredentialDeposit <$> toSpecRep cred)
              <*> toSpecRep val
        Agda.MkHSMap <$> traverse transEntry (Map.toList m)
