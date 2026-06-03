{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Deleg () where

import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert (
  getDRepDelegatee,
  getStakePoolDelegatee,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxCert (DijkstraDelegCert (..))
import qualified Data.Map.Strict as Map
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Base ()
import Test.Cardano.Ledger.Conformance.Utils (hashToInteger)

instance SpecTranslate DijkstraEra DijkstraDelegCert where
  type SpecRep DijkstraEra DijkstraDelegCert = Agda.DCert

  toSpecRep (DijkstraRegCert c d) =
    Agda.Delegate
      <$> toSpecRep c
      <*> pure Nothing
      <*> pure Nothing
      <*> toSpecRep d
  toSpecRep (DijkstraUnRegCert c d) =
    Agda.Dereg
      <$> toSpecRep c
      <*> toSpecRep (Just d)
  toSpecRep (DijkstraDelegCert c d) =
    Agda.Delegate
      <$> toSpecRep c
      <*> toSpecRep (getDRepDelegatee d)
      <*> toSpecRep (hashToInteger . unKeyHash <$> getStakePoolDelegatee d)
      <*> pure 0
  toSpecRep (DijkstraRegDelegCert s d c) =
    Agda.Delegate
      <$> toSpecRep s
      <*> toSpecRep (getDRepDelegatee d)
      <*> toSpecRep (hashToInteger . unKeyHash <$> getStakePoolDelegatee d)
      <*> toSpecRep c

instance SpecTranslate DijkstraEra (DState DijkstraEra) where
  type SpecRep DijkstraEra (DState DijkstraEra) = Agda.DState

  toSpecRep dState =
    Agda.MkDState
      <$> toSpecRepMap (Map.mapMaybe (view dRepDelegationAccountStateL) accountsMap)
      <*> toSpecRepMap (Map.mapMaybe (view stakePoolDelegationAccountStateL) accountsMap)
      <*> toSpecRepMap (Map.map (fromCompact . view balanceAccountStateL) accountsMap)
      <*> toSpecRepMap (Map.map (fromCompact . view depositAccountStateL) accountsMap)
    where
      accountsMap = dState ^. accountsL . accountsMapL
