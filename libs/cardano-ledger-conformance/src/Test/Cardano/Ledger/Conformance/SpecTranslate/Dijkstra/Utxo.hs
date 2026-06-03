{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Utxo () where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTranslate (..),
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Base ()

instance SpecTranslate DijkstraEra (UTxOState DijkstraEra) where
  type SpecRep DijkstraEra (UTxOState DijkstraEra) = Agda.UTxOState

  toSpecRep UTxOState {..} =
    Agda.MkUTxOState
      <$> toSpecRep utxosUtxo
      <*> toSpecRep utxosFees
      <*> toSpecRep utxosDonation
