{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Cert () where

import Cardano.Crypto.Util (bytesToNatural)
import Cardano.Ledger.BaseTypes (Network)
import Cardano.Ledger.Binary (rawEncodeFixedSized)
import Cardano.Ledger.Conway.State (ConwayCertState (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxCert (DijkstraTxCert (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.State (BlsKey (..))
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTranslate (..),
  withCtxSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Deleg ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.GovCert ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Pool ()

instance SpecTranslate DijkstraEra (ConwayCertState DijkstraEra) where
  type SpecRep DijkstraEra (ConwayCertState DijkstraEra) = Agda.CertState

  type SpecContext DijkstraEra (ConwayCertState DijkstraEra) = Network

  toSpecRep ConwayCertState {..} =
    Agda.MkCertState
      <$> withCtxSpecTransM () (toSpecRep conwayCertDState)
      <*> toSpecRep conwayCertPState
      <*> withCtxSpecTransM () (toSpecRep conwayCertVState)

instance SpecTranslate DijkstraEra (DijkstraTxCert DijkstraEra) where
  type SpecRep DijkstraEra (DijkstraTxCert DijkstraEra) = Agda.DCert

  toSpecRep (DijkstraTxCertPool p) = toSpecRep p
  toSpecRep (DijkstraTxCertGov c) = toSpecRep c
  toSpecRep (DijkstraTxCertDeleg x) = toSpecRep x
  -- The specification abstracts the key and its proof as naturals, so both are carried
  -- across as the integer their fixed-size encoding denotes.
  toSpecRep (DijkstraTxCertRegBlsKey (KeyHash poolHash) BlsKey {blsPubKey, blsPossessionProof}) =
    Agda.Regblskey
      <$> toSpecRep poolHash
      <*> pure (bytesToInteger (rawEncodeFixedSized blsPubKey))
      <*> pure (bytesToInteger (rawEncodeFixedSized blsPossessionProof))
    where
      bytesToInteger = toInteger . bytesToNatural
