{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- CanStartFromGenesis
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway (
  Conway,
  ConwayEra,
)
where

import Cardano.Ledger.Alonzo (reapplyAlonzoTx)
import Cardano.Ledger.Alonzo.TxInfo (EraPlutusContext, ExtendedUTxO (..))
import Cardano.Ledger.Babbage.TxBody ()
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Governance (RunConwayRatify (..))
import Cardano.Ledger.Conway.Rules ()
import Cardano.Ledger.Conway.Transition ()
import Cardano.Ledger.Conway.Translation ()
import Cardano.Ledger.Conway.Tx ()
import Cardano.Ledger.Conway.TxInfo (conwayTxInfo)
import Cardano.Ledger.Conway.TxOut ()
import Cardano.Ledger.Conway.UTxO ()
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Plutus.Language (Language (..))
import qualified Cardano.Ledger.Shelley.API as API

type Conway = ConwayEra StandardCrypto

-- =====================================================

instance
  ( Crypto c
  , DSignable c (Hash c EraIndependentTxBody)
  , EraPlutusContext 'PlutusV2 (ConwayEra c)
  , EraPlutusContext 'PlutusV3 (ConwayEra c)
  ) =>
  API.ApplyTx (ConwayEra c)
  where
  reapplyTx = reapplyAlonzoTx

instance
  ( Crypto c
  , DSignable c (Hash c EraIndependentTxBody)
  , EraPlutusContext 'PlutusV2 (ConwayEra c)
  , EraPlutusContext 'PlutusV3 (ConwayEra c)
  ) =>
  API.ApplyBlock (ConwayEra c)

instance Crypto c => API.CanStartFromGenesis (ConwayEra c) where
  type AdditionalGenesisConfig (ConwayEra c) = ConwayGenesis c
  fromShelleyPParams =
    error "Unimplemented: Current interface is too limited and needs replacement for Conway to work"

instance
  ( Crypto c
  , EraPlutusContext 'PlutusV2 (ConwayEra c)
  , EraPlutusContext 'PlutusV3 (ConwayEra c)
  ) =>
  ExtendedUTxO (ConwayEra c)
  where
  txInfo = conwayTxInfo

instance Crypto c => RunConwayRatify (ConwayEra c)
