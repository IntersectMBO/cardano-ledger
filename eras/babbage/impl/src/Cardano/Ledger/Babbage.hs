{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- CanStartFromGenesis
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage (
  Babbage,
  BabbageEra,
  BabbageTxOut,
  BabbageTxBody,
  AlonzoScript,
  AlonzoTxAuxData,
  -- | All of these were defined in a different module.
  getDatumBabbage,
  babbageTxScripts,
  refScripts,
)
where

import Cardano.Ledger.Alonzo (reapplyAlonzoTx)
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx, ScriptPurpose)
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..))
import Cardano.Ledger.Alonzo.TxInfo (EraPlutusContext, ExtendedUTxO (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.PParams ()
import Cardano.Ledger.Babbage.Rules ()
import Cardano.Ledger.Babbage.Transition ()
import Cardano.Ledger.Babbage.Translation ()
import Cardano.Ledger.Babbage.Tx ()
import Cardano.Ledger.Babbage.TxAuxData ()
import Cardano.Ledger.Babbage.TxBody (
  BabbageTxBody,
  BabbageTxOut,
 )
import Cardano.Ledger.Babbage.TxInfo (babbageTxInfo)
import Cardano.Ledger.Babbage.UTxO (
  getBabbageScriptsProvided,
  getBabbageSpendingDatum,
  getReferenceScripts,
 )
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Plutus.Data (Data)
import Cardano.Ledger.Plutus.Language (Language (..))
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (ScriptsProvided (..), UTxO (..))
import qualified Data.Map.Strict as Map
import Data.Set (Set)

type Babbage = BabbageEra StandardCrypto

-- =====================================================

instance (Crypto c, DSignable c (Hash c EraIndependentTxBody)) => API.ApplyTx (BabbageEra c) where
  reapplyTx = reapplyAlonzoTx

instance (Crypto c, DSignable c (Hash c EraIndependentTxBody)) => API.ApplyBlock (BabbageEra c)

instance Crypto c => API.CanStartFromGenesis (BabbageEra c) where
  type AdditionalGenesisConfig (BabbageEra c) = AlonzoGenesis

  fromShelleyPParams ag = translateEra' () . API.fromShelleyPParams ag

instance (Crypto c, EraPlutusContext 'PlutusV2 (BabbageEra c)) => ExtendedUTxO (BabbageEra c) where
  txInfo = babbageTxInfo

-- | Extract binary data either directly from the `Tx` as an "inline datum"
-- or look it up in the witnesses by the hash.
getDatumBabbage ::
  ( AlonzoEraTx era
  , BabbageEraTxOut era
  ) =>
  Tx era ->
  UTxO era ->
  ScriptPurpose era ->
  Maybe (Data era)
getDatumBabbage tx utxo = getBabbageSpendingDatum utxo tx
{-# DEPRECATED getDatumBabbage "In favor of `getBabbageSpendingDatum`" #-}

babbageTxScripts ::
  ( EraTx era
  , BabbageEraTxBody era
  ) =>
  UTxO era ->
  Tx era ->
  Map.Map (ScriptHash (EraCrypto era)) (Script era)
babbageTxScripts utxo = unScriptsProvided . getBabbageScriptsProvided utxo
{-# DEPRECATED babbageTxScripts "In favor of `getScriptsProvided`" #-}

refScripts ::
  BabbageEraTxOut era =>
  Set (TxIn (EraCrypto era)) ->
  UTxO era ->
  Map.Map (ScriptHash (EraCrypto era)) (Script era)
refScripts = flip getReferenceScripts
{-# DEPRECATED refScripts "In favor of `getReferenceScripts`" #-}
