{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Utxo () where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure, shelleyToAllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosPredFailure,
  allegraToAlonzoUtxoPredFailure,
 )
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..))
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Rules.Utxos ()
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure (..))

type instance EraRuleFailure "UTXO" (ConwayEra c) = BabbageUtxoPredFailure (ConwayEra c)

instance InjectRuleFailure "UTXO" BabbageUtxoPredFailure (ConwayEra c)

instance InjectRuleFailure "UTXO" AlonzoUtxoPredFailure (ConwayEra c) where
  injectFailure = AlonzoInBabbageUtxoPredFailure

instance InjectRuleFailure "UTXO" ShelleyUtxoPredFailure (ConwayEra c) where
  injectFailure =
    AlonzoInBabbageUtxoPredFailure
      . allegraToAlonzoUtxoPredFailure
      . shelleyToAllegraUtxoPredFailure

instance InjectRuleFailure "UTXO" AllegraUtxoPredFailure (ConwayEra c) where
  injectFailure = AlonzoInBabbageUtxoPredFailure . allegraToAlonzoUtxoPredFailure

instance InjectRuleFailure "UTXO" AlonzoUtxosPredFailure (ConwayEra c) where
  injectFailure = AlonzoInBabbageUtxoPredFailure . UtxosFailure
