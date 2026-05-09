{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Utxo () where

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Rules.Ppup ()
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "UTXO" MaryEra = Allegra.AllegraUtxoPredFailure MaryEra

instance InjectRuleFailure "UTXO" Allegra.AllegraUtxoPredFailure MaryEra

instance InjectRuleFailure "UTXO" Shelley.ShelleyUtxoPredFailure MaryEra where
  injectFailure = Allegra.shelleyToAllegraUtxoPredFailure

instance InjectRuleFailure "UTXO" Shelley.ShelleyPpupPredFailure MaryEra where
  injectFailure = Allegra.UpdateFailure
