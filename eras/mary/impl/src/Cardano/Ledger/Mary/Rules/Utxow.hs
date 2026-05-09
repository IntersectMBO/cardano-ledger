{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Utxow () where

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Rules.Utxo ()
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "UTXOW" MaryEra = Shelley.ShelleyUtxowPredFailure MaryEra

instance InjectRuleFailure "UTXOW" Shelley.ShelleyUtxowPredFailure MaryEra

instance InjectRuleFailure "UTXOW" Allegra.AllegraUtxoPredFailure MaryEra where
  injectFailure = Shelley.UtxoFailure

instance InjectRuleFailure "UTXOW" Shelley.ShelleyUtxoPredFailure MaryEra where
  injectFailure = Shelley.UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" Shelley.ShelleyPpupPredFailure MaryEra where
  injectFailure = Shelley.UtxoFailure . injectFailure
