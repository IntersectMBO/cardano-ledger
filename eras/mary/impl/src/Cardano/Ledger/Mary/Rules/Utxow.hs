{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Utxow () where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Rules.Utxo ()
import Cardano.Ledger.Shelley.Rules (
  ShelleyPpupPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure (..),
 )

type instance EraRuleFailure "UTXOW" MaryEra = ShelleyUtxowPredFailure MaryEra

instance InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure MaryEra

instance InjectRuleFailure "UTXOW" AllegraUtxoPredFailure MaryEra where
  injectFailure = UtxoFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxoPredFailure MaryEra where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ShelleyPpupPredFailure MaryEra where
  injectFailure = UtxoFailure . injectFailure
