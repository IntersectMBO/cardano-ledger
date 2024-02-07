{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Utxo () where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure (..), shelleyToAllegraUtxoPredFailure)
import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Rules.Ppup ()
import Cardano.Ledger.Shelley.Rules (
  ShelleyPpupPredFailure,
  ShelleyUtxoPredFailure,
 )

type instance EraRuleFailure "UTXO" (MaryEra c) = AllegraUtxoPredFailure (MaryEra c)

instance InjectRuleFailure "UTXO" AllegraUtxoPredFailure (MaryEra c) where
  injectFailure = id

instance InjectRuleFailure "UTXO" ShelleyUtxoPredFailure (MaryEra c) where
  injectFailure = shelleyToAllegraUtxoPredFailure

instance InjectRuleFailure "UTXO" ShelleyPpupPredFailure (MaryEra c) where
  injectFailure = UpdateFailure
