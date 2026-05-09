{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Ppup () where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "PPUP" MaryEra = Shelley.ShelleyPpupPredFailure MaryEra

instance InjectRuleFailure "PPUP" Shelley.ShelleyPpupPredFailure MaryEra
