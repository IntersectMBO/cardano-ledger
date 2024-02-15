{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Ppup () where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Shelley.Rules (ShelleyPpupPredFailure)

type instance EraRuleFailure "PPUP" (MaryEra c) = ShelleyPpupPredFailure (MaryEra c)

instance InjectRuleFailure "PPUP" ShelleyPpupPredFailure (MaryEra c)
