{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.EraMapping where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Core (EraRule)
import Cardano.Protocol.TPraos.Rules.Tickn (TICKN)

type instance EraRule "TICKN" AlonzoEra = TICKN
