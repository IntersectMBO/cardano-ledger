{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Alonzo.EraMapping where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Core (EraRule)
import Cardano.Protocol.TPraos.Rules.Tickn (TICKN)
import Test.Cardano.Ledger.EraBuffet (TestCrypto)

type instance EraRule "TICKN" (AlonzoEra TestCrypto) = TICKN
