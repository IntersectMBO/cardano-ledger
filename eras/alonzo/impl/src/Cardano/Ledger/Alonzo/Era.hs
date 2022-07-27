{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.Era
  ( AlonzoEra,
    AlonzoUTXO,
    AlonzoUTXOS,
    AlonzoUTXOW,
    AlonzoBBODY,
    AlonzoLEDGER,
  )
where

import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Mary.Value (MaryValue)
import qualified Cardano.Ledger.Shelley.API as API
import qualified Cardano.Ledger.Shelley.Rules.Epoch as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Mir as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Newpp as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Rupd as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Snap as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Tick as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Upec as Shelley

-- =====================================================

-- | The Alonzo era
data AlonzoEra c

instance CC.Crypto c => Era (AlonzoEra c) where
  type Crypto (AlonzoEra c) = c
  type ProtVerLow (AlonzoEra c) = 5
  type ProtVerHigh (AlonzoEra c) = 6

type instance Value (AlonzoEra c) = MaryValue c

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

-- These rules are new or changed in Alonzo

data AlonzoUTXOS era

type instance EraRule "UTXOS" (AlonzoEra c) = AlonzoUTXOS (AlonzoEra c)

data AlonzoUTXO era

type instance EraRule "UTXO" (AlonzoEra c) = AlonzoUTXO (AlonzoEra c)

data AlonzoUTXOW era

type instance EraRule "UTXOW" (AlonzoEra c) = AlonzoUTXOW (AlonzoEra c)

data AlonzoLEDGER era

type instance EraRule "LEDGER" (AlonzoEra c) = AlonzoLEDGER (AlonzoEra c)

data AlonzoBBODY era

type instance EraRule "BBODY" (AlonzoEra c) = AlonzoBBODY (AlonzoEra c)

-- Rules inherited from Shelley

type instance EraRule "DELEG" (AlonzoEra c) = API.DELEG (AlonzoEra c)

type instance EraRule "DELEGS" (AlonzoEra c) = API.DELEGS (AlonzoEra c)

type instance EraRule "DELPL" (AlonzoEra c) = API.DELPL (AlonzoEra c)

type instance EraRule "EPOCH" (AlonzoEra c) = Shelley.EPOCH (AlonzoEra c)

type instance EraRule "LEDGERS" (AlonzoEra c) = API.LEDGERS (AlonzoEra c)

type instance EraRule "MIR" (AlonzoEra c) = Shelley.MIR (AlonzoEra c)

type instance EraRule "NEWEPOCH" (AlonzoEra c) = API.NEWEPOCH (AlonzoEra c)

type instance EraRule "NEWPP" (AlonzoEra c) = Shelley.NEWPP (AlonzoEra c)

type instance EraRule "POOL" (AlonzoEra c) = API.POOL (AlonzoEra c)

type instance EraRule "POOLREAP" (AlonzoEra c) = API.POOLREAP (AlonzoEra c)

type instance EraRule "PPUP" (AlonzoEra c) = API.PPUP (AlonzoEra c)

type instance EraRule "RUPD" (AlonzoEra c) = Shelley.RUPD (AlonzoEra c)

type instance EraRule "SNAP" (AlonzoEra c) = Shelley.SNAP (AlonzoEra c)

type instance EraRule "TICK" (AlonzoEra c) = Shelley.TICK (AlonzoEra c)

type instance EraRule "TICKF" (AlonzoEra c) = Shelley.TICKF (AlonzoEra c)

type instance EraRule "UPEC" (AlonzoEra c) = Shelley.UPEC (AlonzoEra c)
