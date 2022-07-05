{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Babbage.Era
  ( BabbageEra,
    BabbageUTXO,
    BabbageUTXOS,
    BabbageUTXOW,
    BabbageLEDGER,
  )
where

import Cardano.Ledger.Alonzo.Rules (AlonzoBBODY)
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

-- | The Babbage era
data BabbageEra c

instance CC.Crypto c => Era (BabbageEra c) where
  type Crypto (BabbageEra c) = c
  type ProtVerLow (BabbageEra c) = 7

type instance Value (BabbageEra c) = MaryValue c

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

data BabbageUTXOS era

type instance EraRule "UTXOS" (BabbageEra c) = BabbageUTXOS (BabbageEra c)

data BabbageUTXO era

type instance EraRule "UTXO" (BabbageEra c) = BabbageUTXO (BabbageEra c)

data BabbageUTXOW era

type instance EraRule "UTXOW" (BabbageEra c) = BabbageUTXOW (BabbageEra c)

data BabbageLEDGER c

type instance EraRule "LEDGER" (BabbageEra c) = BabbageLEDGER (BabbageEra c)

-- Rules inherited from Alonzo

type instance EraRule "BBODY" (BabbageEra c) = AlonzoBBODY (BabbageEra c)

-- Rules inherited from Shelley

type instance EraRule "DELEG" (BabbageEra c) = API.DELEG (BabbageEra c)

type instance EraRule "DELEGS" (BabbageEra c) = API.DELEGS (BabbageEra c)

type instance EraRule "DELPL" (BabbageEra c) = API.DELPL (BabbageEra c)

type instance EraRule "EPOCH" (BabbageEra c) = Shelley.EPOCH (BabbageEra c)

type instance EraRule "LEDGERS" (BabbageEra c) = API.LEDGERS (BabbageEra c)

type instance EraRule "MIR" (BabbageEra c) = Shelley.MIR (BabbageEra c)

type instance EraRule "NEWEPOCH" (BabbageEra c) = API.NEWEPOCH (BabbageEra c)

type instance EraRule "NEWPP" (BabbageEra c) = Shelley.NEWPP (BabbageEra c)

type instance EraRule "POOL" (BabbageEra c) = API.POOL (BabbageEra c)

type instance EraRule "POOLREAP" (BabbageEra c) = API.POOLREAP (BabbageEra c)

type instance EraRule "PPUP" (BabbageEra c) = API.PPUP (BabbageEra c)

type instance EraRule "RUPD" (BabbageEra c) = Shelley.RUPD (BabbageEra c)

type instance EraRule "SNAP" (BabbageEra c) = Shelley.SNAP (BabbageEra c)

type instance EraRule "TICK" (BabbageEra c) = Shelley.TICK (BabbageEra c)

type instance EraRule "TICKF" (BabbageEra c) = Shelley.TICKF (BabbageEra c)

type instance EraRule "UPEC" (BabbageEra c) = Shelley.UPEC (BabbageEra c)

-- =================================================
