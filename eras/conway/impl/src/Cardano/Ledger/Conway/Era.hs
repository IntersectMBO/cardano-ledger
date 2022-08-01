{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Conway.Era
  ( ConwayEra,
  )
where

import Cardano.Ledger.Alonzo.Rules (AlonzoBBODY)
import Cardano.Ledger.Babbage.Rules (BabbageLEDGER, BabbageUTXO, BabbageUTXOS, BabbageUTXOW)
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
data ConwayEra c

instance CC.Crypto c => Era (ConwayEra c) where
  type Crypto (ConwayEra c) = c
  type ProtVerLow (ConwayEra c) = 8

type instance Value (ConwayEra c) = MaryValue c

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

-- Rules inherited from Babbage

type instance EraRule "UTXOS" (ConwayEra c) = BabbageUTXOS (ConwayEra c)

type instance EraRule "UTXO" (ConwayEra c) = BabbageUTXO (ConwayEra c)

type instance EraRule "UTXOW" (ConwayEra c) = BabbageUTXOW (ConwayEra c)

type instance EraRule "LEDGER" (ConwayEra c) = BabbageLEDGER (ConwayEra c)

-- Rules inherited from Alonzo

type instance EraRule "BBODY" (ConwayEra c) = AlonzoBBODY (ConwayEra c)

-- Rules inherited from Shelley

type instance EraRule "DELEG" (ConwayEra c) = API.DELEG (ConwayEra c)

type instance EraRule "DELEGS" (ConwayEra c) = API.DELEGS (ConwayEra c)

type instance EraRule "DELPL" (ConwayEra c) = API.DELPL (ConwayEra c)

type instance EraRule "EPOCH" (ConwayEra c) = Shelley.EPOCH (ConwayEra c)

type instance EraRule "LEDGERS" (ConwayEra c) = API.LEDGERS (ConwayEra c)

type instance EraRule "MIR" (ConwayEra c) = Shelley.MIR (ConwayEra c)

type instance EraRule "NEWEPOCH" (ConwayEra c) = API.NEWEPOCH (ConwayEra c)

type instance EraRule "NEWPP" (ConwayEra c) = Shelley.NEWPP (ConwayEra c)

type instance EraRule "POOL" (ConwayEra c) = API.POOL (ConwayEra c)

type instance EraRule "POOLREAP" (ConwayEra c) = API.POOLREAP (ConwayEra c)

type instance EraRule "PPUP" (ConwayEra c) = API.PPUP (ConwayEra c)

type instance EraRule "RUPD" (ConwayEra c) = Shelley.RUPD (ConwayEra c)

type instance EraRule "SNAP" (ConwayEra c) = Shelley.SNAP (ConwayEra c)

type instance EraRule "TICK" (ConwayEra c) = Shelley.TICK (ConwayEra c)

type instance EraRule "TICKF" (ConwayEra c) = Shelley.TICKF (ConwayEra c)

type instance EraRule "UPEC" (ConwayEra c) = Shelley.UPEC (ConwayEra c)

-- =================================================
