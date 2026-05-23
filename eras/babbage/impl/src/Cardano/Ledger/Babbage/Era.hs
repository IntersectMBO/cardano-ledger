{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-unsafe-ledger-internal #-}
#endif

module Cardano.Ledger.Babbage.Era (
  BabbageEra,
  UTXO,
  UTXOS,
  UTXOW,
  LEDGER,
) where

import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Core
import Cardano.Ledger.Genesis (EraGenesis, NoGenesis)
import Cardano.Ledger.Internal.Era (BabbageEra)
import Cardano.Ledger.Mary.Value (MaryValue)
import qualified Cardano.Ledger.Shelley.API as API
import qualified Cardano.Ledger.Shelley.Rules as Shelley

-- =====================================================

instance EraGenesis BabbageEra

instance EraTxLevel BabbageEra where
  type STxLevel l BabbageEra = STxTopLevel l BabbageEra

type instance TranslationContext BabbageEra = NoGenesis BabbageEra

type instance Value BabbageEra = MaryValue

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

data UTXOS era

type instance EraRule "UTXOS" BabbageEra = UTXOS BabbageEra

data UTXO era

type instance EraRule "UTXO" BabbageEra = UTXO BabbageEra

data UTXOW era

type instance EraRule "UTXOW" BabbageEra = UTXOW BabbageEra

data LEDGER c

type instance EraRule "LEDGER" BabbageEra = LEDGER BabbageEra

-- Rules inherited from Alonzo

type instance EraRule "BBODY" BabbageEra = Alonzo.BBODY BabbageEra

-- Rules inherited from Shelley

type instance EraRule "DELEG" BabbageEra = API.DELEG BabbageEra

type instance EraRule "DELEGS" BabbageEra = API.DELEGS BabbageEra

type instance EraRule "DELPL" BabbageEra = API.DELPL BabbageEra

type instance EraRule "EPOCH" BabbageEra = Shelley.EPOCH BabbageEra

type instance EraRule "LEDGERS" BabbageEra = API.LEDGERS BabbageEra

type instance EraRule "MIR" BabbageEra = Shelley.MIR BabbageEra

type instance EraRule "NEWEPOCH" BabbageEra = API.NEWEPOCH BabbageEra

type instance EraRule "NEWPP" BabbageEra = Shelley.NEWPP BabbageEra

type instance EraRule "POOL" BabbageEra = API.POOL BabbageEra

type instance EraRule "POOLREAP" BabbageEra = API.POOLREAP BabbageEra

type instance EraRule "PPUP" BabbageEra = API.PPUP BabbageEra

type instance EraRule "RUPD" BabbageEra = Shelley.RUPD BabbageEra

type instance EraRule "SNAP" BabbageEra = Shelley.SNAP BabbageEra

type instance EraRule "TICK" BabbageEra = Shelley.TICK BabbageEra

type instance EraRule "TICKF" BabbageEra = Shelley.TICKF BabbageEra

type instance EraRule "UPEC" BabbageEra = Shelley.UPEC BabbageEra

-- =================================================
