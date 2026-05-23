{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-unsafe-ledger-internal #-}
#endif

module Cardano.Ledger.Babbage.Era (
  BabbageEra,
  BabbageUTXO,
  BabbageUTXOS,
  BabbageUTXOW,
  BabbageLEDGER,
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

data BabbageUTXOS era

type instance EraRule "UTXOS" BabbageEra = BabbageUTXOS BabbageEra

data BabbageUTXO era

type instance EraRule "UTXO" BabbageEra = BabbageUTXO BabbageEra

data BabbageUTXOW era

type instance EraRule "UTXOW" BabbageEra = BabbageUTXOW BabbageEra

data BabbageLEDGER c

type instance EraRule "LEDGER" BabbageEra = BabbageLEDGER BabbageEra

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
