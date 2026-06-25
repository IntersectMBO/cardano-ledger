{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-unsafe-ledger-internal #-}
#endif

module Cardano.Ledger.Alonzo.Era (
  AlonzoEra,
  UTXO,
  UTXOS,
  UTXOW,
  BBODY,
  LEDGER,
  LEDGERS,

  -- * Deprecated
  AlonzoUTXO,
  AlonzoUTXOS,
  AlonzoUTXOW,
  AlonzoBBODY,
  AlonzoLEDGER,
) where

import Cardano.Ledger.Internal.Era (AlonzoEra)
import Cardano.Ledger.Mary (MaryValue)
import Cardano.Ledger.Shelley.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

-- =====================================================

instance EraTxLevel AlonzoEra where
  type STxLevel l AlonzoEra = STxTopLevel l AlonzoEra

type instance Value AlonzoEra = MaryValue

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

-- These rules are new or changed in Alonzo

data LEDGERS era

type instance EraRule "LEDGERS" AlonzoEra = LEDGERS AlonzoEra

data UTXOS era

type AlonzoUTXOS = UTXOS

{-# DEPRECATED AlonzoUTXOS "In favor of `UTXOS`" #-}

type instance EraRule "UTXOS" AlonzoEra = UTXOS AlonzoEra

data UTXO era

type AlonzoUTXO = UTXO

{-# DEPRECATED AlonzoUTXO "In favor of `UTXO`" #-}

type instance EraRule "UTXO" AlonzoEra = UTXO AlonzoEra

data UTXOW era

type AlonzoUTXOW = UTXOW

{-# DEPRECATED AlonzoUTXOW "In favor of `UTXOW`" #-}

type instance EraRule "UTXOW" AlonzoEra = UTXOW AlonzoEra

data LEDGER era

type AlonzoLEDGER = LEDGER

{-# DEPRECATED AlonzoLEDGER "In favor of `LEDGER`" #-}

type instance EraRule "LEDGER" AlonzoEra = LEDGER AlonzoEra

data BBODY era

type AlonzoBBODY = BBODY

{-# DEPRECATED AlonzoBBODY "In favor of `BBODY`" #-}

type instance EraRule "BBODY" AlonzoEra = BBODY AlonzoEra

-- Rules inherited from Shelley

type instance EraRule "DELEG" AlonzoEra = Shelley.DELEG AlonzoEra

type instance EraRule "DELEGS" AlonzoEra = Shelley.DELEGS AlonzoEra

type instance EraRule "DELPL" AlonzoEra = Shelley.DELPL AlonzoEra

type instance EraRule "EPOCH" AlonzoEra = Shelley.EPOCH AlonzoEra

type instance EraRule "MIR" AlonzoEra = Shelley.MIR AlonzoEra

type instance EraRule "NEWEPOCH" AlonzoEra = Shelley.NEWEPOCH AlonzoEra

type instance EraRule "NEWPP" AlonzoEra = Shelley.NEWPP AlonzoEra

type instance EraRule "POOL" AlonzoEra = Shelley.POOL AlonzoEra

type instance EraRule "POOLREAP" AlonzoEra = Shelley.POOLREAP AlonzoEra

type instance EraRule "PPUP" AlonzoEra = Shelley.PPUP AlonzoEra

type instance EraRule "RUPD" AlonzoEra = Shelley.RUPD AlonzoEra

type instance EraRule "SNAP" AlonzoEra = Shelley.SNAP AlonzoEra

type instance EraRule "TICK" AlonzoEra = Shelley.TICK AlonzoEra

type instance EraRule "TICKF" AlonzoEra = Shelley.TICKF AlonzoEra

type instance EraRule "UPEC" AlonzoEra = Shelley.UPEC AlonzoEra
