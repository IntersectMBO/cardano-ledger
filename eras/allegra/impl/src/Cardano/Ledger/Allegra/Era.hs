{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-unsafe-ledger-internal #-}
#endif

module Cardano.Ledger.Allegra.Era (
  AllegraEra,
  AllegraUTXO,
  AllegraUTXOW,
) where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Genesis (EraGenesis, NoGenesis)
import Cardano.Ledger.Internal.Era (AllegraEra)
import Cardano.Ledger.Shelley.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

instance EraGenesis AllegraEra

instance EraTxLevel AllegraEra where
  type STxLevel l AllegraEra = STxTopLevel l AllegraEra

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

-- | No context is needed to translate from Shelley to Allegra.
type instance TranslationContext AllegraEra = NoGenesis AllegraEra

type instance Value AllegraEra = Coin

-- These rules are all inherited from Shelley

type instance EraRule "BBODY" AllegraEra = Shelley.BBODY AllegraEra

type instance EraRule "DELEG" AllegraEra = Shelley.DELEG AllegraEra

type instance EraRule "DELEGS" AllegraEra = Shelley.DELEGS AllegraEra

type instance EraRule "DELPL" AllegraEra = Shelley.DELPL AllegraEra

type instance EraRule "EPOCH" AllegraEra = Shelley.EPOCH AllegraEra

type instance EraRule "LEDGER" AllegraEra = Shelley.LEDGER AllegraEra

type instance EraRule "LEDGERS" AllegraEra = Shelley.LEDGERS AllegraEra

type instance EraRule "MIR" AllegraEra = Shelley.MIR AllegraEra

type instance EraRule "NEWEPOCH" AllegraEra = Shelley.NEWEPOCH AllegraEra

type instance EraRule "NEWPP" AllegraEra = Shelley.NEWPP AllegraEra

type instance EraRule "POOL" AllegraEra = Shelley.POOL AllegraEra

type instance EraRule "POOLREAP" AllegraEra = Shelley.POOLREAP AllegraEra

type instance EraRule "PPUP" AllegraEra = Shelley.PPUP AllegraEra

type instance EraRule "RUPD" AllegraEra = Shelley.RUPD AllegraEra

type instance EraRule "SNAP" AllegraEra = Shelley.SNAP AllegraEra

type instance EraRule "TICK" AllegraEra = Shelley.TICK AllegraEra

type instance EraRule "TICKF" AllegraEra = Shelley.TICKF AllegraEra

type instance EraRule "UPEC" AllegraEra = Shelley.UPEC AllegraEra

-- These rules are defined anew in the Allegra era

data AllegraUTXO era

type instance EraRule "UTXO" AllegraEra = AllegraUTXO AllegraEra

data AllegraUTXOW era

type instance EraRule "UTXOW" AllegraEra = AllegraUTXOW AllegraEra
