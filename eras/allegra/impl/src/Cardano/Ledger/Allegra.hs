{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra (
  Allegra,
  AllegraEra,
)
where

import Cardano.Ledger.Allegra.Delegation ()
import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.PParams ()
import Cardano.Ledger.Allegra.Rules ()
import Cardano.Ledger.Allegra.Scripts ()
import Cardano.Ledger.Allegra.Translation ()
import Cardano.Ledger.Allegra.Tx ()
import Cardano.Ledger.Allegra.TxSeq ()
import Cardano.Ledger.Allegra.UTxO ()
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Shelley.API (
  ApplyBlock,
  ApplyTx,
  CanStartFromGenesis (fromShelleyPParams),
 )

type Allegra = AllegraEra StandardCrypto

--------------------------------------------------------------------------------
-- Mempool instances
--------------------------------------------------------------------------------

instance
  (Crypto c, DSignable c (Hash c EraIndependentTxBody)) =>
  ApplyTx (AllegraEra c)

instance
  (Crypto c, DSignable c (Hash c EraIndependentTxBody)) =>
  ApplyBlock (AllegraEra c)

instance Crypto c => CanStartFromGenesis (AllegraEra c) where
  fromShelleyPParams _ = translateEra' ()
