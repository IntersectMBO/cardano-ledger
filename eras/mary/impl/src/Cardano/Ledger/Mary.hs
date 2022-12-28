{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary (
  Mary,
  MaryEra,
  ShelleyTx,
  ShelleyTxOut,
  MaryValue,
  MaryTxBody,
)
where

import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Keys (DSignable)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.PParams ()
import Cardano.Ledger.Mary.Scripts ()
import Cardano.Ledger.Mary.Translation ()
import Cardano.Ledger.Mary.TxAuxData ()
import Cardano.Ledger.Mary.TxBody (MaryTxBody)
import Cardano.Ledger.Mary.TxSeq ()
import Cardano.Ledger.Mary.UTxO ()
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Shelley.API

type Mary = MaryEra StandardCrypto

instance
  (Crypto c, DSignable c (Hash c EraIndependentTxBody)) =>
  ApplyTx (MaryEra c)

instance
  (Crypto c, DSignable c (Hash c EraIndependentTxBody)) =>
  ApplyBlock (MaryEra c)

instance Crypto c => CanStartFromGenesis (MaryEra c) where
  fromShelleyPParams _ = upgradePParams () . upgradePParams ()
