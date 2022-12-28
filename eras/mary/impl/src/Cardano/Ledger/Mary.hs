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
  ShelleyPParams,
  ShelleyPParamsUpdate,
)
where

import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys (DSignable)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Scripts ()
import Cardano.Ledger.Mary.Translation ()
import Cardano.Ledger.Mary.TxAuxData ()
import Cardano.Ledger.Mary.TxBody (MaryTxBody)
import Cardano.Ledger.Mary.TxSeq ()
import Cardano.Ledger.Mary.UTxO ()
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsUpdate)

type Mary = MaryEra StandardCrypto

instance
  (Crypto c, DSignable c (Hash c EraIndependentTxBody)) =>
  ApplyTx (MaryEra c)

instance
  (Crypto c, DSignable c (Hash c EraIndependentTxBody)) =>
  ApplyBlock (MaryEra c)

instance Crypto c => CanStartFromGenesis (MaryEra c) where
  initialState = initialStateFromGenesis const
