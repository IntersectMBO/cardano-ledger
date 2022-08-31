{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra
  ( Allegra,
    AllegraEra,
    Self,
    TxOut,
    TxBody,
    Value,
    Script,
    AuxiliaryData,
    PParams,
    PParamsDelta,
    Tx,
    TxWits,
  )
where

import Cardano.Ledger.Allegra.UTxO ()
import Cardano.Ledger.Core (EraCrypto)
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys (DSignable)
import Cardano.Ledger.Shelley.API hiding (PParams, Tx, TxBody, TxOut)
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsUpdate)
import Cardano.Ledger.ShelleyMA
import Cardano.Ledger.ShelleyMA.Rules ()
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock)
import Cardano.Ledger.ShelleyMA.TxBody ()

type Allegra = AllegraEra StandardCrypto

type AllegraEra = ShelleyMAEra 'Allegra

--------------------------------------------------------------------------------
-- Mempool instances
--------------------------------------------------------------------------------

instance
  (Crypto crypto, DSignable crypto (Hash crypto EraIndependentTxBody)) =>
  ApplyTx (AllegraEra crypto)

instance
  (Crypto crypto, DSignable crypto (Hash crypto EraIndependentTxBody)) =>
  ApplyBlock (AllegraEra crypto)

instance Crypto crypto => CanStartFromGenesis (AllegraEra crypto) where
  initialState = initialStateFromGenesis const

-- Self-Describing type synomyms

type Self c = ShelleyMAEra 'Allegra c

{-# DEPRECATED Self "Use `MaryEra` instead" #-}

type Script era = Timelock (EraCrypto era)

{-# DEPRECATED Script "Use `Timelock` instead" #-}

type Value era = Coin

{-# DEPRECATED Value "Use `Coin` instead" #-}

type TxWits era = ShelleyTxWits (EraCrypto era)

{-# DEPRECATED TxWits "Use `Timelock` instead" #-}

type PParamsDelta era = ShelleyPParamsUpdate era

{-# DEPRECATED PParamsDelta "Use `ShelleyPParamsUpdate` instead" #-}
