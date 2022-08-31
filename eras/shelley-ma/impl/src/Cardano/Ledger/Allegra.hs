{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra
  ( AllegraEra,
    Self,
    TxOut,
    TxBody,
    Value,
    Script,
    AuxiliaryData,
    PParams,
    PParamsDelta,
    Tx,
    Witnesses,
  )
where

import Cardano.Ledger.Core (EraCrypto)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys (DSignable)
import Cardano.Ledger.Shelley.API hiding (PParams, Tx, TxBody, TxOut, WitnessSet)
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsUpdate)
import Cardano.Ledger.ShelleyMA
import Cardano.Ledger.ShelleyMA.Rules (consumed)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock)
import Cardano.Ledger.ShelleyMA.TxBody ()

type AllegraEra = ShelleyMAEra 'Allegra

--------------------------------------------------------------------------------
-- Mempool instances
--------------------------------------------------------------------------------

instance
  (CC.Crypto crypto, DSignable crypto (Hash crypto EraIndependentTxBody)) =>
  ApplyTx (AllegraEra crypto)

instance
  (CC.Crypto crypto, DSignable crypto (Hash crypto EraIndependentTxBody)) =>
  ApplyBlock (AllegraEra crypto)

instance CC.Crypto crypto => CanStartFromGenesis (AllegraEra crypto) where
  initialState = initialStateFromGenesis const

instance CC.Crypto c => CLI (AllegraEra c) where
  evaluateConsumed = consumed

-- Self-Describing type synomyms

type Self c = ShelleyMAEra 'Allegra c

{-# DEPRECATED Self "Use `MaryEra` instead" #-}

type Script era = Timelock (EraCrypto era)

{-# DEPRECATED Script "Use `Timelock` instead" #-}

type Value era = Coin

{-# DEPRECATED Value "Use `Coin` instead" #-}

type Witnesses era = ShelleyWitnesses (EraCrypto era)

{-# DEPRECATED Witnesses "Use `Timelock` instead" #-}

type PParamsDelta era = ShelleyPParamsUpdate era

{-# DEPRECATED PParamsDelta "Use `ShelleyPParamsUpdate` instead" #-}
