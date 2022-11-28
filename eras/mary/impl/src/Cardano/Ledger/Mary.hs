{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary
  ( Mary,
    MaryEra,
    Self,
    ShelleyTx,
    ShelleyTxOut,
    MaryValue,
    MATxBody,
    ShelleyPParams,
    ShelleyPParamsUpdate,
    AllegraTxAuxData,

    -- * Deprecated
    Cardano.Ledger.Shelley.API.Tx,
    Cardano.Ledger.Shelley.API.TxOut,
    Cardano.Ledger.ShelleyMA.TxBody,
    Cardano.Ledger.Shelley.PParams.PParams,
    Cardano.Ledger.Mary.Value,
    Cardano.Ledger.Mary.Script,
    Cardano.Ledger.Mary.PParamsDelta,
    Cardano.Ledger.ShelleyMA.AuxiliaryData.AuxiliaryData,
  )
where

import Cardano.Ledger.Core (EraCrypto)
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys (DSignable)
import Cardano.Ledger.Mary.Translation ()
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Shelley.API hiding (TxBody)
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsUpdate)
import qualified Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.ShelleyMA
import Cardano.Ledger.ShelleyMA.AuxiliaryData (AuxiliaryData)
import Cardano.Ledger.ShelleyMA.Era (MaryEra)
import Cardano.Ledger.ShelleyMA.Rules ()
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock)

instance
  (Crypto c, DSignable c (Hash c EraIndependentTxBody)) =>
  ApplyTx (MaryEra c)

instance
  (Crypto c, DSignable c (Hash c EraIndependentTxBody)) =>
  ApplyBlock (MaryEra c)

instance Crypto c => CanStartFromGenesis (MaryEra c) where
  initialState = initialStateFromGenesis const

-- Self-Describing type synomyms

type Mary = MaryEra StandardCrypto

type Self c = ShelleyMAEra 'Mary c

{-# DEPRECATED Self "Use `MaryEra` instead" #-}

type Script era = Timelock (EraCrypto era)

{-# DEPRECATED Script "Use `Timelock` instead" #-}

type Value era = MaryValue (EraCrypto era)

{-# DEPRECATED Value "Use `MaryValue` instead" #-}

type PParamsDelta era = ShelleyPParamsUpdate era

{-# DEPRECATED PParamsDelta "Use `ShelleyPParamsUpdate` instead" #-}
