{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.ShelleyMA
  ( ShelleyMAEra,
    MaryOrAllegra (..),
    ShelleyTx,
    ShelleyTxOut,
    MATxBody,
    MAAuxiliaryData,
    ShelleyPParams,

    -- * Deprecated
    Tx,
    TxOut,
    TxBody,
    PParams,
    AuxiliaryData,
  )
where

import Cardano.Ledger.Core (EraSegWits (..))
import Cardano.Ledger.Shelley.BlockChain (ShelleyTxSeq (..))
import qualified Cardano.Ledger.Shelley.BlockChain as Shelley
  ( bbHash,
    txSeqTxns,
  )
import Cardano.Ledger.Shelley.PParams (PParams, ShelleyPParams)
import Cardano.Ledger.Shelley.Tx
  ( ShelleyTx,
    ShelleyTxOut,
    Tx,
    TxOut,
  )
import Cardano.Ledger.ShelleyMA.AuxiliaryData (AuxiliaryData, MAAuxiliaryData)
import Cardano.Ledger.ShelleyMA.Era (MAClass, MaryOrAllegra (..), ShelleyMAEra)
import Cardano.Ledger.ShelleyMA.Tx ()
import Cardano.Ledger.ShelleyMA.TxBody (MATxBody, TxBody)

--------------------------------------------------------------------------------
-- Ledger data instances
--------------------------------------------------------------------------------

-- Uses the default instance of hashScript

instance MAClass ma c => EraSegWits (ShelleyMAEra ma c) where
  type TxSeq (ShelleyMAEra ma c) = ShelleyTxSeq (ShelleyMAEra ma c)
  fromTxSeq = Shelley.txSeqTxns
  toTxSeq = ShelleyTxSeq
  hashTxSeq = Shelley.bbHash
  numSegComponents = 3
