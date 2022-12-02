{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Ledger.Api.Tx.Body
  ( -- | Building and inspecting transaction outputs
    module Cardano.Ledger.Api.Tx.Out,
    -- | Working with Timelock scripts and Plutus scripts
    module Cardano.Ledger.Api.Scripts,
    EraTxBody (..),

    -- * Shelley Era
    ShelleyTxBody,
    ShelleyEraTxBody (..),

    -- * Allegra Era
    AllegraEraTxBody (..),

    -- * Mary Era
    MaryEraTxBody (..),

    -- * Alonzo Era
    AlonzoTxBody,
    AlonzoEraTxBody (..),

    -- * Babbage Era
    BabbageTxBody,
    BabbageEraTxBody (..),
  )
where

import Cardano.Ledger.Allegra.Core (AllegraEraTxBody (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody (..), AlonzoTxBody)
import Cardano.Ledger.Api.Scripts
import Cardano.Ledger.Api.Tx.Out
import Cardano.Ledger.Babbage.TxBody (BabbageEraTxBody (..), BabbageTxBody)
import Cardano.Ledger.Core (EraTxBody (..))
import Cardano.Ledger.Mary.Core (MaryEraTxBody (..))
import Cardano.Ledger.Shelley (ShelleyTxBody)
import Cardano.Ledger.Shelley.Core (ShelleyEraTxBody (..))
