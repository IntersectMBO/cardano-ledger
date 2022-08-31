{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.Ledger.Babbage.TxWits
  ( module BabbageTxWitsReExport,
  )
where

import Cardano.Ledger.Alonzo.TxWits
  ( AlonzoEraTxWits (..),
    AlonzoTxWits (..),
    addrAlonzoWitsL,
    bootAddrAlonzoWitsL,
    datsAlonzoWitsL,
    rdmrsAlonzoWitsL,
    scriptAlonzoWitsL,
  )
import Cardano.Ledger.Alonzo.TxWits as BabbageTxWitsReExport
  ( AlonzoEraTxWits (..),
    AlonzoTxWits (..),
  )
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.PParams (BabbagePParamsHKD (..))
import Cardano.Ledger.Babbage.TxBody
  ( BabbageEraTxBody (..),
    BabbageEraTxOut (..),
    BabbageTxBody (..),
    BabbageTxOut (..),
    Datum (..),
    dataHashTxOutL,
  )
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.ShelleyMA.Tx (validateTimelock)
import Cardano.Ledger.TxIn (TxIn)
import Control.Applicative ((<|>))
import Control.SetAlgebra (eval, (◁))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing), strictMaybeToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro

instance CC.Crypto c => EraTxWits (BabbageEra c) where
  {-# SPECIALIZE instance EraTxWits (BabbageEra CC.StandardCrypto) #-}

  type TxWits (BabbageEra c) = AlonzoTxWits (BabbageEra c)

  mkBasicTxWits = mempty

  addrWitsL = addrAlonzoWitsL
  {-# INLINE addrWitsL #-}

  bootAddrWitsL = bootAddrAlonzoWitsL
  {-# INLINE bootAddrWitsL #-}

  scriptWitsL = scriptAlonzoWitsL
  {-# INLINE scriptWitsL #-}

instance CC.Crypto c => AlonzoEraTxWits (BabbageEra c) where
  {-# SPECIALIZE instance AlonzoEraTxWits (BabbageEra CC.StandardCrypto) #-}

  datsWitsL = datsAlonzoWitsL
  {-# INLINE datsWitsL #-}

  rdmrsWitsL = rdmrsAlonzoWitsL
  {-# INLINE rdmrsWitsL #-}
