{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Tx (
  AlonzoTx (..),
  BabbageTxBody (..),
  module X,
  -- Babel Fees
  -- pattern BabbageRequiredTx,
  -- requiredTxs,
  -- BabbageRequiredTx (..),
  -- BabbageRequiredTxRaw (..),
)
where

import Cardano.Ledger.Allegra.Tx (validateTimelock)
import Cardano.Ledger.Alonzo.Tx as X
import Cardano.Ledger.Alonzo.TxSeq (
  AlonzoTxSeq (AlonzoTxSeq, txSeqTxns),
  hashAlonzoTxSeq,
 )
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.TxAuxData ()
import Cardano.Ledger.Babbage.TxBody (
  BabbageTxBody (..),
  BabbageTxBodyUpgradeError,
 )
import Cardano.Ledger.Babbage.TxWits ()
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Control.Arrow (left)
import qualified Data.Sequence.Strict as StrictSeq

newtype BabbageTxUpgradeError
  = BTUEBodyUpgradeError BabbageTxBodyUpgradeError
  deriving (Eq, Show)

instance Crypto c => EraTx (BabbageEra c) where
  {-# SPECIALIZE instance EraTx (BabbageEra StandardCrypto) #-}

  type Tx (BabbageEra c) = AlonzoTx (BabbageEra c)
  type TxUpgradeError (BabbageEra c) = BabbageTxUpgradeError

  mkBasicTx = mkBasicAlonzoTx

  bodyTxL = bodyAlonzoTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsAlonzoTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = auxDataAlonzoTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = sizeAlonzoTxF
  {-# INLINE sizeTxF #-}

  validateNativeScript = validateTimelock
  {-# INLINE validateNativeScript #-}

  getMinFeeTx pp tx _ = alonzoMinFeeTx pp tx

  upgradeTx (AlonzoTx b w valid aux) =
    AlonzoTx
      <$> left BTUEBodyUpgradeError (upgradeTxBody b)
      <*> pure (upgradeTxWits w)
      <*> pure valid
      <*> pure (fmap upgradeTxAuxData aux)

instance Crypto c => AlonzoEraTx (BabbageEra c) where
  {-# SPECIALIZE instance AlonzoEraTx (BabbageEra StandardCrypto) #-}

  isValidTxL = isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

instance Crypto c => EraSegWits (BabbageEra c) where
  type TxStructure (BabbageEra c) = StrictSeq.StrictSeq
  type TxZones (BabbageEra c) = AlonzoTxSeq (BabbageEra c)
  fromTxZones = txSeqTxns
  toTxZones = AlonzoTxSeq
  flatten = fromTxZones
  hashTxZones = hashAlonzoTxSeq
  numSegComponents = 4