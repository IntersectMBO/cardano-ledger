{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Tx (
  module BabbageTxReExport,
  tierRefScriptFee,
  refScriptCostStride,
  refScriptCostMultiplier,
  getConwayMinFeeTx,
  Tx (..),
) where

import Cardano.Ledger.Allegra.Tx (validateTimelock)
import Cardano.Ledger.Alonzo.Core (AlonzoEraTxWits)
import Cardano.Ledger.Alonzo.Tx (
  alonzoMinFeeTx,
  alonzoTxEqRaw,
  auxDataAlonzoTxL,
  bodyAlonzoTxL,
  isValidAlonzoTxL,
  mkBasicAlonzoTx,
  sizeAlonzoTxF,
  witsAlonzoTxL,
 )
import Cardano.Ledger.Alonzo.TxSeq (
  AlonzoTxSeq (AlonzoTxSeq, txSeqTxns),
  hashAlonzoTxSeq,
 )
import Cardano.Ledger.Babbage.Tx as BabbageTxReExport (
  AlonzoEraTx (..),
  AlonzoTx (..),
  Tx (..),
 )
import Cardano.Ledger.BaseTypes (unboundRational)
import Cardano.Ledger.Binary (Annotator, DecCBOR (..), EncCBOR, ToCBOR)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.PParams (ConwayEraPParams, ppMinFeeRefScriptCostPerByteL)
import Cardano.Ledger.Conway.TxAuxData ()
import Cardano.Ledger.Conway.TxBody ()
import Cardano.Ledger.Conway.TxWits ()
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (EqRaw (..))
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import GHC.Stack
import Lens.Micro (Lens', lens, (^.))
import NoThunks.Class (NoThunks)

instance EraTx ConwayEra where
  newtype Tx ConwayEra = MkConwayTx {unConwayTx :: AlonzoTx ConwayEra}
    deriving newtype (Eq, Show, NFData, NoThunks, ToCBOR, EncCBOR)
    deriving (Generic)

  mkBasicTx = MkConwayTx . mkBasicAlonzoTx

  bodyTxL = conwayTxL . bodyAlonzoTxL
  {-# INLINE bodyTxL #-}

  witsTxL = conwayTxL . witsAlonzoTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = conwayTxL . auxDataAlonzoTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = conwayTxL . sizeAlonzoTxF
  {-# INLINE sizeTxF #-}

  validateNativeScript = validateTimelock
  {-# INLINE validateNativeScript #-}

  getMinFeeTx = getConwayMinFeeTx

instance EqRaw (Tx ConwayEra) where
  eqRaw = alonzoTxEqRaw

conwayTxL :: Lens' (Tx ConwayEra) (AlonzoTx ConwayEra)
conwayTxL = lens unConwayTx (\x y -> x {unConwayTx = y})

-- | 25 KiB
refScriptCostStride :: Int
refScriptCostStride = 25_600

refScriptCostMultiplier :: Rational
refScriptCostMultiplier = 1.2

getConwayMinFeeTx ::
  ( EraTx era
  , AlonzoEraTxWits era
  , ConwayEraPParams era
  ) =>
  PParams era ->
  Tx era ->
  Int ->
  Coin
getConwayMinFeeTx pp tx refScriptsSize =
  alonzoMinFeeTx pp tx <+> refScriptsFee
  where
    refScriptCostPerByte = unboundRational (pp ^. ppMinFeeRefScriptCostPerByteL)
    refScriptsFee =
      tierRefScriptFee
        refScriptCostMultiplier
        refScriptCostStride
        refScriptCostPerByte
        refScriptsSize

-- | Calculate the fee for reference scripts using an exponential growth of the price per
-- byte with linear increments
tierRefScriptFee ::
  HasCallStack =>
  -- | Growth factor or step multiplier
  Rational ->
  -- | Increment size in which price grows linearly according to the price
  Int ->
  -- | Base fee. Currently this is customizable by `ppMinFeeRefScriptCostPerByteL`
  Rational ->
  -- | Total RefScript size in bytes
  Int ->
  Coin
tierRefScriptFee multiplier sizeIncrement
  | multiplier <= 0 || sizeIncrement <= 0 = error "Size increment and multiplier must be positive"
  | otherwise = go 0
  where
    go !acc !curTierPrice !n
      | n < sizeIncrement =
          Coin $ floor (acc + toRational n * curTierPrice)
      | otherwise =
          go (acc + sizeIncrementRational * curTierPrice) (multiplier * curTierPrice) (n - sizeIncrement)
    sizeIncrementRational = toRational sizeIncrement

instance AlonzoEraTx ConwayEra where
  isValidTxL = conwayTxL . isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

instance EraSegWits ConwayEra where
  type TxSeq ConwayEra = AlonzoTxSeq ConwayEra
  fromTxSeq = txSeqTxns
  toTxSeq = AlonzoTxSeq
  hashTxSeq = hashAlonzoTxSeq
  numSegComponents = 4

instance DecCBOR (Annotator (Tx ConwayEra)) where
  decCBOR = fmap MkConwayTx <$> decCBOR
