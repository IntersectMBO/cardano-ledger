{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Tx (
  module BabbageTxReExport,
  tierRefScriptFee,
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
import Cardano.Ledger.Babbage.Tx as BabbageTxReExport (
  AlonzoEraTx (..),
  AlonzoTx (..),
  Tx (..),
 )
import Cardano.Ledger.BaseTypes (NonZero (..), unboundRational)
import Cardano.Ledger.Binary (Annotator, DecCBOR (..), EncCBOR, ToCBOR)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..), ppMinFeeRefScriptCostPerByteL)
import Cardano.Ledger.Conway.TxAuxData ()
import Cardano.Ledger.Conway.TxBody ()
import Cardano.Ledger.Conway.TxWits ()
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (EqRaw (..))
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Stack
import Lens.Micro (Lens', lens, (^.))
import NoThunks.Class (NoThunks)

instance HasEraTxLevel Tx ConwayEra where
  toSTxLevel (MkConwayTx AlonzoTx {}) = STopTxOnly @ConwayEra

instance EraTx ConwayEra where
  newtype Tx l ConwayEra = MkConwayTx {unConwayTx :: AlonzoTx l ConwayEra}
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

instance EqRaw (Tx l ConwayEra) where
  eqRaw = alonzoTxEqRaw

conwayTxL :: Lens' (Tx l ConwayEra) (AlonzoTx l ConwayEra)
conwayTxL = lens unConwayTx (\x y -> x {unConwayTx = y})

getConwayMinFeeTx ::
  ( EraTx era
  , AlonzoEraTxWits era
  , ConwayEraPParams era
  ) =>
  PParams era ->
  Tx l era ->
  Int ->
  Coin
getConwayMinFeeTx pp tx refScriptsSize =
  alonzoMinFeeTx pp tx <+> refScriptsFee
  where
    refScriptCostPerByte = unboundRational (pp ^. ppMinFeeRefScriptCostPerByteL)
    refScriptsFee =
      tierRefScriptFee
        (unboundRational $ pp ^. ppRefScriptCostMultiplierG)
        (fromIntegral @Word32 @Int . unNonZero $ pp ^. ppRefScriptCostStrideG)
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

instance Typeable l => DecCBOR (Annotator (Tx l ConwayEra)) where
  decCBOR = fmap MkConwayTx <$> decCBOR
