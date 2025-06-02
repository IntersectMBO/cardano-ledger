{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Tx (
  AlonzoTx (..),
  TxBody (..),
  Tx (..),
  module X,
) where

import Cardano.Ledger.Allegra.Tx (validateTimelock)
import Cardano.Ledger.Alonzo.Tx as X
import Cardano.Ledger.Alonzo.TxSeq (
  AlonzoTxSeq (AlonzoTxSeq, txSeqTxns),
  hashAlonzoTxSeq,
 )
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.TxAuxData ()
import Cardano.Ledger.Babbage.TxBody (
  TxBody (..),
 )
import Cardano.Ledger.Babbage.TxWits ()
import Cardano.Ledger.Binary (Annotator, DecCBOR (..), EncCBOR, ToCBOR)
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (EqRaw (..))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)

instance EraTx BabbageEra where
  newtype Tx BabbageEra = MkBabbageTx {unBabbageTx :: AlonzoTx BabbageEra}
    deriving newtype (Eq, NFData, Show, NoThunks, ToCBOR, EncCBOR)
    deriving (Generic)
  mkBasicTx = MkBabbageTx . mkBasicAlonzoTx

  bodyTxL = babbageTxL . bodyAlonzoTxL
  {-# INLINE bodyTxL #-}

  witsTxL = babbageTxL . witsAlonzoTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = babbageTxL . auxDataAlonzoTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = babbageTxL . sizeAlonzoTxF
  {-# INLINE sizeTxF #-}

  validateNativeScript = validateTimelock
  {-# INLINE validateNativeScript #-}

  getMinFeeTx pp tx _ = alonzoMinFeeTx pp tx

instance EqRaw (Tx BabbageEra) where
  eqRaw = alonzoTxEqRaw

instance AlonzoEraTx BabbageEra where
  isValidTxL = babbageTxL . isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

instance EraSegWits BabbageEra where
  type TxSeq BabbageEra = AlonzoTxSeq BabbageEra
  fromTxSeq = txSeqTxns
  toTxSeq = AlonzoTxSeq
  hashTxSeq = hashAlonzoTxSeq
  numSegComponents = 4

instance DecCBOR (Annotator (Tx BabbageEra)) where
  decCBOR = fmap MkBabbageTx <$> decCBOR

babbageTxL :: Lens' (Tx BabbageEra) (AlonzoTx BabbageEra)
babbageTxL = lens unBabbageTx (\x y -> x {unBabbageTx = y})
