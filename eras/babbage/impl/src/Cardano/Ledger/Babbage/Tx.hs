{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)

instance HasEraTxLevel Tx BabbageEra where
  toSTxLevel (MkBabbageTx AlonzoTx {}) = STopTxOnly @BabbageEra

instance EraTx BabbageEra where
  newtype Tx l BabbageEra = MkBabbageTx {unBabbageTx :: AlonzoTx l BabbageEra}
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

instance EqRaw (Tx l BabbageEra) where
  eqRaw = alonzoTxEqRaw

instance AlonzoEraTx BabbageEra where
  isValidTxL = babbageTxL . isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

instance Typeable l => DecCBOR (Annotator (Tx l BabbageEra)) where
  decCBOR = fmap MkBabbageTx <$> decCBOR

babbageTxL :: Lens' (Tx l BabbageEra) (AlonzoTx l BabbageEra)
babbageTxL = lens unBabbageTx (\x y -> x {unBabbageTx = y})
