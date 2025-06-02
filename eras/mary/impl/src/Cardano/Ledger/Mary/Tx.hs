{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Tx (
  validateTimelock,
  Tx (..),
) where

import Cardano.Ledger.Allegra.Tx (Tx (..), validateTimelock)
import Cardano.Ledger.Binary (Annotator, DecCBOR (..), EncCBOR, ToCBOR)
import Cardano.Ledger.Core (EraTx (..))
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.PParams ()
import Cardano.Ledger.Mary.TxAuxData ()
import Cardano.Ledger.Mary.TxBody ()
import Cardano.Ledger.Mary.TxWits ()
import Cardano.Ledger.MemoBytes (EqRaw (..))
import Cardano.Ledger.Shelley.Tx (
  ShelleyTx (..),
  auxDataShelleyTxL,
  bodyShelleyTxL,
  mkBasicShelleyTx,
  shelleyMinFeeTx,
  shelleyTxEqRaw,
  sizeShelleyTxF,
  witsShelleyTxL,
 )
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)

-- ========================================

instance EraTx MaryEra where
  newtype Tx MaryEra = MkMaryTx {unMaryTx :: ShelleyTx MaryEra}
    deriving newtype (Eq, NFData, NoThunks, Show, ToCBOR, EncCBOR)
    deriving (Generic)

  mkBasicTx = MkMaryTx . mkBasicShelleyTx

  bodyTxL = maryTxL . bodyShelleyTxL
  {-# INLINE bodyTxL #-}

  witsTxL = maryTxL . witsShelleyTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = maryTxL . auxDataShelleyTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = maryTxL . sizeShelleyTxF
  {-# INLINE sizeTxF #-}

  validateNativeScript = validateTimelock
  {-# INLINE validateNativeScript #-}

  getMinFeeTx pp tx _ = shelleyMinFeeTx pp tx

instance EqRaw (Tx MaryEra) where
  eqRaw = shelleyTxEqRaw

maryTxL :: Lens' (Tx MaryEra) (ShelleyTx MaryEra)
maryTxL = lens unMaryTx (\x y -> x {unMaryTx = y})

instance DecCBOR (Annotator (Tx MaryEra)) where
  decCBOR = fmap MkMaryTx <$> decCBOR
