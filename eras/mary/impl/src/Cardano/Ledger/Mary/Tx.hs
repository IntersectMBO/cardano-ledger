{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Tx (
  validateTimelock,
  Tx (..),
) where

import Cardano.Ledger.Allegra.Tx (Tx (..), validateTimelock)
import Cardano.Ledger.Binary (Annotator, DecCBOR (..), EncCBOR, ToCBOR)
import Cardano.Ledger.Core (EraTx (..), HasEraTxLevel (..), STxTopLevel (..))
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
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)

-- ========================================

instance HasEraTxLevel Tx MaryEra where
  toSTxLevel (MkMaryTx ShelleyTx {}) = STopTxOnly @MaryEra

instance EraTx MaryEra where
  newtype Tx t MaryEra = MkMaryTx {unMaryTx :: ShelleyTx t MaryEra}
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

instance EqRaw (Tx t MaryEra) where
  eqRaw = shelleyTxEqRaw

maryTxL :: Lens' (Tx t MaryEra) (ShelleyTx t MaryEra)
maryTxL = lens unMaryTx (\x y -> x {unMaryTx = y})

instance Typeable t => DecCBOR (Annotator (Tx t MaryEra)) where
  decCBOR = fmap MkMaryTx <$> decCBOR
