{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Tx (
  validateTimelock,
  Tx (..),
) where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.PParams ()
import Cardano.Ledger.Allegra.Scripts (AllegraEraScript (..), Timelock, evalTimelock)
import Cardano.Ledger.Allegra.TxAuxData ()
import Cardano.Ledger.Allegra.TxBody (AllegraEraTxBody (..))
import Cardano.Ledger.Allegra.TxWits ()
import Cardano.Ledger.Binary (Annotator, DecCBOR (..), EncCBOR, ToCBOR)
import Cardano.Ledger.Core (
  EraTx (..),
  EraTxWits (..),
  NativeScript,
 )
import Cardano.Ledger.Keys.WitVKey (witVKeyHash)
import Cardano.Ledger.MemoBytes (EqRaw (..))
import Cardano.Ledger.Shelley.Tx (
  ShelleyTx (..),
  Tx (..),
  auxDataShelleyTxL,
  bodyShelleyTxL,
  mkBasicShelleyTx,
  shelleyMinFeeTx,
  shelleyTxEqRaw,
  sizeShelleyTxF,
  witsShelleyTxL,
 )
import Control.DeepSeq (NFData)
import qualified Data.Set as Set (map)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.))
import NoThunks.Class (NoThunks)

-- ========================================

instance EraTx AllegraEra where
  newtype Tx AllegraEra = MkAllegraTx {unAllegraTx :: ShelleyTx AllegraEra}
    deriving newtype (Eq, NFData, NoThunks, Show, ToCBOR, EncCBOR)
    deriving (Generic)

  mkBasicTx = MkAllegraTx . mkBasicShelleyTx

  bodyTxL = allegraTxL . bodyShelleyTxL
  {-# INLINE bodyTxL #-}

  witsTxL = allegraTxL . witsShelleyTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = allegraTxL . auxDataShelleyTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = allegraTxL . sizeShelleyTxF
  {-# INLINE sizeTxF #-}

  validateNativeScript = validateTimelock
  {-# INLINE validateNativeScript #-}

  getMinFeeTx pp tx _ = shelleyMinFeeTx pp tx

instance EqRaw (Tx AllegraEra) where
  eqRaw = shelleyTxEqRaw

instance DecCBOR (Annotator (Tx AllegraEra)) where
  decCBOR = fmap MkAllegraTx <$> decCBOR

allegraTxL :: Lens' (Tx AllegraEra) (ShelleyTx AllegraEra)
allegraTxL = lens unAllegraTx (\x y -> x {unAllegraTx = y})

-- =======================================================
-- Validating timelock scripts
-- We extract ValidityInterval from TxBody with vldtTxBodyL getter
-- We still need to correctly compute the witness set for TxBody as well.

validateTimelock ::
  (EraTx era, AllegraEraTxBody era, AllegraEraScript era, NativeScript era ~ Timelock era) =>
  Tx era -> NativeScript era -> Bool
validateTimelock tx timelock = evalTimelock vhks (tx ^. bodyTxL . vldtTxBodyL) timelock
  where
    vhks = Set.map witVKeyHash (tx ^. witsTxL . addrTxWitsL)
{-# INLINEABLE validateTimelock #-}
