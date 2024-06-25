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
import Control.Monad ((<=<))
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

  -- requiredTxsTxL = lens (const mempty) const
  -- {-# INLINE requiredTxsTxL #-}

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

-- newtype BabbageRequiredTxRaw era = BabbageRequiredTxRaw (Set (TxId (EraCrypto era)))
--   deriving (Eq, Show, Generic)

-- instance EraScript era => NoThunks (BabbageRequiredTxRaw era)

-- deriving newtype instance Era era => EncCBOR (BabbageRequiredTxRaw era)

-- deriving newtype instance Era era => DecCBOR (BabbageRequiredTxRaw era)

-- instance Era era => DecCBOR (Annotator (BabbageRequiredTxRaw era)) where
--   decCBOR = pure <$> decCBOR

-- deriving via
--   (Mem BabbageRequiredTxRaw era)
--   instance
--     Era era => DecCBOR (Annotator (BabbageRequiredTx era))

-- newtype BabbageRequiredTx era
--   = RequiredTxBodyConstr (MemoBytes BabbageRequiredTxRaw era)
--   deriving (Eq, Generic)
--   deriving newtype (Plain.ToCBOR, SafeToHash)

-- deriving newtype instance EraScript era => Show (BabbageRequiredTx era)

-- instance EraScript era => NoThunks (BabbageRequiredTx era)

-- instance Memoized BabbageRequiredTx where
--   type RawType BabbageRequiredTx = BabbageRequiredTxRaw

-- deriving newtype instance EraRequiredTxsData era => NFData (BabbageRequiredTxRaw era)
-- deriving newtype instance EraRequiredTxsData era => NFData (BabbageRequiredTx era)

-- pattern BabbageRequiredTx ::
--   forall era.
--   EraScript era =>
--   Set (TxId (EraCrypto era)) ->
--   BabbageRequiredTx era
-- pattern BabbageRequiredTx {requiredTxs} <-
--   (getMemoRawType -> BabbageRequiredTxRaw requiredTxs)
--   where
--     BabbageRequiredTx requiredTxs' =
--       mkMemoized $ BabbageRequiredTxRaw requiredTxs'

-- {-# COMPLETE BabbageRequiredTx #-}

-- instance EraScript era => Semigroup (BabbageRequiredTx era) where
--   (BabbageRequiredTx a) <> y | Set.null a = y
--   y <> (BabbageRequiredTx a) | Set.null a = y
--   (BabbageRequiredTx a) <> (BabbageRequiredTx a') = BabbageRequiredTx (a <> a')

-- instance EraScript era => Monoid (BabbageRequiredTx era) where
--   mempty = BabbageRequiredTx mempty

-- instance
--   (Era era, Eq (TxOut era), Eq (TxCert era), Eq (PParamsUpdate era)) =>
--   EqRaw (BabbageRequiredTx era)

-- -- | Encodes memoized bytes created upon construction.
-- instance Era era => EncCBOR (BabbageRequiredTx era)

-- type instance MemoHashIndex BabbageRequiredTxRaw = EraIndependentRequiredTxs

-- -- deriving instance
-- --   HashAlgorithm (HASH (EraCrypto era)) =>
-- --   Show (BabbageRequiredTx era)

-- instance c ~ EraCrypto era => HashAnnotated (BabbageRequiredTx era) EraIndependentRequiredTxs c where
--   hashAnnotated = getMemoSafeHash

-- instance Crypto c => EraRequiredTxsData (BabbageEra c) where
--   type RequiredTxs (BabbageEra c) = BabbageRequiredTx (BabbageEra c)

instance Crypto c => EraSegWits (BabbageEra c) where
  type TxZones (BabbageEra c) = AlonzoTxSeq (BabbageEra c)
  fromTxZones = fmap StrictSeq.singleton . txSeqTxns
  toTxZones = AlonzoTxSeq . StrictSeq.forceToStrict . (StrictSeq.fromStrict <=< StrictSeq.fromStrict)
  hashTxZones = hashAlonzoTxSeq
  numSegComponents = 4