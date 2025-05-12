{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Provides Shelley Tx internals
--
-- = Warning
--
-- This module is considered __internal__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
module Cardano.Ledger.Shelley.Tx.Internal (
  -- * Transaction
  ShelleyTx (..),
  bodyShelleyTxL,
  witsShelleyTxL,
  auxDataShelleyTxL,
  mkBasicShelleyTx,
  shelleyMinFeeTx,
  witsFromTxWitnesses,
  shelleyEqTxRaw,
  sizeShelleyTxF,
  wireSizeShelleyTxF,
)
where

import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  EncCBOR (encCBOR),
  ToCBOR,
  decodeNullStrictMaybe,
  encodeNullMaybe, serialize, Encoding, encodeListLen,
 )
import Cardano.Ledger.Binary.Coders
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
 )
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Scripts (validateMultiSig)
import Cardano.Ledger.Shelley.TxAuxData ()
import Cardano.Ledger.Shelley.TxBody ()
import Cardano.Ledger.Shelley.TxWits ()
import Cardano.Ledger.Val ((<+>), (<×>))
import Control.DeepSeq (NFData)
import Data.Functor.Classes (Eq1 (liftEq))
import Data.Maybe.Strict (
  StrictMaybe (..),
  strictMaybeToMaybe,
 )
import Data.Set (Set)
import GHC.Generics (Generic)
import Lens.Micro (Lens', (^.), lens, SimpleGetter)
import NoThunks.Class (NoThunks (..))
import Data.Word (Word32)
import qualified Data.ByteString.Lazy as LBS
import Lens.Micro (to)

-- ========================================================

data ShelleyTx era = ShelleyTx
  { stxBody :: !(TxBody era)
  , stxWits :: !(TxWits era)
  , stxAuxData :: !(StrictMaybe (TxAuxData era))
  }
  deriving (Generic)

instance
  ( NFData (TxBody era)
  , NFData (TxWits era)
  , NFData (TxAuxData era)
  ) =>
  NFData (ShelleyTx era)

deriving instance
  ( Era era
  , Eq (TxBody era)
  , Eq (TxWits era)
  , Eq (TxAuxData era)
  ) =>
  Eq (ShelleyTx era)

deriving instance
  ( Era era
  , Show (TxBody era)
  , Show (TxWits era)
  , Show (TxAuxData era)
  ) =>
  Show (ShelleyTx era)

instance
  ( Era era
  , NoThunks (TxAuxData era)
  , NoThunks (TxBody era)
  , NoThunks (TxWits era)
  ) =>
  NoThunks (ShelleyTx era)

-- | `TxBody` setter and getter for `ShelleyTx`. The setter does update
-- memoized binary representation.
bodyShelleyTxL :: Lens' (ShelleyTx era) (TxBody era)
bodyShelleyTxL =
  lens stxBody $ \tx txBody -> tx {stxBody = txBody}
{-# INLINEABLE bodyShelleyTxL #-}

-- | `TxWits` setter and getter for `ShelleyTx`. The setter does update
-- memoized binary representation.
witsShelleyTxL :: Lens' (ShelleyTx era) (TxWits era)
witsShelleyTxL =
  lens stxWits $ \tx txWits -> tx {stxWits = txWits}
{-# INLINEABLE witsShelleyTxL #-}

-- | `TxAuxData` setter and getter for `ShelleyTx`. The setter does update
-- memoized binary representation.
auxDataShelleyTxL :: Lens' (ShelleyTx era) (StrictMaybe (TxAuxData era))
auxDataShelleyTxL =
  lens stxAuxData $ \tx txAuxData -> tx {stxAuxData = txAuxData}
{-# INLINEABLE auxDataShelleyTxL #-}

mkBasicShelleyTx :: EraTx era => TxBody era -> ShelleyTx era
mkBasicShelleyTx txBody =
  ShelleyTx
    { stxBody = txBody
    , stxWits = mkBasicTxWits
    , stxAuxData = SNothing
    }

toCBORForSizeComputation ::
  ( EncCBOR (TxBody era)
  , EncCBOR (TxWits era)
  , EncCBOR (TxAuxData era)
  ) =>
  ShelleyTx era ->
  Encoding
toCBORForSizeComputation ShelleyTx {stxBody, stxWits, stxAuxData} =
  encodeListLen 3
    <> encCBOR stxBody
    <> encCBOR stxWits
    <> encodeNullMaybe encCBOR (strictMaybeToMaybe stxAuxData)

-- | txsize computes the length of the serialised bytes (for estimations)
sizeShelleyTxF :: forall era. EraTx era => SimpleGetter (ShelleyTx era) Integer
sizeShelleyTxF =
  to $
    fromIntegral
      . LBS.length
      . serialize (eraProtVerLow @era)
      . toCBORForSizeComputation
{-# INLINEABLE sizeShelleyTxF #-}

-- | txsize computes the length of the serialised bytes (actual size)
wireSizeShelleyTxF :: forall era. EraTx era => SimpleGetter (ShelleyTx era) Word32
wireSizeShelleyTxF =
  to $
    checkedFromIntegral
      . LBS.length
      . serialize (eraProtVerLow @era)
      . encCBOR
  where
    checkedFromIntegral n =
      if n <= fromIntegral (maxBound :: Word32)
        then fromIntegral n
        else error $ "Impossible: Size of the transaction is too big: " ++ show n
{-# INLINEABLE wireSizeShelleyTxF #-}

instance EraTx ShelleyEra where
  type Tx ShelleyEra = ShelleyTx ShelleyEra

  mkBasicTx = mkBasicShelleyTx

  bodyTxL = bodyShelleyTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsShelleyTxL
  {-# INLINE witsTxL #-}

  sizeTxF = sizeShelleyTxF
  {-# INLINE sizeTxF #-}

  wireSizeTxF = wireSizeShelleyTxF
  {-# INLINE wireSizeTxF #-}

  auxDataTxL = auxDataShelleyTxL
  {-# INLINE auxDataTxL #-}

  validateNativeScript = validateMultiSig
  {-# INLINE validateNativeScript #-}

  getMinFeeTx pp tx _ = shelleyMinFeeTx pp tx

  upgradeTx =
    error
      "Calling this function will cause a compilation error, since there is no Tx instance for Byron"

instance (Tx era ~ ShelleyTx era, EraTx era) => EqRaw (ShelleyTx era) where
  eqRaw = shelleyEqTxRaw

shelleyEqTxRaw :: EraTx era => Tx era -> Tx era -> Bool
shelleyEqTxRaw tx1 tx2 =
  eqRaw (tx1 ^. bodyTxL) (tx2 ^. bodyTxL)
    && eqRaw (tx1 ^. witsTxL) (tx2 ^. witsTxL)
    && liftEq -- TODO: Implement Eq1 instance for StrictMaybe
      eqRaw
      (strictMaybeToMaybe (tx1 ^. auxDataTxL))
      (strictMaybeToMaybe (tx2 ^. auxDataTxL))

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

encodeShelleyTx ::
  (EncCBOR (TxWits era), EncCBOR (TxBody era), EncCBOR (TxAuxData era)) =>
  ShelleyTx era ->
  Encode ('Closed 'Dense) (ShelleyTx era)
encodeShelleyTx ShelleyTx {stxBody, stxWits, stxAuxData} =
  Rec ShelleyTx
    !> To stxBody
    !> To stxWits
    !> E (encodeNullMaybe encCBOR . strictMaybeToMaybe) stxAuxData

instance
  (Era era, EncCBOR (TxWits era), EncCBOR (TxBody era), EncCBOR (TxAuxData era)) =>
  EncCBOR (ShelleyTx era)
  where
  encCBOR = encode . encodeShelleyTx

instance (Era era, EncCBOR (TxWits era), EncCBOR (TxBody era), EncCBOR (TxAuxData era)) => ToCBOR (ShelleyTx era) where
  toCBOR = toEraCBOR @era

instance
  ( Era era
  , DecCBOR (TxBody era)
  , DecCBOR (TxWits era)
  , DecCBOR (TxAuxData era)
  ) =>
  DecCBOR (ShelleyTx era)
  where
  decCBOR =
    decode $
      RecD ShelleyTx
        <! From
        <! From
        <! D (decodeNullStrictMaybe decCBOR)

--------------------------------------------------------------------------------
-- Segregated witness
--------------------------------------------------------------------------------

-- ========================================

-- | Minimum fee calculation
shelleyMinFeeTx :: EraTx era => PParams era -> Tx era -> Coin
shelleyMinFeeTx pp tx =
  (tx ^. sizeTxF <×> pp ^. ppMinFeeAL) <+> pp ^. ppMinFeeBL

-- | Extract the witness hashes from the Transaction.
witsFromTxWitnesses ::
  EraTx era =>
  Tx era ->
  Set (KeyHash 'Witness)
witsFromTxWitnesses tx = keyHashWitnessesTxWits (tx ^. witsTxL)
{-# DEPRECATED witsFromTxWitnesses "In favor ot `keyHashWitnessesTxWits`" #-}
