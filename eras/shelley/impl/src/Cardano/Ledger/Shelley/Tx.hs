{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module Cardano.Ledger.Shelley.Tx (
  -- * Transaction
  ShelleyTx (..),
  bodyShelleyTxL,
  witsShelleyTxL,
  auxDataShelleyTxL,
  mkBasicShelleyTx,
  shelleyMinFeeTx,
  shelleyEqTxRaw,
  sizeShelleyTxF,
) where

import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (decCBOR),
  EncCBOR (encCBOR),
  Encoding,
  ToCBOR,
  decodeNullStrictMaybe,
  encodeListLen,
  encodeNullStrictMaybe,
  serialize,
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
import qualified Data.ByteString.Lazy as LBS
import Data.Functor.Classes (Eq1 (liftEq))
import Data.Maybe.Strict (
  StrictMaybe (..),
  strictMaybeToMaybe,
 )
import GHC.Generics (Generic)
import Lens.Micro (Lens', SimpleGetter, lens, to, (^.))
import NoThunks.Class (NoThunks (..))

-- ========================================================

data ShelleyTx era = ShelleyTx
  { stBody :: !(TxBody era)
  , stWits :: !(TxWits era)
  , stAuxData :: !(StrictMaybe (TxAuxData era))
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

-- | `TxBody` setter and getter for `ShelleyTx`.
bodyShelleyTxL :: Lens' (ShelleyTx era) (TxBody era)
bodyShelleyTxL =
  lens stBody $ \tx txBody -> tx {stBody = txBody}
{-# INLINEABLE bodyShelleyTxL #-}

-- | `TxWits` setter and getter for `ShelleyTx`.
witsShelleyTxL :: Lens' (ShelleyTx era) (TxWits era)
witsShelleyTxL =
  lens stWits $ \tx txWits -> tx {stWits = txWits}
{-# INLINEABLE witsShelleyTxL #-}

-- | `TxAuxData` setter and getter for `ShelleyTx`.
auxDataShelleyTxL :: Lens' (ShelleyTx era) (StrictMaybe (TxAuxData era))
auxDataShelleyTxL =
  lens stAuxData $ \tx txAuxData -> tx {stAuxData = txAuxData}
{-# INLINEABLE auxDataShelleyTxL #-}

mkBasicShelleyTx :: EraTx era => TxBody era -> ShelleyTx era
mkBasicShelleyTx txBody =
  ShelleyTx
    { stBody = txBody
    , stWits = mkBasicTxWits
    , stAuxData = SNothing
    }

toCBORForSizeComputation ::
  ( EncCBOR (TxBody era)
  , EncCBOR (TxWits era)
  , EncCBOR (TxAuxData era)
  ) =>
  ShelleyTx era ->
  Encoding
toCBORForSizeComputation ShelleyTx {stBody, stWits, stAuxData} =
  encodeListLen 3
    <> encCBOR stBody
    <> encCBOR stWits
    <> encodeNullStrictMaybe encCBOR stAuxData

-- | txsize computes the length of the serialised bytes (for estimations)
sizeShelleyTxF :: forall era. EraTx era => SimpleGetter (ShelleyTx era) Integer
sizeShelleyTxF =
  to $
    fromIntegral
      . LBS.length
      . serialize (eraProtVerLow @era)
      . toCBORForSizeComputation
{-# INLINEABLE sizeShelleyTxF #-}

instance
  ( EraTxBody era
  , EraTxWits era
  , EraTxAuxData era
  ) =>
  DecCBOR (Annotator (ShelleyTx era))
  where
  decCBOR =
    decode $
      Ann (RecD ShelleyTx)
        <*! From
        <*! From
        <*! D (sequence <$> decodeNullStrictMaybe decCBOR)

instance EraTx ShelleyEra where
  type Tx ShelleyEra = ShelleyTx ShelleyEra

  mkBasicTx = mkBasicShelleyTx

  bodyTxL = bodyShelleyTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsShelleyTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = auxDataShelleyTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = sizeShelleyTxF
  {-# INLINE sizeTxF #-}

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

instance
  (Era era, EncCBOR (TxWits era), EncCBOR (TxBody era), EncCBOR (TxAuxData era)) =>
  EncCBOR (ShelleyTx era)
  where
  encCBOR ShelleyTx {..} =
    encode $
      Rec ShelleyTx
        !> To stBody
        !> To stWits
        !> E (encodeNullStrictMaybe encCBOR) stAuxData

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

instance
  (Era era, EncCBOR (TxWits era), EncCBOR (TxBody era), EncCBOR (TxAuxData era)) =>
  ToCBOR (ShelleyTx era)
  where
  toCBOR = toEraCBOR @era

-- | Minimum fee calculation
shelleyMinFeeTx :: EraTx era => PParams era -> Tx era -> Coin
shelleyMinFeeTx pp tx =
  (tx ^. sizeTxF <×> pp ^. ppMinFeeAL) <+> pp ^. ppMinFeeBL
