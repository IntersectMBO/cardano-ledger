{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
  Tx (..),
  bodyShelleyTxL,
  witsShelleyTxL,
  auxDataShelleyTxL,
  mkBasicShelleyTx,
  shelleyMinFeeTx,
  sizeShelleyTxF,
  shelleyTxEqRaw,
) where

import Cardano.Ledger.BaseTypes (integralToBounded)
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
import Cardano.Ledger.MemoBytes (EqRaw (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Scripts (validateMultiSig)
import Cardano.Ledger.Shelley.TxAuxData ()
import Cardano.Ledger.Shelley.TxBody ()
import Cardano.Ledger.Shelley.TxWits ()
import Cardano.Ledger.Val ((<+>), (<×>))
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad.Trans.Fail.String (errorFail)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor.Classes (Eq1 (..))
import Data.Maybe.Strict (
  StrictMaybe (..),
  strictMaybeToMaybe,
 )
import Data.Typeable
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lens.Micro (Lens', SimpleGetter, lens, to, (^.))
import NoThunks.Class (NoThunks (..))

-- ========================================================

data ShelleyTx t era where
  ShelleyTx ::
    { stBody :: !(TxBody FullTx era)
    , stWits :: !(TxWits era)
    , stAuxData :: !(StrictMaybe (TxAuxData era))
    } ->
    ShelleyTx FullTx era

instance
  ( NFData (TxBody t era)
  , NFData (TxWits era)
  , NFData (TxAuxData era)
  ) =>
  NFData (ShelleyTx t era)
  where
  rnf ShelleyTx {stBody, stWits, stAuxData} =
    stBody `deepseq` stWits `deepseq` rnf stAuxData

deriving instance
  ( Era era
  , Eq (TxBody t era)
  , Eq (TxWits era)
  , Eq (TxAuxData era)
  ) =>
  Eq (ShelleyTx t era)

deriving instance
  ( Era era
  , Show (TxBody t era)
  , Show (TxWits era)
  , Show (TxAuxData era)
  ) =>
  Show (ShelleyTx t era)

instance
  ( Era era
  , NoThunks (TxBody t era)
  , NoThunks (TxWits era)
  , NoThunks (TxAuxData era)
  ) =>
  NoThunks (ShelleyTx t era)

-- | `TxBody` setter and getter for `ShelleyTx`.
bodyShelleyTxL :: Lens' (ShelleyTx t era) (TxBody t era)
bodyShelleyTxL =
  lens stBody $ \tx txBody -> tx {stBody = txBody}
{-# INLINEABLE bodyShelleyTxL #-}

-- | `TxWits` setter and getter for `ShelleyTx`.
witsShelleyTxL :: Lens' (ShelleyTx t era) (TxWits era)
witsShelleyTxL =
  lens stWits $ \tx txWits -> tx {stWits = txWits}
{-# INLINEABLE witsShelleyTxL #-}

-- | `TxAuxData` setter and getter for `ShelleyTx`.
auxDataShelleyTxL :: Lens' (ShelleyTx t era) (StrictMaybe (TxAuxData era))
auxDataShelleyTxL =
  lens stAuxData $ \tx txAuxData -> tx {stAuxData = txAuxData}
{-# INLINEABLE auxDataShelleyTxL #-}

mkBasicShelleyTx :: EraTx era => TxBody t era -> ShelleyTx t era
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
  ShelleyTx t era ->
  Encoding
toCBORForSizeComputation ShelleyTx {stBody, stWits, stAuxData} =
  encodeListLen 3
    <> encCBOR stBody
    <> encCBOR stWits
    <> encodeNullStrictMaybe encCBOR stAuxData

-- | txsize computes the length of the serialised bytes (for estimations)
sizeShelleyTxF :: forall era t. (HasCallStack, EraTx era) => SimpleGetter (ShelleyTx t era) Word32
sizeShelleyTxF =
  to $
    errorFail
      . integralToBounded
      . LBS.length
      . serialize (eraProtVerLow @era)
      . toCBORForSizeComputation
{-# INLINEABLE sizeShelleyTxF #-}

instance
  ( EraTxBody era
  , EraTxWits era
  , EraTxAuxData era
  ) =>
  DecCBOR (Annotator (ShelleyTx t era))
  where
  decCBOR =
    decode $
      Ann (RecD ShelleyTx)
        <*! From
        <*! From
        <*! D (sequence <$> decodeNullStrictMaybe decCBOR)

instance Typeable t => DecCBOR (Annotator (Tx t ShelleyEra)) where
  decCBOR = fmap MkShelleyTx <$> decCBOR

instance EraTx ShelleyEra where
  newtype Tx t ShelleyEra = MkShelleyTx {unShelleyTx :: ShelleyTx t ShelleyEra}
    deriving newtype (Eq, EncCBOR, NFData, NoThunks, Show, ToCBOR)
    deriving (Generic)

  mkBasicTx = MkShelleyTx . mkBasicShelleyTx

  bodyTxL = shelleyTxL . bodyShelleyTxL
  {-# INLINE bodyTxL #-}

  witsTxL = shelleyTxL . witsShelleyTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = shelleyTxL . auxDataShelleyTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = shelleyTxL . sizeShelleyTxF
  {-# INLINE sizeTxF #-}

  validateNativeScript = validateMultiSig
  {-# INLINE validateNativeScript #-}

  getMinFeeTx pp tx _ = shelleyMinFeeTx pp tx

shelleyTxEqRaw :: EraTx era => Tx era -> Tx era -> Bool
shelleyTxEqRaw tx1 tx2 =
  eqRaw (tx1 ^. bodyTxL) (tx2 ^. bodyTxL)
    && eqRaw (tx1 ^. witsTxL) (tx2 ^. witsTxL)
    && liftEq -- TODO: Implement Eq1 instance for StrictMaybe
      eqRaw
      (strictMaybeToMaybe (tx1 ^. auxDataTxL))
      (strictMaybeToMaybe (tx2 ^. auxDataTxL))

instance EqRaw (Tx t ShelleyEra) where
  eqRaw = shelleyTxEqRaw

shelleyTxL :: Lens' (Tx ShelleyEra) (ShelleyTx ShelleyEra)
shelleyTxL = lens unShelleyTx (\x y -> x {unShelleyTx = y})

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance
  (Era era, EncCBOR (TxWits era), EncCBOR (TxBody era), EncCBOR (TxAuxData era)) =>
  EncCBOR (ShelleyTx t era)
  where
  encCBOR ShelleyTx {..} =
    encode $
      Rec ShelleyTx
        !> To stBody
        !> To stWits
        !> E (encodeNullStrictMaybe encCBOR) stAuxData

instance
  ( Era era
  , Typeable t
  , DecCBOR (TxBody era)
  , DecCBOR (TxWits era)
  , DecCBOR (TxAuxData era)
  ) =>
  DecCBOR (ShelleyTx t era)
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
