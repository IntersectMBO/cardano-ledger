{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Typeable
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lens.Micro (Lens', SimpleGetter, lens, to, (^.))
import NoThunks.Class (InspectHeap (..), NoThunks (..))

-- ========================================================

data ShelleyTx l era where
  ShelleyTx ::
    { stBody :: !(TxBody TopTx era)
    , stWits :: !(TxWits era)
    , stAuxData :: !(StrictMaybe (TxAuxData era))
    } ->
    ShelleyTx TopTx era

instance
  ( NFData (TxBody l era)
  , NFData (TxWits era)
  , NFData (TxAuxData era)
  ) =>
  NFData (ShelleyTx l era)
  where
  rnf ShelleyTx {stBody, stWits, stAuxData} =
    stBody `deepseq` stWits `deepseq` rnf stAuxData

deriving instance
  ( Era era
  , Eq (TxBody l era)
  , Eq (TxWits era)
  , Eq (TxAuxData era)
  ) =>
  Eq (ShelleyTx l era)

deriving instance
  ( Era era
  , Show (TxBody l era)
  , Show (TxWits era)
  , Show (TxAuxData era)
  ) =>
  Show (ShelleyTx l era)

deriving via
  InspectHeap (ShelleyTx l era)
  instance
    (Typeable era, Typeable l) => NoThunks (ShelleyTx l era)

-- | `TxBody` setter and getter for `ShelleyTx`.
bodyShelleyTxL :: Lens' (ShelleyTx l era) (TxBody l era)
bodyShelleyTxL =
  lens (\ShelleyTx {stBody} -> stBody) $ \tx txBody ->
    case tx of
      ShelleyTx {} -> tx {stBody = txBody}
{-# INLINEABLE bodyShelleyTxL #-}

-- | `TxWits` setter and getter for `ShelleyTx`.
witsShelleyTxL :: Lens' (ShelleyTx l era) (TxWits era)
witsShelleyTxL =
  lens (\ShelleyTx {stWits} -> stWits) $ \tx txWits ->
    case tx of
      ShelleyTx {} -> tx {stWits = txWits}
{-# INLINEABLE witsShelleyTxL #-}

-- | `TxAuxData` setter and getter for `ShelleyTx`.
auxDataShelleyTxL :: Lens' (ShelleyTx l era) (StrictMaybe (TxAuxData era))
auxDataShelleyTxL =
  lens (\ShelleyTx {stAuxData} -> stAuxData) $ \tx txAuxData ->
    case tx of
      ShelleyTx {} -> tx {stAuxData = txAuxData}
{-# INLINEABLE auxDataShelleyTxL #-}

mkBasicShelleyTx ::
  (EraTx era, STxLevel l era ~ STxTopLevel l era) =>
  TxBody l era ->
  ShelleyTx l era
mkBasicShelleyTx txBody =
  case toSTxLevel txBody of
    STopTxOnly ->
      ShelleyTx
        { stBody = txBody
        , stWits = mkBasicTxWits
        , stAuxData = SNothing
        }

toCBORForSizeComputation ::
  ( EncCBOR (TxBody l era)
  , EncCBOR (TxWits era)
  , EncCBOR (TxAuxData era)
  ) =>
  ShelleyTx l era ->
  Encoding
toCBORForSizeComputation ShelleyTx {stBody, stWits, stAuxData} =
  encodeListLen 3
    <> encCBOR stBody
    <> encCBOR stWits
    <> encodeNullStrictMaybe encCBOR stAuxData

-- | txsize computes the length of the serialised bytes (for estimations)
sizeShelleyTxF :: forall era l. (HasCallStack, EraTx era) => SimpleGetter (ShelleyTx l era) Word32
sizeShelleyTxF =
  to $
    errorFail
      . integralToBounded
      . LBS.length
      . serialize (eraProtVerLow @era)
      . toCBORForSizeComputation
{-# INLINEABLE sizeShelleyTxF #-}

instance
  ( Typeable l
  , EraTxBody era
  , EraTxWits era
  , EraTxAuxData era
  , STxLevel l era ~ STxTopLevel l era
  ) =>
  DecCBOR (Annotator (ShelleyTx l era))
  where
  decCBOR =
    withSTxTopLevelM @l @era $ \case
      STopTxOnly ->
        decode $
          Ann (RecD ShelleyTx)
            <*! From
            <*! From
            <*! D (sequence <$> decodeNullStrictMaybe decCBOR)

instance Typeable l => DecCBOR (Annotator (Tx l ShelleyEra)) where
  decCBOR = fmap MkShelleyTx <$> decCBOR

instance HasEraTxLevel Tx ShelleyEra where
  toSTxLevel (MkShelleyTx ShelleyTx {}) = STopTxOnly @ShelleyEra

instance EraTx ShelleyEra where
  newtype Tx l ShelleyEra = MkShelleyTx {unShelleyTx :: ShelleyTx l ShelleyEra}
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

shelleyTxEqRaw :: EraTx era => Tx l era -> Tx l era -> Bool
shelleyTxEqRaw tx1 tx2 =
  eqRaw (tx1 ^. bodyTxL) (tx2 ^. bodyTxL)
    && eqRaw (tx1 ^. witsTxL) (tx2 ^. witsTxL)
    && liftEq eqRaw (tx1 ^. auxDataTxL) (tx2 ^. auxDataTxL)

instance EqRaw (Tx l ShelleyEra) where
  eqRaw = shelleyTxEqRaw

shelleyTxL :: Lens' (Tx l ShelleyEra) (ShelleyTx l ShelleyEra)
shelleyTxL = lens unShelleyTx (\x y -> x {unShelleyTx = y})

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance
  (Era era, EncCBOR (TxWits era), EncCBOR (TxBody l era), EncCBOR (TxAuxData era)) =>
  EncCBOR (ShelleyTx l era)
  where
  encCBOR ShelleyTx {..} =
    encode $
      Rec ShelleyTx
        !> To stBody
        !> To stWits
        !> E (encodeNullStrictMaybe encCBOR) stAuxData

instance
  (Era era, EncCBOR (TxWits era), EncCBOR (TxBody l era), EncCBOR (TxAuxData era), Typeable l) =>
  ToCBOR (ShelleyTx l era)
  where
  toCBOR = toEraCBOR @era

-- | Minimum fee calculation
shelleyMinFeeTx :: EraTx era => PParams era -> Tx l era -> Coin
shelleyMinFeeTx pp tx =
  (tx ^. sizeTxF <×> unCoinPerByte (pp ^. ppMinFeeAL)) <+> pp ^. ppMinFeeBL
