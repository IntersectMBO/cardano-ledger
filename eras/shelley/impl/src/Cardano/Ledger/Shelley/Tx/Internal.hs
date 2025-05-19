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
  ShelleyTx (
    MkShelleyTx,
    ShelleyTx,
    body,
    wits,
    auxiliaryData
  ),
  ShelleyTxRaw (..),
  bodyShelleyTxL,
  witsShelleyTxL,
  auxDataShelleyTxL,
  sizeShelleyTxF,
  wireSizeShelleyTxF,
  segWitTx,
  segWitAnnTx,
  mkBasicShelleyTx,
  shelleyMinFeeTx,
  witsFromTxWitnesses,
  shelleyEqTxRaw,
  unsafeConstructTxWithBytes,
) where

import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (decCBOR),
  EncCBOR (encCBOR),
  ToCBOR,
  decodeNullMaybe,
  decodeNullStrictMaybe,
  encodeNullMaybe,
 )
import Cardano.Ledger.Binary.Coders
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
  Mem,
  MemoBytes,
  Memoized (..),
  getMemoRawBytes,
  getMemoRawType,
  lensMemoRawType,
 )
import Cardano.Ledger.MemoBytes.Internal (memoBytesEra, mkMemoBytes)
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Scripts (validateMultiSig)
import Cardano.Ledger.Shelley.TxAuxData ()
import Cardano.Ledger.Shelley.TxBody ()
import Cardano.Ledger.Shelley.TxWits ()
import Cardano.Ledger.Val ((<+>), (<×>))
import Control.DeepSeq (NFData)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Functor.Classes (Eq1 (liftEq))
import Data.Maybe.Strict (
  StrictMaybe (..),
  maybeToStrictMaybe,
  strictMaybeToMaybe,
 )
import Data.Set (Set)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro (Lens', SimpleGetter, to, (^.))
import NoThunks.Class (NoThunks (..))

-- ========================================================

data ShelleyTxRaw era = ShelleyTxRaw
  { strBody :: !(TxBody era)
  , strWits :: !(TxWits era)
  , strAuxData :: !(StrictMaybe (TxAuxData era))
  }
  deriving (Generic)

instance
  ( NFData (TxBody era)
  , NFData (TxWits era)
  , NFData (TxAuxData era)
  ) =>
  NFData (ShelleyTxRaw era)

deriving instance
  ( Era era
  , Eq (TxBody era)
  , Eq (TxWits era)
  , Eq (TxAuxData era)
  ) =>
  Eq (ShelleyTxRaw era)

deriving instance
  ( Era era
  , Show (TxBody era)
  , Show (TxWits era)
  , Show (TxAuxData era)
  ) =>
  Show (ShelleyTxRaw era)

instance
  ( Era era
  , NoThunks (TxAuxData era)
  , NoThunks (TxBody era)
  , NoThunks (TxWits era)
  ) =>
  NoThunks (ShelleyTxRaw era)

newtype ShelleyTx era = MkShelleyTx (MemoBytes (ShelleyTxRaw era))
  deriving newtype (SafeToHash, ToCBOR)
  deriving (Generic)

instance Memoized (ShelleyTx era) where
  type RawType (ShelleyTx era) = ShelleyTxRaw era

-- | `TxBody` setter and getter for `ShelleyTx`. The setter does update
-- memoized binary representation.
bodyShelleyTxL :: forall era. EraTx era => Lens' (ShelleyTx era) (TxBody era)
bodyShelleyTxL =
  lensMemoRawType @era strBody $ \tx txBody -> tx {strBody = txBody}
{-# INLINEABLE bodyShelleyTxL #-}

-- | `TxWits` setter and getter for `ShelleyTx`. The setter does update
-- memoized binary representation.
witsShelleyTxL :: forall era. EraTx era => Lens' (ShelleyTx era) (TxWits era)
witsShelleyTxL =
  lensMemoRawType @era strWits $ \tx txWits -> tx {strWits = txWits}
{-# INLINEABLE witsShelleyTxL #-}

-- | `TxAuxData` setter and getter for `ShelleyTx`. The setter does update
-- memoized binary representation.
auxDataShelleyTxL :: forall era. EraTx era => Lens' (ShelleyTx era) (StrictMaybe (TxAuxData era))
auxDataShelleyTxL =
  lensMemoRawType @era strAuxData $ \tx txAuxData -> tx {strAuxData = txAuxData}
{-# INLINEABLE auxDataShelleyTxL #-}

-- | Size getter for `ShelleyTx`.
sizeShelleyTxF :: SimpleGetter (ShelleyTx era) Integer
sizeShelleyTxF = to (\(getMemoRawBytes -> bytes) -> fromIntegral $ SBS.length bytes)
{-# INLINEABLE sizeShelleyTxF #-}

wireSizeShelleyTxF :: SimpleGetter (ShelleyTx era) Word32
wireSizeShelleyTxF = to $ \(getMemoRawBytes -> bytes) ->
  let n = SBS.length bytes
   in if n <= fromIntegral (maxBound :: Word32)
        then fromIntegral n
        else error $ "Impossible: Size of the transaction is too big: " ++ show n
{-# INLINEABLE wireSizeShelleyTxF #-}

mkShelleyTx :: forall era. EraTx era => ShelleyTxRaw era -> ShelleyTx era
mkShelleyTx = MkShelleyTx . memoBytesEra @era . encodeShelleyTxRaw
{-# INLINEABLE mkShelleyTx #-}

mkBasicShelleyTx :: EraTx era => TxBody era -> ShelleyTx era
mkBasicShelleyTx txBody =
  mkShelleyTx $
    ShelleyTxRaw
      { strBody = txBody
      , strWits = mkBasicTxWits
      , strAuxData = SNothing
      }

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

  wireSizeTxF = wireSizeShelleyTxF
  {-# INLINE wireSizeTxF #-}

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

deriving newtype instance
  ( NFData (TxBody era)
  , NFData (TxWits era)
  , NFData (TxAuxData era)
  ) =>
  NFData (ShelleyTx era)

deriving newtype instance
  ( Era era
  , Eq (TxBody era)
  , Eq (TxWits era)
  , Eq (TxAuxData era)
  ) =>
  Eq (ShelleyTx era)

deriving newtype instance
  (Era era, Show (TxBody era), Show (TxWits era), Show (TxAuxData era)) =>
  Show (ShelleyTx era)

deriving newtype instance
  ( Era era
  , NoThunks (TxAuxData era)
  , NoThunks (TxBody era)
  , NoThunks (TxWits era)
  ) =>
  NoThunks (ShelleyTx era)

pattern ShelleyTx ::
  EraTx era =>
  TxBody era ->
  TxWits era ->
  StrictMaybe (TxAuxData era) ->
  ShelleyTx era
pattern ShelleyTx {body, wits, auxiliaryData} <-
  ( getMemoRawType ->
      ShelleyTxRaw
        { strBody = body
        , strWits = wits
        , strAuxData = auxiliaryData
        }
    )
  where
    ShelleyTx b w a = mkShelleyTx $ ShelleyTxRaw b w a

{-# COMPLETE ShelleyTx #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

encodeShelleyTxRaw ::
  (EncCBOR (TxWits era), EncCBOR (TxBody era), EncCBOR (TxAuxData era)) =>
  ShelleyTxRaw era ->
  Encode ('Closed 'Dense) (ShelleyTxRaw era)
encodeShelleyTxRaw ShelleyTxRaw {strBody, strWits, strAuxData} =
  Rec ShelleyTxRaw
    !> To strBody
    !> To strWits
    !> E (encodeNullMaybe encCBOR . strictMaybeToMaybe) strAuxData

instance
  (Era era, EncCBOR (TxWits era), EncCBOR (TxBody era), EncCBOR (TxAuxData era)) =>
  EncCBOR (ShelleyTxRaw era)
  where
  encCBOR = encode . encodeShelleyTxRaw

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (ShelleyTx era)

instance
  ( Era era
  , DecCBOR (TxBody era)
  , DecCBOR (TxWits era)
  , DecCBOR (TxAuxData era)
  ) =>
  DecCBOR (ShelleyTxRaw era)
  where
  decCBOR =
    decode $
      RecD ShelleyTxRaw
        <! From
        <! From
        <! D (decodeNullStrictMaybe decCBOR)

instance
  ( EraTx era
  , DecCBOR (Annotator (TxBody era))
  , DecCBOR (Annotator (TxWits era))
  , DecCBOR (Annotator (TxAuxData era))
  ) =>
  DecCBOR (Annotator (ShelleyTxRaw era))
  where
  decCBOR =
    decode $
      Ann (RecD ShelleyTxRaw)
        <*! From
        <*! From
        <*! D
          ( sequence . maybeToStrictMaybe
              <$> decodeNullMaybe decCBOR
          )

deriving newtype instance
  ( Era era
  , DecCBOR (TxBody era)
  , DecCBOR (TxWits era)
  , DecCBOR (TxAuxData era)
  ) =>
  DecCBOR (ShelleyTx era)

deriving via
  Mem (ShelleyTxRaw era)
  instance
    ( EraTx era
    , DecCBOR (Annotator (TxBody era))
    , DecCBOR (Annotator (TxWits era))
    , DecCBOR (Annotator (TxAuxData era))
    ) =>
    DecCBOR (Annotator (ShelleyTx era))

-- | Construct a Tx containing the explicit serialised bytes.
--
--   This function is marked as unsafe since it makes no guarantee that the
--   represented bytes are indeed the correct serialisation of the transaction.
--   Thus, when calling this function, the caller is responsible for making this
--   guarantee.
--
--   The only intended use case for this is for segregated witness.
unsafeConstructTxWithBytes ::
  TxBody era ->
  TxWits era ->
  StrictMaybe (TxAuxData era) ->
  LBS.ByteString ->
  ShelleyTx era
unsafeConstructTxWithBytes b w a bytes = MkShelleyTx (mkMemoBytes (ShelleyTxRaw b w a) bytes)

--------------------------------------------------------------------------------
-- Segregated witness
--------------------------------------------------------------------------------

segWitTx ::
  forall era.
  EraTx era =>
  TxBody era ->
  TxWits era ->
  Maybe (TxAuxData era) ->
  ShelleyTx era
segWitTx body' witnessSet auxData =
  let
    wrappedAuxDataBytes = case auxData of
      Nothing -> Plain.serialize Plain.encodeNull
      Just b -> Plain.serialize b
    fullBytes =
      Plain.serialize (Plain.encodeListLen 3)
        <> Plain.serialize body'
        <> Plain.serialize witnessSet
        <> wrappedAuxDataBytes
   in
    unsafeConstructTxWithBytes
      body'
      witnessSet
      (maybeToStrictMaybe auxData)
      fullBytes

segWitAnnTx ::
  forall era.
  EraTx era =>
  Annotator (TxBody era) ->
  Annotator (TxWits era) ->
  Maybe (Annotator (TxAuxData era)) ->
  Annotator (ShelleyTx era)
segWitAnnTx bodyAnn witsAnn metaAnn = Annotator $ \bytes ->
  let body' = runAnnotator bodyAnn bytes
      witnessSet = runAnnotator witsAnn bytes
      metadata = flip runAnnotator bytes <$> metaAnn
      wrappedMetadataBytes = case metadata of
        Nothing -> Plain.serialize Plain.encodeNull
        Just b -> Plain.serialize b
      fullBytes =
        Plain.serialize (Plain.encodeListLen 3)
          <> Plain.serialize body'
          <> Plain.serialize witnessSet
          <> wrappedMetadataBytes
   in unsafeConstructTxWithBytes
        body'
        witnessSet
        (maybeToStrictMaybe metadata)
        fullBytes

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
