{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.Data
  ( PlutusData (..),
    -- $
    Data (Data, ..),
    DataHash,
    hashData,
    -- $
    AuxiliaryData (AuxiliaryData, scripts, dats, txMD),
    AuxiliaryDataHash,
    -- $
    ppPlutusData,
    ppData,
    ppAuxiliaryData,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeInt, encodeInt)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Pretty
  ( PDoc,
    PrettyA (..),
    ppMetadata,
    ppSet,
    ppSexp,
    ppString,
  )
import Cardano.Ledger.SafeHash
  ( EraIndependentAuxiliaryData,
    EraIndependentData,
    HashAnnotated,
    SafeHash,
    SafeToHash,
    hashAnnotated,
  )
import Data.Coders
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import Shelley.Spec.Ledger.Metadata (Metadata)

-- =====================================================================
-- PlutusData is a placeholder for the type that Plutus expects as data.

data PlutusData = NotReallyData
  deriving (Eq, Show, Ord, Generic)

instance NoThunks PlutusData

-- | TODO appropriate serialisation for the Real Plutus Data
instance ToCBOR PlutusData where
  toCBOR _ = encodeInt 0

instance FromCBOR (PlutusData) where
  fromCBOR = do
    i <- decodeInt
    case i of
      0 -> pure NotReallyData
      _ -> fail "oh no"

instance FromCBOR (Annotator PlutusData) where
  fromCBOR = pure <$> fromCBOR

-- ============================================================================
-- the newtype Data is a wrapper around the type that Plutus expects as data.
-- The newtype will memoize the serialized bytes. The strategy is to replace
-- PlutusData  with the correct type

newtype Data era = DataConstr (MemoBytes PlutusData)
  deriving (Eq, Ord, Generic, Show)
  deriving newtype (SafeToHash, ToCBOR)

deriving via
  (Mem PlutusData)
  instance
    (Era era) =>
    FromCBOR (Annotator (Data era))

instance (Crypto era ~ c) => HashAnnotated (Data era) EraIndependentData c

instance NoThunks (Data era)

pattern Data :: PlutusData -> Data era
pattern Data p <-
  DataConstr (Memo p _)
  where
    Data p = DataConstr (memoBytes (To p))

-- =============================================================================

type DataHash crypto = SafeHash crypto EraIndependentData

hashData :: Era era => Data era -> DataHash (Crypto era)
hashData d = hashAnnotated d

-- =============================================================================
-- Version without serialized bytes

data AuxiliaryDataRaw era = AuxiliaryDataRaw
  { scripts' :: Set (Core.Script era),
    dats' :: Set (Data era),
    txMD' :: Set (Metadata)
  }
  deriving (Generic)

deriving instance Eq (Core.Script era) => Eq (AuxiliaryDataRaw era)

deriving instance Show (Core.Script era) => Show (AuxiliaryDataRaw era)

deriving via InspectHeapNamed "AuxiliaryDataRaw" (AuxiliaryDataRaw era) instance NoThunks (AuxiliaryDataRaw era)

instance
  ( Typeable era,
    Ord (Core.Script era),
    ToCBOR (Core.Script era)
  ) =>
  ToCBOR (AuxiliaryDataRaw era)
  where
  toCBOR (AuxiliaryDataRaw s d m) =
    encode (encodeRaw s d m)

encodeRaw ::
  (ToCBOR (Core.Script era), Typeable era) =>
  Set (Core.Script era) ->
  Set (Data era) ->
  Set Metadata ->
  Encode ('Closed 'Dense) (AuxiliaryDataRaw era)
encodeRaw s d m =
  ( Rec AuxiliaryDataRaw
      !> E encodeFoldable s
      !> E encodeFoldable d
      !> E encodeFoldable m
  )

instance
  ( Era era,
    Ord (Core.Script era),
    FromCBOR (Annotator (Core.Script era))
  ) =>
  FromCBOR (Annotator (AuxiliaryDataRaw era))
  where
  fromCBOR =
    decode
      ( Ann (RecD AuxiliaryDataRaw)
          <*! D (decodeAnnSet fromCBOR)
          <*! D (decodeAnnSet fromCBOR)
          <*! D (decodeAnnSet fromCBOR)
      )

-- ================================================================================
-- Version with serialized bytes.

newtype AuxiliaryData era = AuxiliaryDataConstr (MemoBytes (AuxiliaryDataRaw era))
  deriving newtype (ToCBOR, SafeToHash)

instance (Crypto era ~ c) => HashAnnotated (AuxiliaryData era) EraIndependentAuxiliaryData c

type AuxiliaryDataHash crypto = SafeHash crypto EraIndependentAuxiliaryData

deriving instance Eq (Core.Script era) => Eq (AuxiliaryData era)

deriving instance Show (Core.Script era) => Show (AuxiliaryData era)

deriving via InspectHeapNamed "AuxiliaryDataRaw" (AuxiliaryData era) instance NoThunks (AuxiliaryData era)

deriving via
  (Mem (AuxiliaryDataRaw era))
  instance
    ( Era era,
      Ord (Core.Script era),
      FromCBOR (Annotator (Core.Script era))
    ) =>
    FromCBOR (Annotator (AuxiliaryData era))

pattern AuxiliaryData ::
  (Era era, ToCBOR (Core.Script era), Ord (Core.Script era)) =>
  Set (Core.Script era) ->
  Set (Data era) ->
  Set (Metadata) ->
  AuxiliaryData era
pattern AuxiliaryData {scripts, dats, txMD} <-
  AuxiliaryDataConstr (Memo (AuxiliaryDataRaw scripts dats txMD) _)
  where
    AuxiliaryData s d m =
      AuxiliaryDataConstr
        ( memoBytes
            (encodeRaw s d m)
        )

{-# COMPLETE AuxiliaryData #-}

-- =======================================================

ppPlutusData :: PlutusData -> PDoc
ppPlutusData NotReallyData = ppString "PlutusData"

instance PrettyA PlutusData where prettyA = ppPlutusData

ppData :: Data era -> PDoc
ppData (DataConstr (Memo x _)) = ppSexp "Data" [ppPlutusData x]

instance PrettyA (Data era) where prettyA = ppData

ppAuxiliaryData :: (PrettyA (Core.Script era)) => AuxiliaryData era -> PDoc
ppAuxiliaryData (AuxiliaryDataConstr (Memo (AuxiliaryDataRaw s d m) _)) =
  ppSexp "AuxiliaryData" [ppSet prettyA s, ppSet ppData d, ppSet ppMetadata m]

instance (PrettyA (Core.Script era)) => PrettyA (AuxiliaryData era) where prettyA = ppAuxiliaryData
