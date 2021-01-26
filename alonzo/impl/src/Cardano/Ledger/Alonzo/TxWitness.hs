{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.TxWitness
  ( RdmrPtr (..),
    TxWitness (FlatWitness, TxWitness, witsVKey, witsBoot, witsScriptData),
    witsScript,
    witsData,
    witsRdmr,
    ScriptData (ScriptData),
    EraIndependentScriptData,
    ScriptDataHash (..),
    hashSD,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Alonzo.Data (Data, DataHash)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Tag)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Era (Crypto))
import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Data.Coders
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics
import GHC.Records
import NoThunks.Class (NoThunks)
import Shelley.Spec.Ledger.Address.Bootstrap (BootstrapWitness)
import Shelley.Spec.Ledger.Hashing (HashAnnotated (..))
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Shelley.Spec.Ledger.Serialization
  ( decodeMapTraverse,
    mapFromCBOR,
    mapToCBOR,
  )
import Shelley.Spec.Ledger.TxBody (WitVKey)

-- ==========================================

data RdmrPtr
  = RdmrPtr
      !Tag
      {-# UNPACK #-} !Word64
  deriving (Eq, Ord, Show, Generic)

instance NoThunks RdmrPtr

-- ================================================================================
-- 'TxWitness' has 3 parts
--
--   data TxWitness era = TxWitness
--      (Set (WitVKey 'Witness (Crypto era)))
--      (Set (BootstrapWitness (Crypto era)))
--      (ScriptData era)
--
-- 'ScriptData' has 3 parts
--
--   data ScriptData = ScriptData
--      (Map (ScriptHash (Crypto era)) (Core.Script era))
--      (Map (DataHash (Crypto era)) (Data era))
--      (Map RdmrPtr (Data era, ExUnits))
--
-- There is also a VIEW 'FlatWitness' of 'TxWitness' with 5 parts
--
--   data FlatWitness = FlatWitness
--      (Set (WitVKey 'Witness (Crypto era)))
--      (Set (BootstrapWitness (Crypto era)))
--      (Map (ScriptHash (Crypto era)) (Core.Script era))
--      (Map (DataHash (Crypto era)) (Data era))
--      (Map RdmrPtr (Data era, ExUnits))
--
-- 'ScriptData' is a projection of the parts of 'TxWitness' which may be
-- hashed to include in the transaction body. Note that this cannot be the hash
-- of the entire witness set, since the VKey witnesses themselves contain a hash
-- of the transaction body, creating a circular dependency.
--
-- Because we have to hash both the ScriptData and the TxWitness separately, and
-- hashing is based upon the serialized bytes, so we have to be able to extract
-- the bytes for the ScriptData part from the bytes for TxWitness whole.
--
-- The strategy is for both ScriptData and TxWitness to memoize their bytes.
-- Decoding either over the wire will preserve the exact bytes used in the encoding
-- ================================================================================

-- | Internal 'TxWitness' type, lacking serialised bytes.
data TxWitnessRaw era = TxWitnessRaw
  { _witsVKey :: Set (WitVKey 'Witness (Crypto era)),
    _witsBoot :: Set (BootstrapWitness (Crypto era)),
    _witsScriptData :: ScriptData era
  }
  deriving (Generic, Typeable)

newtype TxWitness era = TxWitnessConstr (MemoBytes (TxWitnessRaw era))
  deriving newtype (ToCBOR)

-- | Internal 'SciptData' type, lacking serialised bytes.
data ScriptDataRaw era = ScriptDataRaw
  { _scriptDataScripts :: Map (ScriptHash (Crypto era)) (Core.Script era),
    _scriptDataData :: Map (DataHash (Crypto era)) (Data era),
    _scriptDataRdmrs :: Map RdmrPtr (Data era, ExUnits)
  }
  deriving (Generic)

newtype ScriptData era = ScriptDataConstr
  { unScriptDataConstr ::
      MemoBytes (ScriptDataRaw era)
  }
  deriving newtype (ToCBOR)

-- =====================================================
-- TxWitness instances

deriving stock instance
  ( Era era,
    Eq (Core.Script era)
  ) =>
  Eq (TxWitnessRaw era)

deriving stock instance
  (Era era, Show (Core.Script era)) =>
  Show (TxWitnessRaw era)

instance (Era era, NoThunks (Core.Script era)) => NoThunks (TxWitnessRaw era)

deriving newtype instance (Era era, Eq (Core.Script era)) => Eq (TxWitness era)

deriving newtype instance
  (Era era, Show (Core.Script era)) =>
  Show (TxWitness era)

deriving newtype instance
  (Era era, NoThunks (Core.Script era)) =>
  NoThunks (TxWitness era)

-- =====================================================
-- Pattern for TxWitness with 3 parts

pattern TxWitness ::
  (Era era, ToCBOR (Core.Script era)) =>
  Set (WitVKey 'Witness (Crypto era)) ->
  Set (BootstrapWitness (Crypto era)) ->
  ScriptData era ->
  TxWitness era
pattern TxWitness
  { witsVKey,
    witsBoot,
    witsScriptData
  } <-
  TxWitnessConstr
    (Memo (TxWitnessRaw witsVKey witsBoot witsScriptData) _)
  where
    TxWitness witsVKey' witsBoot' witsScriptData' =
      TxWitnessConstr
        . memoBytes
        $ encodeWitnessRaw witsVKey' witsBoot' witsScriptData'

{-# COMPLETE TxWitness #-}

-- =====================================================
-- Pattern for TxWitness with 5 parts

pattern FlatWitness ::
  (Era era, ToCBOR (Core.Script era)) =>
  Set (WitVKey 'Witness (Crypto era)) ->
  Set (BootstrapWitness (Crypto era)) ->
  Map (ScriptHash (Crypto era)) (Core.Script era) ->
  Map (DataHash (Crypto era)) (Data era) ->
  Map RdmrPtr (Data era, ExUnits) ->
  TxWitness era
pattern FlatWitness vkey boot script dat rdmr <-
  TxWitnessConstr
    ( Memo
        ( TxWitnessRaw
            vkey
            boot
            ( ScriptDataConstr
                ( Memo
                    ( ScriptDataRaw
                        script
                        dat
                        rdmr
                      )
                    _
                  )
              )
          )
        _
      )
  where
    FlatWitness witsVKey' witsBoot' witsScript' witsDat' witsRdmr' =
      TxWitnessConstr
        . memoBytes
        $ encodeWitnessRaw witsVKey' witsBoot' (ScriptData witsScript' witsDat' witsRdmr')

{-# COMPLETE FlatWitness #-}

-- | Right-biased semigroup - if there are (somehow) multiple entries either for
-- a given 'ScriptHash' or a given 'Data', this will bias to the entry on the
-- right. Note that this should not happen in practise.
instance
  (Era era, ToCBOR (Core.Script era)) =>
  Semigroup (TxWitness era)
  where
  FlatWitness a b c d e <> FlatWitness a' b' c' d' e' =
    FlatWitness
      (a `Set.union` a')
      (b `Set.union` b')
      (c <> c')
      (d <> d')
      (e <> e')

instance
  (Era era, ToCBOR (Core.Script era)) =>
  Monoid (TxWitness era)
  where
  mempty = FlatWitness mempty mempty mempty mempty mempty

-- =======================================================
-- Virtual accessors for the FlatWitness VIEW of TxWitness
-- =======================================================

witsScript ::
  (ToCBOR (Core.Script era), Era era) =>
  TxWitness era ->
  Map (ScriptHash (Crypto era)) (Core.Script era)
witsScript (TxWitnessConstr (Memo (TxWitnessRaw _ _ (ScriptData a _ _)) _)) = a

witsData ::
  (Era era, ToCBOR (Core.Script era)) =>
  TxWitness era ->
  Map (DataHash (Crypto era)) (Data era)
witsData (TxWitnessConstr (Memo (TxWitnessRaw _ _ (ScriptData _ b _)) _)) = b

witsRdmr ::
  (Era era, ToCBOR (Core.Script era)) =>
  TxWitness era ->
  Map RdmrPtr (Data era, ExUnits)
witsRdmr (TxWitnessConstr (Memo (TxWitnessRaw _ _ (ScriptData _ _ c)) _)) = c

-- HasField instances for the virtual accessors

instance
  (Core.Script era ~ script, Crypto era ~ crypto) =>
  HasField "witsScript" (TxWitness era) (Map (ScriptHash crypto) script)
  where
  getField (TxWitnessConstr (Memo m _)) =
    _scriptDataScripts
      . memotype
      . unScriptDataConstr
      $ _witsScriptData m

instance
  (Crypto era ~ crypto) =>
  HasField "witsData" (TxWitness era) (Map (DataHash crypto) (Data era))
  where
  getField (TxWitnessConstr (Memo m _)) =
    _scriptDataData
      . memotype
      . unScriptDataConstr
      $ _witsScriptData m

instance HasField "witsRdmrs" (TxWitness era) (Map RdmrPtr (Data era, ExUnits)) where
  getField (TxWitnessConstr (Memo m _)) =
    _scriptDataRdmrs
      . memotype
      . unScriptDataConstr
      $ _witsScriptData m

--------------------------------------------------------------------------------
-- ScriptData instances
--------------------------------------------------------------------------------

deriving stock instance (Eq (Core.Script era)) => Eq (ScriptDataRaw era)

deriving stock instance (Show (Core.Script era)) => Show (ScriptDataRaw era)

instance
  (NoThunks (Core.Script era), NoThunks (Data era)) =>
  NoThunks (ScriptDataRaw era)

deriving newtype instance (Eq (Core.Script era)) => Eq (ScriptData era)

deriving newtype instance (Show (Core.Script era)) => Show (ScriptData era)

deriving newtype instance
  (Era era, NoThunks (Core.Script era)) =>
  NoThunks (ScriptData era)

pattern ScriptData ::
  (Era era, ToCBOR (Core.Script era)) =>
  Map (ScriptHash (Crypto era)) (Core.Script era) ->
  Map (DataHash (Crypto era)) (Data era) ->
  Map RdmrPtr (Data era, ExUnits) ->
  ScriptData era
pattern ScriptData s d r <-
  ScriptDataConstr (Memo (ScriptDataRaw s d r) _)
  where
    ScriptData s d r =
      ScriptDataConstr $
        memoBytes $
          encodeScriptDataRaw (ScriptDataRaw s d r)

{-# COMPLETE ScriptData #-}

instance
  (Era era, ToCBOR (Core.Script era)) =>
  Semigroup (ScriptData era)
  where
  (ScriptData s d r)
    <> (ScriptData s' d' r') =
      ScriptData (s <> s') (d `Map.union` d') (r <> r')

instance
  (Era era, ToCBOR (Core.Script era)) =>
  Monoid (ScriptData era)
  where
  mempty = ScriptData mempty mempty mempty

data EraIndependentScriptData

newtype ScriptDataHash crypto
  = ScriptDataHash
      (Hash.Hash (HASH crypto) EraIndependentScriptData)
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NFData, NoThunks)

deriving newtype instance CC.Crypto crypto => ToCBOR (ScriptDataHash crypto)

deriving newtype instance CC.Crypto crypto => FromCBOR (ScriptDataHash crypto)

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------
instance ToCBOR RdmrPtr where
  toCBOR (RdmrPtr t w) = encode $ Rec RdmrPtr !> To t !> To w

instance FromCBOR RdmrPtr where
  fromCBOR = decode $ RecD RdmrPtr <! From <! From

encodeScriptDataRaw ::
  (Era era, ToCBOR (Core.Script era), ToCBOR (Data era)) =>
  ScriptDataRaw era ->
  Encode ('Closed 'Dense) (ScriptDataRaw era)
encodeScriptDataRaw sdr =
  Rec ScriptDataRaw
    !> E mapToCBOR (_scriptDataScripts sdr)
    !> E mapToCBOR (_scriptDataData sdr)
    !> E mapToCBOR (_scriptDataRdmrs sdr)

instance
  ( Typeable (Data era),
    Typeable (Core.Script era),
    FromCBOR (Annotator (Core.Script era)),
    Era era
  ) =>
  FromCBOR (Annotator (ScriptDataRaw era))
  where
  fromCBOR =
    decode $
      Ann (RecD ScriptDataRaw)
        <*! D (sequence <$> mapFromCBOR)
        <*! D (sequence <$> mapFromCBOR)
        <*! D (splitMapFromCBOR fromCBOR fromCBOR fromCBOR)

-- ScriptData includes a field with type: (Map RdmrPtr (Data era, ExUnits))
-- We only have a (ToCBOR (Annotator (Data era))) instance, so we need a special
-- way to decode a Map where one half of its range has only a (FromCBOR (Annotator _))
-- instance. We have to be careful since the map is encodedwith 'mapToCBOR' and the
-- decoder needs to be consistent with that encoding.

splitMapFromCBOR ::
  Ord dom =>
  Decoder s dom ->
  Decoder s (Annotator rngLeft) ->
  Decoder s rngRight ->
  Decoder s (Annotator (Map dom (rngLeft, rngRight)))
splitMapFromCBOR a b c = decodeMapTraverse (pure <$> a) (liftPair <$> decodePair b c)
  where
    liftPair :: (Annotator a, b) -> Annotator (a, b)
    liftPair (x, y) = liftA2 (,) x (pure y)

deriving via
  (Mem (ScriptDataRaw era))
  instance
    ( Era era,
      FromCBOR (Annotator (Core.Script era)),
      Typeable (Core.Script era)
    ) =>
    FromCBOR (Annotator (ScriptData era))

-- | Encode witness information.
encodeWitnessRaw ::
  (Era era) =>
  Set (WitVKey 'Witness (Crypto era)) ->
  Set (BootstrapWitness (Crypto era)) ->
  ScriptData era ->
  Encode ('Closed 'Dense) (TxWitnessRaw era)
encodeWitnessRaw a b sd =
  Rec TxWitnessRaw
    !> E encodeFoldable a
    !> E encodeFoldable b
    !> To sd

instance
  ( Era era,
    FromCBOR (Annotator (Core.Script era)),
    ToCBOR (Data era),
    ToCBOR (Core.Script era),
    Typeable (Core.Script era)
  ) =>
  FromCBOR (Annotator (TxWitnessRaw era))
  where
  fromCBOR =
    decode $
      Ann (RecD TxWitnessRaw)
        <*! D (fmap Set.fromList . sequence <$> decodeList fromCBOR)
        <*! D (fmap Set.fromList . sequence <$> decodeList fromCBOR)
        <*! From

deriving via
  (Mem (TxWitnessRaw era))
  instance
    ( Era era,
      FromCBOR (Annotator (Core.Script era)),
      ToCBOR (Core.Script era)
    ) =>
    FromCBOR (Annotator (TxWitness era))

-- =======================================================
-- To compute a ScriptDataHash from a TxWitness we have to hash
-- the embedded (ScriptData era) value in the TxWitness.
-- See function hashSD in  Figure 12 "UTXOW helper functions"
-- This virtual triple is stored inside of the TxWitness as a concrete value
-- (ScriptData era) is a newtype around a MemoBytes, so hashing
-- it is just hashing the memoized bytestring inside the MemoBytes.
-- Furtunately, applying toCBOR to (ScriptData era) is just that memoized
-- bytstring, so we can make a HashAnnotated instance for (ScriptData era)
-- to compute the hash, since (hashAnnotated x) is (hash (toCBOR x))

instance (Era era, Typeable era) => HashAnnotated (ScriptData era) era where
  type HashIndex (ScriptData era) = EraIndependentScriptData

hashSD ::
  (Era era, ToCBOR (Core.Script era)) =>
  TxWitness era ->
  Maybe (ScriptDataHash (Crypto era))
hashSD (w@(TxWitnessConstr (Memo (TxWitnessRaw _ _ scriptdata) _))) =
  if (Map.null (witsScript w) && Map.null (witsData w) && Map.null (witsRdmr w))
    then Nothing
    else Just (ScriptDataHash (hashAnnotated scriptdata))
