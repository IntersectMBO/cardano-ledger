{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
    TxWitness (TxWitness, witsVKey, witsBoot, witsScript, witsData, witsRdmr),
    EraIndependentScriptData,
    ScriptDataHash (..),
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Alonzo.Data (Data, DataHash)
import Cardano.Ledger.Alonzo.Scripts (Tag)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Era (Crypto))
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
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Shelley.Spec.Ledger.Serialization (mapFromCBOR, mapToCBOR)
import Shelley.Spec.Ledger.TxBody (WitVKey)

data RdmrPtr
  = RdmrPtr
      !Tag
      {-# UNPACK #-} !Word64
  deriving (Eq, Ord, Show, Generic)

instance NoThunks RdmrPtr

-- | Internal 'TxWitness' type, lacking serialised bytes.
data TxWitnessRaw era = TxWitnessRaw
  { _witsVKey :: Set (WitVKey 'Witness (Crypto era)),
    _witsBoot :: Set (BootstrapWitness (Crypto era)),
    _witsScriptData :: ScriptData era
  }
  deriving (Generic, Typeable)

deriving stock instance
  ( Era era,
    Eq (Core.Script era)
  ) =>
  Eq (TxWitnessRaw era)

deriving stock instance
  (Era era, Show (Core.Script era)) =>
  Show (TxWitnessRaw era)

instance (Era era, NoThunks (Core.Script era)) => NoThunks (TxWitnessRaw era)

newtype TxWitness era = TxWitnessConstr (MemoBytes (TxWitnessRaw era))
  deriving newtype (ToCBOR)

deriving newtype instance (Era era, Eq (Core.Script era)) => Eq (TxWitness era)

deriving newtype instance
  (Era era, Show (Core.Script era)) =>
  Show (TxWitness era)

deriving newtype instance
  (Era era, NoThunks (Core.Script era)) =>
  NoThunks (TxWitness era)

pattern TxWitness ::
  (Era era, ToCBOR (Core.Script era)) =>
  Set (WitVKey 'Witness (Crypto era)) ->
  Set (BootstrapWitness (Crypto era)) ->
  Map (ScriptHash (Crypto era)) (Core.Script era) ->
  Map (DataHash (Crypto era)) (Data era) ->
  Map RdmrPtr (Data era) ->
  TxWitness era
pattern TxWitness
  { witsVKey,
    witsBoot,
    witsScript,
    witsData,
    witsRdmr
  } <-
  TxWitnessConstr
    ( Memo
        ( TxWitnessRaw
            witsVKey
            witsBoot
            ( ScriptDataConstr
                ( Memo
                    ( ScriptDataRaw
                        witsScript
                        witsData
                        witsRdmr
                      )
                    _
                  )
              )
          )
        _
      )
  where
    TxWitness witsVKey' witsBoot' witsScript' witsDat' witsRdmr' =
      TxWitnessConstr
        . memoBytes
        $ encodeWitnessRaw witsVKey' witsBoot' witsScript' witsDat' witsRdmr'

{-# COMPLETE TxWitness #-}

-- | Right-biased semigroup - if there are (somehow) multiple entries either for
-- a given 'ScriptHash' or a given 'Data', this will bias to the entry on the
-- right. Note that this should not happen in practise.
instance
  (Era era, ToCBOR (Core.Script era)) =>
  Semigroup (TxWitness era)
  where
  TxWitness a b c d e <> TxWitness a' b' c' d' e' =
    TxWitness
      (a `Set.union` a')
      (b `Set.union` b')
      (c <> c')
      (d <> d')
      (e <> e')

instance
  (Era era, ToCBOR (Core.Script era)) =>
  Monoid (TxWitness era)
  where
  mempty = TxWitness mempty mempty mempty mempty mempty

--------------------------------------------------------------------------------
-- Accessors
--------------------------------------------------------------------------------
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

instance HasField "witsRdmrs" (TxWitness era) (Map RdmrPtr (Data era)) where
  getField (TxWitnessConstr (Memo m _)) =
    _scriptDataRdmrs
      . memotype
      . unScriptDataConstr
      $ _witsScriptData m

--------------------------------------------------------------------------------
-- ScriptData
--------------------------------------------------------------------------------
-- TODO now that this has a hash-to-data map, make sure CBOR is still correct!
data ScriptDataRaw era = ScriptDataRaw
  { _scriptDataScripts :: Map (ScriptHash (Crypto era)) (Core.Script era),
    _scriptDataData :: Map (DataHash (Crypto era)) (Data era),
    _scriptDataRdmrs :: Map RdmrPtr (Data era)
  }
  deriving (Generic)

deriving stock instance (Eq (Core.Script era)) => Eq (ScriptDataRaw era)

deriving stock instance (Show (Core.Script era)) => Show (ScriptDataRaw era)

instance
  (NoThunks (Core.Script era), NoThunks (Data era)) =>
  NoThunks (ScriptDataRaw era)

-- | 'ScriptData' is a projection of the parts of 'TxWitness' which may be
-- hashed to include in the transaction body. Note that this cannot be the hash
-- of the entire witness set, since the VKey witnesses themselves contain a hash
-- of the transaction body, creating a circular dependency.
--
-- The 'ScriptData' type itself is internal to this module; it is never directly
-- serialised or deserialised, but will automatically be populated upon
-- deserialisation or creation of 'TxWitness'
newtype ScriptData era = ScriptDataConstr
  { unScriptDataConstr ::
      MemoBytes (ScriptDataRaw era)
  }
  deriving newtype (ToCBOR)

deriving newtype instance (Eq (Core.Script era)) => Eq (ScriptData era)

deriving newtype instance (Show (Core.Script era)) => Show (ScriptData era)

deriving newtype instance
  (Era era, NoThunks (Core.Script era)) =>
  NoThunks (ScriptData era)

pattern ScriptData ::
  (Era era, ToCBOR (Core.Script era)) =>
  Map (ScriptHash (Crypto era)) (Core.Script era) ->
  Map (DataHash (Crypto era)) (Data era) ->
  Map RdmrPtr (Data era) ->
  ScriptData era
pattern ScriptData s d r <-
  ScriptDataConstr (Memo (ScriptDataRaw s d r) _)
  where
    ScriptData s d r =
      ScriptDataConstr $
        memoBytes $
          encodeScriptDataRaw (ScriptDataRaw s d r)

instance
  (Era era, ToCBOR (Core.Script era)) =>
  Semigroup (ScriptData era)
  where
  (ScriptData s d r)
    <> (ScriptData s' d' r') =
      ScriptData (s <> s') (d `Map.union` d') (r <> r')

{-# COMPLETE ScriptData #-}

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
        <*! Ann (D mapFromCBOR)
        <*! Ann (D mapFromCBOR)

deriving via
  (Mem (ScriptDataRaw era))
  instance
    ( Era era,
      FromCBOR (Annotator (Core.Script era)),
      FromCBOR (Data era),
      Typeable (Core.Script era)
    ) =>
    FromCBOR (Annotator (ScriptData era))

-- | Encode witness information.
encodeWitnessRaw ::
  (Era era, ToCBOR (Core.Script era)) =>
  Set (WitVKey 'Witness (Crypto era)) ->
  Set (BootstrapWitness (Crypto era)) ->
  Map (ScriptHash (Crypto era)) (Core.Script era) ->
  Map (DataHash (Crypto era)) (Data era) ->
  Map RdmrPtr (Data era) ->
  Encode ('Closed 'Dense) (TxWitnessRaw era)
encodeWitnessRaw a b s d r =
  Rec TxWitnessRaw
    !> E encodeFoldable a
    !> E encodeFoldable b
    !> To (ScriptData s d r)

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
