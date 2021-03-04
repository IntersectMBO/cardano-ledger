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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.TxWitness
  ( RdmrPtr (..),
    TxWitness
      ( TxWitness,
        txwitsVKey,
        txwitsBoot,
        txscripts,
        txdats,
        txrdmrs,
        TxWitness',
        txwitsVKey',
        txwitsBoot',
        txscripts',
        txdats',
        txrdmrs'
      ),
    ppRdmrPtr,
    ppTxWitness,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Alonzo.Data (Data, DataHash, ppData)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Tag, ppExUnits, ppTag)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.Pretty
  ( PDoc,
    PrettyA (..),
    ppBootstrapWitness,
    ppMap,
    ppPair,
    ppRecord,
    ppSafeHash,
    ppScriptHash,
    ppSet,
    ppSexp,
    ppWitVKey,
    ppWord64,
  )
import Data.Coders
import Data.Map.Strict (Map)
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
import Shelley.Spec.Ledger.TxBody (WitVKey)

-- ==========================================

data RdmrPtr
  = RdmrPtr
      !Tag
      {-# UNPACK #-} !Word64
  deriving (Eq, Ord, Show, Generic)

instance NoThunks RdmrPtr

instance ToCBOR RdmrPtr where
  toCBOR (RdmrPtr t w) = encode $ Rec RdmrPtr !> To t !> To w

instance FromCBOR RdmrPtr where
  fromCBOR = decode $ RecD RdmrPtr <! From <! From

-- ====================================================================
-- In the Spec, TxWitness has 4 logical fields. Here in the implementation
-- we make two physical modifications.
-- 1) The witsVKey field of TxWitness is specified as a (Map VKey Signature)
--    for efficiency this is stored as a (Set WitVKey) where WitVKey is
--    logically a triple (VKey,Signature,VKeyHash).
-- 2) We add a 5th field _witsBoot to be backwards compatible with
--    earlier Eras: Byron, Mary, Allegra
-- So logically things look like this
--   data TxWitness = TxWitness
--      (Set (WitVKey 'Witness (Crypto era)))
--      (Set (BootstrapWitness (Crypto era)))
--      (Map (ScriptHash (Crypto era)) (Core.Script era))
--      (Map (DataHash (Crypto era)) (Data era))
--      (Map RdmrPtr (Data era, ExUnits))

-- | Internal 'TxWitness' type, lacking serialised bytes.
data TxWitnessRaw era = TxWitnessRaw
  { _txwitsVKey :: Set (WitVKey 'Witness (Crypto era)),
    _txwitsBoot :: Set (BootstrapWitness (Crypto era)),
    _txscripts :: Map (ScriptHash (Crypto era)) (Core.Script era),
    _txdats :: Map (DataHash (Crypto era)) (Data era),
    _txrdmrs :: Map RdmrPtr (Data era, ExUnits)
  }
  deriving (Generic, Typeable)

newtype TxWitness era = TxWitnessConstr (MemoBytes (TxWitnessRaw era))
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
-- Pattern for TxWitness

pattern TxWitness' ::
  Set (WitVKey 'Witness (Crypto era)) ->
  Set (BootstrapWitness (Crypto era)) ->
  Map (ScriptHash (Crypto era)) (Core.Script era) ->
  Map (DataHash (Crypto era)) (Data era) ->
  Map RdmrPtr (Data era, ExUnits) ->
  TxWitness era
pattern TxWitness' {txwitsVKey', txwitsBoot', txscripts', txdats', txrdmrs'} <-
  TxWitnessConstr
    (Memo (TxWitnessRaw txwitsVKey' txwitsBoot' txscripts' txdats' txrdmrs') _)

{-# COMPLETE TxWitness' #-}

pattern TxWitness ::
  (Era era, ToCBOR (Core.Script era)) =>
  Set (WitVKey 'Witness (Crypto era)) ->
  Set (BootstrapWitness (Crypto era)) ->
  Map (ScriptHash (Crypto era)) (Core.Script era) ->
  Map (DataHash (Crypto era)) (Data era) ->
  Map RdmrPtr (Data era, ExUnits) ->
  TxWitness era
pattern TxWitness {txwitsVKey, txwitsBoot, txscripts, txdats, txrdmrs} <-
  TxWitnessConstr
    (Memo (TxWitnessRaw txwitsVKey txwitsBoot txscripts txdats txrdmrs) _)
  where
    TxWitness witsVKey' witsBoot' witsScript' witsDat' witsRdmr' =
      TxWitnessConstr
        . memoBytes
        $ encodeWitnessRaw witsVKey' witsBoot' witsScript' witsDat' witsRdmr'

{-# COMPLETE TxWitness #-}

-- ======================================================================

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

-- =======================================================
-- Virtual HasField instances for the accessors
-- =======================================================

instance
  (Core.Script era ~ script, Crypto era ~ crypto) =>
  HasField "txscripts" (TxWitness era) (Map (ScriptHash crypto) script)
  where
  getField (TxWitnessConstr (Memo (TxWitnessRaw _ _ s _ _) _)) = s

instance
  (Crypto era ~ crypto) =>
  HasField "txdats" (TxWitness era) (Map (DataHash crypto) (Data era))
  where
  getField (TxWitnessConstr (Memo (TxWitnessRaw _ _ _ d _) _)) = d

instance HasField "txrdmrs" (TxWitness era) (Map RdmrPtr (Data era, ExUnits)) where
  getField (TxWitnessConstr (Memo (TxWitnessRaw _ _ _ _ r) _)) = r

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

encodeWitnessRaw ::
  (Era era, ToCBOR (Core.Script era), ToCBOR (Data era)) =>
  Set (WitVKey 'Witness (Crypto era)) ->
  Set (BootstrapWitness (Crypto era)) ->
  Map (ScriptHash (Crypto era)) (Core.Script era) ->
  Map (DataHash (Crypto era)) (Data era) ->
  Map RdmrPtr (Data era, ExUnits) ->
  Encode ('Closed 'Dense) (TxWitnessRaw era)
encodeWitnessRaw a b c d e =
  Rec TxWitnessRaw
    !> setEncode a
    !> setEncode b
    !> mapEncode c
    !> mapEncode d
    !> mapEncode e

-- TxWitness includes a field with type: (Map RdmrPtr (Data era, ExUnits))
-- We only have a (ToCBOR (Annotator (Data era))) instance, so we need a special
-- way to decode a Map where one half of its range has only a (FromCBOR (Annotator _))
-- instance. We have to be careful since the map is encodedwith 'mapToCBOR' and the
-- decoder needs to be consistent with that encoding. So we use
-- fromMapXA From (fromPairAX From From)  to decode that field

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
        <*! setDecodeA From
        <*! setDecodeA From
        <*! mapDecodeA (Ann From) From
        <*! mapDecodeA (Ann From) From
        <*! mapDecodeA (Ann From) (pairDecodeA From (Ann From))

deriving via
  (Mem (TxWitnessRaw era))
  instance
    ( Era era,
      FromCBOR (Annotator (Core.Script era)),
      ToCBOR (Core.Script era)
    ) =>
    FromCBOR (Annotator (TxWitness era))

-- ============================================================
-- Pretty Printing

ppRdmrPtr :: RdmrPtr -> PDoc
ppRdmrPtr (RdmrPtr tag w) = ppSexp "RdmrPtr" [ppTag tag, ppWord64 w]

instance PrettyA RdmrPtr where prettyA = ppRdmrPtr

ppTxWitness :: (Era era, PrettyA (Core.Script era)) => TxWitness era -> PDoc
ppTxWitness (TxWitnessConstr (Memo (TxWitnessRaw vk wb sc da rd) _)) =
  ppRecord
    "TxWitness"
    [ ("txwitsVKey", ppSet ppWitVKey vk),
      ("txwitsBoot", ppSet ppBootstrapWitness wb),
      ("txscripts", ppMap ppScriptHash prettyA sc),
      ("txdats", ppMap ppSafeHash ppData da),
      ("txrdmrs", ppMap ppRdmrPtr (ppPair ppData ppExUnits) rd)
    ]

instance
  (Era era, PrettyA (Core.Script era)) =>
  PrettyA (TxWitness era)
  where
  prettyA = ppTxWitness
