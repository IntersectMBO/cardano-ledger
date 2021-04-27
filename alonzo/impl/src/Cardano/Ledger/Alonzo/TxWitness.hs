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
    Redeemers
      ( Redeemers,
        Redeemers'
      ),
    unRedeemers,
    nullRedeemers,
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

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
    serializeEncoding',
  )
import Cardano.Ledger.Alonzo.Data (Data, DataHash, hashData, ppData)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Script (..), Tag, isPlutusScript, ppExUnits, ppTag)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (Crypto), ValidateScript, hashScript)
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
import Cardano.Ledger.SafeHash (SafeToHash (..))
import qualified Data.ByteString.Short as SBS
import Data.Coders
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics
import GHC.Records
import NoThunks.Class (NoThunks)
import Shelley.Spec.Ledger.Address.Bootstrap (BootstrapWitness)
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Shelley.Spec.Ledger.Serialization (FromCBORGroup (..), ToCBORGroup (..))
import Shelley.Spec.Ledger.TxBody (WitVKey)

-- ==========================================

data RdmrPtr
  = RdmrPtr
      !Tag
      {-# UNPACK #-} !Word64
  deriving (Eq, Ord, Show, Generic)

instance NoThunks RdmrPtr

instance ToCBORGroup RdmrPtr where
  listLen _ = 2
  listLenBound _ = 2
  toCBORGroup (RdmrPtr t w) = toCBOR t <> toCBOR w
  encodedGroupSizeExpr size_ _proxy =
    encodedSizeExpr size_ (Proxy :: Proxy Tag)
      + encodedSizeExpr size_ (Proxy :: Proxy Word64)

instance FromCBORGroup RdmrPtr where
  fromCBORGroup = RdmrPtr <$> fromCBOR <*> fromCBOR

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

newtype RedeemersRaw era = RedeemersRaw (Map RdmrPtr (Data era, ExUnits))
  deriving (Eq, Show, Generic, Typeable)
  deriving newtype (NoThunks)

newtype Redeemers era = RedeemersConstr (MemoBytes (RedeemersRaw era))
  deriving newtype (Eq, Show, ToCBOR, NoThunks, Typeable)

-- =====================================================
-- Pattern for Redeemers

pattern Redeemers' ::
  Map RdmrPtr (Data era, ExUnits) ->
  Redeemers era
pattern Redeemers' rs' <-
  RedeemersConstr (Memo (RedeemersRaw rs') _)

{-# COMPLETE Redeemers' #-}

pattern Redeemers ::
  Era era =>
  Map RdmrPtr (Data era, ExUnits) ->
  Redeemers era
pattern Redeemers rs <-
  RedeemersConstr (Memo (RedeemersRaw rs) _)
  where
    Redeemers rs' =
      let enc = encodeFoldableEncoder $ \(ptr, (dats, exs)) ->
            encodeListLen 4
              <> toCBORGroup ptr
              <> toCBOR dats
              <> toCBOR exs
       in RedeemersConstr $
            Memo
              (RedeemersRaw rs')
              (SBS.toShort . serializeEncoding' . enc . Map.assocs $ rs')

{-# COMPLETE Redeemers #-}

unRedeemers :: Redeemers era -> Map RdmrPtr (Data era, ExUnits)
unRedeemers (Redeemers' rs) = rs

nullRedeemers :: Redeemers era -> Bool
nullRedeemers = Map.null . unRedeemers

-- | Internal 'TxWitness' type, lacking serialised bytes.
data TxWitnessRaw era = TxWitnessRaw
  { _txwitsVKey :: Set (WitVKey 'Witness (Crypto era)),
    _txwitsBoot :: Set (BootstrapWitness (Crypto era)),
    _txscripts :: Map (ScriptHash (Crypto era)) (Core.Script era),
    _txdats :: Map (DataHash (Crypto era)) (Data era),
    _txrdmrs :: Redeemers era
  }
  deriving (Generic, Typeable)

newtype TxWitness era = TxWitnessConstr (MemoBytes (TxWitnessRaw era))
  deriving newtype (SafeToHash, ToCBOR)

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
  Redeemers era ->
  TxWitness era
pattern TxWitness' {txwitsVKey', txwitsBoot', txscripts', txdats', txrdmrs'} <-
  TxWitnessConstr
    (Memo (TxWitnessRaw txwitsVKey' txwitsBoot' txscripts' txdats' txrdmrs') _)

{-# COMPLETE TxWitness' #-}

pattern TxWitness ::
  (Era era, Core.Script era ~ Script era) =>
  Set (WitVKey 'Witness (Crypto era)) ->
  Set (BootstrapWitness (Crypto era)) ->
  Map (ScriptHash (Crypto era)) (Core.Script era) ->
  Map (DataHash (Crypto era)) (Data era) ->
  Redeemers era ->
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

instance HasField "txrdmrs" (TxWitness era) (Redeemers era) where
  getField (TxWitnessConstr (Memo (TxWitnessRaw _ _ _ _ r) _)) = r

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

encodeWitnessRaw ::
  (Era era, Core.Script era ~ Script era, ToCBOR (Data era)) =>
  Set (WitVKey 'Witness (Crypto era)) ->
  Set (BootstrapWitness (Crypto era)) ->
  Map (ScriptHash (Crypto era)) (Core.Script era) ->
  Map (DataHash (Crypto era)) (Data era) ->
  Redeemers era ->
  Encode ('Closed 'Sparse) (TxWitnessRaw era)
encodeWitnessRaw vkeys boots scripts dats rdmrs =
  Keyed
    (\a b c d e f -> TxWitnessRaw a b (c <> d) e f)
    !> Omit null (Key 0 $ setEncode vkeys)
    !> Omit null (Key 2 $ setEncode boots)
    !> Omit
      null
      (Key 1 $ E (encodeFoldable . mapMaybe unwrapTS . Map.elems) timelocks)
    !> Omit
      null
      (Key 3 $ E (encodeFoldable . mapMaybe unwrapPS . Map.elems) plutusScripts)
    !> Omit null (Key 4 $ E (encodeFoldable . Map.elems) dats)
    !> Omit nullRedeemers (Key 5 $ To rdmrs)
  where
    unwrapTS (TimelockScript x) = Just x
    unwrapTS _ = Nothing
    unwrapPS (PlutusScript x) = Just x
    unwrapPS _ = Nothing
    (plutusScripts, timelocks) = Map.partition isPlutusScript scripts

instance
  (Era era) =>
  FromCBOR (Annotator (RedeemersRaw era))
  where
  fromCBOR = fmap RedeemersRaw <$> dec
    where
      dec :: forall s. Decoder s (Annotator (Map RdmrPtr (Data era, ExUnits)))
      dec = do
        entries <- fmap sequence . decodeList
          . decodeRecordNamed "redeemer" (const 4)
          $ do
            rdmrPtr <- fromCBORGroup
            dat <- fromCBOR
            ex <- fromCBOR
            let f x y z = (x, (y, z))
            pure $ f <$> pure rdmrPtr <*> dat <*> pure ex
        pure $ Map.fromList <$> entries

deriving via
  (Mem (RedeemersRaw era))
  instance
    (Era era) => FromCBOR (Annotator (Redeemers era))

instance
  ( Era era,
    ToCBOR (Data era),
    ToCBOR (Core.Script era),
    Typeable (Core.Script era),
    ValidateScript era,
    Core.Script era ~ Script era
  ) =>
  FromCBOR (Annotator (TxWitnessRaw era))
  where
  fromCBOR =
    decode $
      SparseKeyed
        "TxWitness"
        (pure emptyTxWitness)
        txWitnessField
        []
    where
      emptyTxWitness = TxWitnessRaw mempty mempty mempty mempty emptyRedeemers
      emptyRedeemers = Redeemers mempty

      txWitnessField :: Word -> Field (Annotator (TxWitnessRaw era))
      txWitnessField 0 =
        fieldAA
          (\x wits -> wits {_txwitsVKey = x})
          (setDecodeA From)
      txWitnessField 2 =
        fieldAA
          (\x wits -> wits {_txwitsBoot = x})
          (setDecodeA From)
      txWitnessField 1 =
        fieldAA
          addScripts
          (listDecodeA (fmap TimelockScript <$> From))
      txWitnessField 3 =
        fieldA
          addScripts
          (fmap PlutusScript <$> listDecode)
      txWitnessField 4 =
        fieldAA
          (\x wits -> wits {_txdats = x})
          (fmap (keyBy hashData) <$> listDecodeA From)
      txWitnessField 5 = fieldAA (\x wits -> wits {_txrdmrs = x}) From
      txWitnessField n = field (\_ t -> t) (Invalid n)

      addScripts :: [Script era] -> TxWitnessRaw era -> TxWitnessRaw era
      addScripts x wits = wits {_txscripts = getKeys ([] :: [era]) x <> _txscripts wits}
      getKeys ::
        forall proxy e.
        ValidateScript e =>
        proxy e ->
        [Core.Script e] ->
        Map (ScriptHash (Crypto e)) (Core.Script e)
      getKeys _ = keyBy (hashScript @e)

      keyBy :: forall a b. Ord b => (a -> b) -> [a] -> Map b a
      keyBy f xs = Map.fromList $ (\x -> (f x, x)) <$> xs

deriving via
  (Mem (TxWitnessRaw era))
  instance
    ( Era era,
      ValidateScript era,
      Core.Script era ~ Script era
    ) =>
    FromCBOR (Annotator (TxWitness era))

-- ============================================================
-- Pretty Printing

ppRdmrPtr :: RdmrPtr -> PDoc
ppRdmrPtr (RdmrPtr tag w) = ppSexp "RdmrPtr" [ppTag tag, ppWord64 w]

instance PrettyA RdmrPtr where prettyA = ppRdmrPtr

ppTxWitness :: (Era era, PrettyA (Core.Script era)) => TxWitness era -> PDoc
ppTxWitness (TxWitnessConstr (Memo (TxWitnessRaw vk wb sc da (Redeemers rd)) _)) =
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
