{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.TxWitness
  ( RdmrPtr (..),
    Redeemers
      ( Redeemers,
        Redeemers'
      ),
    unRedeemers,
    nullRedeemers,
    TxDats (TxDats, TxDats'),
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
    addrAlonzoWitsL,
    bootAddrAlonzoWitsL,
    scriptAlonzoWitsL,
    datsAlonzoWitsL,
    rdmrsAlonzoWitsL,
    AlonzoEraWitnesses (..),
    unTxDats,
    nullDats,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
    serializeEncoding',
  )
import Cardano.Crypto.DSIGN.Class (SigDSIGN, VerKeyDSIGN)
import Cardano.Crypto.Hash.Class (HashAlgorithm)
import Cardano.Ledger.Alonzo.Data (Data, hashData)
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), ExUnits (..), Tag)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Serialization (FromCBORGroup (..), ToCBORGroup (..))
import Cardano.Ledger.Shelley.TxBody (WitVKey)
import Control.DeepSeq
import qualified Data.ByteString.Short as SBS
import Data.Coders
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks)

-- ==========================================

data RdmrPtr
  = RdmrPtr
      !Tag
      {-# UNPACK #-} !Word64
  deriving (Eq, Ord, Show, Generic)

instance NoThunks RdmrPtr

instance NFData RdmrPtr

-- ToCBOR and FromCBOR for RdmrPtr is used in UTXOW for error reporting
instance FromCBOR RdmrPtr where
  fromCBOR = RdmrPtr <$> fromCBOR <*> fromCBOR

instance ToCBOR RdmrPtr where
  toCBOR (RdmrPtr t w) = toCBOR t <> toCBOR w

instance ToCBORGroup RdmrPtr where
  listLen _ = 2
  listLenBound _ = 2
  toCBORGroup (RdmrPtr t w) = toCBOR t <> toCBOR w
  encodedGroupSizeExpr size_ _proxy =
    encodedSizeExpr size_ (Proxy :: Proxy Tag)
      + encodedSizeExpr size_ (Proxy :: Proxy Word64)

instance FromCBORGroup RdmrPtr where
  fromCBORGroup = RdmrPtr <$> fromCBOR <*> fromCBOR

newtype RedeemersRaw era = RedeemersRaw (Map RdmrPtr (Data era, ExUnits))
  deriving (Eq, Generic, Typeable, NFData)
  deriving newtype (NoThunks)

deriving instance HashAlgorithm (HASH (Crypto era)) => Show (RedeemersRaw era)

newtype Redeemers era = RedeemersConstr (MemoBytes RedeemersRaw era)
  deriving newtype (Eq, ToCBOR, NoThunks, SafeToHash, Typeable, NFData)

deriving instance HashAlgorithm (HASH (Crypto era)) => Show (Redeemers era)

-- =====================================================
-- Pattern for Redeemers

pattern Redeemers' ::
  Era era =>
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

unRedeemers :: Era era => Redeemers era -> Map RdmrPtr (Data era, ExUnits)
unRedeemers (Redeemers' rs) = rs

nullRedeemers :: Era era => Redeemers era -> Bool
nullRedeemers = Map.null . unRedeemers

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
--      (TxDats era)
--      (Map RdmrPtr (Data era, ExUnits))

-- | Internal 'TxWitness' type, lacking serialised bytes.
data TxWitnessRaw era = TxWitnessRaw
  { _txwitsVKey :: Set (WitVKey 'Witness (Crypto era)),
    _txwitsBoot :: Set (BootstrapWitness (Crypto era)),
    _txscripts :: Map (ScriptHash (Crypto era)) (Core.Script era),
    _txdats :: TxDats era,
    _txrdmrs :: Redeemers era
  }
  deriving (Generic, Typeable)

instance
  ( Era era,
    Core.Script era ~ AlonzoScript era,
    crypto ~ Crypto era,
    NFData (TxDats era),
    NFData (Redeemers era),
    NFData (SigDSIGN (CC.DSIGN crypto)),
    NFData (VerKeyDSIGN (CC.DSIGN crypto))
  ) =>
  NFData (TxWitnessRaw era)

newtype TxWitness era = TxWitnessConstr (MemoBytes TxWitnessRaw era)
  deriving newtype (SafeToHash, ToCBOR)

instance (Era era, Core.Script era ~ AlonzoScript era) => Semigroup (TxWitness era) where
  (<>) x y | isEmptyTxWitness x = y
  (<>) x y | isEmptyTxWitness y = x
  (<>)
    (TxWitnessConstr (Memo (TxWitnessRaw a b c d (Redeemers' e)) _))
    (TxWitnessConstr (Memo (TxWitnessRaw u v w x (Redeemers' y)) _)) =
      TxWitness (a <> u) (b <> v) (c <> w) (d <> x) (Redeemers (e <> y))

instance (Era era, Core.Script era ~ AlonzoScript era) => Monoid (TxWitness era) where
  mempty = TxWitness mempty mempty mempty mempty (Redeemers mempty)

deriving instance
  ( Era era,
    Core.Script era ~ AlonzoScript era,
    crypto ~ Crypto era,
    NFData (TxDats era),
    NFData (Redeemers era),
    NFData (SigDSIGN (CC.DSIGN crypto)),
    NFData (VerKeyDSIGN (CC.DSIGN crypto))
  ) =>
  NFData (TxWitness era)

isEmptyTxWitness :: Era era => TxWitness era -> Bool
isEmptyTxWitness (TxWitnessConstr (Memo (TxWitnessRaw a b c d (Redeemers' e)) _)) =
  Set.null a && Set.null b && Map.null c && nullDats d && Map.null e

-- =====================================================
newtype TxDatsRaw era = TxDatsRaw (Map (DataHash (Crypto era)) (Data era))
  deriving (Generic, Typeable, Eq)
  deriving newtype (NoThunks, NFData)

deriving instance HashAlgorithm (HASH (Crypto era)) => Show (TxDatsRaw era)

encodeTxDatsRaw ::
  ToCBOR (Data era) =>
  TxDatsRaw era ->
  Encode ('Closed 'Dense) (TxDatsRaw era)
encodeTxDatsRaw = E (encodeFoldable . Map.elems . unTxDatsRaw)
  where
    unTxDatsRaw (TxDatsRaw m) = m

pattern TxDats' :: Era era => Map (DataHash (Crypto era)) (Data era) -> TxDats era
pattern TxDats' m <- TxDatsConstr (Memo (TxDatsRaw m) _)

{-# COMPLETE TxDats' #-}

pattern TxDats :: Era era => Typeable era => Map (DataHash (Crypto era)) (Data era) -> TxDats era
pattern TxDats m <-
  TxDatsConstr (Memo (TxDatsRaw m) _)
  where
    TxDats m = TxDatsConstr $ memoBytes (encodeTxDatsRaw (TxDatsRaw m))

{-# COMPLETE TxDats #-}

unTxDats :: Era era => TxDats era -> Map (DataHash (Crypto era)) (Data era)
unTxDats (TxDats' m) = m

nullDats :: Era era => TxDats era -> Bool
nullDats (TxDats' d) = Map.null d

instance (Typeable era, Era era) => FromCBOR (Annotator (TxDatsRaw era)) where
  fromCBOR = decode $ fmap (TxDatsRaw . keyBy hashData) <$> listDecodeA From

newtype TxDats era = TxDatsConstr (MemoBytes TxDatsRaw era)
  deriving newtype (SafeToHash, ToCBOR, Eq, NoThunks, NFData)

deriving instance HashAlgorithm (HASH (Crypto era)) => Show (TxDats era)

instance Era era => Semigroup (TxDats era) where
  (TxDats m) <> (TxDats m') = TxDats (m <> m')

instance Era era => Monoid (TxDats era) where
  mempty = TxDats mempty

deriving via
  (Mem TxDatsRaw era)
  instance
    (Era era) => FromCBOR (Annotator (TxDats era))

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

deriving newtype instance Eq (TxWitness era)

deriving newtype instance
  (Era era, Show (Core.Script era)) =>
  Show (TxWitness era)

deriving newtype instance
  (Era era, NoThunks (Core.Script era)) =>
  NoThunks (TxWitness era)

-- =====================================================
-- Pattern for TxWitness

pattern TxWitness' ::
  Era era =>
  Set (WitVKey 'Witness (Crypto era)) ->
  Set (BootstrapWitness (Crypto era)) ->
  Map (ScriptHash (Crypto era)) (Core.Script era) ->
  TxDats era ->
  Redeemers era ->
  TxWitness era
pattern TxWitness' {txwitsVKey', txwitsBoot', txscripts', txdats', txrdmrs'} <-
  TxWitnessConstr
    (Memo (TxWitnessRaw txwitsVKey' txwitsBoot' txscripts' txdats' txrdmrs') _)

{-# COMPLETE TxWitness' #-}

pattern TxWitness ::
  (Era era, Core.Script era ~ AlonzoScript era) =>
  Set (WitVKey 'Witness (Crypto era)) ->
  Set (BootstrapWitness (Crypto era)) ->
  Map (ScriptHash (Crypto era)) (Core.Script era) ->
  TxDats era ->
  Redeemers era ->
  TxWitness era
pattern TxWitness {txwitsVKey, txwitsBoot, txscripts, txdats, txrdmrs} <-
  TxWitnessConstr
    (Memo (TxWitnessRaw txwitsVKey txwitsBoot txscripts txdats txrdmrs) _)
  where
    TxWitness witsVKey' witsBoot' witsScript' witsDat' witsRdmr' =
      mkAlonzoTxWitness $ TxWitnessRaw witsVKey' witsBoot' witsScript' witsDat' witsRdmr'

{-# COMPLETE TxWitness #-}

mkAlonzoTxWitness ::
  (Era era, Core.Script era ~ AlonzoScript era) =>
  TxWitnessRaw era ->
  TxWitness era
mkAlonzoTxWitness = TxWitnessConstr . memoBytes . encodeWitnessRaw
{-# INLINEABLE mkAlonzoTxWitness #-}

-- =======================================================
-- Accessors
-- =======================================================

lensWitsRaw ::
  (Era e, Era era, Core.Script era ~ AlonzoScript era) =>
  (TxWitnessRaw e -> a) ->
  (TxWitnessRaw e -> t -> TxWitnessRaw era) ->
  Lens (TxWitness e) (TxWitness era) a t
lensWitsRaw getter setter =
  lens
    (\(TxWitnessConstr (Memo witsRaw _)) -> getter witsRaw)
    (\(TxWitnessConstr (Memo witsRaw _)) val -> mkAlonzoTxWitness $ setter witsRaw val)
{-# INLINEABLE lensWitsRaw #-}

addrAlonzoWitsL ::
  (Era era, Core.Script era ~ AlonzoScript era) =>
  Lens' (TxWitness era) (Set (WitVKey 'Witness (Crypto era)))
addrAlonzoWitsL =
  lensWitsRaw _txwitsVKey (\witsRaw addrWits -> witsRaw {_txwitsVKey = addrWits})
{-# INLINEABLE addrAlonzoWitsL #-}

bootAddrAlonzoWitsL ::
  (Era era, Core.Script era ~ AlonzoScript era) =>
  Lens' (TxWitness era) (Set (BootstrapWitness (Crypto era)))
bootAddrAlonzoWitsL =
  lensWitsRaw _txwitsBoot (\witsRaw bootAddrWits -> witsRaw {_txwitsBoot = bootAddrWits})
{-# INLINEABLE bootAddrAlonzoWitsL #-}

scriptAlonzoWitsL ::
  (Era era, Core.Script era ~ AlonzoScript era) =>
  Lens' (TxWitness era) (Map (ScriptHash (Crypto era)) (Script era))
scriptAlonzoWitsL =
  lensWitsRaw _txscripts (\witsRaw scriptWits -> witsRaw {_txscripts = scriptWits})
{-# INLINEABLE scriptAlonzoWitsL #-}

datsAlonzoWitsL ::
  (Era era, Core.Script era ~ AlonzoScript era) =>
  Lens' (TxWitness era) (TxDats era)
datsAlonzoWitsL =
  lensWitsRaw _txdats (\witsRaw datsWits -> witsRaw {_txdats = datsWits})
{-# INLINEABLE datsAlonzoWitsL #-}

rdmrsAlonzoWitsL ::
  (Era era, Core.Script era ~ AlonzoScript era) =>
  Lens' (TxWitness era) (Redeemers era)
rdmrsAlonzoWitsL =
  lensWitsRaw _txrdmrs (\witsRaw rdmrsWits -> witsRaw {_txrdmrs = rdmrsWits})
{-# INLINEABLE rdmrsAlonzoWitsL #-}

instance (EraScript (AlonzoEra c), CC.Crypto c) => EraWitnesses (AlonzoEra c) where
  {-# SPECIALIZE instance EraWitnesses (AlonzoEra CC.StandardCrypto) #-}

  type Witnesses (AlonzoEra c) = TxWitness (AlonzoEra c)

  mkBasicWitnesses = mempty

  addrWitsL = addrAlonzoWitsL
  {-# INLINE addrWitsL #-}

  bootAddrWitsL = bootAddrAlonzoWitsL
  {-# INLINE bootAddrWitsL #-}

  scriptWitsL = scriptAlonzoWitsL
  {-# INLINE scriptWitsL #-}

class EraWitnesses era => AlonzoEraWitnesses era where
  datsWitsL :: Lens' (Witnesses era) (TxDats era)

  rdmrsWitsL :: Lens' (Witnesses era) (Redeemers era)

instance (EraScript (AlonzoEra c), CC.Crypto c) => AlonzoEraWitnesses (AlonzoEra c) where
  {-# SPECIALIZE instance AlonzoEraWitnesses (AlonzoEra CC.StandardCrypto) #-}

  datsWitsL = datsAlonzoWitsL
  {-# INLINE datsWitsL #-}

  rdmrsWitsL = rdmrsAlonzoWitsL
  {-# INLINE rdmrsWitsL #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

encodeWitnessRaw ::
  (Era era, Core.Script era ~ AlonzoScript era) =>
  TxWitnessRaw era ->
  Encode ('Closed 'Sparse) (TxWitnessRaw era)
encodeWitnessRaw (TxWitnessRaw vkeys boots scripts dats rdmrs) =
  Keyed
    (\a b c d e f g -> TxWitnessRaw a b (c <> d <> e) f g)
    !> Omit null (Key 0 $ setEncode vkeys)
    !> Omit null (Key 2 $ setEncode boots)
    !> Omit
      null
      ( Key 1 $
          E
            (encodeFoldable . mapMaybe unwrapTS . Map.elems)
            (Map.filter isTimelock scripts)
      )
    !> Omit
      null
      ( Key 3 $
          E
            (encodeFoldable . mapMaybe unwrapPS1 . Map.elems)
            (Map.filter (isPlutus PlutusV1) scripts)
      )
    !> Omit
      null
      ( Key 6 $
          E
            (encodeFoldable . mapMaybe unwrapPS2 . Map.elems)
            (Map.filter (isPlutus PlutusV2) scripts)
      )
    !> Omit nullDats (Key 4 $ E toCBOR dats)
    !> Omit nullRedeemers (Key 5 $ To rdmrs)
  where
    unwrapTS (TimelockScript x) = Just x
    unwrapTS _ = Nothing
    unwrapPS1 (PlutusScript PlutusV1 x) = Just x
    unwrapPS1 _ = Nothing
    unwrapPS2 (PlutusScript PlutusV2 x) = Just x
    unwrapPS2 _ = Nothing

    isTimelock (TimelockScript _) = True
    isTimelock (PlutusScript _ _) = False

    isPlutus _ (TimelockScript _) = False
    isPlutus lang (PlutusScript l _) = lang == l

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
            pure $ f rdmrPtr <$> dat <*> pure ex
        pure $ Map.fromList <$> entries

deriving via
  (Mem RedeemersRaw era)
  instance
    (Era era) => FromCBOR (Annotator (Redeemers era))

instance
  ( EraScript era,
    ToCBOR (Data era),
    EraScript era,
    Core.Script era ~ AlonzoScript era
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
          (fmap (PlutusScript PlutusV1) <$> listDecode)
      txWitnessField 4 =
        fieldAA
          (\x wits -> wits {_txdats = x})
          From
      txWitnessField 5 = fieldAA (\x wits -> wits {_txrdmrs = x}) From
      txWitnessField 6 =
        fieldA
          addScripts
          (fmap (PlutusScript PlutusV2) <$> listDecode)
      txWitnessField n = field (\_ t -> t) (Invalid n)

      addScripts :: [AlonzoScript era] -> TxWitnessRaw era -> TxWitnessRaw era
      addScripts x wits = wits {_txscripts = getKeys ([] :: [era]) x <> _txscripts wits}
      getKeys ::
        forall proxy e.
        EraScript e =>
        proxy e ->
        [Core.Script e] ->
        Map (ScriptHash (Crypto e)) (Core.Script e)
      getKeys _ = keyBy (hashScript @e)

keyBy :: forall a b. Ord b => (a -> b) -> [a] -> Map b a
keyBy f xs = Map.fromList $ (\x -> (f x, x)) <$> xs

deriving via
  (Mem TxWitnessRaw era)
  instance
    (EraScript era, Core.Script era ~ AlonzoScript era) =>
    FromCBOR (Annotator (TxWitness era))
