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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.TxWits (
  RdmrPtr (..),
  Redeemers (Redeemers),
  unRedeemers,
  nullRedeemers,
  TxDats (TxDats, TxDats'),
  AlonzoTxWits (
    AlonzoTxWits,
    txwitsVKey,
    txwitsBoot,
    txscripts,
    txdats,
    txrdmrs,
    AlonzoTxWits',
    txwitsVKey',
    txwitsBoot',
    txscripts',
    txdats',
    txrdmrs'
  ),
  addrAlonzoTxWitsL,
  bootAddrAlonzoTxWitsL,
  scriptAlonzoTxWitsL,
  datsAlonzoTxWitsL,
  rdmrsAlonzoTxWitsL,
  AlonzoEraTxWits (..),
  hashDataTxWitsL,
  unTxDats,
  nullDats,
)
where

import Cardano.Crypto.DSIGN.Class (SigDSIGN, VerKeyDSIGN)
import Cardano.Crypto.Hash.Class (HashAlgorithm)
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), ExUnits (..), Tag)
import Cardano.Ledger.Alonzo.Scripts.Data (Data, hashData)
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  DecCBORGroup (..),
  Decoder,
  EncCBOR (..),
  EncCBORGroup (..),
  ToCBOR (..),
  decodeList,
  encodeFoldableEncoder,
  encodeListLen,
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto (DSIGN, HASH), StandardCrypto)
import Cardano.Ledger.Keys (KeyRole (Witness))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.MemoBytes (
  Mem,
  MemoBytes,
  Memoized (..),
  getMemoRawType,
  lensMemoRawType,
  mkMemoized,
 )
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.TxBody (WitVKey)
import Cardano.Ledger.Shelley.TxWits (keyBy)
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
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

-- EncCBOR and DecCBOR for RdmrPtr is used in UTXOW for error reporting
instance DecCBOR RdmrPtr where
  decCBOR = RdmrPtr <$> decCBOR <*> decCBOR

instance EncCBOR RdmrPtr where
  encCBOR (RdmrPtr t w) = encCBOR t <> encCBOR w

instance EncCBORGroup RdmrPtr where
  listLen _ = 2
  listLenBound _ = 2
  encCBORGroup (RdmrPtr t w) = encCBOR t <> encCBOR w
  encodedGroupSizeExpr size_ _proxy =
    encodedSizeExpr size_ (Proxy :: Proxy Tag)
      + encodedSizeExpr size_ (Proxy :: Proxy Word64)

instance DecCBORGroup RdmrPtr where
  decCBORGroup = RdmrPtr <$> decCBOR <*> decCBOR

newtype RedeemersRaw era = RedeemersRaw (Map RdmrPtr (Data era, ExUnits))
  deriving (Eq, Generic, Typeable, NFData)
  deriving newtype (NoThunks)

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (RedeemersRaw era)

instance Typeable era => EncCBOR (RedeemersRaw era) where
  encCBOR (RedeemersRaw rs) = encodeFoldableEncoder keyValueEncoder $ Map.toAscList rs
    where
      keyValueEncoder (ptr, (dats, exs)) =
        encodeListLen (listLen ptr + 2)
          <> encCBORGroup ptr
          <> encCBOR dats
          <> encCBOR exs

instance Memoized Redeemers where
  type RawType Redeemers = RedeemersRaw

-- | Note that 'Redeemers' are based on 'MemoBytes' since we must preserve
-- the original bytes for the 'Cardano.Ledger.Alonzo.Tx.ScriptIntegrity'.
-- Since the 'Redeemers' exist outside of the transaction body,
-- this is how we ensure that they are not manipulated.
newtype Redeemers era = RedeemersConstr (MemoBytes RedeemersRaw era)
  deriving newtype (Eq, ToCBOR, NoThunks, SafeToHash, Typeable, NFData)

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (Redeemers era)

-- =====================================================
-- Pattern for Redeemers

pattern Redeemers ::
  forall era.
  Era era =>
  Map RdmrPtr (Data era, ExUnits) ->
  Redeemers era
pattern Redeemers rs <-
  (getMemoRawType -> RedeemersRaw rs)
  where
    Redeemers rs' = mkMemoized $ RedeemersRaw rs'

{-# COMPLETE Redeemers #-}

unRedeemers :: Era era => Redeemers era -> Map RdmrPtr (Data era, ExUnits)
unRedeemers (Redeemers rs) = rs

nullRedeemers :: Era era => Redeemers era -> Bool
nullRedeemers = Map.null . unRedeemers

-- ====================================================================
-- In the Spec, AlonzoTxWits has 4 logical fields. Here in the implementation
-- we make two physical modifications.
-- 1) The witsVKey field of AlonzoTxWits is specified as a (Map VKey Signature)
--    for efficiency this is stored as a (Set WitVKey) where WitVKey is
--    logically a triple (VKey,Signature,VKeyHash).
-- 2) We add a 5th field _witsBoot to be backwards compatible with
--    earlier Eras: Byron, Mary, Allegra
-- So logically things look like this
--   data AlonzoTxWits = AlonzoTxWits
--      (Set (WitVKey 'Witness (Crypto era)))
--      (Set (BootstrapWitness (Crypto era)))
--      (Map (ScriptHash (Crypto era)) (Script era))
--      (TxDats era)
--      (Map RdmrPtr (Data era, ExUnits))

-- | Internal 'AlonzoTxWits' type, lacking serialised bytes.
data AlonzoTxWitsRaw era = AlonzoTxWitsRaw
  { atwrAddrTxWits :: !(Set (WitVKey 'Witness (EraCrypto era)))
  , atwrBootAddrTxWits :: !(Set (BootstrapWitness (EraCrypto era)))
  , atwrScriptTxWits :: !(Map (ScriptHash (EraCrypto era)) (Script era))
  , atwrDatsTxWits :: !(TxDats era)
  , atwrRdmrsTxWits :: !(Redeemers era)
  }
  deriving (Generic)

instance
  ( Era era
  , Script era ~ AlonzoScript era
  , c ~ EraCrypto era
  , NFData (TxDats era)
  , NFData (Redeemers era)
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  ) =>
  NFData (AlonzoTxWitsRaw era)

newtype AlonzoTxWits era = TxWitnessConstr (MemoBytes AlonzoTxWitsRaw era)
  deriving newtype (SafeToHash, ToCBOR)

instance Memoized AlonzoTxWits where
  type RawType AlonzoTxWits = AlonzoTxWitsRaw

instance (Era era, Script era ~ AlonzoScript era) => Semigroup (AlonzoTxWits era) where
  (<>) x y | isEmptyTxWitness x = y
  (<>) x y | isEmptyTxWitness y = x
  (<>)
    (getMemoRawType -> AlonzoTxWitsRaw a b c d (Redeemers e))
    (getMemoRawType -> AlonzoTxWitsRaw u v w x (Redeemers y)) =
      AlonzoTxWits (a <> u) (b <> v) (c <> w) (d <> x) (Redeemers (e <> y))

instance (Era era, Script era ~ AlonzoScript era) => Monoid (AlonzoTxWits era) where
  mempty = AlonzoTxWits mempty mempty mempty mempty (Redeemers mempty)

deriving instance
  ( Era era
  , Script era ~ AlonzoScript era
  , c ~ EraCrypto era
  , NFData (TxDats era)
  , NFData (Redeemers era)
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  ) =>
  NFData (AlonzoTxWits era)

isEmptyTxWitness :: Era era => AlonzoTxWits era -> Bool
isEmptyTxWitness (getMemoRawType -> AlonzoTxWitsRaw a b c d (Redeemers e)) =
  Set.null a && Set.null b && Map.null c && nullDats d && Map.null e

-- =====================================================
newtype TxDatsRaw era = TxDatsRaw (Map (DataHash (EraCrypto era)) (Data era))
  deriving (Generic, Typeable, Eq)
  deriving newtype (NoThunks, NFData)

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (TxDatsRaw era)

instance (Typeable era, EncCBOR (Data era)) => EncCBOR (TxDatsRaw era) where
  encCBOR (TxDatsRaw m) = encCBOR $ Map.elems m

pattern TxDats' :: Map (DataHash (EraCrypto era)) (Data era) -> TxDats era
pattern TxDats' m <- (getMemoRawType -> TxDatsRaw m)

{-# COMPLETE TxDats' #-}

pattern TxDats :: Era era => Map (DataHash (EraCrypto era)) (Data era) -> TxDats era
pattern TxDats m <- (getMemoRawType -> TxDatsRaw m)
  where
    TxDats m = mkMemoized (TxDatsRaw m)

{-# COMPLETE TxDats #-}

unTxDats :: TxDats era -> Map (DataHash (EraCrypto era)) (Data era)
unTxDats (TxDats' m) = m

nullDats :: TxDats era -> Bool
nullDats (TxDats' d) = Map.null d

instance (Era era) => DecCBOR (Annotator (TxDatsRaw era)) where
  decCBOR = decode $ fmap (TxDatsRaw . keyBy hashData) <$> listDecodeA From
  {-# INLINE decCBOR #-}

-- | Note that 'TxDats' are based on 'MemoBytes' since we must preserve
-- the original bytes for the 'Cardano.Ledger.Alonzo.Tx.ScriptIntegrity'.
-- Since the 'TxDats' exist outside of the transaction body,
-- this is how we ensure that they are not manipulated.
newtype TxDats era = TxDatsConstr (MemoBytes TxDatsRaw era)
  deriving newtype (SafeToHash, ToCBOR, Eq, NoThunks, NFData)

instance Memoized TxDats where
  type RawType TxDats = TxDatsRaw

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (TxDats era)

instance Era era => Semigroup (TxDats era) where
  (TxDats m) <> (TxDats m') = TxDats (m <> m')

instance Era era => Monoid (TxDats era) where
  mempty = TxDats mempty

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (TxDats era)

deriving via
  (Mem TxDatsRaw era)
  instance
    (Era era) => DecCBOR (Annotator (TxDats era))

-- =====================================================
-- AlonzoTxWits instances

deriving stock instance
  ( Era era
  , Eq (Script era)
  ) =>
  Eq (AlonzoTxWitsRaw era)

deriving stock instance
  (Era era, Show (Script era)) =>
  Show (AlonzoTxWitsRaw era)

instance (Era era, NoThunks (Script era)) => NoThunks (AlonzoTxWitsRaw era)

deriving newtype instance
  ( Era era
  , Eq (Script era)
  ) =>
  Eq (AlonzoTxWits era)

deriving newtype instance
  (Era era, Show (Script era)) =>
  Show (AlonzoTxWits era)

deriving newtype instance
  (Era era, NoThunks (Script era)) =>
  NoThunks (AlonzoTxWits era)

-- =====================================================
-- Pattern for AlonzoTxWits

pattern AlonzoTxWits' ::
  Era era =>
  Set (WitVKey 'Witness (EraCrypto era)) ->
  Set (BootstrapWitness (EraCrypto era)) ->
  Map (ScriptHash (EraCrypto era)) (Script era) ->
  TxDats era ->
  Redeemers era ->
  AlonzoTxWits era
pattern AlonzoTxWits' {txwitsVKey', txwitsBoot', txscripts', txdats', txrdmrs'} <-
  (getMemoRawType -> AlonzoTxWitsRaw txwitsVKey' txwitsBoot' txscripts' txdats' txrdmrs')

{-# COMPLETE AlonzoTxWits' #-}

pattern AlonzoTxWits ::
  (Era era, Script era ~ AlonzoScript era) =>
  Set (WitVKey 'Witness (EraCrypto era)) ->
  Set (BootstrapWitness (EraCrypto era)) ->
  Map (ScriptHash (EraCrypto era)) (Script era) ->
  TxDats era ->
  Redeemers era ->
  AlonzoTxWits era
pattern AlonzoTxWits {txwitsVKey, txwitsBoot, txscripts, txdats, txrdmrs} <-
  (getMemoRawType -> AlonzoTxWitsRaw txwitsVKey txwitsBoot txscripts txdats txrdmrs)
  where
    AlonzoTxWits witsVKey' witsBoot' witsScript' witsDat' witsRdmr' =
      mkMemoized $ AlonzoTxWitsRaw witsVKey' witsBoot' witsScript' witsDat' witsRdmr'

{-# COMPLETE AlonzoTxWits #-}

-- =======================================================
-- Accessors
-- =======================================================

addrAlonzoTxWitsL ::
  (Era era, Script era ~ AlonzoScript era) =>
  Lens' (AlonzoTxWits era) (Set (WitVKey 'Witness (EraCrypto era)))
addrAlonzoTxWitsL =
  lensMemoRawType atwrAddrTxWits $ \witsRaw addrWits -> witsRaw {atwrAddrTxWits = addrWits}
{-# INLINEABLE addrAlonzoTxWitsL #-}

bootAddrAlonzoTxWitsL ::
  (Era era, Script era ~ AlonzoScript era) =>
  Lens' (AlonzoTxWits era) (Set (BootstrapWitness (EraCrypto era)))
bootAddrAlonzoTxWitsL =
  lensMemoRawType atwrBootAddrTxWits $
    \witsRaw bootAddrWits -> witsRaw {atwrBootAddrTxWits = bootAddrWits}
{-# INLINEABLE bootAddrAlonzoTxWitsL #-}

scriptAlonzoTxWitsL ::
  (Era era, Script era ~ AlonzoScript era) =>
  Lens' (AlonzoTxWits era) (Map (ScriptHash (EraCrypto era)) (Script era))
scriptAlonzoTxWitsL =
  lensMemoRawType atwrScriptTxWits $ \witsRaw scriptWits -> witsRaw {atwrScriptTxWits = scriptWits}
{-# INLINEABLE scriptAlonzoTxWitsL #-}

datsAlonzoTxWitsL ::
  (Era era, Script era ~ AlonzoScript era) =>
  Lens' (AlonzoTxWits era) (TxDats era)
datsAlonzoTxWitsL =
  lensMemoRawType atwrDatsTxWits $ \witsRaw datsWits -> witsRaw {atwrDatsTxWits = datsWits}
{-# INLINEABLE datsAlonzoTxWitsL #-}

rdmrsAlonzoTxWitsL ::
  (Era era, Script era ~ AlonzoScript era) =>
  Lens' (AlonzoTxWits era) (Redeemers era)
rdmrsAlonzoTxWitsL =
  lensMemoRawType atwrRdmrsTxWits $ \witsRaw rdmrsWits -> witsRaw {atwrRdmrsTxWits = rdmrsWits}
{-# INLINEABLE rdmrsAlonzoTxWitsL #-}

instance (EraScript (AlonzoEra c), Crypto c) => EraTxWits (AlonzoEra c) where
  {-# SPECIALIZE instance EraTxWits (AlonzoEra StandardCrypto) #-}

  type TxWits (AlonzoEra c) = AlonzoTxWits (AlonzoEra c)

  mkBasicTxWits = mempty

  addrTxWitsL = addrAlonzoTxWitsL
  {-# INLINE addrTxWitsL #-}

  bootAddrTxWitsL = bootAddrAlonzoTxWitsL
  {-# INLINE bootAddrTxWitsL #-}

  scriptTxWitsL = scriptAlonzoTxWitsL
  {-# INLINE scriptTxWitsL #-}

class EraTxWits era => AlonzoEraTxWits era where
  datsTxWitsL :: Lens' (TxWits era) (TxDats era)

  rdmrsTxWitsL :: Lens' (TxWits era) (Redeemers era)

instance (EraScript (AlonzoEra c), Crypto c) => AlonzoEraTxWits (AlonzoEra c) where
  {-# SPECIALIZE instance AlonzoEraTxWits (AlonzoEra StandardCrypto) #-}

  datsTxWitsL = datsAlonzoTxWitsL
  {-# INLINE datsTxWitsL #-}

  rdmrsTxWitsL = rdmrsAlonzoTxWitsL
  {-# INLINE rdmrsTxWitsL #-}

-- | This is a convenience Lens that will hash the `Data` when it is being added to the
-- `TxWits`. See `datsTxWitsL` for a version that aloows setting `TxDats` instead.
hashDataTxWitsL :: AlonzoEraTxWits era => Lens (TxWits era) (TxWits era) (TxDats era) [Data era]
hashDataTxWitsL =
  lens
    (\wits -> wits ^. datsTxWitsL)
    (\wits ds -> wits & datsTxWitsL .~ TxDats (Map.fromList [(hashData d, d) | d <- ds]))
{-# INLINEABLE hashDataTxWitsL #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (AlonzoTxWits era)

instance (Era era, Script era ~ AlonzoScript era) => EncCBOR (AlonzoTxWitsRaw era) where
  encCBOR (AlonzoTxWitsRaw vkeys boots scripts dats rdmrs) =
    encode $
      Keyed
        (\a b c d e f g -> AlonzoTxWitsRaw a b (c <> d <> e) f g)
        !> Omit null (Key 0 $ To vkeys)
        !> Omit null (Key 2 $ To boots)
        !> Omit
          null
          ( Key 1 $
              E
                (encCBOR . mapMaybe unwrapTS . Map.elems)
                (Map.filter isTimelock scripts)
          )
        !> Omit
          null
          ( Key 3 $
              E
                (encCBOR . mapMaybe unwrapPS1 . Map.elems)
                (Map.filter (isPlutus PlutusV1) scripts)
          )
        !> Omit
          null
          ( Key 6 $
              E
                (encCBOR . mapMaybe unwrapPS2 . Map.elems)
                (Map.filter (isPlutus PlutusV2) scripts)
          )
        !> Omit nullDats (Key 4 $ To dats)
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

instance Era era => DecCBOR (Annotator (RedeemersRaw era)) where
  decCBOR = do
    entries <- fmap sequence . decodeList $ decodeAnnElement
    pure $ RedeemersRaw . Map.fromList <$> entries
    where
      decodeAnnElement :: forall s. Decoder s (Annotator (RdmrPtr, (Data era, ExUnits)))
      decodeAnnElement = do
        (rdmrPtr, dat, ex) <- decodeElement
        let f x y z = (x, (y, z))
        pure $ f rdmrPtr <$> dat <*> pure ex
      {-# INLINE decodeAnnElement #-}
      decodeElement :: forall s. Decoder s (RdmrPtr, Annotator (Data era), ExUnits)
      decodeElement = do
        decodeRecordNamed
          "Redeemer"
          (\(rdmrPtr, _, _) -> fromIntegral (listLen rdmrPtr) + 2)
          $ (,,) <$> decCBORGroup <*> decCBOR <*> decCBOR
      {-# INLINE decodeElement #-}
  {-# INLINE decCBOR #-}

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (Redeemers era)

deriving via
  (Mem RedeemersRaw era)
  instance
    (Era era) => DecCBOR (Annotator (Redeemers era))

instance
  ( EraScript era
  , EncCBOR (Data era)
  , EraScript era
  , Script era ~ AlonzoScript era
  ) =>
  DecCBOR (Annotator (AlonzoTxWitsRaw era))
  where
  decCBOR =
    decode $
      SparseKeyed
        "AlonzoTxWits"
        (pure emptyTxWitness)
        txWitnessField
        []
    where
      emptyTxWitness = AlonzoTxWitsRaw mempty mempty mempty mempty emptyRedeemers
      emptyRedeemers = Redeemers mempty

      txWitnessField :: Word -> Field (Annotator (AlonzoTxWitsRaw era))
      txWitnessField 0 =
        fieldAA
          (\x wits -> wits {atwrAddrTxWits = x})
          (setDecodeA From)
      txWitnessField 2 =
        fieldAA
          (\x wits -> wits {atwrBootAddrTxWits = x})
          (setDecodeA From)
      txWitnessField 1 =
        fieldAA
          addScripts
          (listDecodeA (fmap TimelockScript <$> From))
      txWitnessField 3 =
        fieldA
          addScripts
          (fmap (PlutusScript PlutusV1) <$> From)
      txWitnessField 4 =
        fieldAA
          (\x wits -> wits {atwrDatsTxWits = x})
          From
      txWitnessField 5 = fieldAA (\x wits -> wits {atwrRdmrsTxWits = x}) From
      txWitnessField 6 =
        fieldA
          addScripts
          (fmap (PlutusScript PlutusV2) <$> From)
      txWitnessField n = field (\_ t -> t) (Invalid n)
      {-# INLINE txWitnessField #-}

      addScripts :: [AlonzoScript era] -> AlonzoTxWitsRaw era -> AlonzoTxWitsRaw era
      addScripts x wits = wits {atwrScriptTxWits = getKeys ([] :: [era]) x <> atwrScriptTxWits wits}
      {-# INLINE addScripts #-}
      getKeys ::
        forall proxy e.
        EraScript e =>
        proxy e ->
        [Script e] ->
        Map (ScriptHash (EraCrypto e)) (Script e)
      getKeys _ = keyBy (hashScript @e)
      {-# INLINE getKeys #-}
  {-# INLINE decCBOR #-}

deriving via
  (Mem AlonzoTxWitsRaw era)
  instance
    (EraScript era, Script era ~ AlonzoScript era) =>
    DecCBOR (Annotator (AlonzoTxWits era))
