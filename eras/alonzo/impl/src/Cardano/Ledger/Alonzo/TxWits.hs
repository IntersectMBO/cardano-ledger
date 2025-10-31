{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.TxWits (
  Redeemers (MkRedeemers, Redeemers),
  RedeemersRaw (..),
  unRedeemersL,
  unRedeemers,
  TxDats (MkTxDats, TxDats),
  TxDatsRaw (..),
  AlonzoTxWits (
    MkAlonzoTxWits,
    AlonzoTxWits,
    txwitsVKey,
    txwitsBoot,
    txscripts,
    txdats,
    txrdmrs
  ),
  AlonzoTxWitsRaw (..),
  addrAlonzoTxWitsL,
  bootAddrAlonzoTxWitsL,
  scriptAlonzoTxWitsL,
  datsAlonzoTxWitsL,
  rdmrsAlonzoTxWitsL,
  AlonzoEraTxWits (..),
  hashDataTxWitsL,
  unTxDats,
  unTxDatsL,
  alonzoEqTxWitsRaw,
  emptyTxWitsRaw,
  addScriptsTxWitsRaw,
  decodeAlonzoPlutusScript,
  asHashedScriptPair,
) where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  AsIx (..),
  decodePlutusScript,
  fromPlutusScript,
  toPlutusSLanguage,
 )
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (..),
  DecCBORGroup (..),
  Decoder,
  EncCBOR (..),
  EncCBORGroup (..),
  Encoding,
  ToCBOR (..),
  TokenType (..),
  allowTag,
  decodeList,
  decodeListLenOrIndef,
  decodeListLikeWithCount,
  decodeMapLenOrIndef,
  decodeMapLikeEnforceNoDuplicates,
  decodeNonEmptyList,
  decodeSetLikeEnforceNoDuplicates,
  encodeFoldableEncoder,
  encodeListLen,
  encodeTag,
  ifDecoderVersionAtLeast,
  ifEncodingVersionAtLeast,
  natVersion,
  peekTokenType,
  setTag,
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (BootstrapWitness, WitVKey)
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
  Mem,
  MemoBytes,
  Memoized (..),
  eqRawType,
  getMemoRawType,
  lensMemoRawType,
  mkMemoizedEra,
 )
import Cardano.Ledger.Plutus.Data (Data, hashData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language (
  Language (..),
  Plutus (..),
  PlutusLanguage,
  SLanguage (..),
  plutusBinary,
 )
import Cardano.Ledger.Shelley.TxWits (
  mapTraverseableDecoderA,
  shelleyEqTxWitsRaw,
 )
import Control.DeepSeq (NFData)
import Control.Monad (when, (>=>))
import Control.Monad.Trans.Fail (runFail)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MapExtras (fromElems)
import qualified Data.MapExtras as Map (fromElems)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

-- ==========================================

newtype RedeemersRaw era = RedeemersRaw
  { unRedeemersRaw :: Map (PlutusPurpose AsIx era) (Data era, ExUnits)
  }
  deriving (Generic)

deriving newtype instance AlonzoEraScript era => Eq (RedeemersRaw era)

deriving newtype instance AlonzoEraScript era => NFData (RedeemersRaw era)

deriving newtype instance AlonzoEraScript era => NoThunks (RedeemersRaw era)

deriving newtype instance AlonzoEraScript era => Show (RedeemersRaw era)

instance AlonzoEraScript era => EncCBOR (RedeemersRaw era) where
  encCBOR (RedeemersRaw rs) =
    ifEncodingVersionAtLeast
      (natVersion @9)
      (encCBOR rs)
      (encodeFoldableEncoder keyValueEncoder $ Map.toAscList rs)
    where
      keyValueEncoder (ptr, (dats, exs)) =
        encodeListLen (listLen ptr + 2)
          <> encCBORGroup ptr
          <> encCBOR dats
          <> encCBOR exs

instance Memoized (Redeemers era) where
  type RawType (Redeemers era) = RedeemersRaw era

-- | Note that 'Redeemers' are based on 'MemoBytes' since we must preserve
-- the original bytes for the 'Cardano.Ledger.Alonzo.Tx.ScriptIntegrity'.
-- Since the 'Redeemers' exist outside of the transaction body,
-- this is how we ensure that they are not manipulated.
newtype Redeemers era = MkRedeemers (MemoBytes (RedeemersRaw era))
  deriving newtype (Generic, ToCBOR, SafeToHash)

deriving via
  Mem (RedeemersRaw era)
  instance
    AlonzoEraScript era => DecCBOR (Annotator (Redeemers era))

deriving newtype instance AlonzoEraScript era => Eq (Redeemers era)

deriving newtype instance AlonzoEraScript era => NFData (Redeemers era)

deriving newtype instance AlonzoEraScript era => NoThunks (Redeemers era)

deriving instance AlonzoEraScript era => Show (Redeemers era)

instance AlonzoEraScript era => Semigroup (Redeemers era) where
  Redeemers x <> Redeemers y = Redeemers $ x <> y

instance AlonzoEraScript era => Monoid (Redeemers era) where
  mempty = Redeemers mempty

-- =====================================================
-- Pattern for Redeemers

pattern Redeemers ::
  forall era.
  AlonzoEraScript era =>
  Map (PlutusPurpose AsIx era) (Data era, ExUnits) ->
  Redeemers era
pattern Redeemers rs <-
  (getMemoRawType -> RedeemersRaw rs)
  where
    Redeemers rs' = mkMemoizedEra @era $ RedeemersRaw rs'

{-# COMPLETE Redeemers #-}

unRedeemers :: Redeemers era -> Map (PlutusPurpose AsIx era) (Data era, ExUnits)
unRedeemers = unRedeemersRaw . getMemoRawType

-- Conceptually, this is an Iso' but vanilla microlens doesn't have Iso's
unRedeemersL ::
  forall era.
  AlonzoEraScript era =>
  Lens' (Redeemers era) (Map.Map (PlutusPurpose AsIx era) (Data era, ExUnits))
unRedeemersL f = fmap Redeemers . f . unRedeemers
{-# INLINE unRedeemersL #-}

emptyTxWitsRaw :: AlonzoEraScript era => AlonzoTxWitsRaw era
emptyTxWitsRaw = AlonzoTxWitsRaw mempty mempty mempty mempty emptyRedeemers

emptyRedeemers :: AlonzoEraScript era => Redeemers era
emptyRedeemers = Redeemers mempty

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
--      (Set (WitVKey Witness (Crypto era)))
--      (Set (BootstrapWitness (Crypto era)))
--      (Map (ScriptHash (Crypto era)) (Script era))
--      (TxDats era)
--      (Map RdmrPtr (Data era, ExUnits))

-- | Internal 'AlonzoTxWits' type, lacking serialised bytes.
data AlonzoTxWitsRaw era = AlonzoTxWitsRaw
  { atwrAddrTxWits :: !(Set (WitVKey Witness))
  , atwrBootAddrTxWits :: !(Set BootstrapWitness)
  , atwrScriptTxWits :: !(Map ScriptHash (Script era))
  , atwrDatsTxWits :: !(TxDats era)
  , atwrRdmrsTxWits :: !(Redeemers era)
  }
  deriving (Generic)

instance
  ( Era era
  , NFData (Script era)
  , NFData (TxDats era)
  , NFData (Redeemers era)
  ) =>
  NFData (AlonzoTxWitsRaw era)

newtype AlonzoTxWits era = MkAlonzoTxWits (MemoBytes (AlonzoTxWitsRaw era))
  deriving newtype (SafeToHash, ToCBOR)
  deriving (Generic)

instance Memoized (AlonzoTxWits era) where
  type RawType (AlonzoTxWits era) = AlonzoTxWitsRaw era

instance AlonzoEraScript era => Semigroup (AlonzoTxWits era) where
  (<>) x y | isEmptyTxWitness x = y
  (<>) x y | isEmptyTxWitness y = x
  (<>)
    (getMemoRawType -> AlonzoTxWitsRaw a b c d (Redeemers e))
    (getMemoRawType -> AlonzoTxWitsRaw u v w x (Redeemers y)) =
      AlonzoTxWits (a <> u) (b <> v) (c <> w) (d <> x) (Redeemers (e <> y))

instance AlonzoEraScript era => Monoid (AlonzoTxWits era) where
  mempty = AlonzoTxWits mempty mempty mempty mempty (Redeemers mempty)

deriving instance
  ( Era era
  , NFData (Script era)
  , NFData (TxDats era)
  , NFData (Redeemers era)
  ) =>
  NFData (AlonzoTxWits era)

isEmptyTxWitness :: AlonzoEraScript era => AlonzoTxWits era -> Bool
isEmptyTxWitness (getMemoRawType -> AlonzoTxWitsRaw a b c d (Redeemers e)) =
  Set.null a && Set.null b && Map.null c && Map.null (d ^. unTxDatsL) && Map.null e

-- =====================================================
newtype TxDatsRaw era = TxDatsRaw {unTxDatsRaw :: Map DataHash (Data era)}
  deriving (Generic, Eq)
  deriving newtype (NoThunks, NFData)

deriving instance Show (TxDatsRaw era)

instance EncCBOR (Data era) => EncCBOR (TxDatsRaw era) where
  encCBOR = encodeWithSetTag . Map.elems . unTxDatsRaw

pattern TxDats :: forall era. Era era => Map DataHash (Data era) -> TxDats era
pattern TxDats m <- (getMemoRawType -> TxDatsRaw m)
  where
    TxDats m = mkMemoizedEra @era (TxDatsRaw m)

{-# COMPLETE TxDats #-}

unTxDats :: TxDats era -> Map DataHash (Data era)
unTxDats (getMemoRawType -> TxDatsRaw m) = m

-- Conceptually, this is an Iso' but vanilla microlens doesn't have Iso's
unTxDatsL :: forall era. Era era => Lens' (TxDats era) (Map DataHash (Data era))
unTxDatsL f = fmap TxDats . f . unTxDats
{-# INLINE unTxDatsL #-}

instance Era era => DecCBOR (Annotator (TxDatsRaw era)) where
  decCBOR =
    ifDecoderVersionAtLeast
      (natVersion @9)
      ( allowTag setTag
          >> mapTraverseableDecoderA
            (decodeNonEmptyList decCBOR)
            (TxDatsRaw . Map.fromElems hashData . NE.toList)
      )
      (mapTraverseableDecoderA (decodeList decCBOR) (TxDatsRaw . Map.fromElems hashData))
  {-# INLINE decCBOR #-}

-- | Note that 'TxDats' are based on 'MemoBytes' since we must preserve
-- the original bytes for the 'Cardano.Ledger.Alonzo.Tx.ScriptIntegrity'.
-- Since the 'TxDats' exist outside of the transaction body,
-- this is how we ensure that they are not manipulated.
newtype TxDats era = MkTxDats (MemoBytes (TxDatsRaw era))
  deriving newtype (SafeToHash, ToCBOR, Eq, NoThunks, NFData)
  deriving (Generic)

instance Memoized (TxDats era) where
  type RawType (TxDats era) = TxDatsRaw era

deriving via
  Mem (TxDatsRaw era)
  instance
    Era era => DecCBOR (Annotator (TxDats era))

deriving instance Show (TxDats era)

instance Era era => Semigroup (TxDats era) where
  (TxDats m) <> (TxDats m') = TxDats (m <> m')

instance Era era => Monoid (TxDats era) where
  mempty = TxDats mempty

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (TxDats era)

-- =====================================================
-- AlonzoTxWits instances

deriving stock instance AlonzoEraScript era => Eq (AlonzoTxWitsRaw era)

deriving stock instance AlonzoEraScript era => Show (AlonzoTxWitsRaw era)

instance AlonzoEraScript era => NoThunks (AlonzoTxWitsRaw era)

deriving newtype instance AlonzoEraScript era => Eq (AlonzoTxWits era)

deriving newtype instance AlonzoEraScript era => Show (AlonzoTxWits era)

deriving newtype instance AlonzoEraScript era => NoThunks (AlonzoTxWits era)

-- =====================================================
-- Pattern for AlonzoTxWits

pattern AlonzoTxWits ::
  forall era.
  AlonzoEraScript era =>
  Set (WitVKey Witness) ->
  Set BootstrapWitness ->
  Map ScriptHash (Script era) ->
  TxDats era ->
  Redeemers era ->
  AlonzoTxWits era
pattern AlonzoTxWits {txwitsVKey, txwitsBoot, txscripts, txdats, txrdmrs} <-
  (getMemoRawType -> AlonzoTxWitsRaw txwitsVKey txwitsBoot txscripts txdats txrdmrs)
  where
    AlonzoTxWits witsVKey' witsBoot' witsScript' witsDat' witsRdmr' =
      mkMemoizedEra @era $ AlonzoTxWitsRaw witsVKey' witsBoot' witsScript' witsDat' witsRdmr'

{-# COMPLETE AlonzoTxWits #-}

-- =======================================================
-- Accessors
-- =======================================================

addrAlonzoTxWitsL ::
  forall era.
  AlonzoEraScript era =>
  Lens' (AlonzoTxWits era) (Set (WitVKey Witness))
addrAlonzoTxWitsL =
  lensMemoRawType @era atwrAddrTxWits $
    \witsRaw addrWits -> witsRaw {atwrAddrTxWits = addrWits}
{-# INLINEABLE addrAlonzoTxWitsL #-}

bootAddrAlonzoTxWitsL ::
  forall era.
  AlonzoEraScript era =>
  Lens' (AlonzoTxWits era) (Set BootstrapWitness)
bootAddrAlonzoTxWitsL =
  lensMemoRawType @era atwrBootAddrTxWits $
    \witsRaw bootAddrWits -> witsRaw {atwrBootAddrTxWits = bootAddrWits}
{-# INLINEABLE bootAddrAlonzoTxWitsL #-}

scriptAlonzoTxWitsL ::
  forall era.
  AlonzoEraScript era =>
  Lens' (AlonzoTxWits era) (Map ScriptHash (Script era))
scriptAlonzoTxWitsL =
  lensMemoRawType @era atwrScriptTxWits $
    \witsRaw scriptWits -> witsRaw {atwrScriptTxWits = scriptWits}
{-# INLINEABLE scriptAlonzoTxWitsL #-}

datsAlonzoTxWitsL ::
  forall era.
  AlonzoEraScript era =>
  Lens' (AlonzoTxWits era) (TxDats era)
datsAlonzoTxWitsL =
  lensMemoRawType @era atwrDatsTxWits $
    \witsRaw datsWits -> witsRaw {atwrDatsTxWits = datsWits}
{-# INLINEABLE datsAlonzoTxWitsL #-}

rdmrsAlonzoTxWitsL ::
  forall era.
  AlonzoEraScript era =>
  Lens' (AlonzoTxWits era) (Redeemers era)
rdmrsAlonzoTxWitsL =
  lensMemoRawType @era atwrRdmrsTxWits $
    \witsRaw rdmrsWits -> witsRaw {atwrRdmrsTxWits = rdmrsWits}
{-# INLINEABLE rdmrsAlonzoTxWitsL #-}

instance EraTxWits AlonzoEra where
  type TxWits AlonzoEra = AlonzoTxWits AlonzoEra

  mkBasicTxWits = mempty

  addrTxWitsL = addrAlonzoTxWitsL
  {-# INLINE addrTxWitsL #-}

  bootAddrTxWitsL = bootAddrAlonzoTxWitsL
  {-# INLINE bootAddrTxWitsL #-}

  scriptTxWitsL = scriptAlonzoTxWitsL
  {-# INLINE scriptTxWitsL #-}

class (EraTxWits era, AlonzoEraScript era) => AlonzoEraTxWits era where
  datsTxWitsL :: Lens' (TxWits era) (TxDats era)

  rdmrsTxWitsL :: Lens' (TxWits era) (Redeemers era)

instance AlonzoEraTxWits AlonzoEra where
  datsTxWitsL = datsAlonzoTxWitsL
  {-# INLINE datsTxWitsL #-}

  rdmrsTxWitsL = rdmrsAlonzoTxWitsL
  {-# INLINE rdmrsTxWitsL #-}

instance (TxWits era ~ AlonzoTxWits era, AlonzoEraTxWits era) => EqRaw (AlonzoTxWits era) where
  eqRaw = alonzoEqTxWitsRaw

-- | This is a convenience Lens that will hash the `Data` when it is being added to the
-- `TxWits`. See `datsTxWitsL` for a version that aloows setting `TxDats` instead.
hashDataTxWitsL :: AlonzoEraTxWits era => Lens (TxWits era) (TxWits era) (TxDats era) [Data era]
hashDataTxWitsL =
  lens
    (\wits -> wits ^. datsTxWitsL)
    (\wits ds -> wits & datsTxWitsL .~ TxDats (fromElems hashData ds))
{-# INLINEABLE hashDataTxWitsL #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (AlonzoTxWits era)

instance AlonzoEraScript era => EncCBOR (AlonzoTxWitsRaw era) where
  encCBOR (AlonzoTxWitsRaw vkeys boots scripts dats rdmrs) =
    encode $
      Keyed
        ( \a b c d e f g h ->
            let ps = toScript @'PlutusV1 d <> toScript @'PlutusV2 e <> toScript @'PlutusV3 f
             in AlonzoTxWitsRaw a b (c <> ps) g h
        )
        !> Omit null (Key 0 $ To vkeys)
        !> Omit null (Key 2 $ To boots)
        !> Omit
          null
          ( Key 1 $
              E
                (encodeWithSetTag . mapMaybe getNativeScript . Map.elems)
                (Map.filter isNativeScript scripts)
          )
        !> Omit null (Key 3 $ encodePlutus SPlutusV1)
        !> Omit null (Key 6 $ encodePlutus SPlutusV2)
        !> Omit null (Key 7 $ encodePlutus SPlutusV3)
        !> Omit (null . unTxDats) (Key 4 $ To dats)
        !> Omit (null . unRedeemers) (Key 5 $ To rdmrs)
    where
      encodePlutus ::
        PlutusLanguage l =>
        SLanguage l ->
        Encode (Closed Dense) (Map.Map ScriptHash (Plutus l))
      encodePlutus slang =
        E
          (encodeWithSetTag . encCBOR . map plutusBinary . Map.elems)
          (Map.mapMaybe (toPlutusScript >=> toPlutusSLanguage slang) scripts)
      toScript ::
        forall l h. PlutusLanguage l => Map.Map h (Plutus l) -> Map.Map h (Script era)
      toScript ps =
        case runFail $ traverse (fmap fromPlutusScript . mkPlutusScript) ps of
          Left e -> error $ "Impossible: Re-constructing unsupported language: " <> e
          Right plutusScripts -> plutusScripts

instance AlonzoEraScript era => DecCBOR (Annotator (RedeemersRaw era)) where
  decCBOR = do
    ifDecoderVersionAtLeast
      (natVersion @9)
      ( peekTokenType >>= \case
          TypeMapLenIndef -> decodeMapRedeemers
          TypeMapLen -> decodeMapRedeemers
          _ ->
            ifDecoderVersionAtLeast
              (natVersion @12)
              (fail "List encoding of redeemers not supported starting with PV 12")
              decodeListRedeemers
      )
      ( mapTraverseableDecoderA
          (decodeList decodeAnnElement)
          (RedeemersRaw . Map.fromList)
      )
    where
      decodeRedeemersWith nonEmptyDecoder =
        mapTraverseableDecoderA
          nonEmptyDecoder
          (RedeemersRaw . Map.fromList . NE.toList)
      decodeMapRedeemers = decodeRedeemersWith $ do
        (_, xs) <- decodeListLikeWithCount decodeMapLenOrIndef (:) $ \_ -> do
          ptr <- decCBOR
          (annData, exUnits) <- decCBOR
          pure $ (\d -> (ptr, (d, exUnits))) <$> annData
        case NE.nonEmpty xs of
          Nothing -> fail "Expected redeemers map to be non-empty"
          Just neList -> pure $ NE.reverse neList
      decodeListRedeemers =
        decodeRedeemersWith (decodeNonEmptyList decodeAnnElement)
      decodeAnnElement ::
        forall s. Decoder s (Annotator (PlutusPurpose AsIx era, (Data era, ExUnits)))
      decodeAnnElement = do
        (rdmrPtr, dat, ex) <- decodeElement
        let f x y z = (x, (y, z))
        pure $ f rdmrPtr <$> dat <*> pure ex
      {-# INLINE decodeAnnElement #-}
      decodeElement ::
        forall s. Decoder s (PlutusPurpose AsIx era, Annotator (Data era), ExUnits)
      decodeElement = do
        decodeRecordNamed "Redeemer" (\(rdmrPtr, _, _) -> fromIntegral (listLen rdmrPtr) + 2) $ do
          !redeemerPtr <- decCBORGroup
          !redeemerData <- decCBOR
          !redeemerExUnits <- decCBOR
          pure (redeemerPtr, redeemerData, redeemerExUnits)
      {-# INLINE decodeElement #-}
  {-# INLINE decCBOR #-}

-- | Encodes memoized bytes created upon construction.
instance AlonzoEraScript era => EncCBOR (Redeemers era)

instance
  (AlonzoEraScript era, DecCBOR (Annotator (NativeScript era))) =>
  DecCBOR (Annotator (AlonzoTxWitsRaw era))
  where
  decCBOR =
    decode $
      SparseKeyed
        "AlonzoTxWits"
        (pure emptyTxWitsRaw)
        txWitnessField
        []
    where
      setDecoder :: (Ord a, DecCBOR a) => Decoder s (Annotator (Set a))
      setDecoder =
        pure
          <$> ifDecoderVersionAtLeast
            (natVersion @12)
            (decodeSetLikeEnforceNoDuplicates Set.insert (\s -> (length s, s)) decCBOR)
            (allowTag setTag >> Set.fromList . NE.toList <$> decodeNonEmptyList decCBOR)
      {-# INLINE setDecoder #-}

      txWitnessField :: Word -> Field (Annotator (AlonzoTxWitsRaw era))
      txWitnessField 0 =
        fieldAA
          (\x wits -> wits {atwrAddrTxWits = x})
          ( D $
              ifDecoderVersionAtLeast
                (natVersion @9)
                setDecoder
                (mapTraverseableDecoderA (decodeList decCBOR) Set.fromList)
          )
      txWitnessField 1 =
        fieldAA addScriptsTxWitsRaw (D scriptsDecoder)
      txWitnessField 2 =
        fieldAA
          (\x wits -> wits {atwrBootAddrTxWits = x})
          ( D $
              ifDecoderVersionAtLeast
                (natVersion @9)
                setDecoder
                (mapTraverseableDecoderA (decodeList decCBOR) Set.fromList)
          )
      txWitnessField 3 = fieldA addScriptsTxWitsRaw (decodeAlonzoPlutusScript SPlutusV1)
      txWitnessField 4 =
        fieldAA
          (\x wits -> wits {atwrDatsTxWits = x})
          ( D $
              ifDecoderVersionAtLeast
                (natVersion @12)
                noDuplicatesDatsDecoder
                decCBOR
          )
      txWitnessField 5 = fieldAA (\x wits -> wits {atwrRdmrsTxWits = x}) From
      txWitnessField 6 = fieldA addScriptsTxWitsRaw (decodeAlonzoPlutusScript SPlutusV2)
      txWitnessField 7 = fieldA addScriptsTxWitsRaw (decodeAlonzoPlutusScript SPlutusV3)
      txWitnessField n = invalidField n
      {-# INLINE txWitnessField #-}

      pairDecoder :: Decoder s (Annotator (ScriptHash, Script era))
      pairDecoder = fmap (asHashedScriptPair @era . fromNativeScript) <$> decCBOR
      {-# INLINE pairDecoder #-}

      noDuplicatesDatsDecoder :: Decoder s (Annotator (TxDats era))
      noDuplicatesDatsDecoder = do
        allowTag setTag
        dats <- decodeList decCBOR
        pure $ TxDats <$> go Map.empty dats
        where
          go m [] = pure m
          go m (x:xs) = do
            x' <- x
            let dh = hashData x'
            if dh `Map.member` m
              then fail $ "Duplicate dats found: " <> show dh
              else go (Map.insert dh x' m) xs

      noDuplicatesScriptsDecoder :: Decoder s (Annotator (Map ScriptHash (Script era)))
      noDuplicatesScriptsDecoder = do
        allowTag setTag
        scripts <- decodeList $ fmap (fromNativeScript @era) <$> decCBOR
        pure $ go Map.empty scripts
        where
          go m [] = pure m
          go m (x : xs) = do
            x' <- x
            let sh = hashScript x'
            if sh `Map.member` m
              then fail $ "Duplicate scripts found: " <> show sh
              else go (Map.insert sh x' m) xs

      scriptsDecoder :: Decoder s (Annotator (Map ScriptHash (Script era)))
      scriptsDecoder =
        ifDecoderVersionAtLeast
          (natVersion @12)
          noDuplicatesScriptsDecoder
          (allowTag setTag >> mapTraverseableDecoderA (decodeList pairDecoder) Map.fromList)
      {-# INLINE scriptsDecoder #-}
  {-# INLINE decCBOR #-}

deriving via
  Mem (AlonzoTxWitsRaw era)
  instance
    ( AlonzoEraScript era
    , DecCBOR (Annotator (NativeScript era))
    ) =>
    DecCBOR (Annotator (AlonzoTxWits era))

addScriptsTxWitsRaw ::
  Map ScriptHash (Script era) ->
  AlonzoTxWitsRaw era ->
  AlonzoTxWitsRaw era
addScriptsTxWitsRaw scriptWitnesses txWits =
  txWits
    { atwrScriptTxWits = scriptWitnesses <> atwrScriptTxWits txWits
    }
{-# INLINE addScriptsTxWitsRaw #-}

decodeAlonzoPlutusScript ::
  (AlonzoEraScript era, PlutusLanguage l) =>
  SLanguage l ->
  Decode (Closed Dense) (Map ScriptHash (Script era))
decodeAlonzoPlutusScript slang =
  D $
    ifDecoderVersionAtLeast
      (natVersion @9)
      (scriptDecoderV9 (fromPlutusScript <$> decodePlutusScript slang))
      (scriptDecoder (fromPlutusScript <$> decodePlutusScript slang))
{-# INLINE decodeAlonzoPlutusScript #-}

scriptDecoderV9 ::
  EraScript era =>
  Decoder s (Script era) ->
  Decoder s (Map ScriptHash (Script era))
scriptDecoderV9 decodeScript = do
  allowTag setTag
  scriptMap <- decodeMapLikeEnforceNoDuplicates decodeListLenOrIndef $ do
    asHashedScriptPair <$> decodeScript
  when (Map.null scriptMap) $ fail "Empty list of scripts is not allowed"
  pure scriptMap
{-# INLINE scriptDecoderV9 #-}

scriptDecoder ::
  EraScript era =>
  Decoder s (Script era) ->
  Decoder s (Map ScriptHash (Script era))
scriptDecoder decodeScript =
  fmap Map.fromList $
    decodeList $
      asHashedScriptPair <$> decodeScript
{-# INLINE scriptDecoder #-}

asHashedScriptPair :: forall era. EraScript era => Script era -> (ScriptHash, Script era)
asHashedScriptPair script =
  let !scriptHash = hashScript @era script
   in (scriptHash, script)
{-# INLINE asHashedScriptPair #-}

alonzoEqTxWitsRaw :: AlonzoEraTxWits era => TxWits era -> TxWits era -> Bool
alonzoEqTxWitsRaw txWits1 txWits2 =
  shelleyEqTxWitsRaw txWits1 txWits2
    && eqRawType (txWits1 ^. datsTxWitsL) (txWits2 ^. datsTxWitsL)
    && eqRawType (txWits1 ^. rdmrsTxWitsL) (txWits2 ^. rdmrsTxWitsL)

encodeWithSetTag :: EncCBOR a => a -> Encoding
encodeWithSetTag xs =
  ifEncodingVersionAtLeast
    (natVersion @9)
    (encodeTag setTag <> encCBOR xs)
    (encCBOR xs)
