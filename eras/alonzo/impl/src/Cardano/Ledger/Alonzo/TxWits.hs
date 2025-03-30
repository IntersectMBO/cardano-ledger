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
  Redeemers (Redeemers, ..),
  RedeemersRaw (..),
  unRedeemersL,
  unRedeemers,
  nullRedeemers,
  lookupRedeemer,
  upgradeRedeemers,
  TxDats (TxDats, TxDats', ..),
  TxDatsRaw (..),
  upgradeTxDats,
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
    txrdmrs',
    ..
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
  nullDats,
  alonzoEqTxWitsRaw,
  emptyTxWitness,
  addScripts,
  decodeAlonzoPlutusScript,
  asHashedScriptPair,
)
where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  AsIx (..),
  decodePlutusScript,
  fromPlutusScript,
  toPlutusSLanguage,
 )
import Cardano.Ledger.Binary (
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
  MemoBytes,
  Memoized (..),
  eqRawType,
  getMemoRawType,
  lensMemoRawType,
  mkMemoizedEra,
 )
import Cardano.Ledger.Plutus.Data (Data, hashData, upgradeData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language (
  Language (..),
  Plutus (..),
  PlutusLanguage,
  SLanguage (..),
  plutusBinary,
  plutusLanguage,
 )
import Cardano.Ledger.Shelley.TxWits (
  ShelleyTxWits (..),
  shelleyEqTxWitsRaw,
 )
import Control.DeepSeq (NFData)
import Control.Monad (when, (>=>))
import Data.Bifunctor (Bifunctor (first))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.MapExtras as Map (fromElems)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
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
newtype Redeemers era = RedeemersConstr (MemoBytes (RedeemersRaw era))
  deriving newtype (Generic, ToCBOR, SafeToHash, Typeable, DecCBOR)

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

nullRedeemers :: Redeemers era -> Bool
nullRedeemers = Map.null . unRedeemers
{-# DEPRECATED nullRedeemers "In favor of `unRedeemersL`" #-}

emptyTxWitness :: AlonzoEraScript era => AlonzoTxWitsRaw era
emptyTxWitness = AlonzoTxWitsRaw mempty mempty mempty mempty emptyRedeemers

emptyRedeemers :: AlonzoEraScript era => Redeemers era
emptyRedeemers = Redeemers mempty

lookupRedeemer ::
  Ord (PlutusPurpose AsIx era) =>
  PlutusPurpose AsIx era ->
  Redeemers era ->
  Maybe (Data era, ExUnits)
lookupRedeemer key = Map.lookup key . unRedeemers
{-# DEPRECATED lookupRedeemer "In favor of `unRedeemersL`" #-}

-- | Upgrade redeemers from one era to another. The underlying data structure
-- will remain identical, but the memoised serialisation may change to reflect
-- the versioned serialisation of the new era.
upgradeRedeemers ::
  forall era.
  (AlonzoEraScript (PreviousEra era), AlonzoEraScript era) =>
  Redeemers (PreviousEra era) ->
  Redeemers era
upgradeRedeemers =
  Redeemers
    . Map.mapKeys upgradePlutusPurposeAsIx
    . Map.map (first upgradeData)
    . unRedeemers

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
  { atwrAddrTxWits :: !(Set (WitVKey 'Witness))
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

newtype AlonzoTxWits era = TxWitnessConstr (MemoBytes (AlonzoTxWitsRaw era))
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
  deriving (Generic, Typeable, Eq)
  deriving newtype (NoThunks, NFData)

deriving instance Show (TxDatsRaw era)

instance (Typeable era, EncCBOR (Data era)) => EncCBOR (TxDatsRaw era) where
  encCBOR = encodeWithSetTag . Map.elems . unTxDatsRaw

pattern TxDats' :: Map DataHash (Data era) -> TxDats era
pattern TxDats' m <- (getMemoRawType -> TxDatsRaw m)

{-# COMPLETE TxDats' #-}

pattern TxDats :: forall era. Era era => Map DataHash (Data era) -> TxDats era
pattern TxDats m <- (getMemoRawType -> TxDatsRaw m)
  where
    TxDats m = mkMemoizedEra @era (TxDatsRaw m)

{-# COMPLETE TxDats #-}

unTxDats :: TxDats era -> Map DataHash (Data era)
unTxDats (TxDats' m) = m

-- Conceptually, this is an Iso' but vanilla microlens doesn't have Iso's
unTxDatsL :: forall era. Era era => Lens' (TxDats era) (Map DataHash (Data era))
unTxDatsL f = fmap TxDats . f . unTxDats
{-# INLINE unTxDatsL #-}

nullDats :: TxDats era -> Bool
nullDats (TxDats' d) = Map.null d
{-# DEPRECATED nullDats "In favor of `unTxDatsL`" #-}

instance Era era => DecCBOR (TxDatsRaw era) where
  decCBOR =
    ifDecoderVersionAtLeast
      (natVersion @9)
      ( allowTag setTag
          >> TxDatsRaw . Map.fromElems hashData . NE.toList <$> decodeNonEmptyList decCBOR
      )
      (TxDatsRaw . Map.fromElems hashData <$> decodeList decCBOR)
  {-# INLINE decCBOR #-}

-- | Note that 'TxDats' are based on 'MemoBytes' since we must preserve
-- the original bytes for the 'Cardano.Ledger.Alonzo.Tx.ScriptIntegrity'.
-- Since the 'TxDats' exist outside of the transaction body,
-- this is how we ensure that they are not manipulated.
newtype TxDats era = TxDatsConstr (MemoBytes (TxDatsRaw era))
  deriving newtype (SafeToHash, ToCBOR, Eq, NoThunks, NFData, DecCBOR)
  deriving (Generic)

instance Memoized (TxDats era) where
  type RawType (TxDats era) = TxDatsRaw era

deriving instance Show (TxDats era)

instance Era era => Semigroup (TxDats era) where
  (TxDats m) <> (TxDats m') = TxDats (m <> m')

instance Era era => Monoid (TxDats era) where
  mempty = TxDats mempty

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (TxDats era)

-- | Upgrade 'TxDats' from one era to another. The underlying data structure
-- will remain identical, but the memoised serialisation may change to reflect
-- the versioned serialisation of the new era.
upgradeTxDats ::
  (Era era1, Era era2) =>
  TxDats era1 ->
  TxDats era2
upgradeTxDats (TxDats datMap) = TxDats $ fmap upgradeData datMap

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

pattern AlonzoTxWits' ::
  Era era =>
  Set (WitVKey 'Witness) ->
  Set BootstrapWitness ->
  Map ScriptHash (Script era) ->
  TxDats era ->
  Redeemers era ->
  AlonzoTxWits era
pattern AlonzoTxWits' {txwitsVKey', txwitsBoot', txscripts', txdats', txrdmrs'} <-
  (getMemoRawType -> AlonzoTxWitsRaw txwitsVKey' txwitsBoot' txscripts' txdats' txrdmrs')

{-# COMPLETE AlonzoTxWits' #-}

pattern AlonzoTxWits ::
  forall era.
  AlonzoEraScript era =>
  Set (WitVKey 'Witness) ->
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
  Lens' (AlonzoTxWits era) (Set (WitVKey 'Witness))
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

instance EraScript AlonzoEra => EraTxWits AlonzoEra where
  type TxWits AlonzoEra = AlonzoTxWits AlonzoEra

  mkBasicTxWits = mempty

  addrTxWitsL = addrAlonzoTxWitsL
  {-# INLINE addrTxWitsL #-}

  bootAddrTxWitsL = bootAddrAlonzoTxWitsL
  {-# INLINE bootAddrTxWitsL #-}

  scriptTxWitsL = scriptAlonzoTxWitsL
  {-# INLINE scriptTxWitsL #-}

  upgradeTxWits (ShelleyTxWits {addrWits, scriptWits, bootWits}) =
    AlonzoTxWits addrWits bootWits (upgradeScript <$> scriptWits) mempty emptyRedeemers

class (EraTxWits era, AlonzoEraScript era) => AlonzoEraTxWits era where
  datsTxWitsL :: Lens' (TxWits era) (TxDats era)

  rdmrsTxWitsL :: Lens' (TxWits era) (Redeemers era)

instance EraScript AlonzoEra => AlonzoEraTxWits AlonzoEra where
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
    (\wits ds -> wits & datsTxWitsL .~ TxDats (Map.fromList [(hashData d, d) | d <- ds]))
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
        Encode ('Closed 'Dense) (Map.Map ScriptHash (Plutus l))
      encodePlutus slang =
        E
          (encodeWithSetTag . encCBOR . map plutusBinary . Map.elems)
          (Map.mapMaybe (toPlutusScript >=> toPlutusSLanguage slang) scripts)
      toScript ::
        forall l h. PlutusLanguage l => Map.Map h (Plutus l) -> Map.Map h (Script era)
      toScript ps =
        case traverse (fmap fromPlutusScript . mkPlutusScript) ps of
          Nothing ->
            error $
              "Impossible: Re-constructing unsupported language: "
                ++ show (plutusLanguage (Proxy @l))
          Just plutusScripts -> plutusScripts

instance AlonzoEraScript era => DecCBOR (RedeemersRaw era) where
  decCBOR =
    ifDecoderVersionAtLeast
      (natVersion @9)
      ( peekTokenType >>= \case
          TypeMapLenIndef -> decodeMapRedeemers
          TypeMapLen -> decodeMapRedeemers
          _ -> decodeListRedeemers
      )
      (RedeemersRaw . Map.fromList <$> decodeList decodeElement)
    where
      decodeMapRedeemers :: Decoder s (RedeemersRaw era)
      decodeMapRedeemers =
        RedeemersRaw . Map.fromList . NE.toList <$> do
          (_, xs) <- decodeListLikeWithCount decodeMapLenOrIndef (:) $ \_ -> do
            ptr <- decCBOR
            (annData, exUnits) <- decCBOR
            pure (ptr, (annData, exUnits))
          case NE.nonEmpty xs of
            Nothing -> fail "Expected redeemers map to be non-empty"
            Just neList -> pure $ NE.reverse neList
      decodeListRedeemers :: Decoder s (RedeemersRaw era)
      decodeListRedeemers =
        RedeemersRaw . Map.fromList . NE.toList
          <$> decodeNonEmptyList decodeElement
      decodeElement :: Decoder s (PlutusPurpose AsIx era, (Data era, ExUnits))
      decodeElement = do
        decodeRecordNamed
          "Redeemer"
          (\(rdmrPtr, _) -> fromIntegral (listLen rdmrPtr) + 2)
          $ (,) <$> decCBORGroup <*> ((,) <$> decCBOR <*> decCBOR)
      {-# INLINE decodeElement #-}
  {-# INLINE decCBOR #-}

-- | Encodes memoized bytes created upon construction.
instance AlonzoEraScript era => EncCBOR (Redeemers era)

instance
  ( AlonzoEraScript era
  , DecCBOR (NativeScript era)
  ) =>
  DecCBOR (AlonzoTxWitsRaw era)
  where
  decCBOR =
    decode $
      SparseKeyed
        "AlonzoTxWits"
        emptyTxWitness
        txWitnessField
        []
    where
      txWitnessField :: Word -> Field (AlonzoTxWitsRaw era)
      txWitnessField 0 =
        field
          (\x wits -> wits {atwrAddrTxWits = x})
          ( D $
              ifDecoderVersionAtLeast
                (natVersion @9)
                ( allowTag setTag
                    >> Set.fromList . NE.toList <$> decodeNonEmptyList decCBOR
                )
                (Set.fromList <$> decodeList decCBOR)
          )
      txWitnessField 1 = field addScripts (D nativeScriptsDecoder)
      txWitnessField 2 =
        field
          (\x wits -> wits {atwrBootAddrTxWits = x})
          ( D $
              ifDecoderVersionAtLeast
                (natVersion @9)
                ( allowTag setTag
                    >> Set.fromList . NE.toList <$> decodeNonEmptyList decCBOR
                )
                (Set.fromList <$> decodeList decCBOR)
          )
      txWitnessField 3 = field addScripts (decodeAlonzoPlutusScript SPlutusV1)
      txWitnessField 4 = field (\x wits -> wits {atwrDatsTxWits = x}) From
      txWitnessField 5 = field (\x wits -> wits {atwrRdmrsTxWits = x}) From
      txWitnessField 6 = field addScripts (decodeAlonzoPlutusScript SPlutusV2)
      txWitnessField 7 = field addScripts (decodeAlonzoPlutusScript SPlutusV3)
      txWitnessField n = invalidField n

      nativeScriptsDecoder :: Decoder s (Map ScriptHash (Script era))
      nativeScriptsDecoder =
        ifDecoderVersionAtLeast
          (natVersion @9)
          ( allowTag setTag
              >> Map.fromList . NE.toList <$> decodeNonEmptyList pairDecoder
          )
          (Map.fromList <$> decodeList pairDecoder)
        where
          pairDecoder :: Decoder s (ScriptHash, Script era)
          pairDecoder = asHashedScriptPair @era . fromNativeScript <$> decCBOR

deriving newtype instance
  ( AlonzoEraScript era
  , DecCBOR (NativeScript era)
  ) =>
  DecCBOR (AlonzoTxWits era)

addScripts ::
  Map ScriptHash (Script era) ->
  AlonzoTxWitsRaw era ->
  AlonzoTxWitsRaw era
addScripts scriptWitnesses txWits =
  txWits
    { atwrScriptTxWits = scriptWitnesses <> atwrScriptTxWits txWits
    }
{-# INLINE addScripts #-}

decodeAlonzoPlutusScript ::
  (AlonzoEraScript era, PlutusLanguage l) =>
  SLanguage l ->
  Decode ('Closed 'Dense) (Map ScriptHash (Script era))
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
