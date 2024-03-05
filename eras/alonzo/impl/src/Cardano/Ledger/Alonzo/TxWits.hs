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
  Redeemers (Redeemers),
  RedeemersRaw,
  unRedeemers,
  nullRedeemers,
  lookupRedeemer,
  upgradeRedeemers,
  TxDats (TxDats, TxDats'),
  TxDatsRaw,
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
    txrdmrs'
  ),
  AlonzoTxWitsRaw,
  addrAlonzoTxWitsL,
  bootAddrAlonzoTxWitsL,
  scriptAlonzoTxWitsL,
  datsAlonzoTxWitsL,
  rdmrsAlonzoTxWitsL,
  AlonzoEraTxWits (..),
  hashDataTxWitsL,
  unTxDats,
  nullDats,
  alonzoEqTxWitsRaw,
)
where

import Cardano.Crypto.DSIGN.Class (SigDSIGN, VerKeyDSIGN)
import Cardano.Crypto.Hash.Class (HashAlgorithm)
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  AsIx (..),
  decodePlutusScript,
  fromPlutusScript,
  toPlutusSLanguage,
 )
import Cardano.Ledger.Binary (
  Annotator,
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
import Cardano.Ledger.Crypto (Crypto (DSIGN, HASH), StandardCrypto)
import Cardano.Ledger.Keys (KeyRole (Witness), WitVKey)
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
  Mem,
  MemoBytes,
  Memoized (..),
  eqRawType,
  getMemoRawType,
  lensMemoRawType,
  mkMemoized,
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
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.TxWits (
  ShelleyTxWits (..),
  mapTraverseableDecoderA,
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

instance Memoized Redeemers where
  type RawType Redeemers = RedeemersRaw

-- | Note that 'Redeemers' are based on 'MemoBytes' since we must preserve
-- the original bytes for the 'Cardano.Ledger.Alonzo.Tx.ScriptIntegrity'.
-- Since the 'Redeemers' exist outside of the transaction body,
-- this is how we ensure that they are not manipulated.
newtype Redeemers era = RedeemersConstr (MemoBytes RedeemersRaw era)
  deriving newtype (Generic, ToCBOR, SafeToHash, Typeable)

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
    Redeemers rs' = mkMemoized $ RedeemersRaw rs'

{-# COMPLETE Redeemers #-}

unRedeemers :: Redeemers era -> Map (PlutusPurpose AsIx era) (Data era, ExUnits)
unRedeemers = unRedeemersRaw . getMemoRawType

nullRedeemers :: Redeemers era -> Bool
nullRedeemers = Map.null . unRedeemers

emptyRedeemers :: AlonzoEraScript era => Redeemers era
emptyRedeemers = Redeemers mempty

lookupRedeemer ::
  Ord (PlutusPurpose AsIx era) =>
  PlutusPurpose AsIx era ->
  Redeemers era ->
  Maybe (Data era, ExUnits)
lookupRedeemer key = Map.lookup key . unRedeemers

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
  { atwrAddrTxWits :: !(Set (WitVKey 'Witness (EraCrypto era)))
  , atwrBootAddrTxWits :: !(Set (BootstrapWitness (EraCrypto era)))
  , atwrScriptTxWits :: !(Map (ScriptHash (EraCrypto era)) (Script era))
  , atwrDatsTxWits :: !(TxDats era)
  , atwrRdmrsTxWits :: !(Redeemers era)
  }
  deriving (Generic)

instance
  ( Era era
  , NFData (Script era)
  , NFData (TxDats era)
  , NFData (Redeemers era)
  , NFData (SigDSIGN (DSIGN (EraCrypto era)))
  , NFData (VerKeyDSIGN (DSIGN (EraCrypto era)))
  ) =>
  NFData (AlonzoTxWitsRaw era)

newtype AlonzoTxWits era = TxWitnessConstr (MemoBytes AlonzoTxWitsRaw era)
  deriving newtype (SafeToHash, ToCBOR)
  deriving (Generic)

instance Memoized AlonzoTxWits where
  type RawType AlonzoTxWits = AlonzoTxWitsRaw

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
  , NFData (SigDSIGN (DSIGN (EraCrypto era)))
  , NFData (VerKeyDSIGN (DSIGN (EraCrypto era)))
  ) =>
  NFData (AlonzoTxWits era)

isEmptyTxWitness :: AlonzoEraScript era => AlonzoTxWits era -> Bool
isEmptyTxWitness (getMemoRawType -> AlonzoTxWitsRaw a b c d (Redeemers e)) =
  Set.null a && Set.null b && Map.null c && nullDats d && Map.null e

-- =====================================================
newtype TxDatsRaw era = TxDatsRaw {unTxDatsRaw :: Map (DataHash (EraCrypto era)) (Data era)}
  deriving (Generic, Typeable, Eq)
  deriving newtype (NoThunks, NFData)

deriving instance HashAlgorithm (HASH (EraCrypto era)) => Show (TxDatsRaw era)

instance (Typeable era, EncCBOR (Data era)) => EncCBOR (TxDatsRaw era) where
  encCBOR = encodeWithSetTag . Map.elems . unTxDatsRaw

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
newtype TxDats era = TxDatsConstr (MemoBytes TxDatsRaw era)
  deriving newtype (SafeToHash, ToCBOR, Eq, NoThunks, NFData)
  deriving (Generic)

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
    Era era => DecCBOR (Annotator (TxDats era))

-- | Upgrade 'TxDats' from one era to another. The underlying data structure
-- will remain identical, but the memoised serialisation may change to reflect
-- the versioned serialisation of the new era.
upgradeTxDats ::
  (Era era1, Era era2, EraCrypto era1 ~ EraCrypto era2) =>
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
  AlonzoEraScript era =>
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
  AlonzoEraScript era =>
  Lens' (AlonzoTxWits era) (Set (WitVKey 'Witness (EraCrypto era)))
addrAlonzoTxWitsL =
  lensMemoRawType atwrAddrTxWits $ \witsRaw addrWits -> witsRaw {atwrAddrTxWits = addrWits}
{-# INLINEABLE addrAlonzoTxWitsL #-}

bootAddrAlonzoTxWitsL ::
  AlonzoEraScript era =>
  Lens' (AlonzoTxWits era) (Set (BootstrapWitness (EraCrypto era)))
bootAddrAlonzoTxWitsL =
  lensMemoRawType atwrBootAddrTxWits $
    \witsRaw bootAddrWits -> witsRaw {atwrBootAddrTxWits = bootAddrWits}
{-# INLINEABLE bootAddrAlonzoTxWitsL #-}

scriptAlonzoTxWitsL ::
  AlonzoEraScript era =>
  Lens' (AlonzoTxWits era) (Map (ScriptHash (EraCrypto era)) (Script era))
scriptAlonzoTxWitsL =
  lensMemoRawType atwrScriptTxWits $ \witsRaw scriptWits -> witsRaw {atwrScriptTxWits = scriptWits}
{-# INLINEABLE scriptAlonzoTxWitsL #-}

datsAlonzoTxWitsL ::
  AlonzoEraScript era =>
  Lens' (AlonzoTxWits era) (TxDats era)
datsAlonzoTxWitsL =
  lensMemoRawType atwrDatsTxWits $ \witsRaw datsWits -> witsRaw {atwrDatsTxWits = datsWits}
{-# INLINEABLE datsAlonzoTxWitsL #-}

rdmrsAlonzoTxWitsL ::
  AlonzoEraScript era =>
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

  upgradeTxWits (ShelleyTxWits {addrWits, scriptWits, bootWits}) =
    AlonzoTxWits addrWits bootWits (upgradeScript <$> scriptWits) mempty emptyRedeemers

class (EraTxWits era, AlonzoEraScript era) => AlonzoEraTxWits era where
  datsTxWitsL :: Lens' (TxWits era) (TxDats era)

  rdmrsTxWitsL :: Lens' (TxWits era) (Redeemers era)

instance (EraScript (AlonzoEra c), Crypto c) => AlonzoEraTxWits (AlonzoEra c) where
  {-# SPECIALIZE instance AlonzoEraTxWits (AlonzoEra StandardCrypto) #-}

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
        !> Omit nullDats (Key 4 $ To dats)
        !> Omit nullRedeemers (Key 5 $ To rdmrs)
    where
      encodePlutus ::
        PlutusLanguage l =>
        SLanguage l ->
        Encode ('Closed 'Dense) (Map.Map (ScriptHash (EraCrypto era)) (Plutus l))
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

instance AlonzoEraScript era => DecCBOR (Annotator (RedeemersRaw era)) where
  decCBOR = do
    ifDecoderVersionAtLeast
      (natVersion @9)
      ( peekTokenType >>= \case
          TypeMapLenIndef -> decodeMapRedeemers
          TypeMapLen -> decodeMapRedeemers
          _ -> decodeListRedeemers
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
        decodeRecordNamed
          "Redeemer"
          (\(rdmrPtr, _, _) -> fromIntegral (listLen rdmrPtr) + 2)
          $ (,,) <$> decCBORGroup <*> decCBOR <*> decCBOR
      {-# INLINE decodeElement #-}
  {-# INLINE decCBOR #-}

-- | Encodes memoized bytes created upon construction.
instance AlonzoEraScript era => EncCBOR (Redeemers era)

deriving via
  (Mem RedeemersRaw era)
  instance
    AlonzoEraScript era => DecCBOR (Annotator (Redeemers era))

instance
  ( AlonzoEraScript era
  , EncCBOR (Data era)
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

      txWitnessField :: Word -> Field (Annotator (AlonzoTxWitsRaw era))
      txWitnessField 0 =
        fieldAA
          (\x wits -> wits {atwrAddrTxWits = x})
          ( D $
              ifDecoderVersionAtLeast
                (natVersion @9)
                ( allowTag setTag
                    >> mapTraverseableDecoderA (decodeNonEmptyList decCBOR) (Set.fromList . NE.toList)
                )
                (mapTraverseableDecoderA (decodeList decCBOR) Set.fromList)
          )
      txWitnessField 1 =
        fieldAA
          addScripts
          (D nativeScriptsDecoder)
      txWitnessField 2 =
        fieldAA
          (\x wits -> wits {atwrBootAddrTxWits = x})
          ( D $
              ifDecoderVersionAtLeast
                (natVersion @9)
                ( allowTag setTag
                    >> mapTraverseableDecoderA (decodeNonEmptyList decCBOR) (Set.fromList . NE.toList)
                )
                (mapTraverseableDecoderA (decodeList decCBOR) Set.fromList)
          )
      txWitnessField 3 = fieldA addScripts (decodePlutus SPlutusV1)
      txWitnessField 4 =
        fieldAA
          (\x wits -> wits {atwrDatsTxWits = x})
          From
      txWitnessField 5 = fieldAA (\x wits -> wits {atwrRdmrsTxWits = x}) From
      txWitnessField 6 = fieldA addScripts (decodePlutus SPlutusV2)
      txWitnessField 7 = fieldA addScripts (decodePlutus SPlutusV3)
      txWitnessField n = field (\_ t -> t) (Invalid n)
      {-# INLINE txWitnessField #-}

      nativeScriptsDecoder :: Decoder s (Annotator (Map (ScriptHash (EraCrypto era)) (Script era)))
      nativeScriptsDecoder =
        ifDecoderVersionAtLeast
          (natVersion @9)
          ( allowTag setTag
              >> mapTraverseableDecoderA (decodeNonEmptyList pairDecoder) (Map.fromList . NE.toList)
          )
          (mapTraverseableDecoderA (decodeList pairDecoder) Map.fromList)
        where
          pairDecoder :: Decoder s (Annotator (ScriptHash (EraCrypto era), Script era))
          pairDecoder = fmap (asHashedPair . fromNativeScript) <$> decCBOR

      addScripts ::
        Map (ScriptHash (EraCrypto era)) (Script era) ->
        AlonzoTxWitsRaw era ->
        AlonzoTxWitsRaw era
      addScripts scriptWitnesses txWits =
        txWits
          { atwrScriptTxWits = scriptWitnesses <> atwrScriptTxWits txWits
          }
      {-# INLINE addScripts #-}

      decodePlutus ::
        PlutusLanguage l =>
        SLanguage l ->
        Decode ('Closed 'Dense) (Map (ScriptHash (EraCrypto era)) (Script era))
      decodePlutus slang =
        D $
          ifDecoderVersionAtLeast
            (natVersion @9)
            (scriptDecoderV9 (fromPlutusScript <$> decodePlutusScript slang))
            (scriptDecoder (fromPlutusScript <$> decodePlutusScript slang))
      {-# INLINE decodePlutus #-}

      scriptDecoderV9 ::
        Decoder s (Script era) ->
        Decoder s (Map (ScriptHash (EraCrypto era)) (Script era))
      scriptDecoderV9 decodeScript = do
        allowTag setTag
        scriptMap <- decodeMapLikeEnforceNoDuplicates decodeListLenOrIndef $ do
          asHashedPair <$> decodeScript
        when (Map.null scriptMap) $ fail "Empty list of scripts is not allowed"
        pure scriptMap
      {-# INLINE scriptDecoderV9 #-}

      scriptDecoder ::
        Decoder s (Script era) ->
        Decoder s (Map (ScriptHash (EraCrypto era)) (Script era))
      scriptDecoder decodeScript =
        fmap Map.fromList $
          decodeList $
            asHashedPair <$> decodeScript
      {-# INLINE scriptDecoder #-}

      asHashedPair script =
        let !scriptHash = hashScript @era script
         in (scriptHash, script)
      {-# INLINE asHashedPair #-}
  {-# INLINE decCBOR #-}

deriving via
  (Mem AlonzoTxWitsRaw era)
  instance
    AlonzoEraScript era => DecCBOR (Annotator (AlonzoTxWits era))

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
