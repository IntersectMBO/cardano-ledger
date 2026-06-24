{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Binary.Annotator (
  module Test.Cardano.Ledger.Mary.Binary.Annotator,
) where

import Cardano.Base.Typeable (TypeName (TypeName))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.BlockBody.Internal
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxAuxData
import Cardano.Ledger.Alonzo.TxBody
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus
import Cardano.Ledger.Shelley.BlockBody (auxDataSeqDecoder)
import Data.Coerce (coerce)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MapExtras as Map (fromElems)
import Data.Maybe.Strict (maybeToStrictMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Lens.Micro
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Binary.Annotator
import Test.Cardano.Ledger.Shelley.Arbitrary ()

instance
  ( AlonzoEraTx era
  , DecCBOR (TxBody TopTx era)
  , DecCBOR (TxAuxData era)
  , DecCBOR (TxWits era)
  , DecCBOR (NativeScript era)
  ) =>
  DecCBOR (AlonzoBlockBody era)
  where
  decCBOR = do
    Annotated bodies bodiesBytes <- decodeAnnotated decCBOR
    Annotated wits witsBytes <- decodeAnnotated decCBOR
    Annotated auxDataMap auxDataBytes <- decodeAnnotated decCBOR
    let bodiesLength = length bodies
        inRange x = (0 <= x) && (x <= (bodiesLength - 1))
        witsLength = length wits
    auxData <- auxDataSeqDecoder @(TxAuxData era) bodiesLength auxDataMap
    Annotated isValidIdxs isValidBytes <- decodeAnnotated decCBOR
    let validFlags = alignedValidFlags bodiesLength isValidIdxs
    unless (bodiesLength == witsLength) $
      fail $
        "different number of transaction bodies ("
          <> show bodiesLength
          <> ") and witness sets ("
          <> show witsLength
          <> ")"
    unless (all inRange isValidIdxs) $
      fail $
        "Some IsValid index is not in the range: 0 .. "
          ++ show (bodiesLength - 1)
          ++ ", "
          ++ show isValidIdxs

    let mkTx body wit isValid aData =
          mkBasicTx body
            & witsTxL .~ wit
            & auxDataTxL .~ maybeToStrictMaybe aData
            & isValidTxL .~ isValid
    let txs =
          StrictSeq.forceToStrict $
            Seq.zipWith4 mkTx bodies wits validFlags auxData
    pure $
      AlonzoBlockBodyInternal
        txs
        (hashAlonzoSegWits bodiesBytes witsBytes auxDataBytes isValidBytes)
        bodiesBytes
        witsBytes
        auxDataBytes
        isValidBytes

deriving newtype instance DecCBOR (TxBody TopTx AlonzoEra)

instance
  ( Typeable era
  , DecCBOR (TxBody TopTx era)
  , DecCBOR (TxWits era)
  , DecCBOR (TxAuxData era)
  ) =>
  DecCBOR (AlonzoTx TopTx era)
  where
  decCBOR =
    ifDecoderVersionAtLeast (natVersion @12) decodeAlonzoTxPv12 $
      decode $
        RecD AlonzoTx
          <! From
          <! From
          <! From
          <! D (decodeNullStrictMaybe decCBOR)
    where
      decodeAlonzoTxPv12 = decodeRecordNamed "AlonzoTx" (const 4) $ do
        body <- decCBOR
        wits <- decCBOR
        isValid <- decCBOR
        auxData <- decodeNullStrictMaybe decCBOR
        pure $ AlonzoTx body wits isValid auxData
      {-# INLINE decodeAlonzoTxPv12 #-}
  {-# INLINE decCBOR #-}

instance
  (Era era, Typeable (NativeScript era), DecCBOR (NativeScript era)) =>
  DecCBOR (AlonzoTxAuxDataRaw era)
  where
  decCBOR =
    decodeTxAuxDataByTokenType @(AlonzoTxAuxDataRaw era)
      (ifDecoderVersionAtLeast (natVersion @12) decodeShelleyPv12 decodeShelley)
      (ifDecoderVersionAtLeast (natVersion @12) decodeAllegraPv12 decodeAllegra)
      (ifDecoderVersionAtLeast (natVersion @12) decodeAlonzoPv12 decodeAlonzo)
    where
      decodeShelleyPv12 = do
        metadata <- decCBOR
        pure $ AlonzoTxAuxDataRaw metadata StrictSeq.empty Map.empty
      {-# INLINE decodeShelleyPv12 #-}
      decodeShelley =
        decode
          (Emit AlonzoTxAuxDataRaw <! From <! Emit StrictSeq.empty <! Emit Map.empty)
      decodeAllegraPv12 =
        decodeRecordNamed "AlonzoTxAuxDataRaw" (const 2) $
          AlonzoTxAuxDataRaw <$> decCBOR <*> decCBOR <*> pure Map.empty
      {-# INLINE decodeAllegraPv12 #-}
      decodeAllegra =
        decode
          (RecD AlonzoTxAuxDataRaw <! From <! From <! Emit Map.empty)
      decodeAlonzoPv12 = do
        assertTag 259
        decodeSparseKeyed TypeName [] emptyAlonzoTxAuxDataRaw decoderByKey
      {-# INLINE decodeAlonzoPv12 #-}
      decodeAlonzo =
        decode $
          TagD 259 $
            SparseKeyed "AlonzoTxAuxData" emptyAlonzoTxAuxDataRaw auxDataField []

      decoderByKey :: AlonzoTxAuxDataRaw era -> Word -> Maybe (Decoder s (AlonzoTxAuxDataRaw era))
      decoderByKey acc = \case
        0 -> Just $ do
          metadata <- decCBOR
          pure $ acc {atadrMetadata = metadata}
        1 -> Just $ do
          scripts <- decCBOR
          pure $ acc {atadrNativeScripts = atadrNativeScripts acc <> scripts}
        2 -> decodeAddPlutus PlutusV1
        3 -> decodeAddPlutus PlutusV2
        4 -> decodeAddPlutus PlutusV3
        5 -> decodeAddPlutus PlutusV4
        _ -> Nothing
        where
          decodeAddPlutus lang = Just $ do
            guardPlutus lang
            ps <- decCBOR
            pure $ addPlutusScripts lang ps acc
          {-# INLINE decodeAddPlutus #-}
      {-# INLINE decoderByKey #-}

      auxDataField :: Word -> Field (AlonzoTxAuxDataRaw era)
      auxDataField 0 = field (\x ad -> ad {atadrMetadata = x}) From
      auxDataField 1 = field (\x ad -> ad {atadrNativeScripts = atadrNativeScripts ad <> x}) From
      auxDataField 2 = field (addPlutusScripts PlutusV1) (D (guardPlutus PlutusV1 >> decCBOR))
      auxDataField 3 = field (addPlutusScripts PlutusV2) (D (guardPlutus PlutusV2 >> decCBOR))
      auxDataField 4 = field (addPlutusScripts PlutusV3) (D (guardPlutus PlutusV3 >> decCBOR))
      auxDataField 5 = field (addPlutusScripts PlutusV4) (D (guardPlutus PlutusV4 >> decCBOR))
      auxDataField n = invalidField n

deriving newtype instance (Era era, DecCBOR (NativeScript era)) => DecCBOR (AlonzoTxAuxData era)

instance (AlonzoEraScript era, DecCBOR (NativeScript era)) => DecCBOR (AlonzoTxWitsRaw era) where
  decCBOR =
    ifDecoderVersionAtLeast
      (natVersion @12)
      (decodeSparseKeyed TypeName [] emptyTxWitsRaw decoderByKey)
      (decode $ SparseKeyed "AlonzoTxWits" emptyTxWitsRaw txWitnessField [])
    where
      setDecoder :: (Ord a, DecCBOR a) => Decoder s (Set a)
      setDecoder =
        ifDecoderVersionAtLeast
          (natVersion @9)
          ( allowTag setTag
              >> ifDecoderVersionAtLeast
                (natVersion @12)
                (decodeSetLikeEnforceNoDuplicates Set.insert (\s -> (length s, s)) decCBOR)
                (Set.fromList . NE.toList <$> decodeNonEmptyList decCBOR)
          )
          (Set.fromList <$> decodeList decCBOR)
      {-# INLINE setDecoder #-}

      addrWitsSetDecoder :: (Ord a, DecCBOR a) => Decoder s (Set a)
      addrWitsSetDecoder =
        do
          let
            nonEmptyDecoder = do
              allowTag setTag
              Set.fromList . NE.toList <$> decodeNonEmptyList decCBOR
            nonEmptyNoDuplicatesDecoder = do
              s <- decodeSetLikeEnforceNoDuplicates Set.insert (\s -> (length s, s)) decCBOR
              when (Set.null s) $ fail "Set cannot be empty"
              pure s
          ifDecoderVersionAtLeast
            (natVersion @9)
            (ifDecoderVersionAtLeast (natVersion @12) nonEmptyNoDuplicatesDecoder nonEmptyDecoder)
            (Set.fromList <$> decodeList decCBOR)
      {-# INLINE addrWitsSetDecoder #-}

      txWitnessField :: Word -> Field (AlonzoTxWitsRaw era)
      txWitnessField 0 =
        field
          (\x wits -> wits {atwrAddrTxWits = x})
          ( D $
              ifDecoderVersionAtLeast
                (natVersion @9)
                addrWitsSetDecoder
                (Set.fromList <$> decodeList decCBOR)
          )
      txWitnessField 1 = field addScriptsTxWitsRaw (D nativeScriptsDecoder)
      txWitnessField 2 =
        field
          (\x wits -> wits {atwrBootAddrTxWits = x})
          ( D $
              ifDecoderVersionAtLeast
                (natVersion @9)
                setDecoder
                (Set.fromList <$> decodeList decCBOR)
          )
      txWitnessField 3 = field addScriptsTxWitsRaw (decodeAlonzoPlutusScript SPlutusV1)
      txWitnessField 4 = field (\x wits -> wits {atwrDatsTxWits = x}) From
      txWitnessField 5 = field (\x wits -> wits {atwrRdmrsTxWits = x}) From
      txWitnessField 6 = field addScriptsTxWitsRaw (decodeAlonzoPlutusScript SPlutusV2)
      txWitnessField 7 = field addScriptsTxWitsRaw (decodeAlonzoPlutusScript SPlutusV3)
      txWitnessField n = invalidField n
      {-# INLINE txWitnessField #-}

      decoderByKey :: AlonzoTxWitsRaw era -> Word -> Maybe (Decoder s (AlonzoTxWitsRaw era))
      decoderByKey acc = \case
        0 -> Just $ do
          x <- addrWitsSetDecoder
          pure $ acc {atwrAddrTxWits = x}
        1 -> Just $ do
          x <- nativeScriptsDecoder
          pure $ addScriptsTxWitsRaw x acc
        2 -> Just $ do
          x <- setDecoder
          pure $ acc {atwrBootAddrTxWits = x}
        3 -> Just $ do
          x <- alonzoPlutusScriptDecoder SPlutusV1
          pure $ addScriptsTxWitsRaw x acc
        4 -> Just $ do
          x <- decCBOR
          pure $ acc {atwrDatsTxWits = x}
        5 -> Just $ do
          x <- decCBOR
          pure $ acc {atwrRdmrsTxWits = x}
        6 -> Just $ do
          x <- alonzoPlutusScriptDecoder SPlutusV2
          pure $ addScriptsTxWitsRaw x acc
        7 -> Just $ do
          x <- alonzoPlutusScriptDecoder SPlutusV3
          pure $ addScriptsTxWitsRaw x acc
        _ -> Nothing
      {-# INLINE decoderByKey #-}

      noDuplicateNativeScriptsDecoder :: Decoder s (Map ScriptHash (Script era))
      noDuplicateNativeScriptsDecoder =
        noDuplicateNonEmptySetAsMapDecoder
          (\ns -> (hashScript $ fromNativeScript ns, fromNativeScript ns))
      {-# INLINE noDuplicateNativeScriptsDecoder #-}

      hashedNativeSciptDecoder :: Decoder s (ScriptHash, Script era)
      hashedNativeSciptDecoder = (\script -> (hashScript script, script)) . fromNativeScript @era <$> decCBOR
      {-# INLINE hashedNativeSciptDecoder #-}

      nativeScriptsDecoder :: Decoder s (Map ScriptHash (Script era))
      nativeScriptsDecoder =
        ifDecoderVersionAtLeast
          (natVersion @9)
          ( ifDecoderVersionAtLeast
              (natVersion @12)
              noDuplicateNativeScriptsDecoder
              ( do
                  allowTag setTag
                  Map.fromList . NE.toList <$> decodeNonEmptyList hashedNativeSciptDecoder
              )
          )
          (Map.fromList <$> decodeList pairDecoder)
        where
          pairDecoder :: Decoder s (ScriptHash, Script era)
          pairDecoder = asHashedScriptPair . fromNativeScript <$> decCBOR
      {-# INLINE nativeScriptsDecoder #-}

deriving newtype instance
  (AlonzoEraScript era, DecCBOR (NativeScript era)) =>
  DecCBOR (AlonzoTxWits era)

instance AlonzoEraScript era => DecCBOR (RedeemersRaw era) where
  decCBOR =
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
      {-# INLINE decodeMapRedeemers #-}
      decodeListRedeemers :: Decoder s (RedeemersRaw era)
      decodeListRedeemers =
        RedeemersRaw . Map.fromList . NE.toList
          <$> decodeNonEmptyList decodeElement
      {-# INLINE decodeListRedeemers #-}
      decodeElement :: Decoder s (PlutusPurpose AsIx era, (Data era, ExUnits))
      decodeElement = do
        let redeemerLen (redeemerPtr, _) = listLenInt (Just redeemerPtr) + 2
        decodeRecordNamed "Redeemer" redeemerLen do
          !redeemerPtr <- decCBORGroup
          !redeemerData <- decCBOR
          !redeemerExUnits <- decCBOR
          pure (redeemerPtr, (redeemerData, redeemerExUnits))
      {-# INLINE decodeElement #-}
  {-# INLINE decCBOR #-}

deriving newtype instance AlonzoEraScript era => DecCBOR (Redeemers era)

instance (AlonzoEraScript era, DecCBOR (NativeScript era)) => DecCBOR (AlonzoScript era) where
  decCBOR =
    ifDecoderVersionAtLeast (natVersion @12) decodeAlonzoScriptPv12 $
      decode (Summands "AlonzoScript" decodeScript)
    where
      decodeScript = \case
        0 -> SumD NativeScript <! From
        1 -> decodePlutus SPlutusV1
        2 -> decodePlutus SPlutusV2
        3 -> decodePlutus SPlutusV3
        4 -> decodePlutus SPlutusV4
        n -> Invalid n
      decodePlutus slang =
        SumD PlutusScript <! D (decodePlutusScript slang)
      decodePlutusVariant slang = do
        ps <- decodePlutusScript slang
        pure (2, PlutusScript ps)
      {-# INLINE decodePlutusVariant #-}
      decodeAlonzoScriptPv12 = decodeRecordSum "AlonzoScript" $ \case
        0 -> do
          ns <- decCBOR
          pure (2, NativeScript ns)
        1 -> decodePlutusVariant SPlutusV1
        2 -> decodePlutusVariant SPlutusV2
        3 -> decodePlutusVariant SPlutusV3
        4 -> decodePlutusVariant SPlutusV4
        n -> invalidKey n
      {-# INLINE decodeAlonzoScriptPv12 #-}

-- | Decodes a set of `a`'s and maps a function over it to get key-value pairs.
-- If the key-value pairs create a non-empty Map without duplicates, then that map is returned,
-- otherwise fail
noDuplicateNonEmptySetAsMapDecoder ::
  (Ord k, DecCBOR a) =>
  (a -> (k, v)) ->
  Decoder s (Map k v)
noDuplicateNonEmptySetAsMapDecoder toKV = do
  allowTag setTag
  vals <- decodeList decCBOR
  go (Map.empty, 0) vals
  where
    go (m, n) []
      | Map.null m = fail "Empty script Set is not allowed"
      | length m /= n = fail "Duplicate elements in the scripts Set were encountered"
      | otherwise = pure m
    go (!m, !n) (x : xs) = do
      let (k, v) = toKV x
      go (Map.insert k v m, n + 1) xs

instance Era era => DecCBOR (TxDatsRaw era) where
  decCBOR =
    ifDecoderVersionAtLeast
      (natVersion @9)
      ( ifDecoderVersionAtLeast
          (natVersion @12)
          noDuplicatesDatsDecoder
          ( allowTag setTag
              >> TxDatsRaw . Map.fromElems hashData . NE.toList <$> decodeNonEmptyList decCBOR
          )
      )
      (TxDatsRaw . Map.fromElems hashData <$> decodeList decCBOR)
    where
      noDuplicatesDatsDecoder :: Decoder s (TxDatsRaw era)
      noDuplicatesDatsDecoder =
        coerce . noDuplicateNonEmptySetAsMapDecoder $ \dat -> (hashData dat, dat)
  {-# INLINE decCBOR #-}

deriving newtype instance Era era => DecCBOR (TxDats era)

deriving newtype instance DecCBOR (Tx TopTx AlonzoEra)
