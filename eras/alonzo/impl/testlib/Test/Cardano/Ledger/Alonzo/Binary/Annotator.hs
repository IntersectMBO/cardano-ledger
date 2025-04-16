{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.Tx hiding (wits)
import Cardano.Ledger.Alonzo.TxAuxData
import Cardano.Ledger.Alonzo.TxBody
import Cardano.Ledger.Alonzo.TxSeq.Internal
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus
import Cardano.Ledger.Shelley.BlockChain (auxDataSeqDecoder)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MapExtras as Map (fromElems)
import Data.Maybe.Strict (maybeToStrictMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Lens.Micro
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Binary.Annotator
import Test.Cardano.Ledger.Shelley.Arbitrary ()

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

deriving via
  Mem (TxDatsRaw era)
  instance
    Era era => DecCBOR (Annotator (TxDats era))

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

deriving via
  Mem (RedeemersRaw era)
  instance
    AlonzoEraScript era => DecCBOR (Annotator (Redeemers era))

deriving via
  Mem (AlonzoTxWitsRaw era)
  instance
    ( AlonzoEraScript era
    , DecCBOR (Annotator (NativeScript era))
    ) =>
    DecCBOR (Annotator (AlonzoTxWits era))

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
          addScriptsTxWitsRaw
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
      txWitnessField 3 = fieldA addScriptsTxWitsRaw (decodeAlonzoPlutusScript SPlutusV1)
      txWitnessField 4 =
        fieldAA
          (\x wits -> wits {atwrDatsTxWits = x})
          From
      txWitnessField 5 = fieldAA (\x wits -> wits {atwrRdmrsTxWits = x}) From
      txWitnessField 6 = fieldA addScriptsTxWitsRaw (decodeAlonzoPlutusScript SPlutusV2)
      txWitnessField 7 = fieldA addScriptsTxWitsRaw (decodeAlonzoPlutusScript SPlutusV3)
      txWitnessField n = invalidField n
      {-# INLINE txWitnessField #-}

      nativeScriptsDecoder :: Decoder s (Annotator (Map ScriptHash (Script era)))
      nativeScriptsDecoder =
        ifDecoderVersionAtLeast
          (natVersion @9)
          ( allowTag setTag
              >> mapTraverseableDecoderA (decodeNonEmptyList pairDecoder) (Map.fromList . NE.toList)
          )
          (mapTraverseableDecoderA (decodeList pairDecoder) Map.fromList)
        where
          pairDecoder :: Decoder s (Annotator (ScriptHash, Script era))
          pairDecoder = fmap (asHashedScriptPair . fromNativeScript) <$> decCBOR
  {-# INLINE decCBOR #-}

instance AlonzoEraScript era => DecCBOR (Annotator (AlonzoScript era)) where
  decCBOR = decode (Summands "AlonzoScript" decodeScript)
    where
      decodeAnnPlutus slang =
        Ann (SumD PlutusScript) <*! Ann (D (decodePlutusScript slang))
      {-# INLINE decodeAnnPlutus #-}
      decodeScript :: Word -> Decode 'Open (Annotator (AlonzoScript era))
      decodeScript = \case
        0 -> Ann (SumD TimelockScript) <*! From
        1 -> decodeAnnPlutus SPlutusV1
        2 -> decodeAnnPlutus SPlutusV2
        3 -> decodeAnnPlutus SPlutusV3
        n -> Invalid n
      {-# INLINE decodeScript #-}
  {-# INLINE decCBOR #-}

instance Era era => DecCBOR (Annotator (AlonzoTxAuxDataRaw era)) where
  decCBOR =
    decodeTxAuxDataByTokenType @(Annotator (AlonzoTxAuxDataRaw era))
      decodeShelley
      decodeAllegra
      decodeAlonzo
    where
      decodeShelley =
        decode
          ( Ann (Emit AlonzoTxAuxDataRaw)
              <*! Ann From
              <*! Ann (Emit StrictSeq.empty)
              <*! Ann (Emit Map.empty)
          )
      decodeAllegra =
        decode
          ( Ann (RecD AlonzoTxAuxDataRaw)
              <*! Ann From
              <*! D
                (sequence <$> decodeStrictSeq decCBOR)
              <*! Ann (Emit Map.empty)
          )
      decodeAlonzo =
        decode $
          TagD 259 $
            SparseKeyed "AlonzoTxAuxData" (pure emptyAlonzoTxAuxDataRaw) auxDataField []

      auxDataField :: Word -> Field (Annotator (AlonzoTxAuxDataRaw era))
      auxDataField 0 = fieldA (\x ad -> ad {atadrMetadata = x}) From
      auxDataField 1 =
        fieldAA
          (\x ad -> ad {atadrTimelock = atadrTimelock ad <> x})
          (D (sequence <$> decodeStrictSeq decCBOR))
      auxDataField 2 = fieldA (addPlutusScripts PlutusV1) (D (guardPlutus PlutusV1 >> decCBOR))
      auxDataField 3 = fieldA (addPlutusScripts PlutusV2) (D (guardPlutus PlutusV2 >> decCBOR))
      auxDataField 4 = fieldA (addPlutusScripts PlutusV3) (D (guardPlutus PlutusV3 >> decCBOR))
      auxDataField n = invalidField n

deriving via
  Mem (AlonzoTxAuxDataRaw era)
  instance
    Era era => DecCBOR (Annotator (AlonzoTxAuxData era))

deriving via
  Mem (AlonzoTxBodyRaw era)
  instance
    (Era era, DecCBOR (TxOut era), DecCBOR (TxCert era), DecCBOR (PParamsUpdate era)) =>
    DecCBOR (Annotator (AlonzoTxBody era))

instance
  (Era era, DecCBOR (TxOut era), DecCBOR (TxCert era), DecCBOR (PParamsUpdate era)) =>
  DecCBOR (Annotator (AlonzoTxBodyRaw era))
  where
  decCBOR = pure <$> decCBOR

instance
  ( AlonzoEraTx era
  , DecCBOR (Annotator (TxAuxData era))
  , DecCBOR (Annotator (TxBody era))
  , DecCBOR (Annotator (TxWits era))
  ) =>
  DecCBOR (Annotator (AlonzoTxSeq era))
  where
  decCBOR = do
    (bodies, bodiesAnn) <- withSlice decCBOR
    (wits, witsAnn) <- withSlice decCBOR
    let bodiesLength = length bodies
        inRange x = (0 <= x) && (x <= (bodiesLength - 1))
        witsLength = length wits
    (auxData, auxDataAnn) <- withSlice $ do
      auxDataMap <- decCBOR
      auxDataSeqDecoder bodiesLength auxDataMap False

    (isValIdxs, isValAnn) <- withSlice decCBOR
    let validFlags = alignedValidFlags bodiesLength isValIdxs
    unless
      (bodiesLength == witsLength)
      ( fail $
          "different number of transaction bodies ("
            <> show bodiesLength
            <> ") and witness sets ("
            <> show witsLength
            <> ")"
      )
    unless
      (all inRange isValIdxs)
      ( fail
          ( "Some IsValid index is not in the range: 0 .. "
              ++ show (bodiesLength - 1)
              ++ ", "
              ++ show isValIdxs
          )
      )

    let txns =
          sequenceA $
            StrictSeq.forceToStrict $
              Seq.zipWith4 alonzoSegwitTx bodies wits validFlags auxData
    pure $
      AlonzoTxSeqRaw
        <$> txns
        <*> bodiesAnn
        <*> witsAnn
        <*> auxDataAnn
        <*> isValAnn

instance
  ( Typeable era
  , Typeable (TxBody era)
  , Typeable (TxWits era)
  , Typeable (TxAuxData era)
  , DecCBOR (Annotator (TxBody era))
  , DecCBOR (Annotator (TxWits era))
  , DecCBOR (Annotator (TxAuxData era))
  ) =>
  DecCBOR (Annotator (AlonzoTx era))
  where
  decCBOR =
    decode $
      Ann (RecD AlonzoTx)
        <*! From
        <*! From
        <*! Ann From
        <*! D
          ( sequence . maybeToStrictMaybe
              <$> decodeNullMaybe decCBOR
          )
  {-# INLINE decCBOR #-}

-- | Construct an annotated Alonzo style transaction.
alonzoSegwitTx ::
  AlonzoEraTx era =>
  Annotator (TxBody era) ->
  Annotator (TxWits era) ->
  IsValid ->
  Maybe (Annotator (TxAuxData era)) ->
  Annotator (Tx era)
alonzoSegwitTx txBodyAnn txWitsAnn txIsValid auxDataAnn = Annotator $ \bytes ->
  let txBody = runAnnotator txBodyAnn bytes
      txWits = runAnnotator txWitsAnn bytes
      txAuxData = maybeToStrictMaybe (flip runAnnotator bytes <$> auxDataAnn)
   in mkBasicTx txBody
        & witsTxL .~ txWits
        & auxDataTxL .~ txAuxData
        & isValidTxL .~ txIsValid
