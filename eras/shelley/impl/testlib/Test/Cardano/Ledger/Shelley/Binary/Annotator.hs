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

module Test.Cardano.Ledger.Shelley.Binary.Annotator (
  mapTraverseableDecoderA,
  module Test.Cardano.Ledger.Core.Binary.Annotator,
) where

import Cardano.Ledger.BaseTypes (maybeToStrictMaybe)
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.BlockChain
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.Shelley.Tx.Internal (
  ShelleyTx (MkShelleyTx),
  ShelleyTxRaw (..),
  unsafeConstructTxWithBytes,
 )
import Cardano.Ledger.Shelley.TxAuxData
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.Shelley.TxWits
import Data.MapExtras (fromElems)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Void
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary.Annotator
import Test.Cardano.Ledger.Shelley.Arbitrary ()

deriving via
  Mem (MultiSigRaw era)
  instance
    Era era => DecCBOR (Annotator (MultiSig era))

instance Era era => DecCBOR (Annotator (MultiSigRaw era)) where
  decCBOR = decodeRecordSum "MultiSig" $
    \case
      0 -> (,) 2 . pure . MultiSigSignature . KeyHash <$> decCBOR
      1 -> do
        multiSigs <- sequence <$> decCBOR
        pure (2, MultiSigAllOf <$> multiSigs)
      2 -> do
        multiSigs <- sequence <$> decCBOR
        pure (2, MultiSigAnyOf <$> multiSigs)
      3 -> do
        m <- decCBOR
        multiSigs <- sequence <$> decCBOR
        pure (3, MultiSigMOf m <$> multiSigs)
      k -> invalidKey k

instance
  ( Era era
  , DecCBOR (PParamsUpdate era)
  , DecCBOR (TxOut era)
  , DecCBOR (TxCert era)
  ) =>
  DecCBOR (Annotator (ShelleyTxBodyRaw era))
  where
  decCBOR = pure <$> decCBOR

deriving via
  Mem (ShelleyTxBodyRaw era)
  instance
    EraTxBody era => DecCBOR (Annotator (ShelleyTxBody era))

instance Era era => DecCBOR (Annotator (ShelleyTxAuxDataRaw era)) where
  decCBOR = pure <$> decCBOR

deriving via
  Mem (ShelleyTxAuxDataRaw era)
  instance
    Era era => DecCBOR (Annotator (ShelleyTxAuxData era))

instance
  (EraScript era, DecCBOR (Annotator (Script era))) =>
  DecCBOR (Annotator (ShelleyTxWitsRaw era))
  where
  decCBOR = decodeWits

deriving via
  Mem (ShelleyTxWitsRaw era)
  instance
    ( EraScript era
    , DecCBOR (Annotator (Script era))
    ) =>
    DecCBOR (Annotator (ShelleyTxWits era))

decodeWits ::
  forall era s.
  (EraScript era, DecCBOR (Annotator (Script era))) =>
  Decoder s (Annotator (ShelleyTxWitsRaw era))
decodeWits =
  decode $
    SparseKeyed
      "ShelleyTxWitsRaw"
      (pure emptyWitnessSet)
      witField
      []
  where
    emptyWitnessSet = ShelleyTxWitsRaw mempty mempty mempty
    witField :: Word -> Field (Annotator (ShelleyTxWitsRaw era))
    witField 0 =
      fieldAA
        (\x wits -> wits {stwrAddrTxWits = x})
        (D $ mapTraverseableDecoderA (decodeList decCBOR) Set.fromList)
    witField 1 =
      fieldAA
        (\x wits -> wits {stwrScriptTxWits = x})
        ( D $
            mapTraverseableDecoderA
              (decodeList decCBOR)
              (fromElems (hashScript @era))
        )
    witField 2 =
      fieldAA
        (\x wits -> wits {stwrBootAddrTxWits = x})
        (D $ mapTraverseableDecoderA (decodeList decCBOR) Set.fromList)
    witField n = fieldAA (\(_ :: Void) wits -> wits) (Invalid n)

mapTraverseableDecoderA ::
  Traversable f =>
  Decoder s (f (Annotator a)) ->
  (f a -> m b) ->
  Decoder s (Annotator (m b))
mapTraverseableDecoderA decList transformList = fmap transformList . sequence <$> decList

deriving via
  Mem (ShelleyTxRaw era)
  instance
    ( EraTx era
    , DecCBOR (Annotator (TxBody era))
    , DecCBOR (Annotator (TxWits era))
    , DecCBOR (Annotator (TxAuxData era))
    ) =>
    DecCBOR (Annotator (ShelleyTx era))

instance
  ( EraTx era
  , DecCBOR (Annotator (TxBody era))
  , DecCBOR (Annotator (TxWits era))
  , DecCBOR (Annotator (TxAuxData era))
  ) =>
  DecCBOR (Annotator (ShelleyTxRaw era))
  where
  decCBOR =
    decode $
      Ann (RecD ShelleyTxRaw)
        <*! From
        <*! From
        <*! D
          ( sequence . maybeToStrictMaybe
              <$> decodeNullMaybe decCBOR
          )

segWitAnnTx ::
  forall era.
  EraTx era =>
  Annotator (TxBody era) ->
  Annotator (TxWits era) ->
  Maybe (Annotator (TxAuxData era)) ->
  Annotator (ShelleyTx era)
segWitAnnTx bodyAnn witsAnn metaAnn = Annotator $ \bytes ->
  let body' = runAnnotator bodyAnn bytes
      witnessSet = runAnnotator witsAnn bytes
      metadata = flip runAnnotator bytes <$> metaAnn
      wrappedMetadataBytes = case metadata of
        Nothing -> Plain.serialize Plain.encodeNull
        Just b -> Plain.serialize b
      fullBytes =
        Plain.serialize (Plain.encodeListLen 3)
          <> Plain.serialize body'
          <> Plain.serialize witnessSet
          <> wrappedMetadataBytes
   in unsafeConstructTxWithBytes
        body'
        witnessSet
        (maybeToStrictMaybe metadata)
        fullBytes

-- | The parts of the Tx in Blocks that have to have DecCBOR(Annotator x) instances.
--   These are exactly the parts that are SafeToHash.
-- | Decode a TxSeq, used in decoding a Block.
txSeqDecoder ::
  ( EraTx era
  , DecCBOR (Annotator (TxAuxData era))
  , DecCBOR (Annotator (TxBody era))
  , DecCBOR (Annotator (TxWits era))
  ) =>
  Bool ->
  Decoder s (Annotator (ShelleyTxSeq era))
txSeqDecoder lax = do
  (bodies, bodiesAnn) <- withSlice decCBOR
  (wits, witsAnn) <- withSlice decCBOR
  let bodiesLength = length bodies
      witsLength = length wits
  (metadata, metadataAnn) <- withSlice $ do
    auxDataMap <- decCBOR
    auxDataSeqDecoder bodiesLength auxDataMap lax

  unless
    (lax || bodiesLength == witsLength)
    ( fail $
        "different number of transaction bodies ("
          <> show bodiesLength
          <> ") and witness sets ("
          <> show witsLength
          <> ")"
    )

  let txns =
        sequenceA $
          StrictSeq.forceToStrict $
            Seq.zipWith3 segWitAnnTx bodies wits metadata
  pure $ TxSeq' <$> txns <*> bodiesAnn <*> witsAnn <*> metadataAnn

instance
  ( EraTx era
  , DecCBOR (Annotator (TxAuxData era))
  , DecCBOR (Annotator (TxBody era))
  , DecCBOR (Annotator (TxWits era))
  ) =>
  DecCBOR (Annotator (ShelleyTxSeq era))
  where
  decCBOR = txSeqDecoder False
