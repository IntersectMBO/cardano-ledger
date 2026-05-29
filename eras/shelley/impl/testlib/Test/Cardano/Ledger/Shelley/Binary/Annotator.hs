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

module Test.Cardano.Ledger.Shelley.Binary.Annotator (
  module Test.Cardano.Ledger.Core.Binary.Annotator,
) where

import Cardano.Base.Typeable (TypeName (TypeName))
import Cardano.Ledger.BaseTypes (maybeToStrictMaybe)
import Cardano.Ledger.Binary
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (decodeMemoized)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.BlockBody.Internal
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..), Tx (..))
import Cardano.Ledger.Shelley.TxAuxData
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.Shelley.TxWits hiding (mapTraverseableDecoderA)
import Data.Functor.Identity (Identity (..))
import Data.IntMap
import qualified Data.MapExtras as Map (fromElems)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as StrictSeq
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary.Annotator
import Test.Cardano.Ledger.Shelley.Arbitrary ()

instance
  ( EraTx era
  , DecCBOR (TxBody TopTx era)
  , DecCBOR (TxAuxData era)
  , DecCBOR (TxWits era)
  ) =>
  DecCBOR (ShelleyBlockBody era)
  where
  decCBOR = do
    Annotated bodies bodiesBytes <- decodeAnnotated decCBOR
    Annotated wits witsBytes <- decodeAnnotated decCBOR
    Annotated (auxDataMap :: IntMap (TxAuxData era)) auxDataBytes <- decodeAnnotated decCBOR
    let bodiesLength = length bodies
    auxData <-
      fmap (fmap runIdentity)
        <$> auxDataSeqDecoder bodiesLength (fmap pure auxDataMap)
    let witsLength = length wits
    unless
      (bodiesLength == witsLength)
      ( fail $
          "different number of transaction bodies ("
            <> show bodiesLength
            <> ") and witness sets ("
            <> show witsLength
            <> ")"
      )
    let
      mkTx body wit aux =
        mkBasicTx body
          & witsTxL .~ wit
          & auxDataTxL .~ aux
      txs =
        StrictSeq.forceToStrict $
          Seq.zipWith3 mkTx bodies wits (maybeToStrictMaybe <$> auxData)
      hash = hashShelleySegWits bodiesBytes witsBytes auxDataBytes
    pure $ ShelleyBlockBodyInternal txs hash bodiesBytes witsBytes auxDataBytes

instance
  ( Era era
  , DecCBOR (TxBody TopTx era)
  , DecCBOR (TxWits era)
  , DecCBOR (TxAuxData era)
  ) =>
  DecCBOR (ShelleyTx TopTx era)
  where
  decCBOR =
    decodeRecordNamed "ShelleyTx" (const 3) $
      ShelleyTx
        <$> decCBOR
        <*> decCBOR
        <*> decodeNullStrictMaybe decCBOR

deriving newtype instance DecCBOR (TxBody TopTx ShelleyEra)

deriving newtype instance DecCBOR (Tx TopTx ShelleyEra)

deriving newtype instance Era era => DecCBOR (ShelleyTxAuxData era)

instance (EraScript era, DecCBOR (Script era)) => DecCBOR (ShelleyTxWitsRaw era) where
  decCBOR = decodeSparseKeyed TypeName [] (ShelleyTxWitsRaw mempty mempty mempty) decoderByKey
    where
      decoderByKey :: ShelleyTxWitsRaw era -> Word -> Maybe (Decoder s (ShelleyTxWitsRaw era))
      decoderByKey acc = \case
        0 -> Just $ do
          x <- decCBOR
          pure $ acc {stwrAddrTxWits = x}
        1 -> Just $ do
          x <- Map.fromElems (hashScript @era) <$> decodeList decCBOR
          pure $ acc {stwrScriptTxWits = x}
        2 -> Just $ do
          x <- decCBOR
          pure $ acc {stwrBootAddrTxWits = x}
        _ -> Nothing
      {-# INLINE decoderByKey #-}

instance (EraScript era, DecCBOR (Script era)) => DecCBOR (ShelleyTxWits era) where
  decCBOR = MkShelleyTxWits <$> decodeMemoized decCBOR

instance Era era => DecCBOR (MultiSig era) where
  decCBOR = MkMultiSig <$> decodeMemoized decCBOR

instance Era era => DecCBOR (MultiSigRaw era) where
  decCBOR = decodeRecordSum "MultiSig" $ do
    \case
      0 -> (,) 2 . MultiSigSignature . KeyHash <$> decCBOR
      1 -> (,) 2 . MultiSigAllOf <$> decCBOR
      2 -> (,) 2 . MultiSigAnyOf <$> decCBOR
      3 -> (,) 3 <$> (MultiSigMOf <$> decCBOR <*> decCBOR)
      k -> invalidKey k
