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

import Cardano.Ledger.BaseTypes (maybeToStrictMaybe)
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (decodeMemoized)
import Cardano.Ledger.Shelley (ShelleyEra, Tx (..))
import Cardano.Ledger.Shelley.BlockChain
import Cardano.Ledger.Shelley.Scripts
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
  , DecCBOR (TxBody era)
  , DecCBOR (TxAuxData era)
  , DecCBOR (TxWits era)
  ) =>
  DecCBOR (ShelleyTxSeq era)
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
    pure $ TxSeq' txs hash bodiesBytes witsBytes auxDataBytes

deriving newtype instance DecCBOR (TxBody ShelleyEra)

deriving newtype instance DecCBOR (Tx ShelleyEra)

deriving newtype instance Era era => DecCBOR (ShelleyTxAuxData era)

instance (EraScript era, DecCBOR (Script era)) => DecCBOR (ShelleyTxWitsRaw era) where
  decCBOR =
    decode $
      SparseKeyed
        "ShelleyTxWits"
        (ShelleyTxWitsRaw mempty mempty mempty)
        witField
        []
    where
      witField :: Word -> Field (ShelleyTxWitsRaw era)
      witField 0 = field (\x wits -> wits {stwrAddrTxWits = x}) From
      witField 1 =
        field
          (\x wits -> wits {stwrScriptTxWits = x})
          (D $ Map.fromElems (hashScript @era) <$> decodeList decCBOR)
      witField 2 = field (\x wits -> wits {stwrBootAddrTxWits = x}) From
      witField n = invalidField n

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
