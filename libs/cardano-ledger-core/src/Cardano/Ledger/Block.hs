{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Block (
  Block (Block, Block', UnserialisedBlock, UnsafeUnserialisedBlock),
  blockHeader,
  blockBody,
  blockMayAnnouncedEb,
  blockCertifiesEb,
  EbHash (..),
  Body (..),
  Certificate (..),
  bheader,
  bbody,
  neededTxInsForBlock,
  certByteSize,
  hashCert,
  hashBody,
  bodyBytesSize,
  bodyTxs,
)
where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.BaseTypes (ProtVer (ProtVer))
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (decCBOR),
  EncCBOR (..),
  EncCBORGroup (encCBORGroup),
  annotatorSlice,
  decodeRecordNamed,
  decodeTag,
  encodeListLen,
  encodeTag,
  serialize,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (MemoBytes (Memo), decodeMemoized)
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

newtype EbHash = EbHash {unEbHash :: BSL.ByteString}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks)
  deriving newtype (DecCBOR, EncCBOR)

newtype Certificate = Certificate {unCertificate :: BSL.ByteString}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks)
  deriving newtype (DecCBOR, EncCBOR)

certByteSize :: ProtVer -> Certificate -> Int
certByteSize (ProtVer v _) = fromIntegral . BSL.length . serialize v . encCBOR

hashCert :: Certificate -> Hash.Hash HASH EraIndependentBlockBody
hashCert = coerce $ Hash.hashWith BSL.toStrict . unCertificate

data Body era = BodyInline !(TxSeq era) | BodyCertificate Certificate (Maybe (TxSeq era))
  deriving stock (Generic)

deriving stock instance
  (Era era, Show (TxSeq era)) =>
  Show (Body era)

deriving stock instance
  (Era era, Eq (TxSeq era)) =>
  Eq (Body era)

deriving anyclass instance
  (Era era, NoThunks (TxSeq era)) =>
  NoThunks (Body era)

hashBody :: EraSegWits era => Body era -> Hash HASH EraIndependentBlockBody
hashBody (BodyInline txs) = hashTxSeq txs
hashBody (BodyCertificate cert _) = hashCert cert

bodyBytesSize :: EraSegWits era => ProtVer -> Body era -> Int
bodyBytesSize pv (BodyInline txs) = bBodySize pv txs
bodyBytesSize pv (BodyCertificate cert _) = certByteSize pv cert

bodyTxs :: Body era -> TxSeq era
bodyTxs (BodyInline txs) = txs
bodyTxs (BodyCertificate cert mayCertifiedEbTxClosure) = case mayCertifiedEbTxClosure of
  Nothing ->
    error $
      "Certificate body must include EB transaction closure (I was expecting this to be resolved) "
        <> show cert
  Just txs -> txs

data Block h era
  = Block'
  { blockHeader :: !h
  , blockBody :: !(Body era)
  , -- FIXME(bladyjoker): This goes into the header
    blockMayAnnouncedEb :: Maybe EbHash
  , blockCertifiesEb :: Bool
  , blockBytes :: BSL.ByteString
  }
  deriving (Generic)

deriving stock instance
  (Era era, Show (TxSeq era), Show h) =>
  Show (Block h era)

deriving stock instance
  (Era era, Eq (TxSeq era), Eq h) =>
  Eq (Block h era)

deriving anyclass instance
  ( Era era
  , NoThunks (TxSeq era)
  , NoThunks h
  ) =>
  NoThunks (Block h era)

pattern Block ::
  forall era h.
  ( Era era
  , EncCBOR (Body era)
  , EncCBORGroup (TxSeq era)
  , EncCBOR h
  ) =>
  h ->
  Body era ->
  Maybe EbHash ->
  Bool ->
  Block h era
pattern Block h body mayAnnouncedEb certifiesEb <-
  Block' h body mayAnnouncedEb certifiesEb _
  where
    Block h body mayAnnouncedEb certifiesEb =
      let bytes =
            serialize (eraProtVerLow @era) $
              encodeListLen 4
                <> encCBOR h
                <> encCBOR body
                <> encCBOR mayAnnouncedEb
                <> encCBOR certifiesEb
       in Block' h body mayAnnouncedEb certifiesEb bytes

{-# COMPLETE Block #-}

-- | Access a block without its serialised bytes. This is often useful when
-- we're using a 'BHeaderView' in place of the concrete header.
pattern UnserialisedBlock ::
  h ->
  Body era ->
  Block h era
pattern UnserialisedBlock h body <- Block' h body _ _ _

{-# COMPLETE UnserialisedBlock #-}

-- | Unsafely construct a block without the ability to serialise its bytes.
--
--   Anyone calling this pattern must ensure that the resulting block is never
--   serialised. Any uses of this pattern outside of testing code should be
--   regarded with suspicion.
pattern UnsafeUnserialisedBlock ::
  h ->
  Body era ->
  Block h era
pattern UnsafeUnserialisedBlock h body <-
  Block' h body _ _ _
  where
    UnsafeUnserialisedBlock h body =
      let bytes = error "`UnsafeUnserialisedBlock` used to construct a block which was later serialised."
       in Block' h body (error "FIXME(bladyjoker): annEb") (error "FIXME(bladyjoker): cert") bytes

{-# COMPLETE UnsafeUnserialisedBlock #-}

instance (EraTx era, Typeable h) => EncCBOR (Block h era)

instance (EraTx era, Typeable h) => Plain.ToCBOR (Block h era) where
  toCBOR = Plain.encodePreEncoded . BSL.toStrict . blockBytes

instance
  ( EraSegWits era
  , DecCBOR (Annotator h)
  , DecCBOR (Annotator (Body era))
  , Typeable h
  ) =>
  DecCBOR (Annotator (Block h era))
  where
  decCBOR = annotatorSlice $
    decodeRecordNamed "Block" (const blockSize) $ do
      header <- decCBOR
      body <- decCBOR
      mayAnnouncedEb <- decCBOR @(Maybe EbHash)
      certifiesEb <- decCBOR @Bool
      pure $ Block' <$> header <*> body <*> pure mayAnnouncedEb <*> pure certifiesEb
    where
      blockSize =
        1 -- header
          + 1 -- body -- NOTE(bladyjoker): This might be wrong
          + 1 -- announced EB
          + 1 -- certified EB

data BlockRaw h era = BlockRaw
  { _blockRawHeader :: !h
  , _blockRawBody :: !(Body era)
  , -- FIXME(bladyjoker): Same as for Block
    _blockRawMayAnnouncedEb :: Maybe EbHash
  , _blockRawUseCertifiedEb :: Bool
  }

instance
  ( EraSegWits era
  , DecCBOR h
  , DecCBOR (Body era)
  ) =>
  DecCBOR (BlockRaw h era)
  where
  decCBOR =
    decodeRecordNamed "Block" (const blockSize) $ do
      header <- decCBOR
      body <- decCBOR
      mayAnnouncedEb <- decCBOR
      certifiesEb <- decCBOR
      pure $ BlockRaw header body mayAnnouncedEb certifiesEb
    where
      blockSize =
        1 -- header
          + 1 -- body -- NOTE: Is this correct???
          + 1 -- announced EB
          + 1 -- certified EB

instance
  (DecCBOR (TxSeq era), EraSegWits era) =>
  DecCBOR (Body era)
  where
  decCBOR =
    decodeTag >>= \case
      0 -> BodyInline <$> decCBOR @(TxSeq era)
      1 -> (`BodyCertificate` Nothing) <$> decCBOR
      tag -> fail $ "Body: unknown tag " ++ show tag

instance
  EraSegWits era =>
  DecCBOR (Annotator (Body era))
  where
  decCBOR = annotatorSlice $ do
    tag <- decodeTag
    case tag of
      0 -> do
        txSeq <- decCBOR @(Annotator (TxSeq era))
        pure (const . BodyInline <$> txSeq)
      1 -> do
        cert <- decCBOR @Certificate
        pure $ pure ((const . BodyCertificate cert) Nothing)
      _ -> fail $ "Body: unknown tag " ++ show tag

instance (Typeable era, EncCBORGroup (TxSeq era)) => EncCBOR (Body era) where
  encCBOR = \case
    BodyInline txs -> encodeTag 0 <> encCBORGroup txs
    BodyCertificate cert _ -> encodeTag 1 <> encCBOR cert

instance
  ( EraSegWits era
  , DecCBOR h
  , DecCBOR (Body era)
  --  , DecCBOR (TxSeq era)
  ) =>
  DecCBOR (Block h era)
  where
  decCBOR = do
    Memo (BlockRaw h body mayAnnouncedEb certifiesEb) bs <-
      decodeMemoized (decCBOR @(BlockRaw h era))
    pure $ Block' h body mayAnnouncedEb certifiesEb (BSL.fromStrict (SBS.fromShort bs))

bheader :: Block h era -> h
bheader = blockHeader

bbody :: Block h era -> Body era
bbody = blockBody

-- | The validity of any individual block depends only on a subset
-- of the UTxO stored in the ledger state. This function returns
-- the transaction inputs corresponding to the required UTxO for a
-- given Block.
--
-- This function will be used by the consensus layer to enable storing
-- the UTxO on disk. In particular, given a block, the consensus layer
-- will use 'neededTxInsForBlock' to retrieve the needed UTxO from disk
-- and present only those to the ledger.
neededTxInsForBlock ::
  forall h era.
  EraSegWits era =>
  Block h era ->
  Set TxIn
neededTxInsForBlock (Block' _ (BodyInline txsSeq) _ _ _) = neededTxInsForTxs txsSeq
neededTxInsForBlock (Block' _ (BodyCertificate cert mayTxsSeq) _ _ _) = case mayTxsSeq of
  Nothing -> error $ "Must have EB transaction closure associated with a Certificate " <> show cert
  Just txsSeq -> neededTxInsForTxs txsSeq

neededTxInsForTxs ::
  forall era.
  EraSegWits era =>
  TxSeq era ->
  Set TxIn
neededTxInsForTxs txsSeq = Set.filter isNotNewInput allTxIns
  where
    txBodies = map (^. bodyTxL) $ toList $ fromTxSeq txsSeq
    allTxIns = Set.unions $ map (^. allInputsTxBodyF) txBodies
    newTxIds = Set.fromList $ map txIdTxBody txBodies
    isNotNewInput (TxIn txID _) = txID `Set.notMember` newTxIds
