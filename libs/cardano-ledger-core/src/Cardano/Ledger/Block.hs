{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  blockTxs,
  blockMayAnnouncedEb,
  blockCertifiesEb,
  EbHash (..),
  bheader,
  bbody,
  neededTxInsForBlock,
)
where

import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (decCBOR),
  EncCBOR (..),
  EncCBORGroup (..),
  annotatorSlice,
  decodeRecordNamed,
  encodeListLen,
  serialize,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (MemoBytes (Memo), decodeMemoized)
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
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

-- FIXME(bladyjoker): Should be Either (TxSeq era) (Maybe EbHash)
data Block h era
  = Block'
  { blockHeader :: !h
  , blockTxs :: !(TxSeq era)
  , blockMayAnnouncedEb :: Maybe EbHash -- FIXME(bladyjoker): This goes into the header
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
  , EncCBORGroup (TxSeq era)
  , EncCBOR h
  ) =>
  h ->
  TxSeq era ->
  Maybe EbHash ->
  Bool ->
  Block h era
pattern Block h txns mayAnnouncedEb certifiesEb <-
  Block' h txns mayAnnouncedEb certifiesEb _
  where
    Block h txns mayAnnouncedEb certifiesEb =
      let bytes =
            serialize (eraProtVerLow @era) $
              encodeListLen (3 + listLen txns)
                <> encCBOR h
                <> encCBORGroup txns
                <> encCBOR mayAnnouncedEb
                <> encCBOR certifiesEb
       in Block' h txns mayAnnouncedEb certifiesEb bytes

{-# COMPLETE Block #-}

-- | Access a block without its serialised bytes. This is often useful when
-- we're using a 'BHeaderView' in place of the concrete header.
pattern UnserialisedBlock ::
  h ->
  TxSeq era ->
  Block h era
pattern UnserialisedBlock h txns <- Block' h txns _ _ _

{-# COMPLETE UnserialisedBlock #-}

-- | Unsafely construct a block without the ability to serialise its bytes.
--
--   Anyone calling this pattern must ensure that the resulting block is never
--   serialised. Any uses of this pattern outside of testing code should be
--   regarded with suspicion.
pattern UnsafeUnserialisedBlock ::
  h ->
  TxSeq era ->
  Block h era
pattern UnsafeUnserialisedBlock h txns <-
  Block' h txns _ _ _
  where
    UnsafeUnserialisedBlock h txns =
      let bytes = error "`UnsafeUnserialisedBlock` used to construct a block which was later serialised."
       in Block' h txns (error "FIXME(bladyjoker): annEb") (error "FIXME(bladyjoker): cert") bytes

{-# COMPLETE UnsafeUnserialisedBlock #-}

instance (EraTx era, Typeable h) => EncCBOR (Block h era)

instance (EraTx era, Typeable h) => Plain.ToCBOR (Block h era) where
  toCBOR = Plain.encodePreEncoded . BSL.toStrict . blockBytes

instance
  ( EraSegWits era
  , DecCBOR (Annotator h)
  , Typeable h
  ) =>
  DecCBOR (Annotator (Block h era))
  where
  decCBOR = annotatorSlice $
    decodeRecordNamed "Block" (const blockSize) $ do
      header <- decCBOR
      txns <- decCBOR
      mayAnnouncedEb <- decCBOR @(Maybe EbHash)
      certifiesEb <- decCBOR @Bool
      pure $ Block' <$> header <*> txns <*> pure mayAnnouncedEb <*> pure certifiesEb
    where
      blockSize =
        1 -- header
          + fromIntegral (numSegComponents @era)
          + 1 -- announced EB -- NOTE(bladyjoker): Not sure about this
          + 1 -- certified EB

data BlockRaw h era = BlockRaw
  { _blockRawHeader :: !h
  , _blockRawTxs :: !(TxSeq era)
  , -- FIXME(bladyjoker): Same as for Block
    _blockRawMayAnnouncedEb :: Maybe EbHash
  , _blockRawUseCertifiedEb :: Bool
  }

instance
  ( EraSegWits era
  , DecCBOR h
  , DecCBOR (TxSeq era)
  ) =>
  DecCBOR (BlockRaw h era)
  where
  decCBOR =
    decodeRecordNamed "Block" (const blockSize) $ do
      header <- decCBOR
      txns <- decCBOR
      mayAnnouncedEb <- decCBOR
      certifiesEb <- decCBOR
      pure $ BlockRaw header txns mayAnnouncedEb certifiesEb
    where
      blockSize =
        1 -- header
          + fromIntegral (numSegComponents @era)
          + 1 -- announced EB -- NOTE(bladyjoker): Not sure about this
          + 1 -- certified EB

instance
  ( EraSegWits era
  , DecCBOR h
  , DecCBOR (TxSeq era)
  ) =>
  DecCBOR (Block h era)
  where
  decCBOR = do
    Memo (BlockRaw h txSeq mayAnnouncedEb certifiesEb) bs <-
      decodeMemoized (decCBOR @(BlockRaw h era))
    pure $ Block' h txSeq mayAnnouncedEb certifiesEb (BSL.fromStrict (SBS.fromShort bs))

bheader :: Block h era -> h
bheader = blockHeader

bbody :: Block h era -> TxSeq era
bbody = blockTxs

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
neededTxInsForBlock (Block' _ txsSeq _ _ _) = Set.filter isNotNewInput allTxIns
  where
    txBodies = map (^. bodyTxL) $ toList $ fromTxSeq txsSeq
    allTxIns = Set.unions $ map (^. allInputsTxBodyF) txBodies
    newTxIds = Set.fromList $ map txIdTxBody txBodies
    isNotNewInput (TxIn txID _) = txID `Set.notMember` newTxIds
