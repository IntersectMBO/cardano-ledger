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
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Block
  ( Block (Block, Block', UnserialisedBlock, UnsafeUnserialisedBlock),
    BlockAnn,
    bheader,
    bbody,
    neededTxInsForBlock,
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    ToCBOR (..),
    annotatorSlice,
    encodeListLen,
    encodePreEncoded,
    serializeEncoding,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, ValidateScript (..))
import qualified Cardano.Ledger.Era as Era
import Cardano.Ledger.Serialization
  ( ToCBORGroup (..),
    decodeRecordNamed,
  )
import Cardano.Ledger.TxIn (TxIn (..), txid)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks (..))

data Block h era
  = Block' !(h (Crypto era)) !(Era.TxSeq era) BSL.ByteString
  deriving (Generic)

deriving stock instance
  (Era era, Show (Era.TxSeq era), Show (h (Crypto era))) =>
  Show (Block h era)

deriving stock instance
  (Era era, Eq (Era.TxSeq era), Eq (h (Crypto era))) =>
  Eq (Block h era)

deriving anyclass instance
  ( Era era,
    NoThunks (Era.TxSeq era),
    NoThunks (h (Crypto era))
  ) =>
  NoThunks (Block h era)

pattern Block ::
  ( Era era,
    ToCBORGroup (Era.TxSeq era),
    ToCBOR (h (Crypto era))
  ) =>
  h (Crypto era) ->
  Era.TxSeq era ->
  Block h era
pattern Block h txns <-
  Block' h txns _
  where
    Block h txns =
      let bytes =
            serializeEncoding $
              encodeListLen (1 + listLen txns) <> toCBOR h <> toCBORGroup txns
       in Block' h txns bytes

{-# COMPLETE Block #-}

-- | Access a block without its serialised bytes. This is often useful when
-- we're using a 'BHeaderView' in place of the concrete header.
pattern UnserialisedBlock ::
  h (Crypto era) ->
  Era.TxSeq era ->
  Block h era
pattern UnserialisedBlock h txns <- Block' h txns _

{-# COMPLETE UnserialisedBlock #-}

-- | Unsafely construct a block without the ability to serialise its bytes.
--
--   Anyone calling this pattern must ensure that the resulting block is never
--   serialised. Any uses of this pattern outside of testing code should be
--   regarded with suspicion.
pattern UnsafeUnserialisedBlock ::
  h (Crypto era) ->
  Era.TxSeq era ->
  Block h era
pattern UnsafeUnserialisedBlock h txns <-
  Block' h txns _
  where
    UnsafeUnserialisedBlock h txns =
      let bytes = error "`UnsafeUnserialisedBlock` used to construct a block which was later serialised."
       in Block' h txns bytes

{-# COMPLETE UnsafeUnserialisedBlock #-}

instance
  (Era era, Typeable h) =>
  ToCBOR (Block h era)
  where
  toCBOR (Block' _ _ blockBytes) = encodePreEncoded $ BSL.toStrict blockBytes

type BlockAnn era =
  ( FromCBOR (Annotator (Core.TxBody era)),
    FromCBOR (Annotator (Core.AuxiliaryData era)),
    FromCBOR (Annotator (Core.Witnesses era)),
    ToCBOR (Core.TxBody era),
    ToCBOR (Core.AuxiliaryData era),
    ToCBOR (Core.Witnesses era)
  )

instance
  forall h era.
  ( BlockAnn era,
    ValidateScript era,
    Era.SupportsSegWit era,
    FromCBOR (Annotator (Era.TxSeq era)),
    FromCBOR (Annotator (h (Crypto era))),
    Typeable h
  ) =>
  FromCBOR (Annotator (Block h era))
  where
  fromCBOR = annotatorSlice $
    decodeRecordNamed "Block" (const blockSize) $ do
      header <- fromCBOR
      txns <- fromCBOR
      pure $ Block' <$> header <*> txns
    where
      blockSize =
        1 -- header
          + fromIntegral (Era.numSegComponents @era)

bheader ::
  Block h era ->
  h (Crypto era)
bheader (Block' bh _ _) = bh

bbody :: Block h era -> Era.TxSeq era
bbody (Block' _ txs _) = txs

-- | The validity of any individual block depends only on a subset
-- of the UTxO stored in the ledger state. This function returns
-- the transaction inputs corresponding to the required UTxO for a
-- given Block.
--
-- This function will be used by the consensus layer to enable storing
-- the UTxO on disk. In particular, given a block, the consensus layer
-- will use 'neededTxInsForBlock' to retrived the needed UTxO from disk
-- and present only those to the ledger.
neededTxInsForBlock ::
  ( Era era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  Block h era ->
  Set (TxIn (Crypto era))
neededTxInsForBlock (Block' _ txsSeq _) = Set.filter isNotNewInput allTxIns
  where
    txBodies = map (getField @"body") $ toList $ Era.fromTxSeq txsSeq
    allTxIns = Set.unions $ map (getField @"inputs") txBodies
    newTxIds = Set.fromList $ map txid txBodies
    isNotNewInput (TxIn txID _) = txID `Set.notMember` newTxIds
