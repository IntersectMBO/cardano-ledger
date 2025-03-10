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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.Block (
  Block (Block, UnserialisedBlock, UnsafeUnserialisedBlock),
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
  decodeRecordNamed,
  encodeListLen,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (
  Mem,
  MemoBytes (Memo),
  Memoized (..),
  decodeMemoized,
  getMemoRawType,
  mkMemoized,
 )
import Cardano.Ledger.TxIn (TxIn (..))
import Data.Foldable (toList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

data BlockRaw h era = BlockRaw
  { brHeader :: !h
  , brTxSeq :: !(TxSeq era)
  }
  deriving (Generic)

deriving stock instance
  (Era era, Show (TxSeq era), Show h) =>
  Show (BlockRaw h era)

deriving stock instance
  (Era era, Eq (TxSeq era), Eq h) =>
  Eq (BlockRaw h era)

deriving anyclass instance
  (Era era, NoThunks (TxSeq era), NoThunks h) =>
  NoThunks (BlockRaw h era)

instance
  ( Era era
  , EncCBOR h
  , EncCBORGroup (TxSeq era)
  ) =>
  EncCBOR (BlockRaw h era)
  where
  encCBOR cr@(BlockRaw _ _) =
    let BlockRaw {..} = cr
     in encodeListLen (1 + listLen brTxSeq) <> encCBOR brHeader <> encCBORGroup brTxSeq

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
      pure $ BlockRaw header txns
    where
      blockSize = 1 + fromIntegral (numSegComponents @era)

instance
  ( EraSegWits era
  , DecCBOR h
  , DecCBOR (TxSeq era)
  ) =>
  DecCBOR (Annotator (BlockRaw h era))
  where
  decCBOR = pure <$> decCBOR

newtype Block h era = BlockConstr (MemoBytes (BlockRaw h era))
  deriving (Generic)

instance Memoized (Block h era) where
  type RawType (Block h era) = BlockRaw h era

deriving newtype instance
  (Era era, Show (TxSeq era), Show h) =>
  Show (Block h era)

deriving newtype instance
  (Era era, Eq (TxSeq era), Eq h) =>
  Eq (Block h era)

deriving newtype instance
  (Era era, NoThunks (TxSeq era), NoThunks h, Typeable h) =>
  NoThunks (Block h era)

deriving newtype instance
  (EraTx era, Typeable h) =>
  Plain.ToCBOR (Block h era)

instance (EraTx era, Typeable h) => EncCBOR (Block h era)

deriving via
  Mem (BlockRaw h era)
  instance
    (EraSegWits era, DecCBOR h, DecCBOR (TxSeq era)) =>
    DecCBOR (Annotator (Block h era))

instance
  (EraSegWits era, DecCBOR h, DecCBOR (TxSeq era)) =>
  DecCBOR (Block h era)
  where
  decCBOR = BlockConstr <$> decodeMemoized decCBOR

pattern Block ::
  forall era h.
  ( Era era
  , EncCBORGroup (TxSeq era)
  , EncCBOR h
  ) =>
  h ->
  TxSeq era ->
  Block h era
pattern Block h txSeq <-
  (getMemoRawType -> BlockRaw h txSeq)
  where
    Block h txSeq =
      mkMemoized (eraProtVerLow @era) $ BlockRaw h txSeq
{-# COMPLETE Block #-}

-- | Access a block without its serialised bytes. This is often useful when
-- we're using a 'BHeaderView' in place of the concrete header.
pattern UnserialisedBlock ::
  ( Era era
  , EncCBORGroup (TxSeq era)
  , EncCBOR h
  ) =>
  h ->
  TxSeq era ->
  Block h era
pattern UnserialisedBlock h txns <- Block h txns

{-# COMPLETE UnserialisedBlock #-}

-- | Unsafely construct a block without the ability to serialise its bytes.
--
--   Anyone calling this pattern must ensure that the resulting block is never
--   serialised. Any uses of this pattern outside of testing code should be
--   regarded with suspicion.
pattern UnsafeUnserialisedBlock ::
  ( Era era
  , EncCBORGroup (TxSeq era)
  , EncCBOR h
  ) =>
  h ->
  TxSeq era ->
  Block h era
pattern UnsafeUnserialisedBlock h txns <-
  Block h txns
  where
    UnsafeUnserialisedBlock h txns =
      let bytes = error "`UnsafeUnserialisedBlock` used to construct a block which was later serialised."
       in BlockConstr (Memo (BlockRaw h txns) bytes)

{-# COMPLETE UnsafeUnserialisedBlock #-}

bheader ::
  ( Era era
  , EncCBORGroup (TxSeq era)
  , EncCBOR h
  ) =>
  Block h era ->
  h
bheader (Block bh _) = bh

bbody ::
  ( Era era
  , EncCBORGroup (TxSeq era)
  , EncCBOR h
  ) =>
  Block h era -> TxSeq era
bbody (Block _ txs) = txs

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
  ( EraSegWits era
  , EncCBOR h
  ) =>
  Block h era ->
  Set TxIn
neededTxInsForBlock (Block _ txsSeq) = Set.filter isNotNewInput allTxIns
  where
    txBodies = map (^. bodyTxL) $ toList $ fromTxSeq txsSeq
    allTxIns = Set.unions $ map (^. allInputsTxBodyF) txBodies
    newTxIds = Set.fromList $ map txIdTxBody txBodies
    isNotNewInput (TxIn txID _) = txID `Set.notMember` newTxIds
