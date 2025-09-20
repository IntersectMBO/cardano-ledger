{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Block (
  Block (..),
  bheader,
  bbody,
  neededTxInsForBlock,
) where

import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (decCBOR),
  EncCBOR (..),
  EncCBORGroup (..),
  decodeRecordNamed,
  encodeListLen,
  toPlainEncoding,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Core
import Cardano.Ledger.TxIn (TxIn (..))
import Data.Foldable (toList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

data Block h era = Block
  { blockHeader :: !h
  , blockBody :: !(BlockBody era)
  }
  deriving (Generic)

deriving stock instance
  (Era era, Show (BlockBody era), Show h) =>
  Show (Block h era)

deriving stock instance
  (Era era, Eq (BlockBody era), Eq h) =>
  Eq (Block h era)

deriving anyclass instance
  ( Era era
  , NoThunks (BlockBody era)
  , NoThunks h
  ) =>
  NoThunks (Block h era)

instance
  forall era h.
  ( Era era
  , EncCBORGroup (BlockBody era)
  , EncCBOR h
  ) =>
  EncCBOR (Block h era)
  where
  encCBOR (Block h txns) =
    encodeListLen (1 + listLen txns) <> encCBOR h <> encCBORGroup txns

instance
  forall era h.
  ( Era era
  , EncCBORGroup (BlockBody era)
  , EncCBOR h
  ) =>
  Plain.ToCBOR (Block h era)
  where
  toCBOR = toPlainEncoding (eraProtVerLow @era) . encCBOR

instance
  ( EraBlockBody era
  , DecCBOR (Annotator h)
  , Typeable h
  ) =>
  DecCBOR (Annotator (Block h era))
  where
  decCBOR = decodeRecordNamed "Block" (const blockSize) $ do
    header <- decCBOR
    txns <- decCBOR
    pure $ Block <$> header <*> txns
    where
      blockSize = 1 + fromIntegral (numSegComponents @era)

bheader ::
  Block h era ->
  h
bheader (Block bh _) = bh
{-# DEPRECATED bheader "In favor of `blockHeader`" #-}

bbody :: Block h era -> BlockBody era
bbody (Block _ txs) = txs
{-# DEPRECATED bbody "In favor of `blockBody`" #-}

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
  EraBlockBody era =>
  Block h era ->
  Set TxIn
neededTxInsForBlock Block {blockBody} = Set.filter isNotNewInput allTxIns
  where
    txBodies = map (^. bodyTxL) $ toList $ blockBody ^. txSeqBlockBodyL
    allTxIns = Set.unions $ map (^. allInputsTxBodyF) txBodies
    newTxIds = Set.fromList $ map txIdTxBody txBodies
    isNotNewInput (TxIn txId _) = txId `Set.notMember` newTxIds
