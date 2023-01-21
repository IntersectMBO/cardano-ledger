{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  bheader,
  bbody,
  neededTxInsForBlock,
  txid,
)
where

import Cardano.Ledger.Binary (
  Annotator (..),
  FromCBOR (fromCBOR),
  ToCBOR (..),
  ToCBORGroup (..),
  annotatorSlice,
  decodeRecordNamed,
  encodeListLen,
  serializeEncoding,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Core
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

data Block h era
  = Block' !h !(TxSeq era) BSL.ByteString
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
  , ToCBORGroup (TxSeq era)
  , ToCBOR h
  ) =>
  h ->
  TxSeq era ->
  Block h era
pattern Block h txns <-
  Block' h txns _
  where
    Block h txns =
      let bytes =
            serializeEncoding (eraProtVerLow @era) $
              encodeListLen (1 + listLen txns) <> toCBOR h <> toCBORGroup txns
       in Block' h txns bytes

{-# COMPLETE Block #-}

-- | Access a block without its serialised bytes. This is often useful when
-- we're using a 'BHeaderView' in place of the concrete header.
pattern UnserialisedBlock ::
  h ->
  TxSeq era ->
  Block h era
pattern UnserialisedBlock h txns <- Block' h txns _

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
  Block' h txns _
  where
    UnsafeUnserialisedBlock h txns =
      let bytes = error "`UnsafeUnserialisedBlock` used to construct a block which was later serialised."
       in Block' h txns bytes

{-# COMPLETE UnsafeUnserialisedBlock #-}

instance (EraTx era, Typeable h) => ToCBOR (Block h era)

instance (EraTx era, Typeable h) => Plain.EncCBOR (Block h era) where
  encCBOR (Block' _ _ blockBytes) = Plain.encodePreEncoded $ BSL.toStrict blockBytes

instance
  forall h era.
  ( EraSegWits era
  , FromCBOR (Annotator h)
  , Typeable h
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
          + fromIntegral (numSegComponents @era)

bheader ::
  Block h era ->
  h
bheader (Block' bh _ _) = bh

bbody :: Block h era -> TxSeq era
bbody (Block' _ txs _) = txs

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
  Set (TxIn (EraCrypto era))
neededTxInsForBlock (Block' _ txsSeq _) = Set.filter isNotNewInput allTxIns
  where
    txBodies = map (^. bodyTxL) $ toList $ fromTxSeq txsSeq
    allTxIns = Set.unions $ map (^. allInputsTxBodyF) txBodies
    newTxIds = Set.fromList $ map txid txBodies
    isNotNewInput (TxIn txID _) = txID `Set.notMember` newTxIds

-- | Compute the id of a transaction.
txid :: EraTxBody era => TxBody era -> TxId (EraCrypto era)
txid = TxId . hashAnnotated
