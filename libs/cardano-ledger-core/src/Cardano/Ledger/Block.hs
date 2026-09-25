{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Block (
  Block (..),
  bheader,
  bbody,
  TPraosBbodySignal (..),
  PraosBbodySignal (..),
  LeiosBbodySignal (..),
  EraBlockHeader (..),
  TPraosEraBlockHeader,
  PraosEraBlockHeader (..),
  LeiosEraBlockHeader (..),
  BlockHeaderVersionInfo (..),
  neededTxInsForBlock,
) where

import Cardano.Ledger.BaseTypes (Nonce (..), ProtVer)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), decodeRecordNamed, encodeListLen)
import Cardano.Ledger.Core
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Slotting.Slot (SlotNo)
import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro (Lens', SimpleGetter, lens, (^.))
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

instance (NFData h, NFData (BlockBody era)) => NFData (Block h era)

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

data TPraosBbodySignal era
  = forall h. (TPraosEraBlockHeader h era, EraBlockHeader h era) => TPraosBbodySignal (Block h era)

data PraosBbodySignal era
  = forall h. (PraosEraBlockHeader h era, EraBlockHeader h era) => PraosBbodySignal (Block h era)

data LeiosBbodySignal era
  = forall h. (LeiosEraBlockHeader h era, EraBlockHeader h era) => LeiosBbodySignal (Block h era)

class Era era => EraBlockHeader h era where
  blockIssuerBlockHeaderG :: SimpleGetter (Block h era) (KeyHash BlockIssuer)
  blockHeaderSizeBlockHeaderG :: SimpleGetter (Block h era) Int
  blockBodySizeBlockHeaderL :: Lens' (Block h era) Word32
  blockBodyHashBlockHeaderL :: Lens' (Block h era) (Hash HASH EraIndependentBlockBody)
  slotNoBlockHeaderL :: Lens' (Block h era) SlotNo

class Era era => TPraosEraBlockHeader h era

class Era era => PraosEraBlockHeader h era where
  protVerBlockHeaderL :: Lens' (Block h era) ProtVer

class Era era => LeiosEraBlockHeader h era where
  -- TODO Peras related:
  --
  -- This interface is slightly off and will need to be ajdusted once Peras gets
  -- implemented. Previous Nonce is probably not going to be part of the block header, but that
  -- doesn't mean we cannot use this interface, it is just the "block header" naming will not be
  -- accurate. In any case, today this is not a problem and we can keep this stub here and tackle it
  -- as part of this ticket: https://github.com/IntersectMBO/cardano-ledger/issues/6098
  prevNonceBlockHeaderL :: Lens' (Block h era) Nonce
  prevNonceBlockHeaderL = lens (const NeutralNonce) (\b _ -> b)

  versionInfoBlockHeaderL :: Lens' (Block h era) BlockHeaderVersionInfo

-- | Version information reported by the block producer in the block header.
--
-- It has the same wire format as 'ProtVer', but neither field is validated upon decoding.
-- See <https://github.com/IntersectMBO/cardano-ledger/issues/5763>.
data BlockHeaderVersionInfo = BlockHeaderVersionInfo
  { bhviHighestSupportedMajorVersion :: !Word32
  -- ^ Highest major protocol version that the block producer is capable of hard forking into
  , bhviSelfReportedSoftwareTag :: !Word32
  -- ^ Arbitrary value chosen by the block producer's software; not interpreted by the ledger
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, NoThunks)

instance EncCBOR BlockHeaderVersionInfo where
  encCBOR (BlockHeaderVersionInfo major tag) = encodeListLen 2 <> encCBOR major <> encCBOR tag

instance DecCBOR BlockHeaderVersionInfo where
  decCBOR =
    decodeRecordNamed "BlockHeaderVersionInfo" (const 2) $
      BlockHeaderVersionInfo <$> decCBOR <*> decCBOR
