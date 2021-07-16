{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Byron.Spec.Chain.STS.Block where

import Byron.Spec.Ledger.Core (Hash (Hash), Sig (Sig), Slot, VKey, hash, owner)
import Byron.Spec.Ledger.Delegation
import Byron.Spec.Ledger.UTxO (Tx, TxIn, TxOut, Wit)
import Byron.Spec.Ledger.Update (ProtVer, STag, UProp, Vote)
import Control.State.Transition.Generator
import Data.AbstractSize
import Data.ByteString (ByteString)
import Data.Data (Data, Typeable)
import Data.Function (on)
import qualified Data.Hashable as H
import qualified Data.Map.Strict as Map
import Data.Sequence ((<|))
import Data.Typeable (typeOf)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)
import Numeric.Natural (Natural)
import Test.Goblin (AddShrinks (..), Goblin (..))
import Test.Goblin.TH (deriveAddShrinks, deriveGoblin)

data BlockHeader = BlockHeader
  { -- | Hash of the previous block header, or 'genesisHash' in case of
    -- the first block in a chain.
    _bhPrevHash :: !Hash,
    -- | Absolute slot for which the block was generated.
    _bhSlot :: !Slot,
    -- | Block issuer.
    _bhIssuer :: !VKey,
    -- | Part of the block header which must be signed.
    _bhSig :: !(Sig Hash),
    -- | UTxO hash
    _bhUtxoHash :: !Hash,
    -- | Delegation hash
    _bhDlgHash :: !Hash,
    -- | Update payload hash
    _bhUpdHash :: !Hash
    -- TODO: BlockVersion – the protocol (block) version that created the block
    -- TODO: SoftwareVersion – the software version that created the block
  }
  deriving (Eq, Generic, Show, Data, Typeable)

makeLenses ''BlockHeader

-- We declare a specific instance here to avoid recursing into cardano-crypto
instance HasTypeReps BlockHeader where
  typeReps x =
    typeOf x
      <| typeOf (undefined :: Hash)
      <| typeOf (x ^. bhUtxoHash :: Hash)
      <| typeOf (x ^. bhDlgHash :: Hash)
      <| typeOf (x ^. bhUpdHash :: Hash)
      <| typeReps (x ^. bhSlot :: Slot)
      <> typeReps (x ^. bhIssuer :: VKey)
      <> typeReps (x ^. bhSig :: Sig Hash)

data BlockBody = BlockBody
  { -- | Delegation certificates
    _bDCerts :: ![DCert],
    -- | UTxO payload
    _bUtxo :: ![Tx],
    -- | Update proposal payload
    _bUpdProp :: !(Maybe UProp),
    -- | Update votes payload
    _bUpdVotes :: ![Vote],
    -- | Protocol version
    _bProtVer :: !ProtVer
  }
  deriving (Generic, Show, Data, Typeable)

makeLenses ''BlockBody

instance HasTypeReps BlockBody

-- | A block in the chain. The specification only models regular blocks since
-- epoch boundary blocks will be largely ignored in the Byron-Shelley bridge.
data Block = Block
  { _bHeader :: BlockHeader,
    _bBody :: BlockBody
  }
  deriving (Generic, Show, Data, Typeable)

makeLenses ''Block

instance HasTypeReps Block

mkBlock ::
  -- | Hash of the previous block
  Hash ->
  -- | Current slot
  Slot ->
  -- | Issuer
  VKey ->
  -- | Protocol version
  ProtVer ->
  -- | Delegation certificates
  [DCert] ->
  -- | Update proposal
  Maybe UProp ->
  -- | Votes on update proposals
  [Vote] ->
  -- | UTxO payload
  [Tx] ->
  Block
mkBlock
  prevHash
  currentSlot
  issuer
  version
  delegationCerts
  maybeUpdateProposal
  updateProposalVotes
  utxoTransactions = Block signedHeader body
    where
      signedHeader =
        unsignedHeader {_bhSig = Sig (hashHeader unsignedHeader) (owner issuer)}
        where
          unsignedHeader =
            BlockHeader
              { _bhPrevHash = prevHash,
                _bhSlot = currentSlot,
                _bhIssuer = issuer,
                _bhSig = dummySig,
                _bhUtxoHash = (hash utxoTransactions),
                _bhDlgHash = (hash delegationCerts),
                _bhUpdHash = (hash (maybeUpdateProposal, updateProposalVotes))
              }
            where
              dummySig = Sig genesisHash (owner issuer)

      body =
        BlockBody
          { _bProtVer = version,
            _bDCerts = delegationCerts,
            _bUpdProp = maybeUpdateProposal,
            _bUpdVotes = updateProposalVotes,
            _bUtxo = utxoTransactions
          }

-- | Dummy genesis hash.
genesisHash :: Hash
-- Not sure we need a concrete hash in the specs ...
genesisHash = Hash $ Just $ H.hash ("" :: ByteString)

-- | Protocol version endorsment
bEndorsment :: Block -> (ProtVer, VKey)
bEndorsment b = (b ^. bBody ^. bProtVer, b ^. bHeader ^. bhIssuer)

-- | Slot the block is published in
bSlot :: Block -> Slot
bSlot b = b ^. bHeader ^. bhSlot

-- | Block update payload
bUpdPayload :: Block -> (Maybe UProp, [Vote])
bUpdPayload b = (b ^. bBody ^. bUpdProp, b ^. bBody ^. bUpdVotes)

-- | Compute the abstract size (in words) that a block takes.
bSize :: Block -> Natural
bSize b = bHeaderSize (b ^. bHeader) + bBodySize (b ^. bBody)

-- | Compute the abstract size (in words) that a block body occupies.
bBodySize :: BlockBody -> Natural
bBodySize = fromIntegral . abstractSize costs
  where
    costs =
      Map.fromList
        [ (typeOf (undefined :: Maybe UProp), 1),
          (typeOf (undefined :: STag), 1),
          (typeOf (undefined :: ProtVer), 1),
          (typeOf (undefined :: DCert), 1),
          (typeOf (undefined :: Vote), 1),
          (typeOf (undefined :: Tx), 1),
          (typeOf (undefined :: Wit), 1),
          (typeOf (undefined :: TxIn), 1),
          (typeOf (undefined :: TxOut), 1)
        ]

-- | Compute the abstract size (in words) that a block header occupies.
bHeaderSize :: BlockHeader -> Natural
bHeaderSize = fromIntegral . abstractSize costs
  where
    costs =
      Map.fromList
        [ (typeOf (undefined :: Hash), 1),
          (typeOf (undefined :: Slot), 1),
          (typeOf (undefined :: VKey), 1),
          (typeOf (undefined :: Sig Hash), 1)
        ]

-- | Computes the hash of a header.
hashHeader :: BlockHeader -> Hash
hashHeader bh = Hash $ Just $ H.hash (bh ^. bhPrevHash, bh ^. bhSlot, bh ^. bhIssuer)

-- | Computes the hash of the header.
bhToSign :: BlockHeader -> Hash
bhToSign = hashHeader

bhHash :: BlockHeader -> Hash
bhHash = hashHeader

-- | Checks if a block is an epoch boundary block.
--
-- The function always returns False because tests will be performed
-- only against chains without EBBs.
bIsEBB :: Block -> Bool
bIsEBB = const False

instance HasSizeInfo Block where
  isTrivial = null . view (bBody . bDCerts)

-- | Update a field of the block body, recomputing the hashes to get a valid
-- block.
updateBody ::
  Block ->
  (BlockBody -> BlockBody) ->
  Block
updateBody block bodyUpdate =
  mkBlock
    (_bhPrevHash . _bHeader $ block)
    (_bhSlot . _bHeader $ block)
    (_bhIssuer . _bHeader $ block)
    (_bProtVer newBody)
    (_bDCerts newBody)
    (_bUpdProp newBody)
    (_bUpdVotes newBody)
    (_bUtxo newBody)
  where
    newBody = bodyUpdate (_bBody block)

--------------------------------------------------------------------------------
-- Block statistics
--------------------------------------------------------------------------------

data BlockStats = BlockStats
  { -- | Number of regular transactions
    blockStatsUtxo :: Word,
    -- | Number of delegation certificates
    blockStatsDCerts :: Word,
    -- | Number of update votes
    blockStatsUpdVotes :: Word,
    -- | Number of update proposals
    --
    -- For a single block this will be 0 or 1.
    blockStatsUpdProp :: Word
  }
  deriving (Show)

-- | Count number of transactions in the block
--
-- Returns the number of
--
-- * Regular transactions
-- * Delegation certificates
-- * Update votes
-- * Update proposals (0 or 1)
blockStats :: Block -> BlockStats
blockStats (Block _header body) =
  BlockStats
    { blockStatsUtxo = fromIntegral . length $ _bUtxo body,
      blockStatsDCerts = fromIntegral . length $ _bDCerts body,
      blockStatsUpdVotes = fromIntegral . length $ _bUpdVotes body,
      blockStatsUpdProp = maybe 0 (const 1) $ _bUpdProp body
    }

-- | Block stats for an entire chain
--
-- Computes minimum, maximum, and average values.
--
-- Returns 'Nothing' for the empty chain.
chainBlockStats :: [BlockStats] -> Maybe (BlockStats, BlockStats, BlockStats)
chainBlockStats [] = Nothing
chainBlockStats (b : bs) = Just $ go b b b 1 bs
  where
    go ::
      BlockStats -> -- Minimum
      BlockStats -> -- Maximum
      BlockStats -> -- Sum
      Word -> -- Count
      [BlockStats] ->
      (BlockStats, BlockStats, BlockStats)
    go !sMin !sMax !sSum !cnt [] =
      ( sMin,
        sMax,
        BlockStats
          { blockStatsUtxo = blockStatsUtxo sSum `div` cnt,
            blockStatsDCerts = blockStatsDCerts sSum `div` cnt,
            blockStatsUpdVotes = blockStatsUpdVotes sSum `div` cnt,
            blockStatsUpdProp = blockStatsUpdProp sSum `div` cnt
          }
      )
    go !sMin !sMax !sSum !cnt (b' : bs') =
      go
        BlockStats
          { blockStatsUtxo = (min `on` blockStatsUtxo) sMin b',
            blockStatsDCerts = (min `on` blockStatsDCerts) sMin b',
            blockStatsUpdVotes = (min `on` blockStatsUpdVotes) sMin b',
            blockStatsUpdProp = (min `on` blockStatsUpdProp) sMin b'
          }
        BlockStats
          { blockStatsUtxo = (max `on` blockStatsUtxo) sMax b',
            blockStatsDCerts = (max `on` blockStatsDCerts) sMax b',
            blockStatsUpdVotes = (max `on` blockStatsUpdVotes) sMax b',
            blockStatsUpdProp = (max `on` blockStatsUpdProp) sMax b'
          }
        BlockStats
          { blockStatsUtxo = ((+) `on` blockStatsUtxo) sSum b',
            blockStatsDCerts = ((+) `on` blockStatsDCerts) sSum b',
            blockStatsUpdVotes = ((+) `on` blockStatsUpdVotes) sSum b',
            blockStatsUpdProp = ((+) `on` blockStatsUpdProp) sSum b'
          }
        (cnt + 1)
        bs'

--------------------------------------------------------------------------------
-- Goblins instances
--------------------------------------------------------------------------------

deriveGoblin ''Block
deriveGoblin ''BlockBody
deriveGoblin ''BlockHeader

--------------------------------------------------------------------------------
-- AddShrinks instances
--------------------------------------------------------------------------------

deriveAddShrinks ''Block
deriveAddShrinks ''BlockBody
deriveAddShrinks ''BlockHeader
