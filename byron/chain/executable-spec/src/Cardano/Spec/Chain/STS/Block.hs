{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Cardano.Spec.Chain.STS.Block where

import           Control.Lens (makeLenses, view, (^.))
import           Data.AbstractSize
import           Data.ByteString (ByteString)
import qualified Data.Hashable as H
import qualified Data.Map.Strict as Map
import           Data.Sequence ((<|))
import           Data.Typeable (typeOf)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Control.State.Transition.Generator

import           Ledger.Core (Hash (Hash), Sig (Sig), Slot, VKey, hash, owner)
import           Ledger.Delegation
import           Ledger.Update (ProtVer, STag, UProp, Vote)
import           Ledger.UTxO (TxIn, TxOut, TxWits, Wit)

import           Test.Goblin (AddShrinks(..), Goblin(..))
import           Test.Goblin.TH (deriveAddShrinks, deriveGoblin)

data BlockHeader
  = BlockHeader
  { -- | Hash of the previous block header, or 'genesisHash' in case of
    -- the first block in a chain.
    _bhPrevHash :: !Hash
    -- | Absolute slot for which the block was generated.
  , _bhSlot :: !Slot
    -- | Block issuer.
  , _bhIssuer :: !VKey
    -- | Part of the block header which must be signed.
  , _bhSig :: !(Sig Hash)
    -- | UTxO hash
  , _bhUtxoHash :: !Hash
    -- | Delegation hash
  , _bhDlgHash :: !Hash
    -- | Update payload hash
  , _bhUpdHash :: !Hash
    -- TODO: BlockVersion – the protocol (block) version that created the block
    -- TODO: SoftwareVersion – the software version that created the block
  } deriving (Eq, Generic, Show)


makeLenses ''BlockHeader


-- We declare a specific instance here to avoid recursing into cardano-crypto
instance HasTypeReps BlockHeader where
  typeReps x = typeOf x
               <| typeOf (undefined :: Hash)
               <| typeOf (x ^. bhUtxoHash :: Hash)
               <| typeOf (x ^. bhDlgHash :: Hash)
               <| typeOf (x ^. bhUpdHash :: Hash)
               <| typeReps (x ^. bhSlot :: Slot)
               <> typeReps (x ^. bhIssuer :: VKey)
               <> typeReps (x ^. bhSig :: Sig Hash)


data BlockBody
  = BlockBody
  { _bDCerts :: ![DCert]
  -- ^ Delegation certificates
  , _bUtxo :: ![TxWits]
  -- ^ UTxO payload
  , _bUpdProp :: !(Maybe UProp)
  -- ^ Update proposal payload
  , _bUpdVotes :: ![Vote]
  -- ^ Update votes payload
  , _bProtVer :: !ProtVer
  -- ^ Protocol version
  } deriving (Generic, Show)


makeLenses ''BlockBody


instance HasTypeReps BlockBody


-- | A block in the chain. The specification only models regular blocks since
-- epoch boundary blocks will be largely ignored in the Byron-Shelley bridge.
data Block
  = Block
  { _bHeader :: BlockHeader
  , _bBody :: BlockBody
  } deriving (Generic, Show)


makeLenses ''Block


instance HasTypeReps Block


mkBlock
  :: Hash
  -- ^ Hash of the previous block
  -> Slot
  -- ^ Current slot
  -> VKey
  -- ^ Issuer
  -> ProtVer
  -- ^ Protocol version
  -> [DCert]
  -- ^ Delegation certificates
  -> Maybe UProp
  -- ^ Update proposal
  -> [Vote]
  -- ^ Votes on update proposals
  -> [TxWits]
  -- ^ UTxO payload
  -> Block
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
      unsignedHeader { _bhSig = Sig (hashHeader unsignedHeader) (owner issuer) }
      where
        unsignedHeader =
          BlockHeader
            { _bhPrevHash = prevHash
            , _bhSlot = currentSlot
            , _bhIssuer = issuer
            , _bhSig = dummySig
            , _bhUtxoHash = (hash utxoTransactions)
            , _bhDlgHash  = (hash delegationCerts)
            , _bhUpdHash = (hash (maybeUpdateProposal, updateProposalVotes))
            }
          where
            dummySig = Sig genesisHash (owner issuer)

    body =
      BlockBody
        { _bProtVer = version
        , _bDCerts = delegationCerts
        , _bUpdProp = maybeUpdateProposal
        , _bUpdVotes = updateProposalVotes
        , _bUtxo = utxoTransactions
        }

-- | Dummy genesis hash.
genesisHash :: Hash
-- Not sure we need a concrete hash in the specs ...
genesisHash = Hash $ H.hash ("" :: ByteString)


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
    costs = Map.fromList [ (typeOf (undefined::Maybe UProp), 1)
                         , (typeOf (undefined::STag), 1)
                         , (typeOf (undefined::ProtVer), 1)
                         , (typeOf (undefined::DCert), 1)
                         , (typeOf (undefined::Vote), 1)
                         , (typeOf (undefined::TxWits), 1)
                         , (typeOf (undefined::Wit), 1)
                         , (typeOf (undefined::TxIn), 1)
                         , (typeOf (undefined::TxOut), 1)]

-- | Compute the abstract size (in words) that a block header occupies.
bHeaderSize :: BlockHeader -> Natural
bHeaderSize = fromIntegral . abstractSize costs
  where
    costs = Map.fromList [ (typeOf (undefined::Hash), 1)
                         , (typeOf (undefined::Slot), 1)
                         , (typeOf (undefined::VKey), 1)
                         , (typeOf (undefined::Sig Hash), 1)]

-- | Computes the hash of a header.
hashHeader :: BlockHeader -> Hash
hashHeader bh = Hash $ H.hash (bh ^. bhPrevHash, bh ^. bhSlot, bh ^. bhIssuer)

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
updateBody
  :: Block
  -> (BlockBody -> BlockBody)
  -> Block
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
