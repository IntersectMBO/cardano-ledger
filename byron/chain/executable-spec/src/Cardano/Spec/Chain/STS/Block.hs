{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Cardano.Spec.Chain.STS.Block where

import Control.Lens ((^.), makeLenses, view)
import Crypto.Hash (hashlazy)
import Data.AbstractSize
import Data.ByteString.Lazy.Char8 (pack)
import Data.Sequence ((<|))
import Data.Typeable (typeOf)
import Numeric.Natural (Natural)
import GHC.Generics (Generic)
import Control.State.Transition.Generator
import Ledger.Core (Hash, VKey, Slot, Sig)
import Ledger.Delegation
import Ledger.Update (ProtVer, UProp, Vote)
import Ledger.UTxO (TxWits, TxId)


data BlockHeader
  = MkBlockHeader
  {
    -- | Hash of the previous block header, or 'genesisHash' in case of
    -- the first block in a chain.
    _bhPrevHash :: Hash
    -- | Absolute slot for which the block was generated.
  , _bhSlot :: Slot
    -- | Block issuer.
  , _bhIssuer   :: VKey
    -- | Part of the block header which must be signed.
  , _bhSig      :: Sig Hash
    -- | UTxO hash
  , _bhUtxoHash :: Hash
    -- | Delegation hash
  , _bhDlgHash   :: Hash
    -- | Update payload hash
  , _bhUpdHash :: Hash
    -- TODO: BlockVersion – the protocol (block) version that created the block

    -- TODO: SoftwareVersion – the software version that created the block
  } deriving (Generic, Show)

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
  { _bDCerts     :: [DCert]
  -- ^ Delegation certificates
  , _bUtxo       :: [TxWits TxId]
  -- ^ UTxO payload
  , _bUpdProp    :: Maybe UProp
  -- ^ Update proposal payload
  , _bUpdVotes   :: [Vote]
  -- ^ Update votes payload
  , _bProtVer    :: ProtVer
  -- ^ Protocol version
  } deriving (Generic, Show)

instance HasTypeReps BlockBody

makeLenses ''BlockBody

-- | A block in the chain. The specification only models regular blocks since
-- epoch boundary blocks will be largely ignored in the Byron-Shelley bridge.
data Block
  = Block
  { _bHeader :: BlockHeader
  , _bBody :: BlockBody
  } deriving (Generic, Show)

instance HasTypeReps Block
makeLenses ''Block

-- | Block update payload
bUpdPayload :: Block -> (Maybe UProp, [Vote])
bUpdPayload b = (b ^. bBody ^. bUpdProp, b ^. bBody ^. bUpdVotes)


-- | Protocol version endorsment
bEndorsment :: Block -> (ProtVer, VKey)
bEndorsment b = (b ^. bBody ^. bProtVer, b ^. bHeader ^. bhIssuer)


-- | Compute the size (in words) that a block takes.
bSize :: Block -> Natural
bSize = fromIntegral . abstractSize acctMap
  where
    acctMap = []

-- | Compute the size (in words) that a block header.
bHeaderSize :: BlockHeader -> Natural
bHeaderSize = fromIntegral . abstractSize acctMap
  where
    acctMap = []

-- | Computes the hash of a header.
hashHeader :: BlockHeader -> Hash
hashHeader bh = hashlazy . pack $
  show (bh ^. bhPrevHash) ++
  show (bh ^. bhSlot    ) ++
  show (bh ^. bhIssuer  )
-- TODO: we might want to serialize this properly, without using show...

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
