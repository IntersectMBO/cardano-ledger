-- | Defines basic types for working with the ledger and the blockchain
module Types
  ( VKey(..)
  , Sig
  , Data
  , HCert
  , Interf
  , BC
  , BlockIx(..)
  , Slot(..)
  , ProtParams(..)
  , Block(..)
  )
where

import Data.Set (Set)
import Numeric.Natural
import UTxO (Hash)


-- TODO: to be implemented
-- | A heavyweight delegation certificate
data HCert


newtype VKey = MkVKey Natural deriving (Eq, Ord)
-- data VKeyGen -- not sure how to encode VKeyGen such that it is a subset
-- of the VKey type. Therefore, find some other way of ensuring this invariant.

-- | Abstract data
data Data

-- | Cryptographic signature of data
data Sig

-- | Phantom type for the delegation interface transition system
data Interf

-- | Phantom type for the blockchain extension transition system
data BC

newtype Slot = MkSlot Natural deriving (Eq, Ord)

newtype BlockIx = MkBlockIx Natural deriving (Eq, Ord)

data ProtParams = MkProtParams
  { maxBlockSize  :: !Natural
  , maxHeaderSize :: !Natural
  }

-- | Block type for two kinds of blocks: a genesis block and a
-- non-genesis block
data Block
  -- a genesis block
  = GBlock {
      gbKeys :: Set VKey
    , gbHash :: Hash -- ^ Hash of itself
    }
  -- a non-genesis block
  | RBlock {
      rbHash   :: Hash -- ^ Hash of the predecessor block
    , rbIx     :: BlockIx -- ^ Index of the block
    , rbSigner :: VKey -- ^ Block signer
    , rbCerts  :: Set HCert -- ^ New certificates posted to the blockchain
    , rbSl     :: Slot -- ^ Slot in which the block was issued
    , rbData   :: Data -- ^ Body of the block
    , rbSig    :: Sig -- ^ Cryptographic signature of the block
    , rbIsEBB  :: Bool -- ^ Indicates if this is an epoch boundary block
    }
