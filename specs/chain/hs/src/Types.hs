-- | Defines basic types for working with the ledger and the blockchain
module Types
  (
    VKey(..)
  , Sig
  , Data
  , HCert
  , Interf
  , BC
  , BlockIx(..)
  , Slot(..)
  , Block(..)
  )
 where

import           Data.Set (Set)
import           UTxO (Hash)


-- TODO: to be implemented
-- | A heavyweight delegation certificate
data HCert


newtype VKey = MkVKey Word deriving (Eq, Ord)
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

newtype Slot = MkSlot Word deriving (Eq, Ord)

newtype BlockIx = MkBlockIx Word deriving (Eq, Ord)


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
    , rbData   :: Data -- ^ Body of the block
    , rbSig    :: Sig -- ^ Cryptographic signature of the block
    }
