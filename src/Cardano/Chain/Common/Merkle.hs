{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Merkle tree implementation.
--
-- See <https://tools.ietf.org/html/rfc6962>.
module Cardano.Chain.Common.Merkle
  ( MerkleRoot(..)
  , MerkleTree(..)
  , mtRoot
  , mkMerkleTree
  , MerkleNode(..)
  , mkBranch
  , mkLeaf
  )
where

-- Cardano.Prelude has its own Rube Goldberg variant of 'Foldable' which we do not
-- want. It would be great if we could write
--   import           Cardano.Prelude hiding (toList, foldMap)
-- but HLint insists that this is not OK because toList and foldMap are never
-- used unqualified. The hiding in fact makes it clearer for the human reader
-- what's going on.
import Cardano.Prelude

import Data.Bits (Bits(..))
import Data.ByteArray (ByteArrayAccess, convert)
import Data.ByteString.Builder (Builder, byteString, word8)
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import qualified Data.Foldable as Foldable
import Formatting.Buildable (Buildable(..))
import qualified Prelude

import Cardano.Binary.Class (Bi(..), Raw, serializeBuilder)
import Cardano.Crypto (AbstractHash(..), Hash, hashRaw)

-- | Data type for root of merkle tree.
newtype MerkleRoot a = MerkleRoot
    { getMerkleRoot :: Hash Raw  -- ^ returns root 'Hash' of Merkle Tree
    } deriving (Show, Eq, Ord, Generic, ByteArrayAccess, NFData)

instance Buildable (MerkleRoot a) where
    build (MerkleRoot h) = "MerkleRoot|" <> build h

instance Bi a => Bi (MerkleRoot a) where
    encode = encode . getMerkleRoot
    decode = MerkleRoot <$> decode

-- | Straightforward merkle tree representation in Haskell.
data MerkleTree a = MerkleEmpty | MerkleTree !Word32 !(MerkleNode a)
    deriving (Eq, Generic)

instance NFData a => NFData (MerkleTree a)

instance Foldable MerkleTree where
    foldMap _ MerkleEmpty      = mempty
    foldMap f (MerkleTree _ n) = Foldable.foldMap f n

    null MerkleEmpty = True
    null _           = False

    length MerkleEmpty      = 0
    length (MerkleTree s _) = fromIntegral s

instance Show a => Show (MerkleTree a) where
  show tree = "Merkle tree: " <> show (Foldable.toList tree)

-- This instance is both faster and more space-efficient (as confirmed by a
-- benchmark). Hashing turns out to be faster than decoding extra data.
instance Bi a => Bi (MerkleTree a) where
    encode = encode . Foldable.toList
    decode = mkMerkleTree <$> decode

data MerkleNode a
    -- | MerkleBranch mRoot mLeft mRight
    = MerkleBranch !(MerkleRoot a) !(MerkleNode a) !(MerkleNode a)
    -- | MerkleLeaf mRoot mVal
    | MerkleLeaf !(MerkleRoot a) a
    deriving (Eq, Show, Generic)

instance NFData a => NFData (MerkleNode a)

instance Foldable MerkleNode where
    foldMap f x = case x of
        MerkleLeaf _ mVal             -> f mVal
        MerkleBranch _ mLeft mRight   ->
            Foldable.foldMap f mLeft `mappend` Foldable.foldMap f mRight

toLazyByteString :: Builder -> LBS.ByteString
toLazyByteString =
  Builder.toLazyByteStringWith (Builder.safeStrategy 1024 4096) mempty

mkLeaf :: forall a . Bi a => a -> MerkleNode a
mkLeaf a = MerkleLeaf mRoot a
 where
  mRoot :: MerkleRoot a
  mRoot = MerkleRoot $ coerce $ hashRaw
    (toLazyByteString (word8 0 <> serializeBuilder a))

mkBranch :: MerkleNode a -> MerkleNode a -> MerkleNode a
mkBranch nodeA nodeB = case (nodeA, nodeB) of
  (MerkleBranch mRootA _ _, MerkleBranch mRootB _ _) -> mkBranch' mRootA mRootB
  (MerkleBranch mRootA _ _, MerkleLeaf mRootB _    ) -> mkBranch' mRootA mRootB
  (MerkleLeaf mRootA _    , MerkleLeaf mRootB _    ) -> mkBranch' mRootA mRootB
  (MerkleLeaf mRootA _    , MerkleBranch mRootB _ _) -> mkBranch' mRootA mRootB
  where mkBranch' a b = MerkleBranch (mkRoot a b) nodeA nodeB

merkleRootToBuilder :: MerkleRoot a -> Builder
merkleRootToBuilder (MerkleRoot (AbstractHash d)) = byteString (convert d)

mkRoot :: MerkleRoot a -> MerkleRoot a -> MerkleRoot a
mkRoot a b = MerkleRoot $ coerce $ hashRaw $ toLazyByteString $ mconcat
  [word8 1, merkleRootToBuilder a, merkleRootToBuilder b]

-- | Smart constructor for 'MerkleTree'.
mkMerkleTree :: forall a . Bi a => [a] -> MerkleTree a
mkMerkleTree [] = MerkleEmpty
mkMerkleTree ls = MerkleTree (fromIntegral lsLen) (go lsLen ls)
 where
  lsLen = length ls
  go :: Int -> [a] -> MerkleNode a
  go _   [x] = mkLeaf x
  go len xs  = mkBranch (go i l) (go (len - i) r)
   where
    i      = powerOfTwo len
    (l, r) = splitAt i xs

-- | Returns root of merkle tree.
mtRoot :: MerkleTree a -> MerkleRoot a
mtRoot MerkleEmpty      = emptyHash
mtRoot (MerkleTree _ x) = case x of
  (MerkleBranch mRoot _ _) -> mRoot
  (MerkleLeaf mRoot _    ) -> mRoot

emptyHash :: MerkleRoot a
emptyHash = MerkleRoot (hashRaw mempty)

-- | Return the largest power of two such that it's smaller than X.
--
-- >>> powerOfTwo 64
-- 32
-- >>> powerOfTwo 65
-- 64
powerOfTwo :: forall a . (Bits a, Num a) => a -> a
powerOfTwo n
  | n .&. (n - 1) == 0 = n `shiftR` 1
  | otherwise          = go n
 where
    {- “x .&. (x - 1)” clears the least significant bit:

           ↓
       01101000     x
       01100111     x - 1
       --------
       01100000     x .&. (x - 1)

       I could've used something like “until (\x -> x*2 > w) (*2) 1”,
       but bit tricks are fun. -}
  go :: a -> a
  go w = if w .&. (w - 1) == 0 then w else go (w .&. (w - 1))
