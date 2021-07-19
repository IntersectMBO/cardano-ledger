{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Merkle tree implementation.
--
-- See <https://tools.ietf.org/html/rfc6962>.
module Cardano.Chain.Common.Merkle
  ( -- * MerkleRoot
    MerkleRoot (..),

    -- * MerkleTree
    MerkleTree (..),
    mtRoot,
    mkMerkleTree,
    mkMerkleTreeDecoded,

    -- * MerkleNode
    MerkleNode (..),
    mkBranch,
    mkLeaf,
    mkLeafDecoded,
  )
where

-- Cardano.Prelude has its own Rube Goldberg variant of 'Foldable' which we do not
-- want. It would be great if we could write
--   import           Cardano.Prelude hiding (toList, foldMap)
-- but HLint insists that this is not OK because toList and foldMap are never
-- used unqualified. The hiding in fact makes it clearer for the human reader
-- what's going on.

import Cardano.Binary
  ( Annotated (..),
    FromCBOR (..),
    Raw,
    ToCBOR (..),
    serializeBuilder,
  )
import Cardano.Crypto (Hash, hashDecoded, hashRaw, hashToBytes)
import Cardano.Prelude
import Data.Aeson (ToJSON)
import Data.ByteString.Builder (Builder, byteString, word8)
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import qualified Data.Foldable as Foldable
import Formatting.Buildable (Buildable (..))
import NoThunks.Class (NoThunks (..))
import qualified Prelude

--------------------------------------------------------------------------------
-- MerkleRoot
--------------------------------------------------------------------------------

-- | Data type for root of Merkle tree
newtype MerkleRoot a = MerkleRoot
  { -- | returns root 'Hash' of Merkle Tree
    getMerkleRoot :: Hash Raw
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, NoThunks)

instance Buildable (MerkleRoot a) where
  build (MerkleRoot h) = "MerkleRoot|" <> build h

-- Used for debugging purposes only
instance ToJSON a => ToJSON (MerkleRoot a)

instance ToCBOR a => ToCBOR (MerkleRoot a) where
  toCBOR = toCBOR . getMerkleRoot
  encodedSizeExpr size = encodedSizeExpr size . fmap getMerkleRoot

instance FromCBOR a => FromCBOR (MerkleRoot a) where
  fromCBOR = MerkleRoot <$> fromCBOR

merkleRootToBuilder :: MerkleRoot a -> Builder
merkleRootToBuilder (MerkleRoot h) = byteString (hashToBytes h)

mkRoot :: MerkleRoot a -> MerkleRoot a -> MerkleRoot a
mkRoot a b =
  MerkleRoot . hashRaw . toLazyByteString $
    mconcat
      [word8 1, merkleRootToBuilder a, merkleRootToBuilder b]

emptyHash :: MerkleRoot a
emptyHash = MerkleRoot (hashRaw mempty)

--------------------------------------------------------------------------------
-- MerkleTree
--------------------------------------------------------------------------------

data MerkleTree a
  = MerkleEmpty
  | MerkleTree !Word32 !(MerkleNode a)
  deriving (Eq, Generic)
  deriving anyclass (NFData)

instance Foldable MerkleTree where
  foldMap _ MerkleEmpty = mempty
  foldMap f (MerkleTree _ n) = Foldable.foldMap f n

  null MerkleEmpty = True
  null _ = False

  length MerkleEmpty = 0
  length (MerkleTree s _) = fromIntegral s

instance Show a => Show (MerkleTree a) where
  show tree = "Merkle tree: " <> show (Foldable.toList tree)

-- This instance is both faster and more space-efficient (as confirmed by a
-- benchmark). Hashing turns out to be faster than decoding extra data.
instance ToCBOR a => ToCBOR (MerkleTree a) where
  toCBOR = toCBOR . Foldable.toList

instance (FromCBOR a, ToCBOR a) => FromCBOR (MerkleTree a) where
  fromCBOR = mkMerkleTree <$> fromCBOR

-- | Smart constructor for 'MerkleTree'
mkMerkleTree :: ToCBOR a => [a] -> MerkleTree a
mkMerkleTree = mkMerkleTree' (mkLeaf . getConst) . fmap Const

-- | Reconstruct a 'MerkleTree' from a decoded list of items
mkMerkleTreeDecoded :: [Annotated a ByteString] -> MerkleTree a
mkMerkleTreeDecoded = mkMerkleTree' mkLeafDecoded

mkMerkleTree' ::
  forall f a b. (f a b -> MerkleNode a) -> [f a b] -> MerkleTree a
mkMerkleTree' _ [] = MerkleEmpty
mkMerkleTree' leafBuilder ls = MerkleTree (fromIntegral lsLen) (go lsLen ls)
  where
    lsLen = length ls
    go :: Int -> [f a b] -> MerkleNode a
    go _ [x] = leafBuilder x
    go len xs = mkBranch (go i l) (go (len - i) r)
      where
        i = powerOfTwo len
        (l, r) = splitAt i xs

-- | Return the largest power of two such that it's smaller than X.
--
-- >>> powerOfTwo 64
-- 32
-- >>> powerOfTwo 65
-- 64
powerOfTwo :: forall a. (Bits a, Num a) => a -> a
powerOfTwo n
  | n .&. (n - 1) == 0 = n `shiftR` 1
  | otherwise = go n
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

-- | Returns root of Merkle tree
mtRoot :: MerkleTree a -> MerkleRoot a
mtRoot MerkleEmpty = emptyHash
mtRoot (MerkleTree _ n) = nodeRoot n

--------------------------------------------------------------------------------
-- MerkleNode
--------------------------------------------------------------------------------

data MerkleNode a
  = -- | MerkleBranch mRoot mLeft mRight
    MerkleBranch !(MerkleRoot a) !(MerkleNode a) !(MerkleNode a)
  | -- | MerkleLeaf mRoot mVal
    MerkleLeaf !(MerkleRoot a) a
  deriving (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Foldable MerkleNode where
  foldMap f x = case x of
    MerkleLeaf _ mVal -> f mVal
    MerkleBranch _ mLeft mRight ->
      Foldable.foldMap f mLeft `mappend` Foldable.foldMap f mRight

toLazyByteString :: Builder -> LBS.ByteString
toLazyByteString =
  Builder.toLazyByteStringWith (Builder.safeStrategy 1024 4096) mempty

nodeRoot :: MerkleNode a -> MerkleRoot a
nodeRoot (MerkleLeaf root _) = root
nodeRoot (MerkleBranch root _ _) = root

mkLeaf :: forall a. ToCBOR a => a -> MerkleNode a
mkLeaf a = MerkleLeaf mRoot a
  where
    mRoot :: MerkleRoot a
    mRoot =
      MerkleRoot $
        hashRaw
          (toLazyByteString (word8 0 <> serializeBuilder a))

mkLeafDecoded :: Annotated a ByteString -> MerkleNode a
mkLeafDecoded a = MerkleLeaf mRoot (unAnnotated a)
  where
    mRoot :: MerkleRoot a
    mRoot = MerkleRoot . coerce . hashDecoded $ prependTag <$> a

    prependTag = (LBS.toStrict (toLazyByteString (word8 0)) <>)

mkBranch :: MerkleNode a -> MerkleNode a -> MerkleNode a
mkBranch nodeA nodeB = MerkleBranch root nodeA nodeB
  where
    root = mkRoot (nodeRoot nodeA) (nodeRoot nodeB)
