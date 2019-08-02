{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BlockChain
  ( HashHeader(..)
  , BHBody(..)
  , BHeader(..)
  , Block(..)
  , Proof(..)
  , ProtVer(..)
  , TxSeq(..)
  , bhHash
  , bhbHash
  , bHeaderSize
  , bBodySize
  , slotToSeed
  , hBbsize
    -- accessor functions
  , bheader
  , bhbody
  , bbody
  , hsig
    --
  , slotsPrior
  , startRewards
  , verifyVrf
  , seedEta
  , seedL
  , bvkcold
  , vrfChecks
  , incrBlocks
  )
where

import qualified Data.ByteString.Char8 as BS (length, pack)
import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map (insert, lookup)
import           Data.Ratio (denominator, numerator)
import           Data.Sequence (Seq)
import           Numeric.Natural (Natural)

import           Cardano.Binary (ToCBOR (toCBOR), encodeListLen)

import           BaseTypes (Seed (..), UnitInterval, intervalValue, mkNonce, seedOp)
import           Delegation.Certificates (PoolDistr (..))
import           EpochBoundary (BlocksMade (..))
import           Keys (DSIGNAlgorithm, Hash, HashAlgorithm, KESAlgorithm, KESig, KeyHash, VKey,
                     hash, hashKey)
import           OCert (OCert (..))
import           Slot (Duration, Slot (..))
import           Tx (Tx (..))

import           NonIntegral ((***))

-- |The hash of a Block Header
newtype HashHeader hashAlgo dsignAlgo kesAlgo =
  HashHeader (Hash hashAlgo (BHeader hashAlgo dsignAlgo kesAlgo))
  deriving (Show, Eq, Ord, ToCBOR)

newtype TxSeq hashAlgo dsignAlgo = TxSeq (Seq (Tx hashAlgo dsignAlgo))
  deriving (Eq, Ord, Show)

instance (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo) =>
  ToCBOR (TxSeq hashAlgo dsignAlgo)  where
  toCBOR (TxSeq s) = toCBOR $ toList s

-- | Hash of block body
newtype HashBBody hashAlgo dsignAlgo kesAlgo =
  HashBBody (Hash hashAlgo (TxSeq hashAlgo dsignAlgo))
  deriving (Show, Eq, Ord, ToCBOR)

-- |Hash a given block header
bhHash
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, KESAlgorithm kesAlgo)
  => BHeader hashAlgo dsignAlgo kesAlgo
  -> HashHeader hashAlgo dsignAlgo kesAlgo
bhHash = HashHeader . hash

-- |Hash a given block body
bhbHash
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => TxSeq hashAlgo dsignAlgo
  -> HashBBody hashAlgo dsignAlgo kesAlgo
bhbHash = HashBBody . hash

data Proof dsignAlgo a =
  Proof (VKey dsignAlgo) Seed a
  deriving (Show, Eq)

instance (DSIGNAlgorithm dsignAlgo, ToCBOR a) => ToCBOR (Proof dsignAlgo a) where
  toCBOR (Proof key s a) =
    encodeListLen 3
      <> toCBOR key
      <> toCBOR s
      <> toCBOR a

data BHeader hashAlgo dsignAlgo kesAlgo
  = BHeader
      (BHBody hashAlgo dsignAlgo kesAlgo)
      (KESig kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo))
  deriving (Show, Eq)

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, KESAlgorithm kesAlgo)
  => ToCBOR (BHeader hashAlgo dsignAlgo kesAlgo)
 where
   toCBOR (BHeader bHBody kESig) =
     encodeListLen 2
       <> toCBOR bHBody
       <> toCBOR kESig

data ProtVer = ProtVer Natural Natural Natural
  deriving (Show, Eq, Ord)

instance ToCBOR ProtVer where
  toCBOR (ProtVer x y z) =
     encodeListLen 3
       <> toCBOR x
       <> toCBOR y
       <> toCBOR z

data BHBody hashAlgo dsignAlgo kesAlgo = BHBody
  { -- | Hash of the previous block header
    -- The first block in a chain will set this field to Nothing.
    -- TODO Since the Shelley chain will begins with blocks from
    -- the Byron era, we should probably use a sum type here,
    -- so that the first shelley block can point to the last Byron block.
    bheaderPrev           :: Maybe (HashHeader hashAlgo dsignAlgo kesAlgo)
    -- | verification key of block issuer
  , bheaderVk             :: VKey dsignAlgo
    -- | VRF verification key for block issuer
  , bheaderVrfVk          :: VKey dsignAlgo
    -- | block slot
  , bheaderSlot           :: Slot
    -- | block nonce
  , bheaderEta            :: Seed
    -- | proof of nonce
  , bheaderPrfEta         :: Proof dsignAlgo Seed
    -- | leader election value
  , bheaderL              :: UnitInterval
    -- | proof of leader election
  , bheaderPrfL           :: Proof dsignAlgo UnitInterval
    -- | Size of the block body
  , bsize                 :: Natural
    -- | Hash of block body
  , bhash                 :: HashBBody hashAlgo dsignAlgo kesAlgo
    -- | operational certificate
  , bheaderOCert          :: OCert dsignAlgo kesAlgo
    -- | protocol version
  , bprotvert          :: ProtVer
  } deriving (Show, Eq)

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, KESAlgorithm kesAlgo)
  => ToCBOR (BHBody hashAlgo dsignAlgo kesAlgo)
 where
  toCBOR bhBody =
    encodeListLen 12
      <> toCBOR (bheaderPrev bhBody)
      <> toCBOR (bheaderVk bhBody)
      <> toCBOR (bheaderVrfVk bhBody)
      <> toCBOR (bheaderSlot bhBody)
      <> toCBOR (bheaderEta bhBody)
      <> toCBOR (bheaderPrfEta bhBody)
      <> toCBOR (bheaderL bhBody)
      <> toCBOR (bheaderPrfL bhBody)
      <> toCBOR (bsize bhBody)
      <> toCBOR (bhash bhBody)
      <> toCBOR (bheaderOCert bhBody)
      <> toCBOR (bprotvert bhBody)

data Block hashAlgo dsignAlgo kesAlgo
  = Block
    (BHeader hashAlgo dsignAlgo kesAlgo)
    (TxSeq hashAlgo dsignAlgo)
  deriving (Show, Eq)

bHeaderSize
  :: (DSIGNAlgorithm dsignAlgo, KESAlgorithm kesAlgo)
  => BHeader hashAlgo dsignAlgo kesAlgo
  -> Int
bHeaderSize = BS.length . BS.pack . show

bBodySize :: DSIGNAlgorithm dsignAlgo => TxSeq hashAlgo dsignAlgo -> Int
bBodySize (TxSeq txs) = sum (map (BS.length . BS.pack . show) $ toList txs)

slotToSeed :: Slot -> Seed
slotToSeed (Slot s) = mkNonce (fromIntegral s)

bheader :: Block hashAlgo dsignAlgo kesAlgo -> BHeader hashAlgo dsignAlgo kesAlgo
bheader (Block bh _) = bh

bbody :: Block hashAlgo dsignAlgo kesAlgo -> TxSeq hashAlgo dsignAlgo
bbody (Block _ txs) = txs

bhbody :: BHeader hashAlgo dsignAlgo kesAlgo -> BHBody hashAlgo dsignAlgo kesAlgo
bhbody (BHeader b _) = b

hsig
  :: BHeader hashAlgo dsignAlgo kesAlgo
  -> KESig kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo)
hsig (BHeader _ s) = s

slotsPrior :: Duration
slotsPrior = 33 -- one third of slots per epoch

startRewards :: Duration
startRewards = 33 -- see above

verifyVrf
  :: (DSIGNAlgorithm dsignAlgo, Eq a)
  => VKey dsignAlgo
  -> Seed
  -> (a, Proof dsignAlgo a)
  -> Bool
verifyVrf vk seed (val, Proof k s v) = vk == k && seed == s && val == v

vrfChecks
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => Seed
  -> PoolDistr hashAlgo dsignAlgo
  -> UnitInterval
  -> BHBody hashAlgo dsignAlgo kesAlgo
  -> Bool
vrfChecks eta0 (PoolDistr pd) f bhb =
  let sigma' = Map.lookup hk pd
  in  case sigma' of
        Nothing -> False
        Just (sigma, vrfHK) ->
          vrfHK == hashKey vrfK
            && verifyVrf vrfK
                         ((eta0 `seedOp` ss) `seedOp` SeedEta)
                         (bheaderEta bhb, bheaderPrfEta bhb)
            && verifyVrf vrfK
                         ((eta0 `seedOp` ss) `seedOp` SeedL)
                         (bheaderL bhb, bheaderPrfL bhb)
            && intervalValue (bheaderL bhb)
            <  1
            -  ((1 - activeSlotsCoeff) *** fromRational sigma)
 where
  hk = hashKey $ bvkcold bhb
  vrfK = bheaderVrfVk bhb
  ss = slotToSeed $ bheaderSlot bhb
  f' = intervalValue f
  activeSlotsCoeff =
    fromIntegral (numerator f') / fromIntegral (denominator f')

seedEta :: Seed
seedEta = mkNonce 0

seedL :: Seed
seedL = mkNonce 1

bvkcold :: BHBody hashAlgo dsignAlgo kesAlgo -> VKey dsignAlgo
bvkcold bhb = ocertVkCold $ bheaderOCert bhb

hBbsize :: BHBody hashAlgo dsignAlgo kesAlgo -> Natural
hBbsize = bsize

incrBlocks
  :: Bool
  -> KeyHash hashAlgo dsignAlgo
  -> BlocksMade hashAlgo dsignAlgo
  -> BlocksMade hashAlgo dsignAlgo
incrBlocks isOverlay hk b'@(BlocksMade b)
  | isOverlay = b'
  | otherwise = BlocksMade $ case hkVal of
    Nothing -> Map.insert hk 1 b
    Just n  -> Map.insert hk (n + 1) b
  where hkVal = Map.lookup hk b
