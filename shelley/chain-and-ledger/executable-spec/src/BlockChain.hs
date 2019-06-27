{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BlockChain
  ( HashHeader(..)
  , BHBody(..)
  , BHeader(..)
  , Block(..)
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

import qualified Data.ByteString.Char8         as BS
import           Numeric.Natural                ( Natural )
import           Data.Ratio
import qualified Data.Map.Strict               as Map

import           Cardano.Binary           (ToCBOR(toCBOR), encodeListLen)

import           BaseTypes
import           Delegation.Certificates
import           EpochBoundary
import           Keys
import           OCert
import qualified Slot
import qualified UTxO                          as U

import           NonIntegral                    ( (***) )

-- |The hash of a Block Header
newtype HashHeader hashAlgo dsignAlgo kesAlgo =
  HashHeader (Hash hashAlgo (BHeader hashAlgo dsignAlgo kesAlgo))
  deriving (Show, Eq, Ord, ToCBOR)

-- | Hash of block body
newtype HashBBody hashAlgo dsignAlgo kesAlgo =
  HashBBody (Hash hashAlgo [U.Tx hashAlgo dsignAlgo])
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
  => [U.Tx hashAlgo dsignAlgo]
  -> HashBBody hashAlgo dsignAlgo kesAlgo
bhbHash = HashBBody . hash

-- | TODO: This is just a mock implementation wihtout too much insight into how
-- VRF actually will work.
class VrfProof a where
  toSeed :: a -> Seed

data Proof dsignAlgo a =
  Proof (VKey dsignAlgo) a
  deriving (Show, Eq)

instance (DSIGNAlgorithm dsignAlgo, ToCBOR a) => ToCBOR (Proof dsignAlgo a) where
  toCBOR (Proof key a) =
    encodeListLen 2
      <> toCBOR key
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

data BHBody hashAlgo dsignAlgo kesAlgo = BHBody
  { -- | Hash of the previous block header
    bheaderPrev           :: HashHeader hashAlgo dsignAlgo kesAlgo
    -- | verification key of block issuer
  , bheaderVk             :: VKey dsignAlgo
    -- | block slot
  , bheaderSlot           :: Slot.Slot
    -- | block nonce
  , bheaderEta            :: Seed
    -- | proof of nonce
  , bheaderPrfEta         :: Proof dsignAlgo Seed
    -- | leader election value
  , bheaderL              :: UnitInterval
    -- | proof of leader election
  , bheaderPrfL           :: Proof dsignAlgo UnitInterval
    -- | signature of block body
  , bheaderBlockSignature :: Sig dsignAlgo [U.Tx hashAlgo dsignAlgo]
    -- | Size of the block body
  , bsize                 :: Natural
    -- | Hash of block body
  , bhash                 :: HashBBody hashAlgo dsignAlgo kesAlgo
    -- | operational certificate
  , bheaderOCert          :: OCert dsignAlgo kesAlgo
  } deriving (Show, Eq)

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, KESAlgorithm kesAlgo)
  => ToCBOR (BHBody hashAlgo dsignAlgo kesAlgo)
 where
  toCBOR body =
    encodeListLen 11
      <> toCBOR (bheaderPrev body)
      <> toCBOR (bheaderVk body)
      <> toCBOR (bheaderSlot body)
      <> toCBOR (bheaderEta body)
      <> toCBOR (bheaderPrfEta body)
      <> toCBOR (bheaderL body)
      <> toCBOR (bheaderPrfL body)
      <> toCBOR (bheaderBlockSignature body)
      <> toCBOR (bsize body)
      <> toCBOR (bhash body)
      <> toCBOR (bheaderOCert body)

data Block hashAlgo dsignAlgo kesAlgo
  = Block
    (BHeader hashAlgo dsignAlgo kesAlgo)
    [U.Tx hashAlgo dsignAlgo]
  deriving (Show, Eq)

bHeaderSize
  :: (DSIGNAlgorithm dsignAlgo, KESAlgorithm kesAlgo)
  => BHeader hashAlgo dsignAlgo kesAlgo
  -> Int
bHeaderSize = BS.length . BS.pack . show

bBodySize :: DSIGNAlgorithm dsignAlgo => [U.Tx hashAlgo dsignAlgo] -> Int
bBodySize txs = foldl (+) 0 (map (BS.length . BS.pack . show) txs)

slotToSeed :: Slot.Slot -> Seed
slotToSeed (Slot.Slot s) = mkNonce (fromIntegral s)

bheader :: Block hashAlgo dsignAlgo kesAlgo -> BHeader hashAlgo dsignAlgo kesAlgo
bheader (Block bh _) = bh

bbody :: Block hashAlgo dsignAlgo kesAlgo -> [U.Tx hashAlgo dsignAlgo]
bbody (Block _ txs) = txs

bhbody :: BHeader hashAlgo dsignAlgo kesAlgo -> BHBody hashAlgo dsignAlgo kesAlgo
bhbody (BHeader b _) = b

hsig
  :: BHeader hashAlgo dsignAlgo kesAlgo
  -> KESig kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo)
hsig (BHeader _ s) = s

slotsPrior :: Slot.Duration
slotsPrior = 33 -- one third of slots per epoch

startRewards :: Slot.Duration
startRewards = 33 -- see above

verifyVrf
  :: (DSIGNAlgorithm dsignAlgo, VrfProof a)
  => VKey dsignAlgo
  -> Seed
  -> Proof dsignAlgo a
  -> Bool
verifyVrf vk seed (Proof k s) = vk == k && seed == toSeed s

instance VrfProof Seed where
  toSeed = id

instance VrfProof UnitInterval where
  toSeed u = mkNonce $ (numerator r * denominator r)
    where r = intervalValue u

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
        Just sigma ->
          verifyVrf vk ((eta0 `seedOp` ss) `seedOp` SeedEta) (bheaderPrfEta bhb)
            && verifyVrf vk
                         ((eta0 `seedOp` ss) `seedOp` SeedL)
                         (bheaderPrfL bhb)
            && intervalValue (bheaderL bhb)
            <  1
            -  ((1 - activeSlotsCoeff) *** (fromRational sigma))
 where
  vk = bvkcold bhb
  hk = hashKey vk
  ss = slotToSeed $ bheaderSlot bhb
  f' = intervalValue f
  activeSlotsCoeff =
    (fromIntegral $ numerator f') / (fromIntegral $ denominator f')

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
  -> HashKey hashAlgo dsignAlgo
  -> BlocksMade hashAlgo dsignAlgo
  -> BlocksMade hashAlgo dsignAlgo
incrBlocks isOverlay hk b'@(BlocksMade b)
  | isOverlay = b'
  | otherwise = BlocksMade $ case hkVal of
    Nothing -> Map.insert hk 1 b
    Just n  -> Map.insert hk (n + 1) b
  where hkVal = Map.lookup hk b
