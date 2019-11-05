{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module BlockChain
  ( HashHeader(..)
  , BHBody(..)
  , BHeader(..)
  , Block(..)
  , ProtVer(..)
  , TxSeq(..)
  , bhHash
  , bhbHash
  , bHeaderSize
  , bBodySize
  , slotToNonce
  , hBbsize
    -- accessor functions
  , bheader
  , bhbody
  , bbody
  , hsig
    --
  , slotsPrior
  , startRewards
  , seedEta
  , seedL
  , bvkcold
  , vrfChecks
  , incrBlocks
  , mkSeed
  )
where

import qualified Data.ByteString.Char8 as BS
import           Data.Coerce (coerce)
import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import           Data.Ratio (denominator, numerator)
import           Data.Sequence (Seq)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Cardano.Binary (ToCBOR (toCBOR), encodeListLen)
import           Cardano.Prelude (NoUnexpectedThunks(..))
import           Cardano.Crypto.Hash (SHA256)
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.VRF.Class as VRF

import           BaseTypes (Nonce (..), Seed(..), UnitInterval, intervalValue, mkNonce)
import           Delegation.Certificates (PoolDistr (..))
import           EpochBoundary (BlocksMade (..))
import           Keys (DSIGNAlgorithm, Hash, HashAlgorithm, KESAlgorithm, KESig, KeyHash,
                     VKey, VRFAlgorithm, VRFValue(..), hash, hashKey, hashKeyVRF)
import           OCert (OCert (..))
import           Slot (Duration, Slot (..), BlockNo(..))
import           Tx (Tx (..))

import           NonIntegral ((***))

-- |The hash of a Block Header
newtype HashHeader hashAlgo dsignAlgo kesAlgo vrfAlgo =
  HashHeader (Hash hashAlgo (BHeader hashAlgo dsignAlgo kesAlgo vrfAlgo))
  deriving (Show, Eq, Generic, Ord, ToCBOR)

instance NoUnexpectedThunks (HashHeader hashAlgo dsignAlgo kesAlgo vrfAlgo)

newtype TxSeq hashAlgo dsignAlgo vrfAlgo
    = TxSeq (Seq (Tx hashAlgo dsignAlgo vrfAlgo))
  deriving (Eq, Show)

instance
    ( HashAlgorithm hashAlgo
    , DSIGNAlgorithm dsignAlgo
    , VRFAlgorithm vrfAlgo
    ) =>
  ToCBOR (TxSeq hashAlgo dsignAlgo vrfAlgo)  where
  toCBOR (TxSeq s) = toCBOR $ toList s

-- | Hash of block body
newtype HashBBody hashAlgo dsignAlgo kesAlgo vrfAlgo =
  HashBBody (Hash hashAlgo (TxSeq hashAlgo dsignAlgo vrfAlgo))
  deriving (Show, Eq, Ord, NoUnexpectedThunks, ToCBOR)

-- |Hash a given block header
bhHash
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , KESAlgorithm kesAlgo
     , VRFAlgorithm vrfAlgo
     )
  => BHeader hashAlgo dsignAlgo kesAlgo vrfAlgo
  -> HashHeader hashAlgo dsignAlgo kesAlgo vrfAlgo
bhHash = HashHeader . hash

-- |Hash a given block body
bhbHash
  :: (HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , VRFAlgorithm vrfAlgo
     )
  => TxSeq hashAlgo dsignAlgo vrfAlgo
  -> HashBBody hashAlgo dsignAlgo kesAlgo vrfAlgo
bhbHash = HashBBody . hash

data BHeader hashAlgo dsignAlgo kesAlgo vrfAlgo
  = BHeader
      (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)
      (KESig kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo))
  deriving (Show, Generic, Eq)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , KESAlgorithm kesAlgo
  , VRFAlgorithm vrfAlgo
  )
  => NoUnexpectedThunks (BHeader hashAlgo dsignAlgo kesAlgo vrfAlgo)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , KESAlgorithm kesAlgo
  , VRFAlgorithm vrfAlgo
  )
  => ToCBOR (BHeader hashAlgo dsignAlgo kesAlgo vrfAlgo)
 where
   toCBOR (BHeader bHBody kESig) =
     encodeListLen 2
       <> toCBOR bHBody
       <> toCBOR kESig

data ProtVer = ProtVer Natural Natural Natural
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks ProtVer

instance ToCBOR ProtVer where
  toCBOR (ProtVer x y z) =
     encodeListLen 3
       <> toCBOR x
       <> toCBOR y
       <> toCBOR z

data BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo = BHBody
  { -- | Hash of the previous block header
    -- The first block in a chain will set this field to Nothing.
    -- TODO Since the Shelley chain will begins with blocks from
    -- the Byron era, we should probably use a sum type here,
    -- so that the first shelley block can point to the last Byron block.
    bheaderPrev           :: HashHeader hashAlgo dsignAlgo kesAlgo vrfAlgo
    -- | verification key of block issuer
  , bheaderVk             :: VKey dsignAlgo
    -- | VRF verification key for block issuer
  , bheaderVrfVk          :: VRF.VerKeyVRF vrfAlgo
    -- | block slot
  , bheaderSlot           :: Slot
    -- | block number
  , bheaderBlockNo        :: BlockNo
    -- | block nonce
  , bheaderEta            :: VRF.CertifiedVRF vrfAlgo Nonce
    -- | leader election value
  , bheaderL              :: VRF.CertifiedVRF vrfAlgo UnitInterval
    -- | Size of the block body
  , bsize                 :: Natural
    -- | Hash of block body
  , bhash                 :: HashBBody hashAlgo dsignAlgo kesAlgo vrfAlgo
    -- | operational certificate
  , bheaderOCert          :: OCert dsignAlgo kesAlgo
    -- | protocol version
  , bprotvert          :: ProtVer
  } deriving (Show, Eq, Generic)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , KESAlgorithm kesAlgo
  , VRFAlgorithm vrfAlgo
  )
  => NoUnexpectedThunks (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , KESAlgorithm kesAlgo
  , VRFAlgorithm vrfAlgo
  )
  => ToCBOR (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)
 where
  toCBOR bhBody =
    encodeListLen 10
      <> toCBOR (bheaderPrev bhBody)
      <> toCBOR (bheaderVk bhBody)
      <> VRF.encodeVerKeyVRF (bheaderVrfVk bhBody)
      <> toCBOR (bheaderSlot bhBody)
      <> toCBOR (bheaderEta bhBody)
      <> toCBOR (bheaderL bhBody)
      <> toCBOR (bsize bhBody)
      <> toCBOR (bhash bhBody)
      <> toCBOR (bheaderOCert bhBody)
      <> toCBOR (bprotvert bhBody)

data Block hashAlgo dsignAlgo kesAlgo vrfAlgo
  = Block
    (BHeader hashAlgo dsignAlgo kesAlgo vrfAlgo)
    (TxSeq hashAlgo dsignAlgo vrfAlgo)
  deriving (Show, Eq)

bHeaderSize
  :: (DSIGNAlgorithm dsignAlgo, KESAlgorithm kesAlgo, VRFAlgorithm vrfAlgo)
  => BHeader hashAlgo dsignAlgo kesAlgo vrfAlgo
  -> Int
bHeaderSize = BS.length . BS.pack . show

bBodySize :: DSIGNAlgorithm dsignAlgo => TxSeq hashAlgo dsignAlgo vrfAlgo -> Int
bBodySize (TxSeq txs) = sum (map (BS.length . BS.pack . show) $ toList txs)

slotToNonce :: Slot -> Nonce
slotToNonce (Slot s) = mkNonce (fromIntegral s)

bheader
  :: Block hashAlgo dsignAlgo kesAlgo vrfAlgo
  -> BHeader hashAlgo dsignAlgo kesAlgo vrfAlgo
bheader (Block bh _) = bh

bbody :: Block hashAlgo dsignAlgo kesAlgo vrfAlgo -> TxSeq hashAlgo dsignAlgo vrfAlgo
bbody (Block _ txs) = txs

bhbody
  :: BHeader hashAlgo dsignAlgo kesAlgo vrfAlgo
  -> BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo
bhbody (BHeader b _) = b

hsig
  :: BHeader hashAlgo dsignAlgo kesAlgo vrfAlgo
  -> KESig kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)
hsig (BHeader _ s) = s

slotsPrior :: Duration
slotsPrior = 33 -- one third of slots per epoch

startRewards :: Duration
startRewards = 33 -- see above

-- | Construct a seed to use in the VRF computation.
mkSeed
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , KESAlgorithm kesAlgo
     , VRFAlgorithm vrfAlgo
     )
  => Nonce -- ^ Universal constant
  -> Slot
  -> Nonce -- ^ Epoch nonce
  -> HashHeader hashAlgo dsignAlgo kesAlgo vrfAlgo
  -> Seed
mkSeed (Nonce uc) slot nonce lastHash =
  Seed . coerce $ uc `Hash.xor` coerce (hash @SHA256 (slot, nonce, lastHash))
mkSeed NeutralNonce slot nonce lastHash =
  Seed . coerce $ (hash @SHA256 (slot, nonce, lastHash))


vrfChecks
  ::  ( HashAlgorithm hashAlgo
      , DSIGNAlgorithm dsignAlgo
      , KESAlgorithm kesAlgo
      , VRFAlgorithm vrfAlgo
      , VRF.Signable vrfAlgo Seed
      )
  => Nonce
  -> PoolDistr hashAlgo dsignAlgo vrfAlgo
  -> UnitInterval
  -> BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo
  -> Bool
vrfChecks eta0 (PoolDistr pd) f bhb =
  let sigma' = Map.lookup hk pd
  in  case sigma' of
        Nothing -> False
        Just (sigma, vrfHK) ->
          vrfHK == hashKeyVRF vrfK
            && VRF.verifyCertified vrfK
                         (mkSeed seedEta slot eta0 prevHash)
                         (coerce $ bheaderEta bhb)
            && VRF.verifyCertified vrfK
                         (mkSeed seedL slot eta0 prevHash)
                         (coerce $ bheaderL bhb)
            && intervalValue (fromNatural . VRF.certifiedNatural $ bheaderL bhb)
            <  1
            -  ((1 - activeSlotsCoeff) *** fromRational sigma)
 where
  hk = hashKey $ bvkcold bhb
  vrfK = bheaderVrfVk bhb
  prevHash = bheaderPrev bhb
  slot = bheaderSlot bhb
  f' = intervalValue f
  activeSlotsCoeff =
    fromIntegral (numerator f') / fromIntegral (denominator f')

seedEta :: Nonce
seedEta = mkNonce 0

seedL :: Nonce
seedL = mkNonce 1

bvkcold :: BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo -> VKey dsignAlgo
bvkcold bhb = ocertVkCold $ bheaderOCert bhb

hBbsize :: BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo -> Natural
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
