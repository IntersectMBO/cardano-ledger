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
  ) where

import           Crypto.Hash           (Digest, SHA256, hash)
import qualified Data.ByteArray        as BA
import qualified Data.ByteString.Char8 as BS
import           Numeric.Natural       (Natural)
import           Data.Ratio
import qualified Data.Map.Strict       as Map

import           BaseTypes
import           Delegation.Certificates
import qualified Keys                  as Keys
import           OCert
import qualified Slot                  as Slot
import qualified UTxO                  as U

import           NonIntegral           ((***))

-- |The hash of a Block Header
newtype HashHeader =
  HashHeader (Digest SHA256)
  deriving (Show, Eq, Ord)

-- | Hash of block body
newtype HashBBody =
  HashBBody (Digest SHA256)
  deriving (Show, Eq, Ord)

-- |Hash a given block header
bhHash :: BHeader -> HashHeader
bhHash = HashHeader . hash

-- |Hash a given block header
bhbHash :: BHBody -> HashBBody
bhbHash = HashBBody . hash

instance BA.ByteArrayAccess BHeader where
  length = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack . show

instance BA.ByteArrayAccess BHBody where
  length = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack . show

-- | TODO: This is just a mock implementation wihtout too much insight into how
-- VRF actually will work.
class VrfProof a where
  toSeed :: a -> Seed

data Proof a =
  Proof Keys.VKey a
  deriving (Show, Eq)

data BHeader =
  BHeader BHBody
          (Keys.KESig BHBody)
  deriving (Show, Eq)

data BHBody = BHBody
  { -- | Hash of the previous block header
    bheaderPrev           :: HashHeader
    -- | verification key of block issuer
  , bheaderVk             :: Keys.VKey
    -- | block slo
  , bheaderSlot           :: Slot.Slot
    -- | block nonce
  , bheaderEta            :: Seed
    -- | proof of nonce
  , bheaderPrfEta         :: Proof Seed
    -- | leader election value
  , bheaderL              :: UnitInterval
    -- | proof of leader election
  , bheaderPrfL           :: Proof UnitInterval
    -- | signature of block body
  , bheaderBlockSignature :: Keys.Sig [U.Tx]
    -- | Size of the block body
  , bsize                 :: Natural
    -- | Hash of block body
  , bhash                 :: HashBBody
    -- | operational certificate
  , bheaderOCert          :: OCert
  } deriving (Show, Eq)

data Block =
  Block BHeader
        [U.Tx]
  deriving (Show, Eq)

bHeaderSize :: BHeader -> Int
bHeaderSize = BA.length . BS.pack . show

bBodySize :: [U.Tx] -> Int
bBodySize txs = foldl (+) 0 (map (BA.length . BS.pack . show) txs)

slotToSeed :: Slot.Slot -> Seed
slotToSeed (Slot.Slot s) = mkNonce (fromIntegral s)

bheader :: Block -> BHeader
bheader (Block bh _) = bh

bbody :: Block -> [U.Tx]
bbody (Block _ txs) = txs

bhbody :: BHeader -> BHBody
bhbody (BHeader b _) = b

hsig :: BHeader -> Keys.KESig BHBody
hsig (BHeader _ s) = s

slotsPrior :: Slot.Duration
slotsPrior = 33 -- one third of slots per epoch

startRewards :: Slot.Duration
startRewards = 33 -- see above

verifyVrf :: VrfProof a => Keys.VKey -> Seed -> Proof a -> Bool
verifyVrf vk seed (Proof k s) = vk == k && seed == toSeed s

instance VrfProof Seed where
  toSeed = id

instance VrfProof UnitInterval where
  toSeed u = mkNonce $ (numerator r * denominator r)
    where r = intervalValue u

vrfChecks :: Seed -> PoolDistr -> UnitInterval -> BHBody -> Bool
vrfChecks eta0 pd'@(PoolDistr pd) f bhb =
  let sigma' = Map.lookup hk pd in
    case sigma' of
      Nothing -> False
      Just sigma ->
          verifyVrf vk ((eta0 `seedOp` ss) `seedOp` SeedEta) (bheaderPrfEta bhb)
       && verifyVrf vk ((eta0 `seedOp` ss) `seedOp` SeedL) (bheaderPrfL bhb)
       && intervalValue (bheaderL bhb) <
           1 - ((1 - activeSlotsCoeff) *** (fromRational sigma))
  where vk = bvkcold bhb
        hk = Keys.hashKey vk
        ss = slotToSeed $ bheaderSlot bhb
        f' = intervalValue f
        activeSlotsCoeff =
          (fromIntegral $ numerator f') / (fromIntegral $ denominator f')

seedEta :: Seed
seedEta = mkNonce 0

seedL :: Seed
seedL = mkNonce 1

bvkcold :: BHBody -> Keys.VKey
bvkcold bhb = ocertVkCold $ bheaderOCert bhb
