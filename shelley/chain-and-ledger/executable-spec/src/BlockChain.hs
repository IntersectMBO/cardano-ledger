module BlockChain
  ( HashHeader(..)
  , BHBody(..)
  , BHeader(..)
  , Block(..)
  , bhHash
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
  ) where

import           Crypto.Hash           (Digest, SHA256, hash)
import qualified Data.ByteArray        as BA
import qualified Data.ByteString.Char8 as BS

import           BaseTypes
import qualified Keys                  as Keys
import           OCert
import qualified Slot                  as Slot
import qualified UTxO                  as U

-- |The hash of public Key
newtype HashHeader =
  HashHeader (Digest SHA256)
  deriving (Show, Eq, Ord)

-- |Hash a given block header
bhHash :: BHeader -> HashHeader
bhHash = HashHeader . hash

instance BA.ByteArrayAccess BHeader where
  length = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack . show

data Proof a =
  Proof Keys.VKey Seed
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

verifyVrf :: Keys.VKey -> Seed -> Proof a -> Bool
verifyVrf vk seed (Proof k s) = vk == k && seed == s

seedEta :: Seed
seedEta = mkNonce 0

seedL :: Seed
seedL = mkNonce 1
