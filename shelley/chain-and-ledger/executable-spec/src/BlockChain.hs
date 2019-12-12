{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module BlockChain
  ( HashHeader(..)
  , BHBody(..)
  , BHeader(..)
  , Block(..)
  , ProtVer(..)
  , TxSeq(..)
  , bhHash
  , bhbHash
  , hashHeaderToNonce
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

import           BaseTypes (Nonce (..), Seed (..), UnitInterval, intervalValue, mkNonce)
import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeListLen, encodeListLen,
                     enforceSize, matchSize)
import           Cardano.Crypto.Hash (SHA256)
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.VRF.Class as VRF
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Delegation.Certificates (PoolDistr (..))
import           EpochBoundary (BlocksMade (..))
import           Keys (Hash, KESig, KeyHash, VKey, VRFValue (..), hash, hashKey, hashKeyVRF)
import           OCert (OCert (..))
import           Slot (BlockNo (..), Duration, SlotNo (..))
import           Tx (Tx (..))

import           NonIntegral ((***))
import           Serialization (CBORGroup (..), FromCBORGroup (..), ToCBORGroup (..))

-- |The hash of a Block Header
newtype HashHeader crypto =
  HashHeader (Hash (HASH crypto) (BHeader crypto))
  deriving (Show, Eq, Generic, Ord)

deriving instance Crypto crypto => ToCBOR (HashHeader crypto)
deriving instance Crypto crypto => FromCBOR (HashHeader crypto)

instance NoUnexpectedThunks (HashHeader crypto)

newtype TxSeq crypto
    = TxSeq (Seq (Tx crypto))
  deriving (Eq, Show)

instance Crypto crypto =>
  ToCBOR (TxSeq crypto)  where
  toCBOR (TxSeq s) = toCBOR $ toList s

-- | Hash of block body
newtype HashBBody crypto =
  HashBBody (Hash (HASH crypto) (TxSeq crypto))
  deriving (Show, Eq, Ord, NoUnexpectedThunks)

deriving instance Crypto crypto => ToCBOR (HashBBody crypto)
deriving instance Crypto crypto => FromCBOR (HashBBody crypto)

-- |Hash a given block header
bhHash
  :: Crypto crypto
  => BHeader crypto
  -> HashHeader crypto
bhHash = HashHeader . hash

-- |Hash a given block body
bhbHash
  :: Crypto crypto
  => TxSeq crypto
  -> HashBBody crypto
bhbHash = HashBBody . hash

-- |HashHeader to Nonce
hashHeaderToNonce :: HashHeader crypto -> Nonce
hashHeaderToNonce = Nonce . coerce

data BHeader crypto
  = BHeader
      (BHBody crypto)
      (KESig crypto (BHBody crypto))
  deriving (Show, Generic, Eq)

instance Crypto crypto
  => NoUnexpectedThunks (BHeader crypto)

instance Crypto crypto
  => ToCBOR (BHeader crypto)
 where
   toCBOR (BHeader bHBody kESig) =
     encodeListLen 2
       <> toCBOR bHBody
       <> toCBOR kESig

instance Crypto crypto
  => FromCBOR (BHeader crypto)
 where
   fromCBOR = do
     enforceSize "Block Header" 2
     bhb <- fromCBOR
     sig <- fromCBOR
     pure $ BHeader bhb sig

data ProtVer = ProtVer Natural Natural Natural
  deriving (Show, Eq, Generic, Ord)
  deriving ToCBOR via (CBORGroup ProtVer)

instance NoUnexpectedThunks ProtVer

instance ToCBORGroup ProtVer where
  toCBORGroup (ProtVer x y z) =
          toCBOR x
       <> toCBOR y
       <> toCBOR z
  listLen _ = 3

instance FromCBORGroup ProtVer where
  fromCBORGroup = do
    x <- fromCBOR
    y <- fromCBOR
    z <- fromCBOR
    pure $ ProtVer x y z

data BHBody crypto = BHBody
  { -- | Hash of the previous block header
    -- The first block in a chain will set this field to Nothing.
    -- TODO Since the Shelley chain will begins with blocks from
    -- the Byron era, we should probably use a sum type here,
    -- so that the first shelley block can point to the last Byron block.
    bheaderPrev           :: HashHeader crypto
    -- | verification key of block issuer
  , bheaderVk             :: VKey crypto
    -- | VRF verification key for block issuer
  , bheaderVrfVk          :: VRF.VerKeyVRF (VRF crypto)
    -- | block slot
  , bheaderSlotNo           :: SlotNo
    -- | block number
  , bheaderBlockNo        :: BlockNo
    -- | block nonce
  , bheaderEta            :: VRF.CertifiedVRF (VRF crypto) Nonce
    -- | leader election value
  , bheaderL              :: VRF.CertifiedVRF (VRF crypto) UnitInterval
    -- | Size of the block body
  , bsize                 :: Natural
    -- | Hash of block body
  , bhash                 :: HashBBody crypto
    -- | operational certificate
  , bheaderOCert          :: OCert crypto
    -- | protocol version
  , bprotvert          :: ProtVer
  } deriving (Show, Eq, Generic)

instance Crypto crypto
  => NoUnexpectedThunks (BHBody crypto)

instance Crypto crypto
  => ToCBOR (BHBody crypto)
 where
  toCBOR bhBody =
    encodeListLen (9 + listLen (bheaderOCert bhBody)  + listLen (bprotvert bhBody))
      <> toCBOR (bheaderPrev bhBody)
      <> toCBOR (bheaderVk bhBody)
      <> VRF.encodeVerKeyVRF (bheaderVrfVk bhBody)
      <> toCBOR (bheaderSlotNo bhBody)
      <> toCBOR (bheaderEta bhBody)
      <> toCBOR (bheaderL bhBody)
      <> toCBOR (bsize bhBody)
      <> toCBOR (bheaderBlockNo bhBody)
      <> toCBOR (bhash bhBody)
      <> toCBORGroup (bheaderOCert bhBody)
      <> toCBORGroup (bprotvert bhBody)

instance Crypto crypto
  => FromCBOR (BHBody crypto)
 where
  fromCBOR = do
    n <- decodeListLen
    bheaderPrev <- fromCBOR
    bheaderVk <- fromCBOR
    bheaderVrfVk <- VRF.decodeVerKeyVRF
    bheaderSlotNo <- fromCBOR
    bheaderEta <- fromCBOR
    bheaderL  <- fromCBOR
    bsize <- fromCBOR
    bheaderBlockNo <- fromCBOR
    bhash <- fromCBOR
    bheaderOCert <- fromCBORGroup
    bprotvert <- fromCBORGroup
    matchSize "Block header body" (fromIntegral $ 9 + listLen bheaderOCert + listLen bprotvert) n
    pure $ BHBody
           { bheaderPrev
           , bheaderVk
           , bheaderVrfVk
           , bheaderSlotNo
           , bheaderEta
           , bheaderL
           , bsize
           , bheaderBlockNo
           , bhash
           , bheaderOCert
           , bprotvert
           }

data Block crypto
  = Block
    (BHeader crypto)
    (TxSeq crypto)
  deriving (Show, Eq)

bHeaderSize
  :: ( Crypto crypto)
  => BHeader crypto
  -> Int
bHeaderSize = BS.length . BS.pack . show

bBodySize :: Crypto crypto => TxSeq crypto-> Int
bBodySize (TxSeq txs) = sum (map (BS.length . BS.pack . show) $ toList txs)

slotToNonce :: SlotNo -> Nonce
slotToNonce (SlotNo s) = mkNonce (fromIntegral s)

bheader
  :: Block crypto
  -> BHeader crypto
bheader (Block bh _) = bh

bbody :: Block crypto -> TxSeq crypto
bbody (Block _ txs) = txs

bhbody
  :: BHeader crypto
  -> BHBody crypto
bhbody (BHeader b _) = b

hsig
  :: BHeader crypto
  -> KESig crypto (BHBody crypto)
hsig (BHeader _ s) = s

slotsPrior :: Duration
slotsPrior = 33 -- one third of slots per epoch

startRewards :: Duration
startRewards = 33 -- see above

-- | Construct a seed to use in the VRF computation.
mkSeed
  :: Crypto crypto
  => Nonce -- ^ Universal constant
  -> SlotNo
  -> Nonce -- ^ Epoch nonce
  -> HashHeader crypto
  -> Seed
mkSeed (Nonce uc) slot nonce lastHash =
  Seed . coerce $ uc `Hash.xor` coerce (hash @SHA256 (slot, nonce, lastHash))
mkSeed NeutralNonce slot nonce lastHash =
  Seed . coerce $ (hash @SHA256 (slot, nonce, lastHash))


vrfChecks
  ::  forall crypto
  . ( Crypto crypto
    , VRF.Signable (VRF crypto) Seed
    , VRF.ContextVRF (VRF crypto) ~ ()
    )
  => Nonce
  -> PoolDistr crypto
  -> UnitInterval
  -> BHBody crypto
  -> Bool
vrfChecks eta0 (PoolDistr pd) f bhb =
  let sigma' = Map.lookup hk pd
  in  case sigma' of
        Nothing -> False
        Just (sigma, vrfHK) ->
          vrfHK == hashKeyVRF @crypto vrfK
            && VRF.verifyCertified () vrfK
                         (mkSeed seedEta slot eta0 prevHash)
                         (coerce $ bheaderEta bhb)
            && VRF.verifyCertified () vrfK
                         (mkSeed seedL slot eta0 prevHash)
                         (coerce $ bheaderL bhb)
            && intervalValue (fromNatural . VRF.certifiedNatural $ bheaderL bhb)
            <  1
            -  ((1 - activeSlotsCoeff) *** fromRational sigma)
 where
  hk = hashKey $ bvkcold bhb
  vrfK = bheaderVrfVk bhb
  prevHash = bheaderPrev bhb
  slot = bheaderSlotNo bhb
  f' = intervalValue f
  activeSlotsCoeff =
    fromIntegral (numerator f') / fromIntegral (denominator f')

seedEta :: Nonce
seedEta = mkNonce 0

seedL :: Nonce
seedL = mkNonce 1

bvkcold :: BHBody crypto -> VKey crypto
bvkcold bhb = ocertVkCold $ bheaderOCert bhb

hBbsize :: BHBody crypto -> Natural
hBbsize = bsize

incrBlocks
  :: Bool
  -> KeyHash crypto
  -> BlocksMade crypto
  -> BlocksMade crypto
incrBlocks isOverlay hk b'@(BlocksMade b)
  | isOverlay = b'
  | otherwise = BlocksMade $ case hkVal of
    Nothing -> Map.insert hk 1 b
    Just n  -> Map.insert hk (n + 1) b
  where hkVal = Map.lookup hk b
