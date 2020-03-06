{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.BlockChain
  ( HashHeader(..)
  , BHBody(..)
  , BHeader(..)
  , Block(..)
  , ProtVer(..)
  , TxSeq(..)
  , bhHash
  , bbHash
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
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)
import           Shelley.Spec.Ledger.MetaData (MetaData)
import           Numeric.Natural (Natural)

import           Shelley.Spec.Ledger.BaseTypes (Nonce (..), Seed (..), UnitInterval, intervalValue, mkNonce)
import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeListLen, encodeListLen,
                     matchSize, serializeEncoding')
import           Cardano.Crypto.Hash (SHA256)
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.VRF.Class as VRF
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.Monad (unless)
import           Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..))
import           Shelley.Spec.Ledger.Keys (Hash, KESig, KeyHash, VKey, VRFValue (..), hash, hashKey, hashKeyVRF)
import           Shelley.Spec.Ledger.OCert (OCert (..))
import           Shelley.Spec.Ledger.PParams (ActiveSlotCoeff, activeSlotLog)
import           Shelley.Spec.Ledger.Serialization (CBORGroup (..), CBORMap (..), CborSeq (..), FromCBORGroup (..),
                     ToCBORGroup (..))
import           Shelley.Spec.NonIntegral (CompareResult (..), taylorExpCmp)
import           Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr (..))
import           Shelley.Spec.Ledger.Slot (BlockNo (..), SlotNo (..))
import           Shelley.Spec.Ledger.Tx (Tx (..), cborWitsToTx, txToCBORWits)

-- |The hash of a Block Header
newtype HashHeader crypto =
  HashHeader { unHashHeader :: (Hash (HASH crypto) (BHeader crypto)) }
  deriving (Show, Eq, Generic, Ord)

deriving instance Crypto crypto => ToCBOR (HashHeader crypto)
deriving instance Crypto crypto => FromCBOR (HashHeader crypto)

instance NoUnexpectedThunks (HashHeader crypto)

newtype TxSeq crypto
    = TxSeq (Seq (Tx crypto))
  deriving (Eq, Show)

instance Crypto crypto
  => ToCBORGroup (TxSeq crypto)
 where
  toCBORGroup (TxSeq txns) =
       toCBOR bodies
    <> toCBOR wits
    <> toCBOR metadata
      where
        bodies = CborSeq $ fmap _body txns
        wits = CborSeq $ fmap txToCBORWits txns
        metadata = extractMetaData txns
  listLen _ = 3

-- | Hash of block body
newtype HashBBody crypto =
  HashBBody { unHashBody :: (Hash (HASH crypto) (TxSeq crypto)) }
  deriving (Show, Eq, Ord, NoUnexpectedThunks)

deriving instance Crypto crypto => ToCBOR (HashBBody crypto)
deriving instance Crypto crypto => FromCBOR (HashBBody crypto)

-- |Hash a given block header
bhHash
  :: Crypto crypto
  => BHeader crypto
  -> HashHeader crypto
bhHash = HashHeader . Hash.hashWithSerialiser toCBORGroup

-- |Hash a given block body
bbHash
  :: forall crypto
   . Crypto crypto
  => TxSeq crypto
  -> HashBBody crypto
bbHash (TxSeq txns) = (HashBBody . coerce) $
  hash @(HASH crypto) (bodies <> wits <> md)
  where
    hashBytes :: forall a. ToCBOR a => a -> Hash.ByteString
    hashBytes = Hash.getHash . hash @(HASH crypto)
    bodies = hashBytes . CborSeq $ _body       <$> txns
    wits =   hashBytes . CborSeq $ txToCBORWits  <$> txns
    md =     hashBytes           $ extractMetaData txns

-- |HashHeader to Nonce
hashHeaderToNonce :: HashHeader crypto -> Nonce
hashHeaderToNonce = Nonce . coerce

data BHeader crypto
  = BHeader
      (BHBody crypto)
      (KESig crypto (BHBody crypto))
  deriving (Show, Generic, Eq)
  deriving ToCBOR via (CBORGroup (BHeader crypto))
  deriving FromCBOR via (CBORGroup (BHeader crypto))

instance Crypto crypto
  => NoUnexpectedThunks (BHeader crypto)

instance Crypto crypto
  => ToCBORGroup (BHeader crypto)
  where
    listLen (BHeader bHBody _kESig) = listLen bHBody + 1
    toCBORGroup (BHeader bHBody kESig) = toCBORGroup bHBody <> toCBOR kESig

instance Crypto crypto
  => FromCBORGroup (BHeader crypto)
 where
   fromCBORGroup = do
     bhb <- fromCBORGroup
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
    deriving ToCBOR via (CBORGroup (BHBody crypto))
    deriving FromCBOR via (CBORGroup (BHBody crypto))

instance Crypto crypto
  => NoUnexpectedThunks (BHBody crypto)

instance Crypto crypto
  => ToCBORGroup (BHBody crypto)
 where
  listLen bhBody =  9 + listLen (bheaderOCert bhBody)  + listLen (bprotvert bhBody)
  toCBORGroup bhBody =
         toCBOR (bheaderPrev bhBody)
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
  => FromCBORGroup (BHBody crypto)
 where
  fromCBORGroup = do
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

-- |Given a sequence of transactions, return a mapping
-- from indices in the original sequence to the non-Nothing metadata value
extractMetaData :: Seq (Tx crypto) -> CBORMap Int MetaData
extractMetaData txns =
  let metadata = Seq.mapWithIndex (\i -> \t -> (i, _metadata t)) txns
  in CBORMap $ ((Map.mapMaybe id) . Map.fromList . toList) metadata

-- |Given a size and a mapping from indices to maybe metadata,
-- return a sequence whose size is the size paramater and
-- whose non-Nothing values correspond no the values in the mapping.
constructMetaData :: Int -> Map Int MetaData -> Seq (Maybe MetaData)
constructMetaData n md = fmap (`Map.lookup` md) (Seq.fromList [0 .. n-1])

instance Crypto crypto
  => ToCBOR (Block crypto)
 where
  toCBOR (Block h txns) =
      encodeListLen (listLen h + listLen txns)
        <> toCBORGroup h
        <> toCBORGroup txns

instance Crypto crypto
  => FromCBOR (Block crypto)
 where
  fromCBOR = do
    n <- decodeListLen
    header <- fromCBORGroup
    matchSize "Block" ((fromIntegral . toInteger . listLen) header + 3) n
    bodies <- unwrapCborSeq <$> fromCBOR
    wits <- unwrapCborSeq <$> fromCBOR
    let b = length bodies
        w = length wits

    metadata <- constructMetaData b . unwrapCBORMap <$> fromCBOR
    let m = length metadata

    unless (b == w)
      (fail $ "different number of transaction bodies ("
        <> show b <> ") and witness sets ("
        <> show w <> ")"
      )
    unless (b == m)
      (fail $ "mismatch between transaction bodies ("
        <> show b <> ") and metadata ("
        <> show w <> ")"
      )
    let txns = Seq.zipWith3 cborWitsToTx bodies wits metadata
    pure $ Block header (TxSeq txns)

bHeaderSize
  :: forall crypto. (Crypto crypto)
  => BHeader crypto
  -> Int
bHeaderSize = BS.length . serializeEncoding' . toCBORGroup

bBodySize
  :: forall crypto. (Crypto crypto)
  => TxSeq crypto
  -> Int
bBodySize = BS.length . serializeEncoding' . toCBORGroup

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
  Seed . coerce $ hash @SHA256 (slot, nonce, lastHash)

vrfChecks
  ::  forall crypto
  . ( Crypto crypto
    , VRF.Signable (VRF crypto) Seed
    , VRF.ContextVRF (VRF crypto) ~ ()
    )
  => Nonce
  -> PoolDistr crypto
  -> ActiveSlotCoeff
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
            && checkVRFValue (VRF.certifiedNatural $ bheaderL bhb) sigma f
 where
  hk = hashKey $ bvkcold bhb
  vrfK = bheaderVrfVk bhb
  prevHash = bheaderPrev bhb
  slot = bheaderSlotNo bhb

-- | Check that the certified input natural is valid for being slot leader. This
-- means we check that
--
-- fromNat (certNat) < 1 - (1 - f)^σ
--
-- where fromNat creates an appropriate value in [0;1] from the certified
-- natural. The calculation is done using the following optimization:
--
-- let p = fromNat (certNat) and c = ln(1 - f)
--
-- then           p < 1 - (1 - f)^σ
-- <=>  1 / (1 - p) > exp(-σ * c)
--
-- this can be efficiently be computed by `taylorExpCmp` which returns `ABOVE`
-- in case the reference value `1 / (1 - p)` is above the exponential function
-- at `-σ * c`, `BELOW` if it is below or `MaxReached` if it couldn't
-- conclusively compute this within the given iteration bounds.
checkVRFValue :: Natural -> Rational -> ActiveSlotCoeff -> Bool
checkVRFValue certNat σ f =
  case taylorExpCmp 3 (1 / q) x of
    ABOVE _ _    -> False
    BELOW _ _    -> True
    MaxReached _ -> False
  where
    c = activeSlotLog f
    q = fromRational $ 1 - (intervalValue . fromNatural) certNat
    x = (- fromRational σ * c)

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
