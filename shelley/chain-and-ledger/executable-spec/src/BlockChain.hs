{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
import           Data.Ratio (denominator, numerator)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           MetaData (MetaData)
import           Numeric.Natural (Natural)

import           BaseTypes (Nonce (..), Seed (..), UnitInterval, intervalValue, invalidKey, mkNonce)
import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeListLen, decodeWord,
                     encodeListLen, matchSize)
import           Cardano.Crypto.Hash (SHA256)
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.VRF.Class as VRF
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.Monad (unless)
import           Delegation.Certificates (PoolDistr (..))
import           EpochBoundary (BlocksMade (..))
import           Keys (Hash, KESig, KeyHash, VKey, VRFValue (..), hash, hashKey, hashKeyVRF)
import           OCert (OCert (..))
import           Slot (BlockNo (..), SlotNo (..))
import           Tx (Tx (..), TxBody (..), WitVKey (..), hashScript)
import           TxData (MultiSig (..), ScriptHash (..))

import           NonIntegral ((***))
import           Serialization (CBORGroup (..), CBORMap (..), CborSeq (..), FromCBORGroup (..),
                     ToCBORGroup (..))

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
    wits =   hashBytes . CborSeq $ txToSegWit  <$> txns
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

-- The SegWits data type is only used for serializing and deserializing
-- blocks using a "segregated witness" format:
--
--   block =
--     [ header
--     , transaction_bodies         : [* transaction_body]
--     , transaction_witness_sets   : [* transaction_witness_set]
--     , transaction_metadata_set   : { * uint => transaction_metadata }
--     ]
--
--     and transaction_witness_set is serialized depending on what
--     values are present:
--
--  transaction_witness_set =
--    (  0, vkeywitness
--    // 1, $script
--    // 2, [* vkeywitness]
--    // 3, [* $script]
--    // 4, [* vkeywitness],[* $script]
--    )
data SegWits crypto
  = SegWitsVKey (WitVKey crypto)
  | SegWitsVKeys (Set (WitVKey crypto))
  | SegWitsScript (ScriptHash crypto) (MultiSig crypto)
  | SegWitsScripts (Map (ScriptHash crypto) (MultiSig crypto))
  | SegWitsAll (Set (WitVKey crypto)) (Map (ScriptHash crypto) (MultiSig crypto))
  deriving (Show, Eq)

segWitToTx
  :: (Crypto crypto)
  => (TxBody crypto, SegWits crypto, Maybe MetaData)
  -> Tx crypto
segWitToTx (body, (SegWitsVKey w), md) = Tx body (Set.singleton w) mempty md
segWitToTx (body, (SegWitsVKeys ws), md) = Tx body ws mempty md
segWitToTx (body, (SegWitsScript s m), md) = Tx body mempty (Map.singleton s m) md
segWitToTx (body, (SegWitsScripts ss), md) = Tx body mempty ss md
segWitToTx (body, (SegWitsAll ws ss), md) = Tx body ws ss md

txToSegWit
  :: (Crypto crypto)
  => Tx crypto
  -> SegWits crypto
txToSegWit (Tx _ ws ss _) =
  case (Set.toList ws, Map.toList ss) of
    ([x]      , [])        -> SegWitsVKey x
    (zs@(_:_), [])        -> SegWitsVKeys $ Set.fromList zs
    ([]       , [p])       -> uncurry SegWitsScript p
    ([]       , qs@(_:_)) -> SegWitsScripts $ Map.fromList qs
    (zs       , qs)        -> SegWitsAll (Set.fromList zs) (Map.fromList qs)

instance Crypto crypto
  => ToCBOR (SegWits crypto)
 where
  toCBOR = \case
    SegWitsVKey w ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR w

    SegWitsVKeys ws ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR ws

    SegWitsScript _ m ->
      encodeListLen 2
        <> toCBOR (2 :: Word8)
        <> toCBOR m

    SegWitsScripts ss ->
      encodeListLen 2
        <> toCBOR (3 :: Word8)
        <> toCBOR (Map.elems ss)

    SegWitsAll ws ss ->
      encodeListLen 3
        <> toCBOR (4 :: Word8)
        <> toCBOR ws
        <> toCBOR ss

instance Crypto crypto
  => FromCBOR (SegWits crypto)
 where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> do
        matchSize "SegWitsVKey" 2 n
        a <- fromCBOR
        pure $ SegWitsVKey a
      1 -> do
        a <- fromCBOR
        matchSize "SegWitsVKeys" 2 n
        pure $ SegWitsVKeys a
      2 -> do
        matchSize "SegWitsScript" 2 n
        a <- fromCBOR
        pure $ SegWitsScript (hashScript a) a
      3 -> do
        a <- fromCBOR
        matchSize "SegWitsScripts" 2 n
        (pure . SegWitsScripts . Map.fromList) $ fmap (\x -> (hashScript x, x)) a
      4 -> do
        a <- fromCBOR
        b <- fromCBOR
        matchSize "SegWitsAll" 3 n
        pure $ SegWitsAll a b
      k -> invalidKey k

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
  toCBOR (Block h (TxSeq txns)) =
      encodeListLen (listLen h + 3)
        <> toCBORGroup h
        <> toCBOR bodies
        <> toCBOR wits
        <> toCBOR metadata
      where
        bodies = CborSeq $ fmap _body txns
        wits = CborSeq $ fmap txToSegWit txns
        metadata = extractMetaData txns

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
    let txns = fmap segWitToTx (Seq.zip3 bodies wits metadata)
    pure $ Block header (TxSeq txns)


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
