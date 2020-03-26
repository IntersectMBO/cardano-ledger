{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Shelley.Spec.Ledger.BlockChain
  ( HashHeader(..)
  , PrevHash(..)
  , LastAppliedBlock(..)
  , lastAppliedHash
  , BHBody(..)
  , BHeader(BHeader)
  , Block(Block)
  , LaxBlock(..)
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
  , vrfChecks
  , incrBlocks
  , mkSeed
  , checkVRFValue
  )
where


import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Coerce (coerce)
import           Data.Foldable (toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)
import           Shelley.Spec.Ledger.MetaData (MetaData)

import           Cardano.Binary (Annotator (..), Decoder, FromCBOR (fromCBOR), ToCBOR (toCBOR),
                     TokenType (TypeNull), annotatorSlice, decodeListLen, decodeListLenOf,
                     decodeNull, encodeListLen, encodeNull, encodePreEncoded, matchSize,
                     peekTokenType, serialize', serializeEncoding, serializeEncoding')
import           Cardano.Crypto.Hash (SHA256)
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.VRF.Class as VRF
import           Cardano.Prelude (AllowThunksIn (..), LByteString, NoUnexpectedThunks (..))
import           Cardano.Slotting.Slot (WithOrigin (..))
import           Control.Monad (unless)
import           Shelley.Spec.Ledger.BaseTypes (Nonce (..), Seed (..), UnitInterval, intervalValue,
                     mkNonce)
import           Shelley.Spec.Ledger.Crypto
import           Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr (..))
import           Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..))
import           Shelley.Spec.Ledger.Keys (Hash, KESig, KeyHash, VKey, VRFValue (..), hash, hashKey,
                     hashKeyVRF)
import           Shelley.Spec.Ledger.OCert (OCert (..))
import           Shelley.Spec.Ledger.PParams (ActiveSlotCoeff, ProtVer (..), activeSlotLog,
                     activeSlotVal)
import           Shelley.Spec.Ledger.Serialization (CBORGroup (..), CborSeq (..),
                     FromCBORGroup (..), ToCBORGroup (..), mapFromCBOR)
import           Shelley.Spec.Ledger.Slot (BlockNo (..), SlotNo (..))
import           Shelley.Spec.Ledger.Tx (Tx (..), cborWitsToTx, txToCBORWits)
import           Shelley.Spec.NonIntegral (CompareResult (..), taylorExpCmp)

-- |The hash of a Block Header
newtype HashHeader crypto =
  HashHeader { unHashHeader :: (Hash (HASH crypto) (BHeader crypto)) }
  deriving (Show, Eq, Generic, Ord)

deriving instance Crypto crypto => ToCBOR (HashHeader crypto)
deriving instance Crypto crypto => FromCBOR (HashHeader crypto)

instance NoUnexpectedThunks (HashHeader crypto)

newtype TxSeq crypto
    = TxSeq (StrictSeq (Tx crypto))
  deriving (Eq, Show)

instance Crypto crypto
  => ToCBORGroup (TxSeq crypto)
 where
  toCBORGroup (TxSeq txns) =
       toCBOR bodies
    <> toCBOR wits
    <> toCBOR metadata
      where
        bodies = CborSeq $ StrictSeq.getSeq $ fmap _body txns
        wits = CborSeq $ StrictSeq.getSeq $ fmap txToCBORWits txns
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
bhHash = HashHeader . Hash.hashWithSerialiser toCBOR

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
    bodies = hashBytes . CborSeq . StrictSeq.getSeq $ _body       <$> txns
    wits =   hashBytes . CborSeq . StrictSeq.getSeq $ txToCBORWits  <$> txns
    md =     hashBytes           $ extractMetaData txns

-- |HashHeader to Nonce
hashHeaderToNonce :: HashHeader crypto -> Nonce
hashHeaderToNonce = Nonce . coerce

data BHeader crypto
  = BHeader' {
      bHeaderBody' :: !(BHBody crypto)
    , bHeaderSig' :: !(KESig crypto (BHBody crypto))
    , bHeaderBytes :: !LByteString
    }
  deriving (Generic)
  deriving NoUnexpectedThunks via
             AllowThunksIn '["bHeaderBytes"] (BHeader crypto)

instance Crypto crypto => Eq (BHeader crypto) where
  (BHeader a b) == (BHeader a' b') = (a == a') && (b == b')

instance Crypto crypto => Show (BHeader crypto) where
  showsPrec n b@(BHeader body sig)
    | n >= 10 = ('(':) .  showsPrec 0 b  . (')':)
    | otherwise = (<>) $ unwords ["BHeader ", showsPrec 10 body [], showsPrec 10 sig []]

pattern BHeader :: Crypto crypto => BHBody crypto -> KESig crypto (BHBody crypto) -> BHeader crypto
pattern BHeader bHeaderBody' bHeaderSig' <- BHeader' { bHeaderBody', bHeaderSig' }
  where
  BHeader body sig =
    let mkBytes bhBody kESig = serializeEncoding $
          encodeListLen (listLen bhBody + 1)
          <> toCBORGroup bhBody
          <> toCBOR kESig
     in BHeader' body sig (mkBytes body sig)

{-# COMPLETE BHeader #-}

instance Crypto crypto
  => ToCBOR (BHeader crypto)
  where
    toCBOR (BHeader' _ _ bytes) = encodePreEncoded (BSL.toStrict bytes)

instance Crypto crypto
  => FromCBOR (Annotator (BHeader crypto))
 where
   fromCBOR = annotatorSlice $ do
     n <- decodeListLen
     bhb <- fromCBORGroup
     sig <- fromCBOR
     matchSize "Header" ((fromIntegral . toInteger . listLen) bhb + 1) n
     pure $ BHeader' <$> pure bhb <*> pure sig

-- |The previous hash of a block
data PrevHash crypto = GenesisHash | BlockHash !(HashHeader crypto)
  deriving (Show, Eq, Generic, Ord)

instance Crypto crypto => NoUnexpectedThunks (PrevHash crypto)

instance Crypto crypto
  => ToCBOR (PrevHash crypto)
  where
    toCBOR GenesisHash = encodeNull
    toCBOR (BlockHash h) = toCBOR h

instance Crypto crypto
  => FromCBOR (PrevHash crypto)
 where
   fromCBOR = do
     peekTokenType >>= \case
       TypeNull -> do
         decodeNull
         pure GenesisHash
       _ -> BlockHash <$> fromCBOR

data LastAppliedBlock crypto = LastAppliedBlock {
     labBlockNo :: !BlockNo
   , labSlotNo  :: !SlotNo
   , labHash    :: !(HashHeader crypto)
   }
  deriving (Show, Eq, Generic)

instance Crypto crypto => NoUnexpectedThunks (LastAppliedBlock crypto)

instance Crypto crypto => ToCBOR (LastAppliedBlock crypto) where
  toCBOR (LastAppliedBlock b s h) =
      encodeListLen 3 <> toCBOR b <> toCBOR s <> toCBOR h

instance Crypto crypto => FromCBOR (LastAppliedBlock crypto) where
  fromCBOR = decodeListLenOf 3 >>
    LastAppliedBlock
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

lastAppliedHash :: WithOrigin (LastAppliedBlock crypto) -> PrevHash crypto
lastAppliedHash Origin   = GenesisHash
lastAppliedHash (At lab) = BlockHash $ labHash lab

data BHBody crypto = BHBody
  { -- | Hash of the previous block header
    bheaderPrev           :: !(PrevHash crypto)
    -- | verification key of block issuer
  , bheaderVk             :: !(VKey crypto)
    -- | VRF verification key for block issuer
  , bheaderVrfVk          :: !(VRF.VerKeyVRF (VRF crypto))
    -- | block slot
  , bheaderSlotNo         :: !SlotNo
    -- | block number
  , bheaderBlockNo        :: !BlockNo
    -- | block nonce
  , bheaderEta            :: !(VRF.CertifiedVRF (VRF crypto) Nonce)
    -- | leader election value
  , bheaderL              :: !(VRF.CertifiedVRF (VRF crypto) UnitInterval)
    -- | Size of the block body
  , bsize                 :: !Natural
    -- | Hash of block body
  , bhash                 :: !(HashBBody crypto)
    -- | operational certificate
  , bheaderOCert          :: !(OCert crypto)
    -- | protocol version
  , bprotver              :: !ProtVer
  } deriving (Show, Eq, Generic)
    deriving ToCBOR via (CBORGroup (BHBody crypto))
    deriving FromCBOR via (CBORGroup (BHBody crypto))

instance Crypto crypto
  => NoUnexpectedThunks (BHBody crypto)

instance Crypto crypto
  => ToCBORGroup (BHBody crypto)
 where
  listLen bhBody =  9 + listLen (bheaderOCert bhBody)  + listLen (bprotver bhBody)
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
      <> toCBORGroup (bprotver bhBody)

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
    bprotver <- fromCBORGroup
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
           , bprotver
           }

data Block crypto
  = Block' !(BHeader crypto) !(TxSeq crypto) !LByteString

instance Crypto crypto => Show (Block crypto) where
  showsPrec n b@(Block h txns)
    | n >= 10 = ('(':) . showsPrec 0 b . (')':)
    | otherwise = (<>) $ unwords [ "Block", showsPrec 10 h [], showsPrec 10 txns []]

instance Crypto crypto => Eq (Block crypto) where
  (Block h txns) == (Block h' txns') = (h == h') && (txns == txns')

pattern Block :: Crypto crypto => BHeader crypto -> TxSeq crypto -> Block crypto
pattern Block h txns <- Block' h txns _
  where
  Block h txns =
    let bytes = serializeEncoding $
          encodeListLen (1 + listLen txns) <> toCBOR h <> toCBORGroup txns
    in Block' h txns bytes

{-# COMPLETE Block #-}

-- |Given a sequence of transactions, return a mapping
-- from indices in the original sequence to the non-Nothing metadata value
extractMetaData :: StrictSeq (Tx crypto) -> Map Int MetaData
extractMetaData txns =
  let metadata =
          StrictSeq.toStrict
        . Seq.mapWithIndex (\i -> \t -> (i, _metadata t))
        . StrictSeq.getSeq
        $ txns
  in ((Map.mapMaybe id) . Map.fromList . toList) metadata

-- |Given a size and a mapping from indices to maybe metadata,
-- return a sequence whose size is the size paramater and
-- whose non-Nothing values correspond no the values in the mapping.
constructMetaData :: Int -> Map Int MetaData -> Seq (Maybe MetaData)
constructMetaData n md = fmap (`Map.lookup` md) (Seq.fromList [0 .. n-1])

instance Crypto crypto
  => ToCBOR (Block crypto)
 where
  toCBOR (Block' _ _ blockBytes) = encodePreEncoded $ BSL.toStrict blockBytes

blockDecoder :: Crypto crypto => Bool -> forall s. Decoder s (Annotator (Block crypto))
blockDecoder lax = annotatorSlice $ do
  n <- decodeListLen
  matchSize "Block" 4 n
  header <- fromCBOR
  bodies <- unwrapCborSeq <$> fromCBOR
  wits <- unwrapCborSeq <$> fromCBOR
  let b = length bodies
      w = length wits

  metadata <- constructMetaData b <$> mapFromCBOR
  let m = length metadata

  unless (lax || b == w)
      (fail $ "different number of transaction bodies ("
        <> show b <> ") and witness sets ("
        <> show w <> ")"
      )
  unless (lax || b == m)
      (fail $ "mismatch between transaction bodies ("
        <> show b <> ") and metadata ("
        <> show w <> ")"
      )
  let txns = Seq.zipWith3 cborWitsToTx bodies wits metadata
  pure $ Block' <$> header <*> pure (TxSeq (StrictSeq.toStrict txns))

instance Crypto crypto
  => FromCBOR (Annotator (Block crypto))
 where
  fromCBOR = blockDecoder False

newtype LaxBlock crypto
  = LaxBlock (Block crypto)
  deriving (Show, Eq)
  deriving ToCBOR via (Block crypto)

instance Crypto crypto
  => FromCBOR (Annotator (LaxBlock crypto))
 where
  fromCBOR = fmap LaxBlock <$> blockDecoder True

bHeaderSize
  :: forall crypto. (Crypto crypto)
  => BHeader crypto
  -> Int
bHeaderSize = BS.length . serialize'

bBodySize
  :: forall crypto. (Crypto crypto)
  => TxSeq crypto
  -> Int
bBodySize = BS.length . serializeEncoding' . toCBORGroup

slotToNonce :: SlotNo -> Nonce
slotToNonce (SlotNo s) = mkNonce (fromIntegral s)

bheader
  :: Crypto crypto => Block crypto
  -> BHeader crypto
bheader (Block bh _) = bh

bbody :: Crypto crypto => Block crypto -> TxSeq crypto
bbody (Block _ txs) = txs

bhbody
  :: Crypto crypto => BHeader crypto
  -> BHBody crypto
bhbody (BHeader b _) = b

hsig
  :: Crypto crypto => BHeader crypto
  -> KESig crypto (BHBody crypto)
hsig (BHeader _ s) = s

-- | Construct a seed to use in the VRF computation.
mkSeed
  :: Nonce -- ^ Universal constant
  -> SlotNo
  -> Nonce -- ^ Epoch nonce
  -> Seed
mkSeed (Nonce uc) slot nonce =
  Seed . coerce $ uc `Hash.xor` coerce (hash @SHA256 (slot, nonce))
mkSeed NeutralNonce slot nonce =
  Seed . coerce $ hash @SHA256 (slot, nonce)

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
                         (mkSeed seedEta slot eta0)
                         (coerce $ bheaderEta bhb)
            && VRF.verifyCertified () vrfK
                         (mkSeed seedL slot eta0)
                         (coerce $ bheaderL bhb)
            && checkVRFValue (VRF.certifiedNatural $ bheaderL bhb) sigma f
 where
  hk = hashKey $ bheaderVk bhb
  vrfK = bheaderVrfVk bhb
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
  if (intervalValue $ activeSlotVal f) == 1
    -- If the active slot coefficient is equal to one,
    -- then nearly every stake pool can produce a block every slot.
    -- In this degenerate case, where ln (1-f) is not defined,
    -- we let the VRF leader check always succeed.
    -- This is a testing convenience, the active slot coefficient should not
    -- bet set above one half otherwise.
  then True
  else if leaderVal == 1
    -- Having a VRF value of one is always a failure.
    -- Moreover, it would cause division by zero.
  then False
  else
    case taylorExpCmp 3 (1 / q) x of
      ABOVE _ _    -> False
      BELOW _ _    -> True
      MaxReached _ -> False
  where
    leaderVal = (intervalValue . fromNatural) certNat
    c = activeSlotLog f
    q = fromRational $ 1 - leaderVal
    x = (- fromRational σ * c)

seedEta :: Nonce
seedEta = mkNonce 0

seedL :: Nonce
seedL = mkNonce 1

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
