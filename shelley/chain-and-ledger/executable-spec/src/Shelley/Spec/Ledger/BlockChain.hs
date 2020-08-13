{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.BlockChain
  ( HashHeader (..),
    PrevHash (..),
    LastAppliedBlock (..),
    lastAppliedHash,
    BHBody (..),
    poolIDfromBHBody,
    BHeader (BHeader),
    Block (Block),
    LaxBlock (..),
    TxSeq (TxSeq, txSeqTxns'),
    HashBBody,
    bhHash,
    bbHash,
    hashHeaderToNonce,
    prevHashToNonce,
    bHeaderSize,
    bBodySize,
    slotToNonce,
    hBbsize,
    -- accessor functions
    bheader,
    bhbody,
    bbody,
    hsig,
    --
    seedEta,
    seedL,
    incrBlocks,
    mkSeed,
    checkLeaderValue,
  )
where

import Cardano.Binary
  ( Annotator (..),
    Case (..),
    Decoder,
    FromCBOR (fromCBOR),
    ToCBOR (..),
    TokenType (TypeNull),
    annotatorSlice,
    decodeNull,
    encodeListLen,
    encodeNull,
    encodePreEncoded,
    peekTokenType,
    serialize',
    serializeEncoding,
    serializeEncoding',
    szCases,
    withSlice,
    withWordSize,
  )
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation (..))
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Prelude
  ( AllowThunksIn (..),
    ByteString,
    LByteString,
    NFData,
    NoUnexpectedThunks (..),
  )
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Monad (unless)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Word (Word64)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes
  ( ActiveSlotCoeff,
    FixedPoint,
    Nonce (..),
    Seed (..),
    activeSlotLog,
    activeSlotVal,
    intervalValue,
    mkNonceFromNumber,
    strictMaybeToMaybe,
  )
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..))
import Shelley.Spec.Ledger.Hashing (HashAnnotated (..))
import Shelley.Spec.Ledger.Keys
  ( CertifiedVRF,
    Hash,
    KeyHash,
    KeyRole (..),
    SignedKES,
    VKey,
    VerKeyVRF,
    decodeSignedKES,
    decodeVerKeyVRF,
    encodeSignedKES,
    encodeVerKeyVRF,
    hashKey,
  )
import Shelley.Spec.Ledger.OCert (OCert (..))
import Shelley.Spec.Ledger.PParams (ProtVer (..))
import Shelley.Spec.Ledger.Serialization
  ( FromCBORGroup (..),
    ToCBORGroup (..),
    decodeMap,
    decodeRecordNamed,
    decodeSeq,
    encodeFoldableEncoder,
    encodeFoldableMapEncoder,
    listLenInt,
    runByteBuilder,
  )
import Shelley.Spec.Ledger.Slot (BlockNo (..), SlotNo (..))
import Shelley.Spec.Ledger.Tx (Tx (..), decodeWits, segwitTx, txWitsBytes)
import Shelley.Spec.Ledger.Value
import Shelley.Spec.NonIntegral (CompareResult (..), taylorExpCmp)

-- | The hash of a Block Header
newtype HashHeader crypto v = HashHeader {unHashHeader :: (Hash crypto (BHeader crypto v))}
  deriving (Show, Eq, Generic, Ord)
  deriving newtype (NFData, NoUnexpectedThunks)

deriving instance CV crypto v => ToCBOR (HashHeader crypto v)

deriving instance CV crypto v => FromCBOR (HashHeader crypto v)

data TxSeq crypto v = TxSeq'
  { txSeqTxns' :: !(StrictSeq (Tx crypto v)),
    txSeqBodyBytes :: LByteString,
    txSeqWitsBytes :: LByteString,
    txSeqMetadataBytes :: LByteString
  }
  deriving (Eq, Show, Generic)
  deriving
    (NoUnexpectedThunks)
    via AllowThunksIn
          '[ "txSeqBodyBytes",
             "txSeqWitsBytes",
             "txSeqMetadataBytes"
           ]
          (TxSeq crypto v)

pattern TxSeq :: CV crypto v => StrictSeq (Tx crypto v) -> TxSeq crypto v
pattern TxSeq xs <-
  TxSeq' xs _ _ _
  where
    TxSeq txns =
      let serializeFoldable x =
            serializeEncoding $
              encodeFoldableEncoder (encodePreEncoded . BSL.toStrict) x
          metaChunk index m =
            ( \metadata ->
                toCBOR index <> toCBOR metadata
            )
              <$> strictMaybeToMaybe m
       in TxSeq'
            { txSeqTxns' = txns,
              txSeqBodyBytes =
                serializeEncoding . encodeFoldableEncoder (toCBOR . _body) $ txns,
              txSeqWitsBytes = serializeFoldable $ txWitsBytes . _witnessSet <$> txns,
              txSeqMetadataBytes =
                serializeEncoding . encodeFoldableMapEncoder metaChunk $
                  _metadata <$> txns
            }

{-# COMPLETE TxSeq #-}

instance
  CV crypto v =>
  ToCBORGroup (TxSeq crypto v)
  where
  toCBORGroup (TxSeq' _ bodyBytes witsBytes metadataBytes) =
    encodePreEncoded $
      BSL.toStrict $
        bodyBytes <> witsBytes <> metadataBytes
  encodedGroupSizeExpr size _proxy =
    encodedSizeExpr size (Proxy :: Proxy ByteString)
      + encodedSizeExpr size (Proxy :: Proxy ByteString)
      + encodedSizeExpr size (Proxy :: Proxy ByteString)
  listLen _ = 3
  listLenBound _ = 3

-- | Hash of block body
newtype HashBBody crypto v = HashBBody {unHashBody :: (Hash crypto (TxSeq crypto v))}
  deriving (Show, Eq, Ord, NoUnexpectedThunks)

deriving instance CV crypto v => ToCBOR (HashBBody crypto v)

deriving instance CV crypto v => FromCBOR (HashBBody crypto v)

-- | Hash a given block header
bhHash ::
  CV crypto v =>
  BHeader crypto v ->
  HashHeader crypto v
bhHash = HashHeader . hashAnnotated

-- | Hash a given block body
bbHash ::
  forall crypto v.
  CV crypto v =>
  TxSeq crypto v ->
  HashBBody crypto v
bbHash (TxSeq' _ bodies wits md) =
  (HashBBody . coerce) $
    hashStrict (hashPart bodies <> hashPart wits <> hashPart md)
  where
    hashStrict :: ByteString -> Hash crypto ByteString
    hashStrict = Hash.hashWith id
    hashPart = Hash.hashToBytes . hashStrict . BSL.toStrict

-- | HashHeader to Nonce
hashHeaderToNonce :: forall crypto v. HashHeader crypto v -> Nonce
hashHeaderToNonce = Nonce . coerce

data BHeader crypto v = BHeader'
  { bHeaderBody' :: !(BHBody crypto v),
    bHeaderSig' :: !(SignedKES crypto (BHBody crypto v)),
    bHeaderBytes :: !LByteString
  }
  deriving (Generic)
  deriving
    (NoUnexpectedThunks)
    via AllowThunksIn '["bHeaderBytes"] (BHeader crypto v)

instance CV crypto v => HashAnnotated (BHeader crypto v) crypto

deriving instance Crypto crypto => Eq (BHeader crypto v)

deriving instance Crypto crypto => Show (BHeader crypto v)

pattern BHeader :: CV crypto v => BHBody crypto v -> SignedKES crypto (BHBody crypto v) -> BHeader crypto v
pattern BHeader bHeaderBody' bHeaderSig' <-
  BHeader' {bHeaderBody', bHeaderSig'}
  where
    BHeader body sig =
      let mkBytes bhBody kESig =
            serializeEncoding $
              encodeListLen 2
                <> toCBOR bhBody
                <> encodeSignedKES kESig
       in BHeader' body sig (mkBytes body sig)

{-# COMPLETE BHeader #-}

instance
  CV crypto v =>
  ToCBOR (BHeader crypto v)
  where
  toCBOR (BHeader' _ _ bytes) = encodePreEncoded (BSL.toStrict bytes)
  encodedSizeExpr size proxy =
    1
      + encodedSizeExpr size (bHeaderBody' <$> proxy)
      + KES.encodedSigKESSizeExpr ((KES.getSig . bHeaderSig') <$> proxy)

instance
  CV crypto v =>
  FromCBOR (Annotator (BHeader crypto v))
  where
  fromCBOR = annotatorSlice $
    decodeRecordNamed "Header" (const 2) $ do
      bhb <- fromCBOR
      sig <- decodeSignedKES
      pure $ BHeader' <$> pure bhb <*> pure sig

-- | The previous hash of a block
data PrevHash crypto v = GenesisHash | BlockHash !(HashHeader crypto v)
  deriving (Show, Eq, Generic, Ord)

instance Crypto crypto => NoUnexpectedThunks (PrevHash crypto v)

instance
  CV crypto v =>
  ToCBOR (PrevHash crypto v)
  where
  toCBOR GenesisHash = encodeNull
  toCBOR (BlockHash h) = toCBOR h
  encodedSizeExpr size proxy =
    szCases
      [ Case "GenesisHash" 1,
        Case
          "BlockHash"
          ( encodedSizeExpr
              size
              ( ( \case
                    -- we are mapping a 'Proxy', so nothing can
                    -- go wrong here
                    GenesisHash -> error "impossible happend"
                    BlockHash h -> h
                )
                  <$> proxy
              )
          )
      ]

instance
  CV crypto v =>
  FromCBOR (PrevHash crypto v)
  where
  fromCBOR = do
    peekTokenType >>= \case
      TypeNull -> do
        decodeNull
        pure GenesisHash
      _ -> BlockHash <$> fromCBOR

prevHashToNonce ::
  PrevHash crypto v ->
  Nonce
prevHashToNonce = \case
  GenesisHash -> NeutralNonce -- This case can only happen when starting Shelley from genesis,
  -- setting the intial chain state to some epoch e,
  -- and having the first block be in epoch e+1.
  -- In this edge case there is no need to add any extra
  -- entropy via the previous header hash to the next epoch nonce,
  -- so using the neutral nonce is appropriate.
  BlockHash ph -> hashHeaderToNonce ph

data LastAppliedBlock crypto v = LastAppliedBlock
  { labBlockNo :: !BlockNo,
    labSlotNo :: !SlotNo,
    labHash :: !(HashHeader crypto v)
  }
  deriving (Show, Eq, Generic)

instance Crypto crypto => NoUnexpectedThunks (LastAppliedBlock crypto v)

instance NFData (LastAppliedBlock crypto v)

instance CV crypto v => ToCBOR (LastAppliedBlock crypto v) where
  toCBOR (LastAppliedBlock b s h) =
    encodeListLen 3 <> toCBOR b <> toCBOR s <> toCBOR h

instance CV crypto v => FromCBOR (LastAppliedBlock crypto v) where
  fromCBOR =
    decodeRecordNamed
      "lastAppliedBlock"
      (const 3)
      ( LastAppliedBlock
          <$> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
      )

lastAppliedHash :: WithOrigin (LastAppliedBlock crypto v) -> PrevHash crypto v
lastAppliedHash Origin = GenesisHash
lastAppliedHash (At lab) = BlockHash $ labHash lab

data BHBody crypto v = BHBody
  { -- | block number
    bheaderBlockNo :: !BlockNo,
    -- | block slot
    bheaderSlotNo :: !SlotNo,
    -- | Hash of the previous block header
    bheaderPrev :: !(PrevHash crypto v),
    -- | verification key of block issuer
    bheaderVk :: !(VKey 'BlockIssuer crypto),
    -- | VRF verification key for block issuer
    bheaderVrfVk :: !(VerKeyVRF crypto),
    -- | block nonce
    bheaderEta :: !(CertifiedVRF crypto Nonce),
    -- | leader election value
    bheaderL :: !(CertifiedVRF crypto Natural),
    -- | Size of the block body
    bsize :: !Natural,
    -- | Hash of block body
    bhash :: !(HashBBody crypto v),
    -- | operational certificate
    bheaderOCert :: !(OCert crypto),
    -- | protocol version
    bprotver :: !ProtVer
  }
  deriving (Show, Eq, Generic)

instance
  CV crypto v =>
  SignableRepresentation (BHBody crypto v)
  where
  getSignableRepresentation = serialize'

instance
  Crypto crypto =>
  NoUnexpectedThunks (BHBody crypto v)

instance
  CV crypto v =>
  ToCBOR (BHBody crypto v)
  where
  toCBOR bhBody =
    encodeListLen (9 + listLen oc + listLen pv)
      <> toCBOR (bheaderBlockNo bhBody)
      <> toCBOR (bheaderSlotNo bhBody)
      <> toCBOR (bheaderPrev bhBody)
      <> toCBOR (bheaderVk bhBody)
      <> encodeVerKeyVRF (bheaderVrfVk bhBody)
      <> toCBOR (bheaderEta bhBody)
      <> toCBOR (bheaderL bhBody)
      <> toCBOR (bsize bhBody)
      <> toCBOR (bhash bhBody)
      <> toCBORGroup oc
      <> toCBORGroup pv
    where
      oc = bheaderOCert bhBody
      pv = bprotver bhBody

  encodedSizeExpr size proxy =
    fromInteger (withWordSize $ 9 + listLenBound oc + listLenBound pv)
      + encodedSizeExpr size (bheaderBlockNo <$> proxy)
      + encodedSizeExpr size (bheaderSlotNo <$> proxy)
      + encodedSizeExpr size (bheaderPrev <$> proxy)
      + encodedSizeExpr size (bheaderVk <$> proxy)
      + VRF.encodedVerKeyVRFSizeExpr (bheaderVrfVk <$> proxy)
      + encodedSizeExpr size (bheaderEta <$> proxy)
      + encodedSizeExpr size (bheaderL <$> proxy)
      + encodedSizeExpr size ((toWord64 . bsize) <$> proxy)
      + encodedSizeExpr size (bhash <$> proxy)
      + encodedSizeExpr size (bheaderOCert <$> proxy)
      + encodedSizeExpr size (bprotver <$> proxy)
    where
      oc = bheaderOCert <$> proxy
      pv = bprotver <$> proxy
      toWord64 :: Natural -> Word64
      toWord64 = fromIntegral

instance
  CV crypto v =>
  FromCBOR (BHBody crypto v)
  where
  fromCBOR = decodeRecordNamed "BHBody" bhBodySize $ do
    bheaderBlockNo <- fromCBOR
    bheaderSlotNo <- fromCBOR
    bheaderPrev <- fromCBOR
    bheaderVk <- fromCBOR
    bheaderVrfVk <- decodeVerKeyVRF
    bheaderEta <- fromCBOR
    bheaderL <- fromCBOR
    bsize <- fromCBOR
    bhash <- fromCBOR
    bheaderOCert <- fromCBORGroup
    bprotver <- fromCBORGroup
    pure $
      BHBody
        { bheaderBlockNo,
          bheaderSlotNo,
          bheaderPrev,
          bheaderVk,
          bheaderVrfVk,
          bheaderEta,
          bheaderL,
          bsize,
          bhash,
          bheaderOCert,
          bprotver
        }
    where
      bhBodySize body = 9 + listLenInt (bheaderOCert body) + listLenInt (bprotver body)

-- | Retrieve the pool id (the hash of the pool operator's cold key)
-- from the body of the block header.
poolIDfromBHBody :: Crypto crypto => BHBody crypto v -> KeyHash 'BlockIssuer crypto
poolIDfromBHBody = hashKey . bheaderVk

data Block crypto v
  = Block' !(BHeader crypto v) !(TxSeq crypto v) LByteString
  deriving (Eq, Show)

pattern Block :: CV crypto v => BHeader crypto v -> TxSeq crypto v -> Block crypto v
pattern Block h txns <-
  Block' h txns _
  where
    Block h txns =
      let bytes =
            serializeEncoding $
              encodeListLen (1 + listLen txns) <> toCBOR h <> toCBORGroup txns
       in Block' h txns bytes

{-# COMPLETE Block #-}

-- | Given a size and a mapping from indices to maybe metadata,
--  return a sequence whose size is the size paramater and
--  whose non-Nothing values correspond no the values in the mapping.
constructMetaData :: Int -> Map Int a -> Seq (Maybe a)
constructMetaData n md = fmap (`Map.lookup` md) (Seq.fromList [0 .. n -1])

instance
  CV crypto v =>
  ToCBOR (Block crypto v)
  where
  toCBOR (Block' _ _ blockBytes) = encodePreEncoded $ BSL.toStrict blockBytes

blockDecoder :: CV crypto v => Bool -> forall s. Decoder s (Annotator (Block crypto v))
blockDecoder lax = annotatorSlice $
  decodeRecordNamed "Block" (const 4) $ do
    header <- fromCBOR
    txns <- txSeqDecoder lax
    pure $ Block' <$> header <*> txns

txSeqDecoder :: CV crypto v => Bool -> forall s. Decoder s (Annotator (TxSeq crypto v))
txSeqDecoder lax = do
  (bodies, bodiesAnn) <- withSlice $ decodeSeq fromCBOR
  (wits, witsAnn) <- withSlice $ decodeSeq decodeWits
  let b = length bodies
      w = length wits

  (metadata, metadataAnn) <-
    withSlice $
      constructMetaData b
        <$> decodeMap fromCBOR fromCBOR
  let m = length metadata

  unless
    (lax || b == w)
    ( fail $
        "different number of transaction bodies ("
          <> show b
          <> ") and witness sets ("
          <> show w
          <> ")"
    )
  unless
    (lax || b == m)
    ( fail $
        "mismatch between transaction bodies ("
          <> show b
          <> ") and metadata ("
          <> show w
          <> ")"
    )
  let txns = sequenceA $ StrictSeq.toStrict $ Seq.zipWith3 segwitTx bodies wits metadata
  pure $ TxSeq' <$> txns <*> bodiesAnn <*> witsAnn <*> metadataAnn

instance
  CV crypto v =>
  FromCBOR (Annotator (Block crypto v))
  where
  fromCBOR = blockDecoder False

newtype LaxBlock crypto v
  = LaxBlock (Block crypto v)
  deriving (Show, Eq)
  deriving (ToCBOR) via (Block crypto v)

instance
  CV crypto v =>
  FromCBOR (Annotator (LaxBlock crypto v))
  where
  fromCBOR = fmap LaxBlock <$> blockDecoder True

bHeaderSize ::
  forall crypto v.
  (CV crypto v) =>
  BHeader crypto v ->
  Int
bHeaderSize = BS.length . serialize'

bBodySize ::
  forall crypto v.
  (CV crypto v) =>
  TxSeq crypto v ->
  Int
bBodySize = BS.length . serializeEncoding' . toCBORGroup

slotToNonce :: SlotNo -> Nonce
slotToNonce (SlotNo s) = mkNonceFromNumber s

bheader ::
  CV crypto v =>
  Block crypto v ->
  BHeader crypto v
bheader (Block bh _) = bh

bbody :: CV crypto v => Block crypto v -> TxSeq crypto v
bbody (Block _ txs) = txs

bhbody ::
  CV crypto v =>
  BHeader crypto v ->
  BHBody crypto v
bhbody (BHeader b _) = b

hsig ::
  CV crypto v =>
  BHeader crypto v ->
  SignedKES crypto (BHBody crypto v)
hsig (BHeader _ s) = s

-- | Construct a seed to use in the VRF computation.
mkSeed ::
  -- | Universal constant
  Nonce ->
  SlotNo ->
  -- | Epoch nonce
  Nonce ->
  Seed
mkSeed ucNonce (SlotNo slot) eNonce =
  Seed
    . ( case ucNonce of
          NeutralNonce -> id
          Nonce h -> Hash.xor (Hash.castHash h)
      )
    . Hash.castHash
    . Hash.hashWith id
    . runByteBuilder (8 + 32)
    $ BS.word64BE slot
      <> ( case eNonce of
             NeutralNonce -> mempty
             Nonce h -> BS.byteStringCopy (Hash.hashToBytes h)
         )

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
-- <=>  1 / (1 - p) < exp(-σ * c)
--
-- this can be efficiently be computed by `taylorExpCmp` which returns `ABOVE`
-- in case the reference value `1 / (1 - p)` is above the exponential function
-- at `-σ * c`, `BELOW` if it is below or `MaxReached` if it couldn't
-- conclusively compute this within the given iteration bounds.
checkLeaderValue ::
  forall v.
  (VRF.VRFAlgorithm v) =>
  VRF.OutputVRF v ->
  Rational ->
  ActiveSlotCoeff ->
  Bool
checkLeaderValue certVRF σ f =
  if (intervalValue $ activeSlotVal f) == 1
    then -- If the active slot coefficient is equal to one,
    -- then nearly every stake pool can produce a block every slot.
    -- In this degenerate case, where ln (1-f) is not defined,
    -- we let the VRF leader check always succeed.
    -- This is a testing convenience, the active slot coefficient should not
    -- bet set above one half otherwise.
      True
    else case taylorExpCmp 3 recip_q x of
      ABOVE _ _ -> False
      BELOW _ _ -> True
      MaxReached _ -> False
  where
    certNatMax :: Natural
    certNatMax = (2 :: Natural) ^ (8 * VRF.sizeOutputVRF (Proxy @v))
    c, recip_q, x :: FixedPoint
    c = activeSlotLog f
    recip_q = fromRational (toInteger certNatMax % toInteger (certNatMax - certNat))
    x = (- fromRational σ * c)
    certNat :: Natural
    certNat = VRF.getOutputVRFNatural certVRF

seedEta :: Nonce
seedEta = mkNonceFromNumber 0

seedL :: Nonce
seedL = mkNonceFromNumber 1

hBbsize :: BHBody crypto v -> Natural
hBbsize = bsize

incrBlocks ::
  Bool ->
  KeyHash 'StakePool crypto ->
  BlocksMade crypto ->
  BlocksMade crypto
incrBlocks isOverlay hk b'@(BlocksMade b)
  | isOverlay = b'
  | otherwise = BlocksMade $ case hkVal of
    Nothing -> Map.insert hk 1 b
    Just n -> Map.insert hk (n + 1) b
  where
    hkVal = Map.lookup hk b
