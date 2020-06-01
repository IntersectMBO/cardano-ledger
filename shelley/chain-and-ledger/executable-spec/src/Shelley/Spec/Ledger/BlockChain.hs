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
    BHeader (BHeader),
    Block (Block),
    LaxBlock (..),
    TxSeq (TxSeq),
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
    checkVRFValue,
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
    decodeListLen,
    decodeListLenOf,
    decodeNull,
    encodeListLen,
    encodeNull,
    encodePreEncoded,
    matchSize,
    peekTokenType,
    serialize',
    serializeEncoding,
    serializeEncoding',
    szCases,
    withSlice,
    withWordSize,
  )
import Cardano.Crypto.Hash (SHA256)
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Prelude
  ( AllowThunksIn (..),
    ByteString,
    LByteString,
    NoUnexpectedThunks (..),
  )
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Word (Word64)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes
  ( ActiveSlotCoeff,
    Nonce (..),
    Seed (..),
    UnitInterval,
    activeSlotLog,
    activeSlotVal,
    intervalValue,
    mkNonce,
    strictMaybeToMaybe,
  )
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..))
import Shelley.Spec.Ledger.Keys
  ( CertifiedVRF,
    Hash,
    KeyHash,
    KeyRole (..),
    SignedKES,
    VKey,
    VRFValue (..),
    VerKeyVRF,
    decodeSignedKES,
    decodeVerKeyVRF,
    encodeSignedKES,
    encodeVerKeyVRF,
    hash,
  )
import Shelley.Spec.Ledger.OCert (OCert (..))
import Shelley.Spec.Ledger.PParams (ProtVer (..))
import Shelley.Spec.Ledger.Serialization
  ( FromCBORGroup (..),
    ToCBORGroup (..),
    decodeMap,
    decodeSeq,
    encodeFoldableEncoder,
    encodeFoldableMapEncoder,
  )
import Shelley.Spec.Ledger.Slot (BlockNo (..), SlotNo (..))
import Shelley.Spec.Ledger.Tx (Tx (..), decodeWits, segwitTx)
import Shelley.Spec.NonIntegral (CompareResult (..), taylorExpCmp)

-- | The hash of a Block Header
newtype HashHeader crypto = HashHeader {unHashHeader :: (Hash crypto (BHeader crypto))}
  deriving (Show, Eq, Generic, Ord)

deriving instance Crypto crypto => ToCBOR (HashHeader crypto)

deriving instance Crypto crypto => FromCBOR (HashHeader crypto)

instance NoUnexpectedThunks (HashHeader crypto)

data TxSeq crypto = TxSeq'
  { txSeqTxns' :: !(StrictSeq (Tx crypto)),
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
          (TxSeq crypto)

pattern TxSeq :: Crypto crypto => StrictSeq (Tx crypto) -> TxSeq crypto
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
              txSeqWitsBytes = serializeFoldable $ txWitsBytes <$> txns,
              txSeqMetadataBytes =
                serializeEncoding . encodeFoldableMapEncoder metaChunk $
                  _metadata <$> txns
            }

{-# COMPLETE TxSeq #-}

instance
  Crypto crypto =>
  ToCBORGroup (TxSeq crypto)
  where
  toCBORGroup (TxSeq' _ bodyBytes witsBytes metadataBytes) =
    encodePreEncoded $ BSL.toStrict $
      bodyBytes <> witsBytes <> metadataBytes
  encodedGroupSizeExpr size _proxy =
    encodedSizeExpr size (Proxy :: Proxy ByteString)
      + encodedSizeExpr size (Proxy :: Proxy ByteString)
      + encodedSizeExpr size (Proxy :: Proxy ByteString)
  listLen _ = 3
  listLenBound _ = 3

-- | Hash of block body
newtype HashBBody crypto = HashBBody {unHashBody :: (Hash crypto (TxSeq crypto))}
  deriving (Show, Eq, Ord, NoUnexpectedThunks)

deriving instance Crypto crypto => ToCBOR (HashBBody crypto)

deriving instance Crypto crypto => FromCBOR (HashBBody crypto)

-- | Hash a given block header
bhHash ::
  Crypto crypto =>
  BHeader crypto ->
  HashHeader crypto
bhHash = HashHeader . Hash.hashWithSerialiser toCBOR

-- | Hash a given block body
bbHash ::
  forall crypto.
  Crypto crypto =>
  TxSeq crypto ->
  HashBBody crypto
bbHash (TxSeq' _ bodies wits md) =
  (HashBBody . coerce) $
    hashStrict (hashPart bodies <> hashPart wits <> hashPart md)
  where
    -- FIXME: hash this with less indirection
    -- This should be directly hashing the provided bytes with no funny business.
    hashStrict :: ByteString -> Hash crypto ByteString
    hashStrict = Hash.hashWithSerialiser encodePreEncoded
    hashPart = Hash.getHash . hashStrict . BSL.toStrict

-- | HashHeader to Nonce
hashHeaderToNonce :: HashHeader crypto -> Nonce
hashHeaderToNonce = Nonce . coerce

data BHeader crypto = BHeader'
  { bHeaderBody' :: !(BHBody crypto),
    bHeaderSig' :: !(SignedKES crypto (BHBody crypto)),
    bHeaderBytes :: !LByteString
  }
  deriving (Generic)
  deriving
    (NoUnexpectedThunks)
    via AllowThunksIn '["bHeaderBytes"] (BHeader crypto)

deriving instance Crypto crypto => Eq (BHeader crypto)

deriving instance Crypto crypto => Show (BHeader crypto)

pattern BHeader :: Crypto crypto => BHBody crypto -> SignedKES crypto (BHBody crypto) -> BHeader crypto
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
  Crypto crypto =>
  ToCBOR (BHeader crypto)
  where
  toCBOR (BHeader' _ _ bytes) = encodePreEncoded (BSL.toStrict bytes)
  encodedSizeExpr size proxy =
    1
      + encodedSizeExpr size (bHeaderBody' <$> proxy)
      + KES.encodedSigKESSizeExpr ((KES.getSig . bHeaderSig') <$> proxy)

instance
  Crypto crypto =>
  FromCBOR (Annotator (BHeader crypto))
  where
  fromCBOR = annotatorSlice $ do
    n <- decodeListLen
    bhb <- fromCBOR
    sig <- decodeSignedKES
    matchSize "Header" 2 n
    pure $ BHeader' <$> pure bhb <*> pure sig

-- | The previous hash of a block
data PrevHash crypto = GenesisHash | BlockHash !(HashHeader crypto)
  deriving (Show, Eq, Generic, Ord)

instance Crypto crypto => NoUnexpectedThunks (PrevHash crypto)

instance
  Crypto crypto =>
  ToCBOR (PrevHash crypto)
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
  Crypto crypto =>
  FromCBOR (PrevHash crypto)
  where
  fromCBOR = do
    peekTokenType >>= \case
      TypeNull -> do
        decodeNull
        pure GenesisHash
      _ -> BlockHash <$> fromCBOR

prevHashToNonce ::
  PrevHash crypto ->
  Nonce
prevHashToNonce = \case
  GenesisHash -> NeutralNonce -- This case can only happen when starting Shelley from genesis,
  -- setting the intial chain state to some epoch e,
  -- and having the first block be in epoch e+1.
  -- In this edge case there is no need to add any extra
  -- entropy via the previous header hash to the next epoch nonce,
  -- so using the neutral nonce is appropriate.
  BlockHash ph -> hashHeaderToNonce ph

data LastAppliedBlock crypto = LastAppliedBlock
  { labBlockNo :: !BlockNo,
    labSlotNo :: !SlotNo,
    labHash :: !(HashHeader crypto)
  }
  deriving (Show, Eq, Generic)

instance Crypto crypto => NoUnexpectedThunks (LastAppliedBlock crypto)

instance Crypto crypto => ToCBOR (LastAppliedBlock crypto) where
  toCBOR (LastAppliedBlock b s h) =
    encodeListLen 3 <> toCBOR b <> toCBOR s <> toCBOR h

instance Crypto crypto => FromCBOR (LastAppliedBlock crypto) where
  fromCBOR =
    decodeListLenOf 3
      >> LastAppliedBlock
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

lastAppliedHash :: WithOrigin (LastAppliedBlock crypto) -> PrevHash crypto
lastAppliedHash Origin = GenesisHash
lastAppliedHash (At lab) = BlockHash $ labHash lab

data BHBody crypto = BHBody
  { -- | block number
    bheaderBlockNo :: !BlockNo,
    -- | block slot
    bheaderSlotNo :: !SlotNo,
    -- | Hash of the previous block header
    bheaderPrev :: !(PrevHash crypto),
    -- | verification key of block issuer
    bheaderVk :: !(VKey 'BlockIssuer crypto),
    -- | VRF verification key for block issuer
    bheaderVrfVk :: !(VerKeyVRF crypto),
    -- | block nonce
    bheaderEta :: !(CertifiedVRF crypto Nonce),
    -- | leader election value
    bheaderL :: !(CertifiedVRF crypto UnitInterval),
    -- | Size of the block body
    bsize :: !Natural,
    -- | Hash of block body
    bhash :: !(HashBBody crypto),
    -- | operational certificate
    bheaderOCert :: !(OCert crypto),
    -- | protocol version
    bprotver :: !ProtVer
  }
  deriving (Show, Eq, Generic)

instance
  Crypto crypto =>
  NoUnexpectedThunks (BHBody crypto)

instance
  Crypto crypto =>
  ToCBOR (BHBody crypto)
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
  Crypto crypto =>
  FromCBOR (BHBody crypto)
  where
  fromCBOR = do
    n <- decodeListLen
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
    matchSize "BHBody" (fromIntegral $ 9 + listLen bheaderOCert + listLen bprotver) n
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

data Block crypto
  = Block' !(BHeader crypto) !(TxSeq crypto) LByteString
  deriving (Eq, Show)

pattern Block :: Crypto crypto => BHeader crypto -> TxSeq crypto -> Block crypto
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
  Crypto crypto =>
  ToCBOR (Block crypto)
  where
  toCBOR (Block' _ _ blockBytes) = encodePreEncoded $ BSL.toStrict blockBytes

blockDecoder :: Crypto crypto => Bool -> forall s. Decoder s (Annotator (Block crypto))
blockDecoder lax = annotatorSlice $ do
  n <- decodeListLen
  matchSize "Block" 4 n
  header <- fromCBOR
  txns <- txSeqDecoder lax
  pure $ Block' <$> header <*> txns

txSeqDecoder :: Crypto crypto => Bool -> forall s. Decoder s (Annotator (TxSeq crypto))
txSeqDecoder lax = do
  (bodies, bodiesAnn) <- withSlice $ decodeSeq fromCBOR
  (wits, witsAnn) <- withSlice $ decodeSeq (withSlice decodeWits)
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
  Crypto crypto =>
  FromCBOR (Annotator (Block crypto))
  where
  fromCBOR = blockDecoder False

newtype LaxBlock crypto
  = LaxBlock (Block crypto)
  deriving (Show, Eq)
  deriving (ToCBOR) via (Block crypto)

instance
  Crypto crypto =>
  FromCBOR (Annotator (LaxBlock crypto))
  where
  fromCBOR = fmap LaxBlock <$> blockDecoder True

bHeaderSize ::
  forall crypto.
  (Crypto crypto) =>
  BHeader crypto ->
  Int
bHeaderSize = BS.length . serialize'

bBodySize ::
  forall crypto.
  (Crypto crypto) =>
  TxSeq crypto ->
  Int
bBodySize = BS.length . serializeEncoding' . toCBORGroup

slotToNonce :: SlotNo -> Nonce
slotToNonce (SlotNo s) = mkNonce (fromIntegral s)

bheader ::
  Crypto crypto =>
  Block crypto ->
  BHeader crypto
bheader (Block bh _) = bh

bbody :: Crypto crypto => Block crypto -> TxSeq crypto
bbody (Block _ txs) = txs

bhbody ::
  Crypto crypto =>
  BHeader crypto ->
  BHBody crypto
bhbody (BHeader b _) = b

hsig ::
  Crypto crypto =>
  BHeader crypto ->
  SignedKES crypto (BHBody crypto)
hsig (BHeader _ s) = s

-- | Construct a seed to use in the VRF computation.
mkSeed ::
  -- | Universal constant
  Nonce ->
  SlotNo ->
  -- | Epoch nonce
  Nonce ->
  Seed
mkSeed (Nonce uc) slot nonce =
  Seed . coerce $ uc `Hash.xor` coerce (hash @SHA256 (slot, nonce))
mkSeed NeutralNonce slot nonce =
  Seed . coerce $ hash @SHA256 (slot, nonce)

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
    then -- If the active slot coefficient is equal to one,
    -- then nearly every stake pool can produce a block every slot.
    -- In this degenerate case, where ln (1-f) is not defined,
    -- we let the VRF leader check always succeed.
    -- This is a testing convenience, the active slot coefficient should not
    -- bet set above one half otherwise.
      True
    else
      if leaderVal == 1
        then -- Having a VRF value of one is always a failure.
        -- Moreover, it would cause division by zero.
          False
        else case taylorExpCmp 3 (1 / q) x of
          ABOVE _ _ -> False
          BELOW _ _ -> True
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
