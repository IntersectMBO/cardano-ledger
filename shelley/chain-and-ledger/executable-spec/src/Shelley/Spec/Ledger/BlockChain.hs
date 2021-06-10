{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
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
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.BlockChain
  ( HashHeader (..),
    PrevHash (..),
    LastAppliedBlock (..),
    lastAppliedHash,
    BHBody (..),
    poolIDfromBHBody,
    issuerIDfromBHBody,
    BHeader (BHeader),
    Block (Block, Block'),
    LaxBlock (..),
    TxSeq (TxSeq, txSeqTxns', TxSeq'),
    constructMetadata,
    txSeqTxns,
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
    bnonce,
    --
    seedEta,
    seedL,
    incrBlocks,
    mkSeed,
    checkLeaderValue,
    coreAuxDataBytes,
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
import Cardano.Ledger.BaseTypes
  ( ActiveSlotCoeff,
    FixedPoint,
    Nonce (..),
    Seed (..),
    StrictMaybe (..),
    activeSlotLog,
    activeSlotVal,
    mkNonceFromNumber,
    mkNonceFromOutputVRF,
    strictMaybeToMaybe,
  )
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era, ValidateScript (..))
import qualified Cardano.Ledger.Era as Era
import Cardano.Ledger.Hashes (EraIndependentBlockBody)
import Cardano.Ledger.Keys
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
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Serialization
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
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..))
import Shelley.Spec.Ledger.OCert (OCert (..))
import Shelley.Spec.Ledger.PParams (ProtVer (..))
import Shelley.Spec.Ledger.Slot (BlockNo (..), SlotNo (..))
import Shelley.Spec.Ledger.Tx (segwitTx)
import Shelley.Spec.NonIntegral (CompareResult (..), taylorExpCmp)

-- =======================================================

-- | The hash of a Block Header
newtype HashHeader crypto = HashHeader {unHashHeader :: Hash crypto (BHeader crypto)}
  deriving stock (Show, Eq, Generic, Ord)
  deriving newtype (NFData, NoThunks)

deriving newtype instance CC.Crypto crypto => ToCBOR (HashHeader crypto)

deriving newtype instance CC.Crypto crypto => FromCBOR (HashHeader crypto)

data TxSeq era = TxSeq'
  { txSeqTxns' :: !(StrictSeq (Core.Tx era)),
    txSeqBodyBytes :: BSL.ByteString,
    txSeqWitsBytes :: BSL.ByteString,
    txSeqMetadataBytes :: BSL.ByteString
    -- bytes representing a (Map index metadata). Missing indices have SNothing for metadata
  }
  deriving (Generic)

deriving via
  AllowThunksIn
    '[ "txSeqBodyBytes",
       "txSeqWitsBytes",
       "txSeqMetadataBytes"
     ]
    (TxSeq era)
  instance
    (Typeable era, NoThunks (Core.Tx era)) => NoThunks (TxSeq era)

deriving stock instance
  Show (Core.Tx era) =>
  Show (TxSeq era)

deriving stock instance
  Eq (Core.Tx era) =>
  Eq (TxSeq era)

-- ===========================
-- Getting bytes from pieces of a Core.Tx

coreWitnessBytes ::
  forall era.
  ( SafeToHash (Core.Witnesses era),
    HasField "wits" (Core.Tx era) (Core.Witnesses era)
  ) =>
  (Core.Tx era) ->
  ByteString
coreWitnessBytes coretx =
  originalBytes @(Core.Witnesses era) $
    getField @"wits" coretx

coreBodyBytes ::
  forall era.
  ( SafeToHash (Core.TxBody era),
    HasField "body" (Core.Tx era) (Core.TxBody era)
  ) =>
  (Core.Tx era) ->
  ByteString
coreBodyBytes coretx =
  originalBytes @(Core.TxBody era) $
    getField @"body" coretx

coreAuxDataBytes ::
  forall era.
  ( SafeToHash (Core.AuxiliaryData era),
    HasField "auxiliaryData" (Core.Tx era) (StrictMaybe (Core.AuxiliaryData era))
  ) =>
  (Core.Tx era) ->
  StrictMaybe ByteString
coreAuxDataBytes coretx = getbytes <$> getField @"auxiliaryData" @(Core.Tx era) coretx
  where
    getbytes auxdata = originalBytes @(Core.AuxiliaryData era) auxdata

-- ===========================

-- | Constuct a TxSeq (with all it bytes) from just Core.Tx's
pattern TxSeq ::
  forall era.
  ( Era era,
    SafeToHash (Core.Witnesses era)
  ) =>
  StrictSeq (Core.Tx era) ->
  TxSeq era
pattern TxSeq xs <-
  TxSeq' xs _ _ _
  where
    TxSeq txns =
      let serializeFoldable x =
            serializeEncoding $
              encodeFoldableEncoder encodePreEncoded x
          metaChunk index m = encodePair <$> strictMaybeToMaybe m
            where
              encodePair metadata = toCBOR index <> encodePreEncoded metadata
       in TxSeq'
            { txSeqTxns' = txns,
              -- bytes encoding Seq(Core.TxBody era)
              txSeqBodyBytes = serializeFoldable $ coreBodyBytes @era <$> txns,
              -- bytes encoding Seq(Core.Witnesses era)
              txSeqWitsBytes = serializeFoldable $ coreWitnessBytes @era <$> txns,
              -- bytes encoding a (Map Int (Core.AuxiliaryData))
              txSeqMetadataBytes =
                serializeEncoding . encodeFoldableMapEncoder metaChunk $
                  coreAuxDataBytes @era <$> txns
            }

{-# COMPLETE TxSeq #-}

txSeqTxns :: TxSeq era -> StrictSeq (Core.Tx era)
txSeqTxns (TxSeq' ts _ _ _) = ts

instance
  forall era.
  (Era era) =>
  ToCBORGroup (TxSeq era)
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

-- | Hash a given block header
bhHash ::
  forall crypto.
  CC.Crypto crypto =>
  BHeader crypto ->
  HashHeader crypto
bhHash = HashHeader . Hash.hashWithSerialiser @(CC.HASH crypto) toCBOR

-- | Hash a given block body
bbHash ::
  forall era.
  (Era era) =>
  TxSeq era ->
  Hash (Crypto era) EraIndependentBlockBody
bbHash (TxSeq' _ bodies wits md) =
  coerce $
    hashStrict
      ( hashPart bodies
          <> hashPart wits
          <> hashPart md
      )
  where
    hashStrict :: ByteString -> Hash (Crypto era) ByteString
    hashStrict = Hash.hashWith id
    hashPart = Hash.hashToBytes . hashStrict . BSL.toStrict

-- | HashHeader to Nonce
hashHeaderToNonce :: HashHeader crypto -> Nonce
hashHeaderToNonce = Nonce . coerce

data BHeader crypto = BHeader'
  { bHeaderBody' :: !(BHBody crypto),
    bHeaderSig' :: !(SignedKES crypto (BHBody crypto)),
    bHeaderBytes :: !BSL.ByteString
  }
  deriving (Generic)

deriving via
  AllowThunksIn '["bHeaderBytes"] (BHeader crypto)
  instance
    CC.Crypto crypto => NoThunks (BHeader crypto)

deriving instance CC.Crypto crypto => Eq (BHeader crypto)

deriving instance CC.Crypto crypto => Show (BHeader crypto)

pattern BHeader ::
  CC.Crypto crypto =>
  BHBody crypto ->
  SignedKES crypto (BHBody crypto) ->
  BHeader crypto
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
  CC.Crypto crypto =>
  ToCBOR (BHeader crypto)
  where
  toCBOR (BHeader' _ _ bytes) = encodePreEncoded (BSL.toStrict bytes)
  encodedSizeExpr size proxy =
    1
      + encodedSizeExpr size (bHeaderBody' <$> proxy)
      + KES.encodedSigKESSizeExpr (KES.getSig . bHeaderSig' <$> proxy)

instance
  CC.Crypto crypto =>
  FromCBOR (Annotator (BHeader crypto))
  where
  fromCBOR = annotatorSlice $
    decodeRecordNamed "Header" (const 2) $ do
      bhb <- fromCBOR
      sig <- decodeSignedKES
      pure $ BHeader' <$> pure bhb <*> pure sig

-- | The previous hash of a block
data PrevHash crypto = GenesisHash | BlockHash !(HashHeader crypto)
  deriving (Show, Eq, Generic, Ord)

instance CC.Crypto crypto => NoThunks (PrevHash crypto)

instance
  CC.Crypto crypto =>
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
  CC.Crypto crypto =>
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

instance CC.Crypto crypto => NoThunks (LastAppliedBlock crypto)

instance NFData (LastAppliedBlock crypto)

instance CC.Crypto crypto => ToCBOR (LastAppliedBlock crypto) where
  toCBOR (LastAppliedBlock b s h) =
    encodeListLen 3 <> toCBOR b <> toCBOR s <> toCBOR h

instance CC.Crypto crypto => FromCBOR (LastAppliedBlock crypto) where
  fromCBOR =
    decodeRecordNamed
      "lastAppliedBlock"
      (const 3)
      ( LastAppliedBlock
          <$> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
      )

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
    bheaderL :: !(CertifiedVRF crypto Natural),
    -- | Size of the block body
    bsize :: !Natural,
    -- | Hash of block body
    bhash :: !(Hash crypto EraIndependentBlockBody),
    -- | operational certificate
    bheaderOCert :: !(OCert crypto),
    -- | protocol version
    bprotver :: !ProtVer
  }
  deriving (Generic)

deriving instance CC.Crypto crypto => Show (BHBody crypto)

deriving instance CC.Crypto crypto => Eq (BHBody crypto)

instance
  CC.Crypto crypto =>
  SignableRepresentation (BHBody crypto)
  where
  getSignableRepresentation = serialize'

instance
  CC.Crypto crypto =>
  NoThunks (BHBody crypto)

instance
  CC.Crypto crypto =>
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
      + encodedSizeExpr size (toWord64 . bsize <$> proxy)
      + encodedSizeExpr size (bhash <$> proxy)
      + encodedSizeExpr size (bheaderOCert <$> proxy)
      + encodedSizeExpr size (bprotver <$> proxy)
    where
      oc = bheaderOCert <$> proxy
      pv = bprotver <$> proxy
      toWord64 :: Natural -> Word64
      toWord64 = fromIntegral

instance
  CC.Crypto crypto =>
  FromCBOR (BHBody crypto)
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
poolIDfromBHBody :: CC.Crypto crypto => BHBody crypto -> KeyHash 'BlockIssuer crypto
poolIDfromBHBody = hashKey . bheaderVk
{-# DEPRECATED poolIDfromBHBody "poolIDfromBHBody has been deprecated (the name is misleading), use issuerIDfromBHBody" #-}

-- | Retrieve the issuer id (the hash of the cold key) from the body of the block header.
-- This corresponds to either a genesis/core node or a stake pool.
issuerIDfromBHBody :: CC.Crypto crypto => BHBody crypto -> KeyHash 'BlockIssuer crypto
issuerIDfromBHBody = hashKey . bheaderVk

-- | Retrieve the new nonce from the block header body.
bnonce :: BHBody crypto -> Nonce
bnonce = mkNonceFromOutputVRF . VRF.certifiedOutput . bheaderEta

data Block era
  = Block' !(BHeader (Crypto era)) !(Era.TxSeq era) BSL.ByteString
  deriving (Generic)

deriving stock instance
  (Era era, Show (Era.TxSeq era)) =>
  Show (Block era)

deriving stock instance
  (Era era, Eq (Era.TxSeq era)) =>
  Eq (Block era)

deriving anyclass instance
  (Era era, NoThunks (Era.TxSeq era)) =>
  NoThunks (Block era)

pattern Block ::
  (Era era, ToCBORGroup (Era.TxSeq era)) =>
  BHeader (Crypto era) ->
  Era.TxSeq era ->
  Block era
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
--  whose non-Nothing values correspond to the values in the mapping.
constructMetadata ::
  forall era.
  Int ->
  Map Int (Annotator (Core.AuxiliaryData era)) ->
  Seq (Maybe (Annotator (Core.AuxiliaryData era)))
constructMetadata n md = fmap (`Map.lookup` md) (Seq.fromList [0 .. n -1])

instance
  Era era =>
  ToCBOR (Block era)
  where
  toCBOR (Block' _ _ blockBytes) = encodePreEncoded $ BSL.toStrict blockBytes

-- | The parts of the Tx in Blocks that have to have FromCBOR(Annotator x) instances.
--   These are exactly the parts that are SafeToHash.
type BlockAnn era =
  ( FromCBOR (Annotator (Core.TxBody era)),
    FromCBOR (Annotator (Core.AuxiliaryData era)),
    FromCBOR (Annotator (Core.Witnesses era)),
    ToCBOR (Core.TxBody era),
    ToCBOR (Core.AuxiliaryData era),
    ToCBOR (Core.Witnesses era)
  )

-- | Decode a TxSeq, used in decoding a Block.
txSeqDecoder ::
  forall era.
  BlockAnn era =>
  Bool ->
  forall s. Decoder s (Annotator (TxSeq era))
txSeqDecoder lax = do
  (bodies, bodiesAnn) <- withSlice $ decodeSeq fromCBOR
  (wits, witsAnn) <- withSlice $ decodeSeq fromCBOR
  let b = length bodies
      inRange x = (0 <= x) && (x <= (b -1))
      w = length wits
  (metadata, metadataAnn) <- withSlice $
    do
      m <- decodeMap fromCBOR fromCBOR
      unless -- TODO this PR introduces this new test, That didn't used to run in the Shelley
        (lax || all inRange (Map.keysSet m)) -- Era,  Is it possible there might be some blocks, that should have been caught on the chain?
        (fail ("Some Auxiliarydata index is not in the range: 0 .. " ++ show (b -1)))
      pure (constructMetadata @era b m)

  unless
    (lax || b == w)
    ( fail $
        "different number of transaction bodies ("
          <> show b
          <> ") and witness sets ("
          <> show w
          <> ")"
    )

  let txns =
        sequenceA $
          StrictSeq.forceToStrict $
            Seq.zipWith3 segwitTx bodies wits metadata
  pure $ TxSeq' <$> txns <*> bodiesAnn <*> witsAnn <*> metadataAnn

instance
  (BlockAnn era, Typeable era) =>
  FromCBOR (Annotator (TxSeq era))
  where
  fromCBOR = txSeqDecoder False

instance
  forall era.
  ( BlockAnn era,
    ValidateScript era,
    Era.SupportsSegWit era,
    FromCBOR (Annotator (Era.TxSeq era))
  ) =>
  FromCBOR (Annotator (Block era))
  where
  fromCBOR = annotatorSlice $
    decodeRecordNamed "Block" (const blockSize) $ do
      header <- fromCBOR
      txns <- fromCBOR
      pure $ Block' <$> header <*> txns
    where
      blockSize =
        1 -- header
          + fromIntegral (Era.numSegComponents @era)

-- | A block in which we do not validate the matched encoding of parts of the
--   segwit. TODO This is purely a test concern, and as such should be moved out
--   of the library.
newtype LaxBlock era = LaxBlock (Block era)

blockDecoder ::
  ( BlockAnn era,
    ValidateScript era,
    Era.TxSeq era ~ TxSeq era
  ) =>
  Bool ->
  forall s. Decoder s (Annotator (Block era))
blockDecoder lax = annotatorSlice $
  decodeRecordNamed "Block" (const 4) $ do
    header <- fromCBOR
    txns <- txSeqDecoder lax
    pure $ Block' <$> header <*> txns

instance (Era era, Typeable era) => ToCBOR (LaxBlock era) where
  toCBOR (LaxBlock x) = toCBOR x

deriving stock instance
  (Era era, Show (Era.TxSeq era)) =>
  Show (LaxBlock era)

instance
  ( Era era,
    BlockAnn era,
    ValidateScript era,
    Era.TxSeq era ~ TxSeq era
  ) =>
  FromCBOR (Annotator (LaxBlock era))
  where
  fromCBOR = fmap LaxBlock <$> blockDecoder True

bHeaderSize ::
  forall crypto.
  (CC.Crypto crypto) =>
  BHeader crypto ->
  Int
bHeaderSize = BS.length . serialize'

bBodySize ::
  ToCBORGroup txSeq => txSeq -> Int
bBodySize = BS.length . serializeEncoding' . toCBORGroup

slotToNonce :: SlotNo -> Nonce
slotToNonce (SlotNo s) = mkNonceFromNumber s

bheader ::
  Block era ->
  BHeader (Crypto era)
bheader (Block' bh _ _) = bh

bbody :: Block era -> Era.TxSeq era
bbody (Block' _ txs _) = txs

bhbody ::
  CC.Crypto crypto =>
  BHeader crypto ->
  BHBody crypto
bhbody (BHeader b _) = b

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
  if activeSlotVal f == maxBound
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
