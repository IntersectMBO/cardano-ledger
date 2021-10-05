{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
-- We are enabling orphans since BlockNo is orphaned until the instance is added to cardano-base
-- https://github.com/input-output-hk/cardano-base/pull/233
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Protocol.TPraos.BHeader
  ( HashHeader (..),
    PrevHash (..),
    BHeader (BHeader),
    BHBody (..),
    LastAppliedBlock (..),
    lastAppliedHash,
    issuerIDfromBHBody,
    checkLeaderValue,
    bhHash,
    hashHeaderToNonce,
    prevHashToNonce,
    bHeaderSize,
    bhbody,
    hBbsize,
    seedEta,
    seedL,
    mkSeed,
    bnonce,
    makeHeaderView,
  )
where

import Cardano.Binary
  ( Annotator (..),
    Case (..),
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
    szCases,
    withWordSize,
  )
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation (..))
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BHeaderView (BHeaderView (..))
import Cardano.Ledger.BaseTypes
  ( ActiveSlotCoeff,
    FixedPoint,
    Nonce (..),
    Seed (..),
    activeSlotLog,
    activeSlotVal,
    mkNonceFromNumber,
    mkNonceFromOutputVRF,
  )
import qualified Cardano.Ledger.Crypto as CC
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
import Cardano.Ledger.NonIntegral (CompareResult (..), taylorExpCmp)
import Cardano.Ledger.Serialization
  ( FromCBORGroup (..),
    ToCBORGroup (..),
    decodeRecordNamed,
    listLenInt,
    runByteBuilder,
  )
import Cardano.Ledger.Slot (BlockNo (..), SlotNo (..))
import Cardano.Protocol.TPraos (ProtVer (..))
import Cardano.Protocol.TPraos.OCert (OCert (..))
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Ratio ((%))
import Data.Typeable
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Numeric.Natural (Natural)

-- | The hash of a Block Header
newtype HashHeader crypto = HashHeader {unHashHeader :: Hash crypto (BHeader crypto)}
  deriving stock (Show, Eq, Generic, Ord)
  deriving newtype (NFData, NoThunks)

deriving newtype instance CC.Crypto crypto => ToCBOR (HashHeader crypto)

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

deriving newtype instance CC.Crypto crypto => FromCBOR (HashHeader crypto)

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
      pure $ pure $ BHeader' bhb sig

-- | Hash a given block header
bhHash ::
  CC.Crypto crypto =>
  BHeader crypto ->
  HashHeader crypto
bhHash = HashHeader . Hash.hashWithSerialiser toCBOR

-- | HashHeader to Nonce
-- What is going on here?
-- This is here because the surrounding code is parametrized in the hash algorithm used,
-- but the nonce is hard-coded to Blake2b_256.
-- We require the nonce to have the right length (the size of a Blake2b_256 hash), so
-- if the hash size differs, we pad or remove bytes accordingly.
hashHeaderToNonce :: HashHeader crypto -> Nonce
hashHeaderToNonce (HashHeader h) = case Hash.hashFromBytes bytes of
  Nothing -> Nonce (Hash.castHash (Hash.hashWith id bytes))
  Just hash -> Nonce hash
  where
    bytes = Hash.hashToBytes h

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

-- | Retrieve the issuer id (the hash of the cold key) from the body of the block header.
-- This corresponds to either a genesis/core node or a stake pool.
issuerIDfromBHBody :: CC.Crypto crypto => BHBody crypto -> KeyHash 'BlockIssuer crypto
issuerIDfromBHBody = hashKey . bheaderVk

bHeaderSize ::
  forall crypto.
  (CC.Crypto crypto) =>
  BHeader crypto ->
  Int
bHeaderSize = BS.length . serialize'

bhbody ::
  CC.Crypto crypto =>
  BHeader crypto ->
  BHBody crypto
bhbody (BHeader b _) = b

hBbsize :: BHBody crypto -> Natural
hBbsize = bsize

-- | Check that the certified input natural is valid for being slot leader. This
-- means we check that
--
-- p < 1 - (1 - f)^σ
--
-- where p = certNat / certNatMax.
--
-- The calculation is done using the following optimization:
--
-- let q = 1 - p and c = ln(1 - f)
--
-- then           p < 1 - (1 - f)^σ
-- <=>  1 / (1 - p) < exp(-σ * c)
-- <=>  1 / q       < exp(-σ * c)
--
-- This can be efficiently be computed by `taylorExpCmp` which returns `ABOVE`
-- in case the reference value `1 / (1 - p)` is above the exponential function
-- at `-σ * c`, `BELOW` if it is below or `MaxReached` if it couldn't
-- conclusively compute this within the given iteration bounds.
--
-- Note that  1       1               1                         certNatMax
--           --- =  ----- = ---------------------------- = ----------------------
--            q     1 - p    1 - (certNat / certNatMax)    (certNatMax - certNat)
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
    x = - fromRational σ * c
    certNat :: Natural
    certNat = VRF.getOutputVRFNatural certVRF

seedEta :: Nonce
seedEta = mkNonceFromNumber 0

seedL :: Nonce
seedL = mkNonceFromNumber 1

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

-- | Retrieve the new nonce from the block header body.
bnonce :: BHBody crypto -> Nonce
bnonce = mkNonceFromOutputVRF . VRF.certifiedOutput . bheaderEta

makeHeaderView :: CC.Crypto crypto => BHeader crypto -> BHeaderView crypto
makeHeaderView bh =
  BHeaderView
    (hashKey . bheaderVk $ bhb)
    (bsize $ bhb)
    (bHeaderSize bh)
    (bhash bhb)
    (bheaderSlotNo bhb)
  where
    bhb = bHeaderBody' bh
