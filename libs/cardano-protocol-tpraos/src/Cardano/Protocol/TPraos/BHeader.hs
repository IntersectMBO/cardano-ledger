{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.Protocol.TPraos.BHeader (
  HashHeader (..),
  PrevHash (..),
  BHeader (BHeader, ..),
  BHeaderRaw (..),
  BHBody (..),
  LastAppliedBlock (..),
  BoundedNatural (bvValue, bvMaxValue),
  assertBoundedNatural,
  lastAppliedHash,
  issuerIDfromBHBody,
  checkLeaderValue,
  checkLeaderNatValue,
  bhHash,
  hashHeaderToNonce,
  prevHashToNonce,
  bhbody,
  hBbsize,
  seedEta,
  seedL,
  mkSeed,
  bnonce,
  makeHeaderView,
) where

import Cardano.Base.Proxy (asProxy)
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation (..))
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.BHeaderView (BHeaderView (..))
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.BaseTypes (
  ActiveSlotCoeff,
  FixedPoint,
  Nonce (..),
  ProtVer (..),
  Seed (..),
  activeSlotLog,
  activeSlotVal,
  mkNonceFromNumber,
  mkNonceFromOutputVRF,
 )
import Cardano.Ledger.BaseTypes.NonZero (nonZero, (%.))
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (decCBOR),
  DecCBORGroup (..),
  EncCBOR (..),
  EncCBORGroup (..),
  TokenType (TypeNull),
  decodeNull,
  decodeRecordNamed,
  encodeListLen,
  encodeNull,
  listLenInt,
  peekTokenType,
  runByteBuilder,
  serialize',
 )
import Cardano.Ledger.Binary.Crypto
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Block (Block (..), EraBlockHeader (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Hashes (
  EraIndependentBlockBody,
  EraIndependentBlockHeader,
  HASH,
  Hash,
  HashAnnotated (..),
  HashHeader (..),
  KeyHash,
  KeyRole (..),
  SafeToHash,
  extractHash,
  hashKey,
  originalBytesSize,
 )
import Cardano.Ledger.Keys (VKey)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.MemoBytes (
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (..),
  getMemoRawType,
  getMemoSafeHash,
  mkMemoized,
 )
import Cardano.Ledger.NonIntegral (CompareResult (..), taylorExpCmp)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Slot (BlockNo (..), SlotNo (..))
import Cardano.Protocol.Crypto
import Cardano.Protocol.TPraos.OCert (OCert (..))
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.DeepSeq (NFData)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro (lens)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

-- | The previous hash of a block
data PrevHash = GenesisHash | BlockHash !HashHeader
  deriving (Show, Eq, Generic, Ord)

instance NoThunks PrevHash

instance EncCBOR PrevHash where
  encCBOR GenesisHash = encodeNull
  encCBOR (BlockHash h) = encCBOR h

instance DecCBOR PrevHash where
  decCBOR = do
    peekTokenType >>= \case
      TypeNull -> do
        decodeNull
        pure GenesisHash
      _ -> BlockHash <$> decCBOR

data BHBody c = BHBody
  { bheaderBlockNo :: !BlockNo
  -- ^ block number
  , bheaderSlotNo :: !SlotNo
  -- ^ block slot
  , bheaderPrev :: !PrevHash
  -- ^ Hash of the previous block header
  , bheaderVk :: !(VKey BlockIssuer)
  -- ^ verification key of block issuer
  , bheaderVrfVk :: !(VRF.VerKeyVRF (VRF c))
  -- ^ VRF verification key for block issuer
  , bheaderEta :: !(VRF.CertifiedVRF (VRF c) Nonce)
  -- ^ block nonce
  , bheaderL :: !(VRF.CertifiedVRF (VRF c) Natural)
  -- ^ leader election value
  , bsize :: !Word32
  -- ^ Size of the block body
  , bhash :: !(Hash HASH EraIndependentBlockBody)
  -- ^ Hash of block body
  , bheaderOCert :: !(OCert c)
  -- ^ operational certificate
  , bprotver :: !ProtVer
  -- ^ protocol version
  }
  deriving (Generic)

deriving instance Crypto c => Show (BHBody c)

deriving instance Crypto c => Eq (BHBody c)

instance Crypto c => SignableRepresentation (BHBody c) where
  getSignableRepresentation bh = serialize' (pvMajor (bprotver bh)) bh

instance Crypto c => NoThunks (BHBody c)

instance Crypto c => EncCBOR (BHBody c) where
  encCBOR bhBody =
    encodeListLen (9 + listLen (asProxy oc) + listLen (asProxy pv))
      <> encCBOR (bheaderBlockNo bhBody)
      <> encCBOR (bheaderSlotNo bhBody)
      <> encCBOR (bheaderPrev bhBody)
      <> encCBOR (bheaderVk bhBody)
      <> encodeVerKeyVRF (bheaderVrfVk bhBody)
      <> encCBOR (bheaderEta bhBody)
      <> encCBOR (bheaderL bhBody)
      <> encCBOR (bsize bhBody)
      <> encCBOR (bhash bhBody)
      <> encCBORGroup oc
      <> encCBORGroup pv
    where
      oc = bheaderOCert bhBody
      pv = bprotver bhBody

instance Crypto c => DecCBOR (BHBody c) where
  decCBOR = decodeRecordNamed "BHBody" bhBodySize $ do
    bheaderBlockNo <- decCBOR
    bheaderSlotNo <- decCBOR
    bheaderPrev <- decCBOR
    bheaderVk <- decCBOR
    bheaderVrfVk <- decodeVerKeyVRF
    bheaderEta <- decCBOR
    bheaderL <- decCBOR
    bsize <- decCBOR
    bhash <- decCBOR
    bheaderOCert <- decCBORGroup
    bprotver <- decCBORGroup
    pure $
      BHBody
        { bheaderBlockNo
        , bheaderSlotNo
        , bheaderPrev
        , bheaderVk
        , bheaderVrfVk
        , bheaderEta
        , bheaderL
        , bsize
        , bhash
        , bheaderOCert
        , bprotver
        }
    where
      bhBodySize body = 9 + listLenInt (Just $ bheaderOCert body) + listLenInt (Just $ bprotver body)

data BHeaderRaw c = BHeaderRaw
  { bhrBody :: !(BHBody c)
  , bhrSignature :: !(KES.SignedKES (KES c) (BHBody c))
  }
  deriving (Generic, Eq, Show)

instance Crypto c => NoThunks (BHeaderRaw c)

instance Crypto c => EncCBOR (BHeaderRaw c) where
  encCBOR bh@(BHeaderRaw _ _) =
    let BHeaderRaw {..} = bh
     in encodeListLen 2
          <> encCBOR bhrBody
          <> encodeSignedKES bhrSignature

instance Crypto c => DecCBOR (BHeaderRaw c) where
  decCBOR =
    decodeRecordNamed "BHeaderRaw" (const 2) $
      BHeaderRaw <$> decCBOR <*> decodeSignedKES

instance Crypto c => DecCBOR (Annotator (BHeaderRaw c)) where
  decCBOR = pure <$> decCBOR

newtype BHeader c = BHeaderConstr (MemoBytes (BHeaderRaw c))
  deriving (Generic)
  deriving newtype (Eq, Show, NoThunks, Plain.ToCBOR, SafeToHash)

type instance MemoHashIndex (BHeaderRaw c) = EraIndependentBlockHeader

instance HashAnnotated (BHeader c) EraIndependentBlockHeader where
  hashAnnotated = getMemoSafeHash

instance Crypto c => EncCBOR (BHeader c)

instance Memoized (BHeader c) where
  type RawType (BHeader c) = BHeaderRaw c

deriving via
  Mem (BHeaderRaw c)
  instance
    Crypto c => DecCBOR (Annotator (BHeader c))

pattern BHeader ::
  Crypto c =>
  BHBody c ->
  KES.SignedKES (KES c) (BHBody c) ->
  BHeader c
pattern BHeader bHeaderBody' bHeaderSig' <-
  (getMemoRawType -> BHeaderRaw bHeaderBody' bHeaderSig')
  where
    BHeader bHeaderBody bHeaderSig =
      mkMemoized (pvMajor (bprotver bHeaderBody)) $ BHeaderRaw bHeaderBody bHeaderSig

{-# COMPLETE BHeader #-}

-- | Hash a given block header
bhHash :: BHeader c -> HashHeader
bhHash = HashHeader . extractHash . hashAnnotated

-- | HashHeader to Nonce
hashHeaderToNonce :: HashHeader -> Nonce
hashHeaderToNonce (HashHeader h) = Nonce $ Hash.castHash h

prevHashToNonce :: PrevHash -> Nonce
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
issuerIDfromBHBody :: BHBody c -> KeyHash BlockIssuer
issuerIDfromBHBody = hashKey . bheaderVk

bhbody ::
  Crypto c =>
  BHeader c ->
  BHBody c
bhbody (BHeader b _) = b

hBbsize :: BHBody c -> Word32
hBbsize = bsize

-- | Natural value with some additional bound. It must always be the base that
-- 'bvValue <= bvMaxValue'. The creator is responsible for checking this value.
data BoundedNatural = UnsafeBoundedNatural
  { bvMaxValue :: Natural
  , bvValue :: Natural
  }

-- | Assert that a natural is bounded by a certain value. Throws an error when
-- this is not the case.
assertBoundedNatural ::
  -- | Maximum bound
  Natural ->
  -- | Value
  Natural ->
  BoundedNatural
assertBoundedNatural maxVal val =
  if val <= maxVal
    then UnsafeBoundedNatural maxVal val
    else error $ show val <> " is greater than max value " <> show maxVal

-- | Check that the certified VRF output, when used as a natural, is valid for
-- being slot leader.
checkLeaderValue ::
  forall v.
  VRF.VRFAlgorithm v =>
  VRF.OutputVRF v ->
  Rational ->
  ActiveSlotCoeff ->
  Bool
checkLeaderValue certVRF σ f =
  checkLeaderNatValue (assertBoundedNatural certNatMax (VRF.getOutputVRFNatural certVRF)) σ f
  where
    certNatMax :: Natural
    certNatMax = (2 :: Natural) ^ (8 * VRF.sizeOutputVRF certVRF)

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
checkLeaderNatValue ::
  -- | Certified nat value
  BoundedNatural ->
  -- | Stake proportion
  Rational ->
  ActiveSlotCoeff ->
  Bool
checkLeaderNatValue bn σ f =
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
    c, recip_q, x :: FixedPoint
    c = activeSlotLog f
    recip_q =
      case nonZero . toInteger $ certNatMax - certNat of
        Just d -> fromRational (toInteger certNatMax %. d)
        Nothing -> fromIntegral @Natural @FixedPoint certNatMax
    x = -fromRational σ * c
    certNatMax = bvMaxValue bn
    certNat = bvValue bn

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

data LastAppliedBlock = LastAppliedBlock
  { labBlockNo :: !BlockNo
  , labSlotNo :: !SlotNo
  , labHash :: !HashHeader
  }
  deriving (Show, Eq, Generic)

instance NoThunks LastAppliedBlock

instance NFData LastAppliedBlock

instance EncCBOR LastAppliedBlock where
  encCBOR (LastAppliedBlock b s h) =
    encodeListLen 3 <> encCBOR b <> encCBOR s <> encCBOR h

instance DecCBOR LastAppliedBlock where
  decCBOR =
    decodeRecordNamed
      "lastAppliedBlock"
      (const 3)
      ( LastAppliedBlock
          <$> decCBOR
          <*> decCBOR
          <*> decCBOR
      )

lastAppliedHash :: WithOrigin LastAppliedBlock -> PrevHash
lastAppliedHash Origin = GenesisHash
lastAppliedHash (At lab) = BlockHash $ labHash lab

-- | Retrieve the new nonce from the block header body.
bnonce :: BHBody c -> Nonce
bnonce = mkNonceFromOutputVRF . VRF.certifiedOutput . bheaderEta

makeHeaderView :: Crypto c => BHeader c -> Maybe Nonce -> BHeaderView
makeHeaderView bh@(BHeader bhb _) nonce =
  BHeaderView
    (hashKey . bheaderVk $ bhb)
    (bsize bhb)
    (originalBytesSize bh)
    (bhash bhb)
    (bheaderSlotNo bhb)
    nonce
    (bprotver bhb)

instance Crypto c => EraBlockHeader (BHeader c) ShelleyEra where
  blockHeaderIssuerL =
    lens
      (\(Block (BHeader bhb _) _) -> hashKey $ bheaderVk bhb)
      (\(Block (BHeader bhb sig) body) _ -> Block (BHeader bhb sig) body) -- QUESTION: What does it mean to "unhash" and set this?
  blockHeaderBSizeL =
    lens
      (\(Block (BHeader bhb _) _) -> bsize bhb)
      (\(Block (BHeader bhb sig) body) newSize -> Block (BHeader (bhb {bsize = newSize}) sig) body)
  blockHeaderHSizeL =
    lens
      (\(Block bh _) -> originalBytesSize bh)
      (\(Block bh body) _ -> Block bh body) -- QUESTION: What does it mean to do this here?
  blockHeaderBHashL =
    lens
      (\(Block (BHeader bhb _) _) -> bhash bhb)
      (\(Block (BHeader bhb sig) body) newHash -> Block (BHeader (bhb {bhash = newHash}) sig) body)
  blockHeaderSlotL =
    lens
      (\(Block (BHeader bhb _) _) -> bheaderSlotNo bhb)
      (\(Block (BHeader bhb sig) body) newSlot -> Block (BHeader (bhb {bheaderSlotNo = newSlot}) sig) body)
  blockHeaderProtVerL =
    lens
      (\(Block (BHeader bhb _) _) -> bprotver bhb)
      (\(Block (BHeader bhb sig) body) protVer -> Block (BHeader (bhb {bprotver = protVer}) sig) body)

instance Crypto c => EraBlockHeader (BHeader c) AllegraEra where
  blockHeaderIssuerL =
    lens
      (\(Block (BHeader bhb _) _) -> hashKey $ bheaderVk bhb)
      (\(Block (BHeader bhb sig) body) _ -> Block (BHeader bhb sig) body)
  blockHeaderBSizeL =
    lens
      (\(Block (BHeader bhb _) _) -> bsize bhb)
      (\(Block (BHeader bhb sig) body) newSize -> Block (BHeader (bhb {bsize = newSize}) sig) body)
  blockHeaderHSizeL =
    lens
      (\(Block bh _) -> originalBytesSize bh)
      (\(Block bh body) _ -> Block bh body)
  blockHeaderBHashL =
    lens
      (\(Block (BHeader bhb _) _) -> bhash bhb)
      (\(Block (BHeader bhb sig) body) newHash -> Block (BHeader (bhb {bhash = newHash}) sig) body)
  blockHeaderSlotL =
    lens
      (\(Block (BHeader bhb _) _) -> bheaderSlotNo bhb)
      (\(Block (BHeader bhb sig) body) newSlot -> Block (BHeader (bhb {bheaderSlotNo = newSlot}) sig) body)
  blockHeaderProtVerL =
    lens
      (\(Block (BHeader bhb _) _) -> bprotver bhb)
      (\(Block (BHeader bhb sig) body) protVer -> Block (BHeader (bhb {bprotver = protVer}) sig) body)

instance Crypto c => EraBlockHeader (BHeader c) MaryEra where
  blockHeaderIssuerL =
    lens
      (\(Block (BHeader bhb _) _) -> hashKey $ bheaderVk bhb)
      (\(Block (BHeader bhb sig) body) _ -> Block (BHeader bhb sig) body)
  blockHeaderBSizeL =
    lens
      (\(Block (BHeader bhb _) _) -> bsize bhb)
      (\(Block (BHeader bhb sig) body) newSize -> Block (BHeader (bhb {bsize = newSize}) sig) body)
  blockHeaderHSizeL =
    lens
      (\(Block bh _) -> originalBytesSize bh)
      (\(Block bh body) _ -> Block bh body)
  blockHeaderBHashL =
    lens
      (\(Block (BHeader bhb _) _) -> bhash bhb)
      (\(Block (BHeader bhb sig) body) newHash -> Block (BHeader (bhb {bhash = newHash}) sig) body)
  blockHeaderSlotL =
    lens
      (\(Block (BHeader bhb _) _) -> bheaderSlotNo bhb)
      (\(Block (BHeader bhb sig) body) newSlot -> Block (BHeader (bhb {bheaderSlotNo = newSlot}) sig) body)
  blockHeaderProtVerL =
    lens
      (\(Block (BHeader bhb _) _) -> bprotver bhb)
      (\(Block (BHeader bhb sig) body) protVer -> Block (BHeader (bhb {bprotver = protVer}) sig) body)

instance Crypto c => EraBlockHeader (BHeader c) AlonzoEra where
  blockHeaderIssuerL =
    lens
      (\(Block (BHeader bhb _) _) -> hashKey $ bheaderVk bhb)
      (\(Block (BHeader bhb sig) body) _ -> Block (BHeader bhb sig) body)
  blockHeaderBSizeL =
    lens
      (\(Block (BHeader bhb _) _) -> bsize bhb)
      (\(Block (BHeader bhb sig) body) newSize -> Block (BHeader (bhb {bsize = newSize}) sig) body)
  blockHeaderHSizeL =
    lens
      (\(Block bh _) -> originalBytesSize bh)
      (\(Block bh body) _ -> Block bh body)
  blockHeaderBHashL =
    lens
      (\(Block (BHeader bhb _) _) -> bhash bhb)
      (\(Block (BHeader bhb sig) body) newHash -> Block (BHeader (bhb {bhash = newHash}) sig) body)
  blockHeaderSlotL =
    lens
      (\(Block (BHeader bhb _) _) -> bheaderSlotNo bhb)
      (\(Block (BHeader bhb sig) body) newSlot -> Block (BHeader (bhb {bheaderSlotNo = newSlot}) sig) body)
  blockHeaderProtVerL =
    lens
      (\(Block (BHeader bhb _) _) -> bprotver bhb)
      (\(Block (BHeader bhb sig) body) protVer -> Block (BHeader (bhb {bprotver = protVer}) sig) body)

instance Crypto c => EraBlockHeader (BHeader c) BabbageEra where
  blockHeaderIssuerL =
    lens
      (\(Block (BHeader bhb _) _) -> hashKey $ bheaderVk bhb)
      (\(Block (BHeader bhb sig) body) _ -> Block (BHeader bhb sig) body)
  blockHeaderBSizeL =
    lens
      (\(Block (BHeader bhb _) _) -> bsize bhb)
      (\(Block (BHeader bhb sig) body) newSize -> Block (BHeader (bhb {bsize = newSize}) sig) body)
  blockHeaderHSizeL =
    lens
      (\(Block bh _) -> originalBytesSize bh)
      (\(Block bh body) _ -> Block bh body)
  blockHeaderBHashL =
    lens
      (\(Block (BHeader bhb _) _) -> bhash bhb)
      (\(Block (BHeader bhb sig) body) newHash -> Block (BHeader (bhb {bhash = newHash}) sig) body)
  blockHeaderSlotL =
    lens
      (\(Block (BHeader bhb _) _) -> bheaderSlotNo bhb)
      (\(Block (BHeader bhb sig) body) newSlot -> Block (BHeader (bhb {bheaderSlotNo = newSlot}) sig) body)
  blockHeaderProtVerL =
    lens
      (\(Block (BHeader bhb _) _) -> bprotver bhb)
      (\(Block (BHeader bhb sig) body) protVer -> Block (BHeader (bhb {bprotver = protVer}) sig) body)

instance Crypto c => EraBlockHeader (BHeader c) ConwayEra where
  blockHeaderIssuerL =
    lens
      (\(Block (BHeader bhb _) _) -> hashKey $ bheaderVk bhb)
      (\(Block (BHeader bhb sig) body) _ -> Block (BHeader bhb sig) body)
  blockHeaderBSizeL =
    lens
      (\(Block (BHeader bhb _) _) -> bsize bhb)
      (\(Block (BHeader bhb sig) body) newSize -> Block (BHeader (bhb {bsize = newSize}) sig) body)
  blockHeaderHSizeL =
    lens
      (\(Block bh _) -> originalBytesSize bh)
      (\(Block bh body) _ -> Block bh body)
  blockHeaderBHashL =
    lens
      (\(Block (BHeader bhb _) _) -> bhash bhb)
      (\(Block (BHeader bhb sig) body) newHash -> Block (BHeader (bhb {bhash = newHash}) sig) body)
  blockHeaderSlotL =
    lens
      (\(Block (BHeader bhb _) _) -> bheaderSlotNo bhb)
      (\(Block (BHeader bhb sig) body) newSlot -> Block (BHeader (bhb {bheaderSlotNo = newSlot}) sig) body)
  blockHeaderProtVerL =
    lens
      (\(Block (BHeader bhb _) _) -> bprotver bhb)
      (\(Block (BHeader bhb sig) body) protVer -> Block (BHeader (bhb {bprotver = protVer}) sig) body)

instance Crypto c => EraBlockHeader (BHeader c) DijkstraEra where
  blockHeaderIssuerL =
    lens
      (\(Block (BHeader bhb _) _) -> hashKey $ bheaderVk bhb)
      (\(Block (BHeader bhb sig) body) _ -> Block (BHeader bhb sig) body)
  blockHeaderBSizeL =
    lens
      (\(Block (BHeader bhb _) _) -> bsize bhb)
      (\(Block (BHeader bhb sig) body) newSize -> Block (BHeader (bhb {bsize = newSize}) sig) body)
  blockHeaderHSizeL =
    lens
      (\(Block bh _) -> originalBytesSize bh)
      (\(Block bh body) _ -> Block bh body)
  blockHeaderBHashL =
    lens
      (\(Block (BHeader bhb _) _) -> bhash bhb)
      (\(Block (BHeader bhb sig) body) newHash -> Block (BHeader (bhb {bhash = newHash}) sig) body)
  blockHeaderSlotL =
    lens
      (\(Block (BHeader bhb _) _) -> bheaderSlotNo bhb)
      (\(Block (BHeader bhb sig) body) newSlot -> Block (BHeader (bhb {bheaderSlotNo = newSlot}) sig) body)
  blockHeaderProtVerL =
    lens
      (\(Block (BHeader bhb _) _) -> bprotver bhb)
      (\(Block (BHeader bhb sig) body) protVer -> Block (BHeader (bhb {bprotver = protVer}) sig) body)
