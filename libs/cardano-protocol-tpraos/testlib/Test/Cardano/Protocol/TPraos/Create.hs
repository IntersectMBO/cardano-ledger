{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Protocol.TPraos.Create (
  AllIssuerKeys (..),
  KESKeyPair (..),
  VRFKeyPair (..),
  mkOCert,
  mkBHBody,
  mkBHBodyFakeVRF,
  mkBHeader,
  mkBlock,
  mkBlockFakeVRF,
  evolveKESUntil,
) where

import Cardano.Crypto.DSIGN (Signable)
import qualified Cardano.Crypto.KES.Class as KES
import qualified Cardano.Crypto.VRF.Class as VRF
import Cardano.Ledger.BaseTypes (
  BlockNo,
  Nonce,
  ProtVer (..),
  Seed,
  SlotNo,
  UnitInterval,
  unboundRational,
 )
import Cardano.Ledger.Block
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (
  HasKeyRole (coerceKeyRole),
  Hash,
  KeyHash,
  KeyRole (..),
  signedDSIGN,
  signedKES,
 )
import Cardano.Protocol.TPraos.BHeader (
  BHBody (..),
  BHeader (..),
  HashHeader,
  PrevHash (BlockHash),
  mkSeed,
  seedEta,
  seedL,
 )
import Cardano.Protocol.TPraos.OCert (
  KESPeriod (..),
  OCert (..),
  OCertSignable (..),
 )
import Data.Coerce
import Data.List.NonEmpty as NE
import Data.Ratio (denominator, numerator, (%))
import Data.Sequence.Strict as StrictSeq
import Data.Word
import Numeric.Natural
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Protocol.Crypto.KES (KESKeyPair (..))
import Test.Cardano.Protocol.Crypto.VRF (VRFKeyPair (..))
import Test.Cardano.Protocol.Crypto.VRF.Fake (NatNonce (..), WithResult (..))

data AllIssuerKeys v (r :: KeyRole) = AllIssuerKeys
  { aikCold :: KeyPair r v
  , aikVrf :: VRFKeyPair v
  , aikHot :: NonEmpty (KESPeriod, KESKeyPair v)
  , aikColdKeyHash :: KeyHash r v
  }
  deriving (Show)

mkOCert ::
  forall c r.
  (Crypto c, Signable (DSIGN c) (OCertSignable c)) =>
  AllIssuerKeys c r ->
  Word64 ->
  KESPeriod ->
  OCert c
mkOCert pKeys kesPeriod keyRegKesPeriod =
  let vKeyHot = kesVerKey $ snd $ NE.head $ aikHot pKeys
      sKeyCold = sKey $ aikCold pKeys
   in OCert
        { ocertVkHot = vKeyHot
        , ocertN = kesPeriod
        , ocertKESPeriod = keyRegKesPeriod
        , ocertSigma = signedDSIGN @c sKeyCold (OCertSignable vKeyHot kesPeriod keyRegKesPeriod)
        }

mkBHBody ::
  ( VRF.ContextVRF (VRF v) ~ ()
  , VRF.Signable (VRF v) Seed
  , VRF.VRFAlgorithm (VRF v)
  ) =>
  ProtVer ->
  HashHeader v ->
  AllIssuerKeys v r ->
  SlotNo ->
  BlockNo ->
  Nonce ->
  OCert v ->
  Word32 ->
  Hash v EraIndependentBlockBody ->
  BHBody v
mkBHBody = mkBHBodyWithVRF (VRF.evalCertified ()) (VRF.evalCertified ())

mkBHBodyFakeVRF ::
  ( VRF.ContextVRF (VRF v) ~ ()
  , VRF.Signable (VRF v) (WithResult Seed)
  , VRF.VRFAlgorithm (VRF v)
  ) =>
  NatNonce ->
  UnitInterval ->
  ProtVer ->
  HashHeader v ->
  AllIssuerKeys v r ->
  SlotNo ->
  BlockNo ->
  Nonce ->
  OCert v ->
  Word32 ->
  Hash v EraIndependentBlockBody ->
  BHBody v
mkBHBodyFakeVRF (NatNonce bnonce) l =
  mkBHBodyWithVRF
    (\nonce -> VRF.evalCertified () (WithResult nonce (fromIntegral bnonce)))
    (\nonce -> VRF.evalCertified () (WithResult nonce (unitIntervalToWord64 l)))

-- | Try to map the unit interval to a 64bit natural number. We don't care whether
-- this is surjective. But it should be right inverse to `fromNatural` - that
-- is, one should be able to recover the `UnitInterval` value used here.
unitIntervalToWord64 :: UnitInterval -> Word64
unitIntervalToWord64 ui =
  toWord64 ((toInteger (maxBound :: Word64) % 1) * unboundRational ui)
  where
    toWord64 r = fromInteger (numerator r `quot` denominator r)

mkBHBodyWithVRF ::
  ( Coercible a (VRF.CertifiedVRF (VRF c) Nonce)
  , Coercible b (VRF.CertifiedVRF (VRF c) Natural)
  ) =>
  (Seed -> VRF.SignKeyVRF (VRF c) -> a) ->
  (Seed -> VRF.SignKeyVRF (VRF c) -> b) ->
  ProtVer ->
  HashHeader c ->
  AllIssuerKeys c r ->
  SlotNo ->
  BlockNo ->
  Nonce ->
  OCert c ->
  Word32 ->
  Hash c EraIndependentBlockBody ->
  BHBody c
mkBHBodyWithVRF mkVrfEta mkVrfL protVer prev pKeys slotNo blockNo enonce oCert bodySize bodyHash =
  let nonceNonce = mkSeed seedEta slotNo enonce
      leaderNonce = mkSeed seedL slotNo enonce
      vKeyCold = vKey $ aikCold pKeys
   in BHBody
        { bheaderBlockNo = blockNo
        , bheaderSlotNo = slotNo
        , bheaderPrev = BlockHash prev
        , bheaderVk = coerceKeyRole vKeyCold
        , bheaderVrfVk = vrfVerKey $ aikVrf pKeys
        , bheaderEta = coerce $ mkVrfEta nonceNonce (vrfSignKey $ aikVrf pKeys)
        , bheaderL = coerce $ mkVrfL leaderNonce (vrfSignKey $ aikVrf pKeys)
        , bsize = bodySize
        , bhash = bodyHash
        , bheaderOCert = oCert
        , bprotver = protVer
        }

mkBHeader ::
  (Crypto c, KES.Signable (KES c) (BHBody c)) =>
  AllIssuerKeys c r ->
  Word ->
  -- | KES period of key registration
  Word ->
  BHBody c ->
  BHeader c
mkBHeader pKeys kesPeriod keyRegKesPeriod bhBody =
  let sHot = kesSignKey $ snd $ NE.head $ aikHot pKeys
      kpDiff = kesPeriod - keyRegKesPeriod
      hotKey = case evolveKESUntil sHot (KESPeriod 0) (KESPeriod kpDiff) of
        Nothing ->
          error $
            mconcat
              [ "Could not evolve key to iteration. "
              , "keyRegKesPeriod: " ++ show keyRegKesPeriod
              , "kesPeriod: " ++ show kesPeriod
              , "kpDiff: " ++ show kpDiff
              ]
        Just hKey -> hKey
      sig = signedKES () kpDiff bhBody hotKey
   in BHeader bhBody sig

-- | Try to evolve KES key until specific KES period is reached, given the
-- current KES period.
evolveKESUntil ::
  (KES.KESAlgorithm v, KES.ContextKES v ~ ()) =>
  KES.SignKeyKES v ->
  -- | Current KES period
  KESPeriod ->
  -- | Target KES period
  KESPeriod ->
  Maybe (KES.SignKeyKES v)
evolveKESUntil sk1 (KESPeriod current) (KESPeriod target) = go sk1 current target
  where
    go !_ c t | t < c = Nothing
    go !sk c t | c == t = Just sk
    go !sk c t = case KES.updateKES () sk c of
      Nothing -> Nothing
      Just sk' -> go sk' (c + 1) t

mkBlock ::
  forall era r.
  ( EraSegWits era
  , VRF.Signable (VRF (EraCrypto era)) Seed
  , KES.Signable (KES (EraCrypto era)) (BHBody (EraCrypto era))
  ) =>
  -- | Hash of previous block
  HashHeader (EraCrypto era) ->
  -- | All keys in the stake pool
  AllIssuerKeys (EraCrypto era) r ->
  -- | Transactions to record
  [Tx era] ->
  -- | Current slot
  SlotNo ->
  -- | Block number/chain length/chain "difficulty"
  BlockNo ->
  -- | EpochNo nonce
  Nonce ->
  -- | Period of KES (key evolving signature scheme)
  Word ->
  -- | KES period of key registration
  Word ->
  -- | Operational certificate
  OCert (EraCrypto era) ->
  Block (BHeader (EraCrypto era)) era
mkBlock prev pKeys txns slotNo blockNo enonce kesPeriod keyRegKesPeriod oCert =
  let protVer = ProtVer (eraProtVerHigh @era) 0
      txseq = toTxSeq @era (StrictSeq.fromList txns)
      bodySize = fromIntegral $ bBodySize protVer txseq
      bodyHash = hashTxSeq @era txseq
      bhBody = mkBHBody protVer prev pKeys slotNo blockNo enonce oCert bodySize bodyHash
      bHeader = mkBHeader pKeys kesPeriod keyRegKesPeriod bhBody
   in Block bHeader txseq

-- | Create a block with a faked VRF result.
mkBlockFakeVRF ::
  forall era r.
  ( EraSegWits era
  , VRF.Signable (VRF (EraCrypto era)) (WithResult Seed)
  , KES.Signable (KES (EraCrypto era)) (BHBody (EraCrypto era))
  ) =>
  -- | Hash of previous block
  HashHeader (EraCrypto era) ->
  -- | All keys in the stake pool
  AllIssuerKeys (EraCrypto era) r ->
  -- | Transactions to record
  [Tx era] ->
  -- | Current slot
  SlotNo ->
  -- | Block number\/chain length\/chain "difficulty"
  BlockNo ->
  -- | EpochNo nonce
  Nonce ->
  -- | Block nonce
  NatNonce ->
  -- | Praos leader value
  UnitInterval ->
  -- | Period of KES (key evolving signature scheme)
  Word ->
  -- | KES period of key registration
  Word ->
  -- | Operational certificate
  OCert (EraCrypto era) ->
  Block (BHeader (EraCrypto era)) era
mkBlockFakeVRF prev pKeys txns slotNo blockNo enonce bnonce l kesPeriod keyRegKesPeriod oCert =
  let protVer = ProtVer (eraProtVerHigh @era) 0
      txSeq = toTxSeq @era (StrictSeq.fromList txns)
      bodySize = fromIntegral $ bBodySize protVer txSeq
      bodyHash = hashTxSeq txSeq
      bhBody =
        mkBHBodyFakeVRF bnonce l protVer prev pKeys slotNo blockNo enonce oCert bodySize bodyHash
      bHeader = mkBHeader pKeys kesPeriod keyRegKesPeriod bhBody
   in Block bHeader txSeq
