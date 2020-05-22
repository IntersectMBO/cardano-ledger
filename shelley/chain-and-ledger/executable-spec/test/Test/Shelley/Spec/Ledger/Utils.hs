{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.Utils
  ( assertAll,
    mkSeedFromWords,
    mkCertifiedVRF,
    epochFromSlotNo,
    evolveKESUntil,
    slotFromEpoch,
    mkKeyPair,
    mkGenKey,
    mkKESKeyPair,
    mkVRFKeyPair,
    mkAddr,
    runShelleyBase,
    testGlobals,
    maxKESIterations,
    unsafeMkUnitInterval,
    slotsPerKESIteration,
    maxLLSupply,
  )
where

import Cardano.Binary (ToCBOR (..))
import Cardano.Crypto.DSIGN (deriveVerKeyDSIGN, genKeyDSIGN)
import Cardano.Crypto.KES (deriveVerKeyKES, genKeyKES)
import Cardano.Crypto.Seed (Seed, mkSeedFromBytes)
import Cardano.Crypto.VRF (deriveVerKeyVRF, evalCertified, genKeyVRF)
import Cardano.Prelude (asks)
import Cardano.Slotting.EpochInfo (epochInfoEpoch, epochInfoFirst, fixedSizeEpochInfo)
import Control.Monad.Trans.Reader (runReaderT)
import Crypto.Random (drgNewTest, withDRG)
import Data.ByteString.Conversion (toByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Hedgehog ((===), MonadTest)
import Shelley.Spec.Ledger.Address (pattern Addr)
import Shelley.Spec.Ledger.BaseTypes
  ( Globals (..),
    Network (..),
    ShelleyBase,
    UnitInterval,
    mkActiveSlotCoeff,
    mkUnitInterval,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Credential (..), StakeReference (..))
import Shelley.Spec.Ledger.Keys (KeyRole (..), hashKey, updateKES, vKey)
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.Slot (EpochNo, EpochSize (..), SlotNo)
import Test.Cardano.Crypto.VRF.Fake (WithResult (..))
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( Addr,
    CertifiedVRF,
    KeyPair,
    SignKeyDSIGN,
    SignKeyKES,
    SignKeyVRF,
    VKey,
    VKeyGenesis,
    VerKeyKES,
    VerKeyVRF,
    pattern VKey,
  )

assertAll :: (MonadTest m, Show a, Eq a) => (a -> Bool) -> [a] -> m ()
assertAll p xs = [] === filter (not . p) xs

-- | Construct a seed from a bunch of Word64s
--
--   We multiply these words by some extra stuff to make sure they contain
--   enough bits for our seed.
mkSeedFromWords ::
  (Word64, Word64, Word64, Word64, Word64) ->
  Seed
mkSeedFromWords (w1, w2, w3, w4, w5) =
  mkSeedFromBytes . BSL.toStrict $
    toByteString (w1 * 15485867)
      <> toByteString (w2 * 32452867)
      <> toByteString (w3 * 49979693)
      <> toByteString (w4 * 67867979)
      <> toByteString (w5 * 86028157)
      <> toByteString (2147483647 :: Word64)

-- | For testing purposes, generate a deterministic genesis key pair given a seed.
mkGenKey :: (Word64, Word64, Word64, Word64, Word64) -> (SignKeyDSIGN, VKeyGenesis)
mkGenKey seed =
  let sk = genKeyDSIGN $ mkSeedFromWords seed
   in (sk, VKey $ deriveVerKeyDSIGN sk)

-- | For testing purposes, generate a deterministic key pair given a seed.
mkKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> (SignKeyDSIGN, VKey kr)
mkKeyPair seed =
  let sk = genKeyDSIGN $ mkSeedFromWords seed
   in (sk, VKey $ deriveVerKeyDSIGN sk)

-- | For testing purposes, generate a deterministic VRF key pair given a seed.
mkVRFKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> (SignKeyVRF, VerKeyVRF)
mkVRFKeyPair seed =
  let sk = genKeyVRF $ mkSeedFromWords seed
   in (sk, deriveVerKeyVRF sk)

-- | For testing purposes, create a VRF value
mkCertifiedVRF ::
  ToCBOR a =>
  WithResult a ->
  SignKeyVRF ->
  CertifiedVRF a
mkCertifiedVRF a sk =
  fst . withDRG (drgNewTest seed) $
    coerce <$> evalCertified () a sk
  where
    seed = (4, 0, 0, 0, 1)

-- | For testing purposes, generate a deterministic KES key pair given a seed.
mkKESKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> (SignKeyKES, VerKeyKES)
mkKESKeyPair seed =
  let sk = genKeyKES $ mkSeedFromWords seed
   in (sk, deriveVerKeyKES sk)

mkAddr :: (KeyPair 'Payment, KeyPair 'Staking) -> Addr
mkAddr (payKey, stakeKey) =
  Addr
    Testnet
    (KeyHashObj . hashKey $ vKey payKey)
    (StakeRefBase . KeyHashObj . hashKey $ vKey stakeKey)

-- | You vouch that argument is in [0; 1].
unsafeMkUnitInterval :: Rational -> UnitInterval
unsafeMkUnitInterval r =
  fromMaybe (error "could not construct unit interval") $ mkUnitInterval r

testGlobals :: Globals
testGlobals =
  Globals
    { epochInfo = fixedSizeEpochInfo $ EpochSize 100,
      slotsPerKESPeriod = 20,
      stabilityWindow = 33,
      randomnessStabilisationWindow = 33,
      securityParameter = 10,
      maxKESEvo = 10,
      quorum = 5,
      maxMajorPV = 1000,
      maxLovelaceSupply = 45 * 1000 * 1000 * 1000 * 1000 * 1000,
      activeSlotCoeff = mkActiveSlotCoeff . unsafeMkUnitInterval $ 0.9,
      networkId = Testnet
    }

runShelleyBase :: ShelleyBase a -> a
runShelleyBase act = runIdentity $ runReaderT act testGlobals

epochFromSlotNo :: SlotNo -> EpochNo
epochFromSlotNo = runIdentity . epochInfoEpoch (epochInfo testGlobals)

slotFromEpoch :: EpochNo -> SlotNo
slotFromEpoch = runIdentity . epochInfoFirst (epochInfo testGlobals)

-- | Try to evolve KES key until specific KES period is reached, given the
-- current KES period.
evolveKESUntil ::
  SignKeyKES ->
  -- | Current KES period
  KESPeriod ->
  -- | Target KES period
  KESPeriod ->
  Maybe SignKeyKES
evolveKESUntil sk1 (KESPeriod current) (KESPeriod target) = go sk1 current target
  where
    go !_ c t | t < c = Nothing
    go !sk c t | c == t = Just sk
    go !sk c t = case updateKES () sk c of
      Nothing -> Nothing
      Just sk' -> go sk' (c + 1) t

maxKESIterations :: Word64
maxKESIterations = runShelleyBase (asks maxKESEvo)

slotsPerKESIteration :: Word64
slotsPerKESIteration = runShelleyBase (asks slotsPerKESPeriod)

maxLLSupply :: Coin
maxLLSupply = Coin $ fromIntegral $ runShelleyBase (asks maxLovelaceSupply)
