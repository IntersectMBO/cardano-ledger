{-# LANGUAGE PatternSynonyms #-}

module Test.Shelley.Spec.Ledger.Utils
  ( assertAll
  , mkCertifiedVRF
  , epochFromSlotNo
  , evolveKESUntil
  , slotFromEpoch
  , mkKeyPair
  , mkGenKey
  , mkKESKeyPair
  , mkVRFKeyPair
  , mkAddr
  , runShelleyBase
  , testGlobals
  , maxKESIterations
  , unsafeMkUnitInterval
  , slotsPerKESIteration
  , maxLLSupply
  ) where

import           Cardano.Binary (ToCBOR (..))
import           Cardano.Crypto.DSIGN (deriveVerKeyDSIGN, genKeyDSIGN)
import           Cardano.Crypto.KES (deriveVerKeyKES, genKeyKES)
import           Cardano.Crypto.VRF (deriveVerKeyVRF, evalCertified, genKeyVRF)
import           Cardano.Prelude (asks)
import           Cardano.Slotting.EpochInfo (epochInfoEpoch, epochInfoFirst, fixedSizeEpochInfo)
import           Control.Monad.Trans.Reader (runReaderT)
import           Crypto.Random (drgNewTest, withDRG)
import           Data.Coerce (coerce)
import           Data.Functor.Identity (runIdentity)
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)
import           Hedgehog (MonadTest, (===))
import           Shelley.Spec.Ledger.BaseTypes (Globals (..), ShelleyBase, UnitInterval,
                     mkActiveSlotCoeff, mkUnitInterval)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Keys (pattern SKey, pattern SKeyES, pattern VKey,
                     pattern VKeyES, pattern VKeyGenesis, hashKey, updateKESKey, vKey)
import           Shelley.Spec.Ledger.OCert (KESPeriod (..))
import           Shelley.Spec.Ledger.Slot (EpochNo, EpochSize (..), SlotNo)
import           Shelley.Spec.Ledger.TxData (pattern Addr, pattern KeyHashObj, pattern StakeRefBase)

import           Test.Cardano.Crypto.VRF.Fake (WithResult (..))
import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Addr, CertifiedVRF, KeyPair, SKey,
                     SKeyES, SignKeyVRF, VKey, VKeyES, VKeyGenesis, VerKeyVRF)

assertAll :: (MonadTest m, Show a, Eq a) => (a -> Bool) -> [a] -> m ()
assertAll p xs = [] === filter (not . p) xs

-- | For testing purposes, generate a deterministic genesis key pair given a seed.
mkGenKey :: (Word64, Word64, Word64, Word64, Word64) -> (SKey, VKeyGenesis)
mkGenKey seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyDSIGN
  return (SKey sk, VKeyGenesis $ deriveVerKeyDSIGN sk)


-- | For testing purposes, generate a deterministic key pair given a seed.
mkKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> (SKey, VKey)
mkKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyDSIGN
  return (SKey sk, VKey $ deriveVerKeyDSIGN sk)

-- | For testing purposes, generate a deterministic VRF key pair given a seed.
mkVRFKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> (SignKeyVRF, VerKeyVRF)
mkVRFKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyVRF
  return (sk, deriveVerKeyVRF sk)

-- | For testing purposes, create a VRF value
mkCertifiedVRF
  :: ToCBOR a
  => WithResult a
  -> SignKeyVRF
  -> CertifiedVRF a
mkCertifiedVRF a sk = fst . withDRG (drgNewTest seed) $
    coerce <$> evalCertified () a sk
  where
    seed = (4,0,0,0,1)

-- | For testing purposes, generate a deterministic KES key pair given a seed.
mkKESKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> (SKeyES, VKeyES)
mkKESKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyKES $ fromIntegral (runShelleyBase (asks maxKESEvo))
  return (SKeyES sk, VKeyES $ deriveVerKeyKES sk)

mkAddr :: (KeyPair, KeyPair) -> Addr
mkAddr (payKey, stakeKey) =
  Addr (KeyHashObj . hashKey $ vKey payKey)
       (StakeRefBase . KeyHashObj . hashKey $ vKey stakeKey)

-- | You vouch that argument is in [0; 1].
unsafeMkUnitInterval :: Rational -> UnitInterval
unsafeMkUnitInterval r =
  fromMaybe (error "could not construct unit interval") $ mkUnitInterval r

testGlobals :: Globals
testGlobals = Globals
  { epochInfo = fixedSizeEpochInfo $ EpochSize 100
  , slotsPerKESPeriod = 20
  , startRewards = 33
  , slotsPrior = 33
  , securityParameter = 10
  , maxKESEvo = 10
  , quorum = 5
  , maxMajorPV = 1000
  , maxLovelaceSupply = 45*1000*1000*1000*1000*1000
  , activeSlotCoeff = mkActiveSlotCoeff . unsafeMkUnitInterval $ 0.9
  }

runShelleyBase :: ShelleyBase a -> a
runShelleyBase act = runIdentity $ runReaderT act testGlobals

epochFromSlotNo :: SlotNo -> EpochNo
epochFromSlotNo = runIdentity . epochInfoEpoch (epochInfo testGlobals)

slotFromEpoch :: EpochNo -> SlotNo
slotFromEpoch = runIdentity  . epochInfoFirst (epochInfo testGlobals)

-- | Try to evolve KES key until specific KES period is reached.
evolveKESUntil :: SKeyES -> KESPeriod -> Maybe SKeyES
evolveKESUntil key (KESPeriod period) = updateKESKey key period

maxKESIterations :: Word64
maxKESIterations = runShelleyBase (asks maxKESEvo)

slotsPerKESIteration :: Word64
slotsPerKESIteration = runShelleyBase (asks slotsPerKESPeriod)

maxLLSupply :: Coin
maxLLSupply = Coin $ fromIntegral $ runShelleyBase (asks maxLovelaceSupply)
