{-# LANGUAGE PatternSynonyms #-}

module Test.Utils
  ( assertAll
  , mkCertifiedVRF
  , epochFromSlotNo
  , slotFromEpoch
  , mkKeyPair
  , mkGenKey
  , mkKESKeyPair
  , mkVRFKeyPair
  , mkAddr
  , runShelleyBase
  , testGlobals
  , unsafeMkUnitInterval
  ) where

import           BaseTypes (Globals (..), ShelleyBase, UnitInterval, mkUnitInterval)
import           Cardano.Binary (ToCBOR (..))
import           Cardano.Crypto.DSIGN (deriveVerKeyDSIGN, genKeyDSIGN)
import           Cardano.Crypto.KES (deriveVerKeyKES, genKeyKES)
import           Cardano.Crypto.VRF (deriveVerKeyVRF, evalCertified, genKeyVRF)
import           Cardano.Crypto.VRF.Fake (WithResult (..))
import           Cardano.Slotting.EpochInfo (epochInfoEpoch, epochInfoFirst, fixedSizeEpochInfo)
import           Control.Monad.Trans.Reader (runReaderT)
import           Crypto.Random (drgNewTest, withDRG)
import           Data.Coerce (coerce)
import           Data.Functor.Identity (runIdentity)
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)
import           Hedgehog (MonadTest, (===))
import           Keys (pattern SKey, pattern SKeyES, pattern VKey, pattern VKeyES,
                     pattern VKeyGenesis, hashKey, vKey)
import           MockTypes (Addr, CertifiedVRF, KeyPair, SKey, SKeyES, SignKeyVRF, VKey, VKeyES,
                     VKeyGenesis, VerKeyVRF)
import           Slot (EpochNo, EpochSize (..), SlotNo)
import           TxData (pattern AddrBase, pattern KeyHashObj)


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
  sk <- genKeyKES 90
  return (SKeyES sk, VKeyES $ deriveVerKeyKES sk)

mkAddr :: (KeyPair, KeyPair) -> Addr
mkAddr (payKey, stakeKey) =
  AddrBase (KeyHashObj . hashKey $ vKey payKey)
           (KeyHashObj . hashKey $ vKey stakeKey)

-- | You vouch that argument is in [0; 1].
unsafeMkUnitInterval :: Rational -> UnitInterval
unsafeMkUnitInterval r =
  fromMaybe (error "could not construct unit interval") $ mkUnitInterval r

testGlobals :: Globals
testGlobals = Globals
  { epochInfo = fixedSizeEpochInfo $ EpochSize 100
  , slotsPerKESPeriod = 90
  }

runShelleyBase :: ShelleyBase a -> a
runShelleyBase act = runIdentity $ runReaderT act testGlobals

epochFromSlotNo :: SlotNo -> EpochNo
epochFromSlotNo = runIdentity . epochInfoEpoch (epochInfo testGlobals)

slotFromEpoch :: EpochNo -> SlotNo
slotFromEpoch = runIdentity  . epochInfoFirst (epochInfo testGlobals)
