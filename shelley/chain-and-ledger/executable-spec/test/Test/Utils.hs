{-# LANGUAGE PatternSynonyms #-}

module Test.Utils
  ( assertAll
  , mkGenKey
  , mkCertifiedVRF
  , mkKeyPair
  , mkKESKeyPair
  , mkVRFKeyPair
  , mkAddr
  , unsafeMkUnitInterval
  ) where

import           BaseTypes (UnitInterval, mkUnitInterval)
import           Cardano.Binary (ToCBOR)
import           Cardano.Crypto.DSIGN (deriveVerKeyDSIGN, genKeyDSIGN)
import           Cardano.Crypto.KES (deriveVerKeyKES, genKeyKES)
import           Cardano.Crypto.VRF (deriveVerKeyVRF, evalCertified, genKeyVRF)
import           Cardano.Crypto.VRF.Fake (WithResult (..))
import           Crypto.Random (drgNewTest, withDRG)
import           Data.Coerce (coerce)
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)
import           Keys (pattern SKey, pattern SKeyES, pattern VKey, pattern VKeyES,
                     pattern VKeyGenesis, hashKey, vKey)
import           MockTypes (Addr, CertifiedVRF, KeyPair, SKey, SKeyES, SignKeyVRF, VKey, VKeyES,
                     VKeyGenesis, VerKeyVRF)
import           TxData (pattern AddrBase, pattern KeyHashObj)

import           Hedgehog (MonadTest, (===))


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
