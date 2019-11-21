{-# LANGUAGE PatternSynonyms #-}

module Test.Utils
  ( assertAll
  , mkGenKeys
  , mkKeyPair
  , mkVRFKeyPair
  , mkAddr
  , unsafeMkUnitInterval
  ) where

import           BaseTypes (UnitInterval, mkUnitInterval)
import           Cardano.Crypto.DSIGN (deriveVerKeyDSIGN, genKeyDSIGN)
import           Cardano.Crypto.VRF (deriveVerKeyVRF, genKeyVRF)
import           Crypto.Random (drgNewTest, withDRG)
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)
import           Keys (pattern SKey, pattern VKey, pattern VKeyGenesis, hashKey, vKey)
import           MockTypes (Addr, KeyPair, SKey, SignKeyVRF, VKey, VKeyGenesis, VerKeyVRF)
import           TxData (pattern AddrBase, pattern KeyHashObj)

import           Hedgehog (MonadTest, (===))


assertAll :: (MonadTest m, Show a, Eq a) => (a -> Bool) -> [a] -> m ()
assertAll p xs = [] === filter (not . p) xs

mkGenKeys :: (Word64, Word64, Word64, Word64, Word64) -> (SKey, VKeyGenesis)
mkGenKeys seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyDSIGN
  return (SKey sk, VKeyGenesis $ deriveVerKeyDSIGN sk)


mkKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> (SKey, VKey)
mkKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyDSIGN
  return (SKey sk, VKey $ deriveVerKeyDSIGN sk)

mkVRFKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> (SignKeyVRF, VerKeyVRF)
mkVRFKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyVRF
  return (sk, deriveVerKeyVRF sk)

mkAddr :: (KeyPair, KeyPair) -> Addr
mkAddr (payKey, stakeKey) =
  AddrBase (KeyHashObj . hashKey $ vKey payKey)
           (KeyHashObj . hashKey $ vKey stakeKey)

-- | You vouch that argument is in [0; 1].
unsafeMkUnitInterval :: Rational -> UnitInterval
unsafeMkUnitInterval r =
  fromMaybe (error "could not construct unit interval") $ mkUnitInterval r
