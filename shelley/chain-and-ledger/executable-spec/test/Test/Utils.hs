{-# LANGUAGE PatternSynonyms #-}

module Test.Utils
  ( assertAll
  , mkGenKeys
  , mkKeyPair
  , mkAddr
  , unsafeMkUnitInterval
  ) where

import           BaseTypes (UnitInterval, mkUnitInterval)
import           Cardano.Crypto.DSIGN (deriveVerKeyDSIGN, genKeyDSIGN)
import           Crypto.Random (drgNewTest, withDRG)
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)
import           Keys (pattern SKey, pattern VKey, pattern VKeyGenesis, hashKey, vKey)
import           MockTypes (Addr, KeyPair, SKey, VKey, VKeyGenesis)
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

mkAddr :: (KeyPair, KeyPair) -> Addr
mkAddr (payKey, stakeKey) =
  AddrBase (KeyHashObj . hashKey $ vKey payKey)
           (KeyHashObj . hashKey $ vKey stakeKey)

-- | You vouch that argument is in [0; 1].
unsafeMkUnitInterval :: Rational -> UnitInterval
unsafeMkUnitInterval r =
  fromMaybe (error "could not construct unit interval") $ mkUnitInterval r
