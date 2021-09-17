{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Secure generation of random numbers and 'ByteString's
module Cardano.Crypto.Random
  ( SecureRandom (..),
    deterministic,
    randomNumber,
    randomNumberInRange,
  )
where

import Cardano.Prelude
import Crypto.Number.Basic (numBytes)
import Crypto.Number.Serialize (os2ip)
import Crypto.Random
  ( ChaChaDRG,
    MonadPseudoRandom,
    MonadRandom,
    drgNewSeed,
    getRandomBytes,
    seedFromInteger,
    withDRG,
  )
import Crypto.Random.Entropy (getEntropy)

-- | You can use 'runSecureRandom' on any 'MonadRandom' computation to
-- use the operating  system entropy source to satisfy every request for
-- randomness. That is, this does not use a fixed entropy pool shared across
-- all requests; it gets entropy from the operating  system for every request.
--
-- This is suitable for key generation but is inappropriate for other uses
-- since it can quickly drain the operating system entropy.
newtype SecureRandom a = SecureRandom
  { runSecureRandom :: IO a
  }
  deriving (Functor, Applicative, Monad)

instance MonadRandom SecureRandom where
  getRandomBytes n = SecureRandom (getEntropy n)

-- | You can use 'deterministic' on any 'MonadRandom' computation to make it use
--   a seed (hopefully produced by a Really Secure™ randomness source). The seed
--   has to have enough entropy to make this function secure.
deterministic :: ByteString -> MonadPseudoRandom ChaChaDRG a -> a
deterministic seed gen = fst $ withDRG chachaSeed gen
  where
    chachaSeed = drgNewSeed . seedFromInteger . os2ip $ seed

-- | Generate a random number in range [0, n)
--
--   We want to avoid modulo bias, so we use the arc4random_uniform
--   implementation (http://stackoverflow.com/a/20051580/615030). Specifically,
--   we repeatedly generate a random number in range [0, 2^x) until we hit on
--   something outside of [0, 2^x mod n), which means that it'll be in range
--   [2^x mod n, 2^x). The amount of numbers in this interval is guaranteed to
--   be divisible by n, and thus applying 'mod' to it will be safe.
randomNumber :: forall m. MonadRandom m => Integer -> m Integer
randomNumber n
  | n <= 0 = panic "randomNumber: n <= 0"
  | otherwise = gen
  where
    size = max 4 (numBytes n) -- size of integers, in bytes
    rangeMod = 2 ^ (size * 8) `rem` n -- 2^x mod n
    gen :: m Integer
    gen = do
      x <- os2ip @ByteString <$> getRandomBytes size
      if x < rangeMod then gen else return (x `rem` n)

-- | Generate a random number in range [a, b]
randomNumberInRange :: MonadRandom m => Integer -> Integer -> m Integer
randomNumberInRange a b
  | a > b = panic "randomNumberInRange: a > b"
  | otherwise = (a +) <$> randomNumber (b - a + 1)
