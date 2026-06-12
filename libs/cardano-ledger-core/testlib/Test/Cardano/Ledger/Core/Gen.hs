{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.Core.Gen (
  -- * Generators
  genOutputCoin,
  genBalanceCoin,
  genChainAccountState,

  -- * Testing hooks
  outputCoinGenRange,
  outputCoinGenQuantile,
  balanceCoinGenRange,
  balanceCoinGenQuantile,
) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.State
import Data.Bifunctor (bimap)
import Test.Cardano.Ledger.Imp.Common (
  HasStatefulGen,
  MonadGen,
  truncatedLogNormalDouble,
 )

coinToDouble :: Coin -> Double
coinToDouble = fromInteger . unCoin

doubleToCoin :: Double -> Coin
doubleToCoin = Coin . round

-- | The number of standard deviations away from the mean that has this cumulative probability.
--
-- It's the inverse of the cumulative distribution function (CDF), which for the standard normal
-- distribution is named Φ.
--
-- Generate these with `quantile (normalDistr 0 1)` from the `statistics` package.
stdNormalQuantile :: Rational -> Double
stdNormalQuantile = \case
  p | p < 0.5 -> negate $ stdNormalQuantile (1 - p)
  0.5 -> 0.0
  0.6 -> 0.253_347_103_135_800
  0.7 -> 0.524_400_512_708_041
  0.8 -> 0.841_621_233_572_914
  0.9 -> 1.281_551_565_544_601
  0.99 -> 2.326_347_874_040_841
  0.999 -> 3.090_232_306_167_8136
  0.999_9 -> 3.719_016_485_455_709
  0.999_99 -> 4.264_890_793_923_841
  0.999_999 -> 4.753_424_308_817_088
  p -> error $ "No precomputed quantile for " <> show p

-- | Generate a Coin amount in the given range using a log-normal probability distribution
--
-- Uses the INLINE pragma so that the params can be floated
genCoin :: (MonadGen m, HasStatefulGen g m) => (Coin, Coin) -> m Coin
genCoin (coinToDouble -> lo, coinToDouble -> hi) = do
  let (mu, sigma) = coinGenParams (lo, hi)
  doubleToCoin <$> truncatedLogNormalDouble mu sigma lo hi
{-# INLINE genCoin #-}

-- | Calculate the parameters for a truncated log-normal distribution with the given endpoints
--
-- We set the parameters (µ and σ of the underlying normal distribution) such
-- that the probability of a non-truncated value falling below the minimum is
-- 0.01 and below the maximum is 0.999. This reduces the frequency with which
-- `truncatedLogNormalDouble` has to retry, and provides a reasonably smooth
-- taper at each end of the truncated range.
coinGenParams :: (Double, Double) -> (Double, Double)
coinGenParams (lo, hi) = (mu, sigma)
  where
    logLo = log lo
    logHi = log hi
    quantileLo = stdNormalQuantile 0.01
    quantileHi = stdNormalQuantile 0.999
    -- The log-range should be equal to (quantileHi - quantileLo) sigmas
    sigma = (logHi - logLo) / (quantileHi - quantileLo)
    -- The lower log-limit should be equal to mu + sigma * quantileLo
    mu = logLo - sigma * quantileLo

-- | Generate a Coin value suitable for a transaction output
genOutputCoin :: (MonadGen m, HasStatefulGen g m) => m Coin
genOutputCoin = genCoin outputCoinGenRange

-- Exposed for testing
--
-- | With this range, the deciles (in ADA) are approximately:
-- >>> (`div` 1000000) . unCoin <$> [outputCoinGenQuantile p | p <- [0.1, 0.2 .. 0.9]]
-- [22,82,213,477,1014,2156,4831,12418,45982]
outputCoinGenRange :: (Coin, Coin)
outputCoinGenRange = (Coin 1_000_000, Coin 5_000_000_000_000)

-- Exposed for testing
outputCoinGenQuantile :: Rational -> Coin
outputCoinGenQuantile p = doubleToCoin $ exp $ mu + sigma * stdNormalQuantile p
  where
    (mu, sigma) = coinGenParams $ bimap coinToDouble coinToDouble outputCoinGenRange

-- | Generate a Coin value suitable for a balance, eg in a wallet
genBalanceCoin :: (MonadGen m, HasStatefulGen g m) => m Coin
genBalanceCoin = genCoin balanceCoinGenRange

-- Exposed for testing
--
-- | With this range, the deciles (in ADA) are approximately:
-- >>> (`div` 1000000) . unCoin <$> [balanceCoinGenQuantile p | p <- [0.1, 0.2 .. 0.9]]
-- [22398,82938,213160,477526,1014857,2156814,4831732,12418130,45982295]
balanceCoinGenRange :: (Coin, Coin)
balanceCoinGenRange = (Coin 1_000_000_000, Coin 5_000_000_000_000_000)

-- Exposed for testing
balanceCoinGenQuantile :: Rational -> Coin
balanceCoinGenQuantile p = doubleToCoin $ exp $ mu + sigma * stdNormalQuantile p
  where
    (mu, sigma) = coinGenParams $ bimap coinToDouble coinToDouble balanceCoinGenRange

-- | Generate a `ChainAccountState`
genChainAccountState :: (MonadGen m, HasStatefulGen g m) => m ChainAccountState
genChainAccountState = ChainAccountState <$> genBalanceCoin <*> genBalanceCoin
