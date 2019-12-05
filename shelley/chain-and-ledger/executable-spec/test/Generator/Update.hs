{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Generator.Update
  ( genPParams )
  where

import           Data.Ratio ((%))
import           Hedgehog (Gen)

import qualified Hedgehog.Gen as Gen

import           BaseTypes (Nonce (NeutralNonce), UnitInterval, mkNonce)
import           Coin (Coin (..))
import           Examples (unsafeMkUnitInterval)
import           Generator.Core (genInteger, genNatural, increasingProbabilityAt)
import qualified Hedgehog.Range as Range
import           Numeric.Natural (Natural)
import           PParams (PParams (..))
import           Slot (Epoch (Epoch))


genRationalInThousands :: Integer -> Integer -> Gen Rational
genRationalInThousands lower upper =
  (% 1000) <$>
    Gen.integral (Range.linear lower upper)

genIntervalInThousands :: Integer -> Integer -> Gen UnitInterval
genIntervalInThousands lower upper =
  unsafeMkUnitInterval <$> genRationalInThousands lower upper

-- TODO @uroboros for now, keeping minA/B at zero until we generate fees in genTx
genPParams :: Gen PParams
genPParams = mkPParams <$> pure 0 -- _minfeeA
                       <*> pure 0 -- _minfeeB
                       <*> szGen  -- (maxBBSize, maxBHSize, maxTxSize)
                       -- keyDeposit
                       -- NOTE: we need to keep these deposits small, otherwise
                       -- when we generate sequences of transactions we will bleed too
                       -- much funds into the deposit pool (i.e. funds not available as utxo)
                       <*> increasingProbabilityAt
                             (Coin <$> genInteger 0 20)
                             (Coin 0, Coin 20)
                       -- keyMinRefund: 0.1-0.5
                       <*> genIntervalInThousands 100 500
                       -- keyDecayRate: 0.001-0.1
                       <*> genRationalInThousands 1 100
                       -- poolDeposit
                       -- NOTE: we need to keep these deposits small, otherwise
                       -- when we generate sequences of transactions we will bleed too
                       -- much funds into the deposit pool (i.e. funds not available as utxo)
                       <*> increasingProbabilityAt
                             (Coin <$> genInteger 0 100)
                             (Coin 0, Coin 100)
                       -- poolMinRefund: 0.1-0.7
                       <*> genIntervalInThousands 100 700
                       -- poolDecayRate: 0.001-0.1
                       <*> genRationalInThousands 1 100
                       -- eMax (for an epoch per 5 days, say, this is between a month and 7yrs)
                       <*> (Epoch <$> genNatural 6 500)
                       -- nOpt
                       <*> Gen.integral (Range.linear 1 100)
                       -- a0: 0.01-1.0
                       <*> genRationalInThousands 10 1000
                       -- rho: 0.001-0.009
                       <*> genIntervalInThousands 1 9
                       -- tau: 0.1-0.3
                       <*> genIntervalInThousands 100 300
                       -- activeSlotCoeff: 0-1
                       <*> increasingProbabilityAt
                             (genIntervalInThousands 0 1000)
                             (unsafeMkUnitInterval 0, unsafeMkUnitInterval 1)
                       -- decentralisation param: 0,0.1,0.2..1
                       <*> (unsafeMkUnitInterval <$> Gen.element [0, 0.1 .. 1])
                       <*> genExtraEntropy
                       -- protocolVersion
                       <*> ((,,) <$> genNatural 1 10 <*> genNatural 1 50 <*> genNatural 1 100)
  where
    -- Note: we keep the lower bound high enough so that we can more likely
    -- generate valid transactions and blocks
    low = 10000
    hi = 200000

    -- A wrapper to enable the dependent generators for the max sizes
    mkPParams minFeeA minFeeB (maxBBSize, maxTxSize, maxBHSize) =
      PParams minFeeA minFeeB maxBBSize maxTxSize maxBHSize

    -- | Generates max block, header and transaction size. First generates the
    -- body size and then header and tx sizes no larger than half the body size.
    szGen :: Gen (Natural, Natural, Natural)
    szGen = do
      blockBodySize <- Gen.integral (Range.linear low hi)
      (blockBodySize,,)
        <$> rangeUpTo (blockBodySize `div` 2)
        <*> rangeUpTo (blockBodySize `div` 2)

    rangeUpTo :: Natural -> Gen Natural
    rangeUpTo upper = Gen.integral (Range.linear low upper)

    -- Generates a Neutral or actual Nonces with equal frequency
    genExtraEntropy = Gen.frequency [ (1, pure NeutralNonce)
                                    , (1, mkNonce <$> Gen.integral (Range.linear 1 123))]
