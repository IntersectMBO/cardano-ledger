{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Generator.Update.QuickCheck
  ( genPParams
  , genUpdate
  )
  where

import           Control.Monad (join)
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.Map.Strict as Map (fromList)
import           Data.Ratio ((%))
import           Data.Set (Set)
import qualified Data.Set as Set (fromList)

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

import           BaseTypes (Nonce (NeutralNonce), UnitInterval, mkNonce)
import           Cardano.Crypto.Hash (HashAlgorithm)
import           Cardano.Ledger.Shelley.Crypto (HASH)
import           Coin (Coin (..))
import           Examples (unsafeMkUnitInterval)
import           Generator.Core.QuickCheck (genInteger, genNatural, genWord64,
                     increasingProbabilityAt)
import           Keys (hash)
import           Keys (GenKeyHash)
import           Numeric.Natural (Natural)
import           PParams (PParams (..))
import           Slot (EpochNo (EpochNo))
import           Updates (AVUpdate (..), ApName (..), ApVer (..), Applications (..),
                     InstallerHash (..), Mdt (..), PPUpdate (..), PParamsUpdate (..), Ppm (..),
                     SystemTag (..), Update (..))

genRationalInThousands :: Integer -> Integer -> Gen Rational
genRationalInThousands lower upper =
  (% 1000) <$> genInteger lower upper

genIntervalInThousands :: Integer -> Integer -> Gen UnitInterval
genIntervalInThousands lower upper =
  unsafeMkUnitInterval <$> genRationalInThousands lower upper

-- TODO @uroboros for now, keeping minA/B at zero until we generate fees in genTx
genPParams :: Gen PParams
genPParams = mkPParams <$> pure 0 -- _minfeeA
                       <*> pure 0 -- _minfeeB
                       <*> szGen  -- (maxBBSize, maxBHSize, maxTxSize)
                       <*> genKeyDeposit
                       <*> genKeyMinRefund
                       <*> genKeyDecayRate
                       <*> genPoolDeposit
                       <*> genIntervalInThousands 100 700
                       <*> genPoolDecayRate
                       <*> genEMax
                       <*> genNOpt
                       <*> genA0
                       <*> genRho
                       <*> genTau
                       <*> genActiveSlotCoeff
                       <*> genDecentralisationParam
                       <*> genExtraEntropy
                       <*> genProtocolVersion
  where

    -- | Generates max block, header and transaction size. First generates the
    -- body size and then header and tx sizes no larger than half the body size.
    szGen :: Gen (Natural, Natural, Natural)
    szGen = do
      blockBodySize <- genNatural low hi
      (blockBodySize,,)
        <$> rangeUpTo (blockBodySize `div` 2)
        <*> rangeUpTo (blockBodySize `div` 2)

    -- A wrapper to enable the dependent generators for the max sizes
    mkPParams minFeeA minFeeB (maxBBSize, maxTxSize, maxBHSize) =
      PParams minFeeA minFeeB maxBBSize maxTxSize maxBHSize

    rangeUpTo :: Natural -> Gen Natural
    rangeUpTo upper = genNatural low upper

-- keyDecayRate: 0.001-0.1
genKeyDecayRate :: Gen Rational
genKeyDecayRate = genRationalInThousands 1 100


-- poolDeposit
-- NOTE: we need to keep these deposits small, otherwise
-- when we generate sequences of transactions we will bleed too
-- much funds into the deposit pool (i.e. funds not available as utxo)
genPoolDeposit :: Gen Coin
genPoolDeposit =
    increasingProbabilityAt
          (Coin <$> genInteger 0 100)
          (Coin 0, Coin 100)

-- poolMinRefund: 0.1-0.7
genPoolMinRefund :: Gen UnitInterval
genPoolMinRefund = genIntervalInThousands 100 700

-- poolDecayRate: 0.001-0.1
genPoolDecayRate :: Gen Rational
genPoolDecayRate = genRationalInThousands 1 100

-- Generates a Neutral or actual Nonces with equal frequency
genExtraEntropy :: Gen Nonce
genExtraEntropy  = QC.frequency [ (1, pure NeutralNonce)
                                , (1, mkNonce <$> genNatural 1 123)]


-- Note: we keep the lower bound high enough so that we can more likely
-- generate valid transactions and blocks
low, hi :: Natural
low = 20000
hi = 200000

-- keyMinRefund: 0.1-0.5
genKeyMinRefund :: Gen UnitInterval
genKeyMinRefund = genIntervalInThousands 100 500

-- eMax (for an epoch per 5 days, say, this is between a month and 7yrs)
genEMax :: Gen EpochNo
genEMax = EpochNo <$> genWord64 6 500

-- | nOpt
genNOpt :: Gen Natural
genNOpt  = genNatural 1 100

-- | genKeyDeposit
-- NOTE: we need to keep these deposits small, otherwise
-- when we generate sequences of transactions we will bleed too
-- much funds into the deposit pool (i.e. funds not available as utxo)
genKeyDeposit :: Gen Coin
genKeyDeposit = increasingProbabilityAt
                  (Coin <$> genInteger 0 20)
                  (Coin 0, Coin 20)

-- | a0: 0.01-1.0
genA0 :: Gen Rational
genA0 = genRationalInThousands 10 1000

-- | rho: 0.001-0.009
genRho :: Gen UnitInterval
genRho = genIntervalInThousands 1 9

-- | tau: 0.1-0.3
genTau :: Gen UnitInterval
genTau = genIntervalInThousands 100 300

-- | activeSlotCoeff: 0.1-1
genActiveSlotCoeff :: Gen UnitInterval
genActiveSlotCoeff = unsafeMkUnitInterval <$> QC.elements [0.025, 0.05, 0.075, 0.1, 0.2, 0.5]
-- ^^ This is a somewhat arbitrary group of values.
-- In the real system, we will probably be using a value near 1/10,
-- and we know that we would not ever choose values too small (say below 1/40)
-- or greater than a 1/2.

genDecentralisationParam :: Gen UnitInterval
genDecentralisationParam = unsafeMkUnitInterval <$> QC.elements [0.1, 0.2 .. 1]
-- ^^ TODO jc - generating d=0 takes some care, if there are no registered
-- stake pools then d=0 deadlocks the system.

-- | protocolVersion is a triple of numbers
genProtocolVersion :: Gen (Natural, Natural, Natural)
genProtocolVersion  = ((,,) <$> genNatural 1 10 <*> genNatural 1 50 <*> genNatural 1 100)

-- | Generate a subset of protocol parameter assignments.
genSetOfPpm :: Gen (Set Ppm)
genSetOfPpm =
  subsetOf [
        MinFeeB               <$> genNatural 0 10
      , MaxBBSize             <$> genSize
      , MaxTxSize             <$> genSize
      , MaxBHSize             <$> genSize
      , KeyDeposit            <$> genKeyDeposit
      , KeyMinRefund          <$> genKeyMinRefund
      , KeyDecayRate          <$> genKeyDecayRate
      , PoolDeposit           <$> genPoolDeposit
      , PoolMinRefund         <$> genPoolMinRefund
      , PoolDecayRate         <$> genPoolDecayRate
      , EMax                  <$> genEMax
      , Nopt                  <$> genNOpt
      , A0                    <$> genA0
      , Rho                   <$> genRho
      , Tau                   <$> genTau
      , ActiveSlotCoefficient <$> genActiveSlotCoeff
      , D                     <$> genDecentralisationParam
      , ExtraEntropy          <$> genExtraEntropy
      , ProtocolVersion       <$> genProtocolVersion
      ]
  where
    genSize = genNatural low hi

-- | Generate subset from a list of generators for each element.
subsetOf :: Ord a => [Gen a] -> Gen (Set a)
subsetOf = fmap Set.fromList
         . join
         . fmap sequence
         . QC.sublistOf

-- | Generate a proposal for protocol parameters update from a subset
--   of genesis nodes.
genPPUpdate :: [GenKeyHash crypto] -> Gen (PPUpdate crypto)
genPPUpdate =
       fmap (PPUpdate . Map.fromList)
    .  (>>=mapM genPpmKV)
    .  QC.sublistOf
  where
    genPpmKV :: GenKeyHash crypto -> Gen (GenKeyHash crypto, PParamsUpdate)
    genPpmKV genesisKey = (genesisKey,) <$>
                          PParamsUpdate <$> genSetOfPpm

-- | Generate application version assignment update.
genAVUpdate :: HashAlgorithm (HASH crypto)
            => [GenKeyHash         crypto]
            ->  Gen (AVUpdate      crypto)
genAVUpdate  =
       fmap (AVUpdate . Map.fromList)
    .  (>>=mapM genApplicationKV)
    .  QC.sublistOf
  where
    genApplicationKV :: HashAlgorithm (HASH crypto)
                     =>      GenKeyHash     crypto
                     -> Gen (GenKeyHash     crypto
                            ,Applications   crypto)
    genApplicationKV genesisKey = (genesisKey,) <$>
                                   genApplications

-- | Generate a list of applications, their versions,
--   and installer hashes.
genApplications :: HashAlgorithm (HASH crypto)
                => Gen (Applications  crypto)
genApplications =
  (Applications . Map.fromList) <$>
    QC.listOf genApEntry
  where
    genApEntry =
      (,) <$> genApName <*> ((,) <$> (ApVer <$> genNatural 1 1000)
                                 <*> genInstallers
                            )
    genApName = do
      tf <- QC.arbitrary
      (ApName . BS.pack) <$> case tf of
        True  -> pure "Daedalus"
        False -> QC.arbitrary
    genInstallers :: HashAlgorithm (HASH crypto)
                  => Gen (Mdt            crypto)
    genInstallers = Mdt          <$>
                    Map.fromList <$>
                    QC.listOf
                      ((,) <$> ((SystemTag     .        BS.pack) <$> QC.arbitrary)
                           <*> ((InstallerHash . hash . BS.pack) <$> QC.arbitrary))

-- | Generate entire @Update of protocol parameters
genUpdate :: HashAlgorithm (HASH crypto)
          => EpochNo -- to be valid, this must be the current epoch
          -> [GenKeyHash         crypto]
          ->  Gen (Update        crypto)
genUpdate e genesisKeys = Update <$> genPPUpdate genesisKeys
                                 <*> genAVUpdate genesisKeys
                                 <*> pure (Just e)
