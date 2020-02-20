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

import           Control.Monad (join, replicateM)
import qualified Data.ByteString.Char8 as BS (pack)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Ratio ((%))
import           Data.Set (Set)
import qualified Data.Set as Set (fromList)
import qualified Data.Text as T (pack)

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

import           BaseTypes (Nonce (NeutralNonce), UnitInterval, mkNonce)
import           Coin (Coin (..))
import           ConcreteCryptoTypes (AVUpdate, Applications, CoreKeyPair, DPState, GenKeyHash,
                     KeyHash, KeyPair, Mdt, PPUpdate, UTxOState, Update)
import           Examples (unsafeMkUnitInterval)
import           Generator.Core.Constants (frequencyLowMaxEpoch, frequencyTxUpdates)
import           Generator.Core.QuickCheck (AllPoolKeys (cold), genInteger, genNatural, genWord64,
                     increasingProbabilityAt, tooLateInEpoch)
import           Keys (GenDelegs (..), hash, hashKey, vKey)
import           LedgerState (_dstate, _genDelegs, _ups)
import           Numeric.Natural (Natural)
import           PParams (PParams (..))
import           Slot (EpochNo (EpochNo), SlotNo)
import           Updates (pattern AVUpdate, pattern ApName, pattern ApVer, pattern Applications,
                     InstallerHash (..), pattern Mdt, pattern PPUpdate, PParamsUpdate (..),
                     Ppm (..), SystemTag (..), pattern Update, pattern UpdateState, apps,
                     emptyUpdate, maxVer)

import           Test.Utils (epochFromSlotNo)

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
                       <*> genPoolMinRefund
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
low = 30000
hi = 200000

-- keyMinRefund: 0.1-0.5
genKeyMinRefund :: Gen UnitInterval
genKeyMinRefund = genIntervalInThousands 100 500

-- eMax (for an epoch per 5 days, say, this is between a month and 7yrs)
genEMax :: Gen EpochNo
genEMax = EpochNo <$> genWord64 frequencyLowMaxEpoch 500

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

genProtocolVersion :: Gen (Natural, Natural, Natural)
genProtocolVersion  = ((,,) <$> genNatural 1 10 <*> genNatural 1 50 <*> genNatural 1 100)

-- | Generate a possible next Protocol version based on the previous version.
-- Increments the Major or Minor versions and possibly the Alt version.
genNextProtocolVersion
  :: PParams
  -> Gen (Natural, Natural, Natural)
genNextProtocolVersion pp = do
  n <- genNatural 1 100
  QC.elements
    [ (major + 1, 0        , 0)
    , (major    , minor + 1, alt)
    , (major    , minor + 1, alt + n)]
  where
    (major, minor, alt) = _protocolVersion pp

-- | Given the current protocol params generates a subset of protocol parameter assignments.
genSetOfPpm
  :: PParams -- existing params
  -> Gen (Set Ppm)
genSetOfPpm pp = do
  n <- QC.elements [1 .. 3] -- pick up to three param updates
  subsetOf n [
        MinFeeB               <$> pure 0 -- TODO @uroboros disable until tx fees are dealt with (genNatural 0 10)
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
      , ProtocolVersion       <$> genNextProtocolVersion pp
      ]
  where
    genSize = genNatural low hi

-- | Pick a subset (size n) of the given generators and produce a set of generated values.
subsetOf :: Ord a => Int -> [Gen a] -> Gen (Set a)
subsetOf n =
  fmap Set.fromList
  . join
  . fmap sequence
  . (take n <$>)
  . QC.shuffle

-- | Generate a proposal for protocol parameter updates for all the given genesis keys.
-- Return an empty update if it is too late in the epoch for updates.
genPPUpdate
  :: SlotNo
  -> PParams
  -> [GenKeyHash]
  -> Gen PPUpdate
genPPUpdate s pp genesisKeys =
  if (tooLateInEpoch s)
    then
      pure (PPUpdate Map.empty)
    else do
      pps <- PParamsUpdate <$> genSetOfPpm pp
      let ppUpdate = zip genesisKeys (repeat pps)
      pure $
        (PPUpdate . Map.fromList) ppUpdate

-- | Generate a proposal for application updates for all the given genesis keys.
genAVUpdate
  :: UTxOState
  -> [GenKeyHash]
  ->  Gen AVUpdate
genAVUpdate utxoSt genesisKeys =do
  apps_ <- genApplications utxoSt
  let avUpdate = zip genesisKeys (repeat apps_)
  pure $
    (AVUpdate . Map.fromList) avUpdate

-- | Generate a mix of
-- * new applications
-- * version updates of existing applications
genApplications
  :: UTxOState
  -> Gen Applications
genApplications utxoSt = do
  n <- QC.elements [1,2] -- one or two application updates per Update
  Applications . Map.fromList <$>
    replicateM n (genApplication n)
  where
    (UpdateState _ _ favs avs) = _ups utxoSt
    avs_ = Map.toList (apps avs)
    favs_ = concat (Map.toList . apps <$> Map.elems favs)

    genInstallers :: Int -> Gen Mdt
    genInstallers i =
      Mdt <$> Map.fromList
          <$> QC.vectorOf i
                          ((,) <$> (SystemTag . T.pack <$> genShortAscii)
                               <*> (InstallerHash . hash . BS.pack <$> genShortAscii))

    genApplication i = QC.frequency [ (2, genNewApp i)
                                    , (8, genNextApp i)]

    genNextApp i = case avs_ ++ favs_ of
        [] -> genNewApp i
        avs' -> do
          (an, (_, _)) <- QC.elements avs'
          pure . incrVersion . (an,) $ maxVer an avs favs
    incrVersion (apName, (ApVer apVer, mdt)) = (apName, (ApVer (apVer+1), mdt))

    genNewApp i = (,) <$> genNewAppName <*> ((ApVer 1,) <$> (genInstallers i))
    genNewAppName = ApName . T.pack <$> genShortAscii

    genShortAscii = QC.vectorOf 5 QC.arbitraryASCIIChar

-- | Generate an @Update (where all the given nodes participate)
-- with a 50% chance of having non-empty PPUpdates or AVUpdates
-- and a 25% chance of both being empty or non-empty
genUpdateForNodes
  :: SlotNo
  -> EpochNo -- current epoch
  -> [CoreKeyPair]
  -> PParams
  -> UTxOState
  -> Gen Update
genUpdateForNodes s e coreKeys pp utxoSt =
  Update <$> genPPUpdate_ <*> genAVUpdate_ <*> pure (Just e)
  where
    genesisKeys = hashKey . vKey <$> coreKeys

    genPPUpdate_ = QC.frequency [ (50, pure (PPUpdate Map.empty))
                                , (50, genPPUpdate s pp genesisKeys)]

    genAVUpdate_ = QC.frequency [ (50, pure (AVUpdate Map.empty))
                                , (50, genAVUpdate utxoSt genesisKeys)]

-- | Occasionally generate an update and return with the witness keys
genUpdate
  :: SlotNo
  -> [(CoreKeyPair, AllPoolKeys)]
  -> Map KeyHash KeyPair -- indexed keys By StakeHash
  -> PParams
  -> (UTxOState, DPState)
  -> Gen (Update, [KeyPair])
genUpdate s coreKeyPairs keysByStakeHash pp (utxoSt, delegPoolSt) = do
  nodes <- take 5 <$> QC.shuffle coreKeyPairs

  let e = epochFromSlotNo s
      (GenDelegs genDelegs) = (_genDelegs . _dstate) delegPoolSt
      genesisKeys = (fst . unzip) nodes
      updateWitnesses = latestPoolColdKey genDelegs <$> nodes

  QC.frequency [ ( frequencyTxUpdates
                 , (,updateWitnesses) <$> genUpdateForNodes s e genesisKeys pp utxoSt)
               , ( 100 - frequencyTxUpdates
                 , pure (emptyUpdate, []))]

  where
    -- | Lookup the cold key for the given node in the genesis delegations map.
    -- Then lookup the cold key hash in the `keysByStakeHash` reverse index.
    -- If we find the key there, we can assume that a GenesisDeleg certificate
    -- has changed the cold key, in which case we use the new key (otherwise we
    -- can use the original cold key)
    latestPoolColdKey genDelegs_ (gkey, pkeys) =
      case Map.lookup (hashKey . vKey $ gkey) genDelegs_ of
          Nothing ->
            error "genUpdate: NoGenesisStaking"
          Just gkeyHash ->
            fromMaybe (cold pkeys)
                      (Map.lookup gkeyHash keysByStakeHash)
