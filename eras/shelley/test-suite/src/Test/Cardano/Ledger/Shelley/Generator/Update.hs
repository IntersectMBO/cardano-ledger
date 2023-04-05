{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Test.Cardano.Ledger.Shelley.Generator.Update (
  genPParams,
  genUpdate,
  genShelleyPParamsUpdate,
  genM,
  genDecentralisationParam,
)
where

import Cardano.Ledger.BaseTypes (
  BoundedRational,
  NonNegativeInterval,
  Nonce (NeutralNonce),
  ProtVer (..),
  StrictMaybe (..),
  UnitInterval,
  Version,
  getVersion64,
  mkNonceFromNumber,
  mkVersion,
  mkVersion64,
  succVersion,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Era (EraCrypto)
import Cardano.Ledger.Keys (
  GenDelegPair (..),
  GenDelegs (..),
  KeyHash,
  KeyRole (..),
  asWitness,
  hashKey,
 )
import Cardano.Ledger.Shelley.API (
  ProposedPPUpdates,
  Update,
 )
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
  DState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Slot (EpochNo (EpochNo), SlotNo)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Ratio (Ratio, (%))
import Data.Word (Word64)
import GHC.Records
import GHC.Stack (HasCallStack)
import Lens.Micro
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Binary.Arbitrary (genVersion)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair, vKey)
import Test.Cardano.Ledger.Shelley.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.Core (
  AllIssuerKeys (aikCold),
  genInteger,
  genNatural,
  genWord64,
  increasingProbabilityAt,
  tooLateInEpoch,
 )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import Test.Cardano.Ledger.Shelley.Utils (
  GenesisKeyPair,
  epochFromSlotNo,
  unsafeBoundRational,
 )
import Test.QuickCheck (Gen, choose, frequency)
import qualified Test.QuickCheck as QC

-- ====================================

genRationalInThousands :: HasCallStack => Integer -> Integer -> Gen Rational
genRationalInThousands lower upper =
  (% 1000) <$> genInteger lower upper

genIntervalInThousands :: (BoundedRational a, HasCallStack) => Integer -> Integer -> Gen a
genIntervalInThousands lower upper =
  unsafeBoundRational <$> genRationalInThousands lower upper

genPParams ::
  (EraPParams era, ProtVerAtMost era 4, ProtVerAtMost era 6) =>
  Constants ->
  Gen (PParams era)
genPParams c@Constants {maxMinFeeA, maxMinFeeB, minMajorPV, maxMajorPV} = do
  minFeeA <- genInteger 0 (unCoin maxMinFeeA)
  minFeeB <- genInteger 0 (unCoin maxMinFeeB)
  (maxBBSize, maxTxSize, maxBHSize) <- szGen
  keyDeposit <- genKeyDeposit
  poolDeposit <- genPoolDeposit
  eMax <- genEMax c
  nOpt <- genNOpt
  a0 <- genA0
  rho <- genRho
  tau <- genTau
  d <- genDecentralisationParam
  extraEntropy <- genExtraEntropy
  protocolVersion <- genProtocolVersion minMajorPV maxMajorPV
  minUTxOValue <- genMinUTxOValue
  minPoolCost <- genMinPoolCost
  pure $
    emptyPParams
      & ppMinFeeAL .~ Coin minFeeA
      & ppMinFeeBL .~ Coin minFeeB
      & ppMaxBBSizeL .~ maxBBSize
      & ppMaxTxSizeL .~ maxTxSize
      & ppMaxBHSizeL .~ maxBHSize
      & ppKeyDepositL .~ keyDeposit
      & ppPoolDepositL .~ poolDeposit
      & ppEMaxL .~ eMax
      & ppNOptL .~ nOpt
      & ppA0L .~ a0
      & ppRhoL .~ rho
      & ppTauL .~ tau
      & ppDL .~ d
      & ppExtraEntropyL .~ extraEntropy
      & ppProtocolVersionL .~ protocolVersion
      & ppMinUTxOValueL .~ minUTxOValue
      & ppMinPoolCostL .~ minPoolCost
  where
    szGen :: Gen (Natural, Natural, Natural)
    szGen = do
      blockBodySize <- genNatural low hi
      (blockBodySize,,)
        <$> rangeUpTo (blockBodySize `div` 2)
        <*> rangeUpTo (blockBodySize `div` 2)
    rangeUpTo :: Natural -> Gen Natural
    rangeUpTo upper = genNatural low upper

-- poolDeposit
-- NOTE: we need to keep these deposits small, otherwise
-- when we generate sequences of transactions we will bleed too
-- much funds into the deposit pool (i.e. funds not available as utxo)
genPoolDeposit :: HasCallStack => Gen Coin
genPoolDeposit =
  increasingProbabilityAt
    (Coin <$> genInteger 0 100)
    (Coin 0, Coin 100)

-- Generates a Neutral or actual Nonces with equal frequency
genExtraEntropy :: HasCallStack => Gen Nonce
genExtraEntropy =
  QC.frequency
    [ (1, pure NeutralNonce)
    , (1, mkNonceFromNumber <$> genWord64 1 123)
    ]

-- Note: we keep the lower bound high enough so that we can more likely
-- generate valid transactions and blocks
low, hi :: Natural
low = 50000
hi = 200000

-- eMax (for an epoch per 5 days, say, this is between a month and 7yrs)
genEMax ::
  HasCallStack =>
  Constants ->
  Gen EpochNo
genEMax Constants {frequencyLowMaxEpoch} =
  EpochNo <$> genWord64 frequencyLowMaxEpoch 500

-- | nOpt
genNOpt :: HasCallStack => Gen Natural
genNOpt = genNatural 1 100

-- | genKeyDeposit
-- NOTE: we need to keep these deposits small, otherwise
-- when we generate sequences of transactions we will bleed too
-- much funds into the deposit pool (i.e. funds not available as utxo)
genKeyDeposit :: HasCallStack => Gen Coin
genKeyDeposit =
  increasingProbabilityAt
    (Coin <$> genInteger 0 20)
    (Coin 0, Coin 20)

-- | a0: 0.01-1.0
genA0 :: HasCallStack => Gen NonNegativeInterval
genA0 = genIntervalInThousands 10 1000

-- | rho: 0.001-0.009
genRho :: HasCallStack => Gen UnitInterval
genRho = genIntervalInThousands 1 9

-- | tau: 0.1-0.3
genTau :: HasCallStack => Gen UnitInterval
genTau = genIntervalInThousands 100 300

genDecentralisationParam :: HasCallStack => Gen UnitInterval
genDecentralisationParam = unsafeBoundRational <$> QC.elements [0.1, 0.2 .. 1]
-- ^ ^ TODO jc - generating d=0 takes some care, if there are no registered
--  stake pools then d=0 deadlocks the system.

genProtocolVersion :: HasCallStack => Version -> Version -> Gen ProtVer
genProtocolVersion minMajPV maxMajPV =
  ProtVer <$> genVersion minMajPV maxMajPV <*> genNatural 1 50

genMinUTxOValue :: HasCallStack => Gen Coin
genMinUTxOValue = Coin <$> genInteger 1 20

genMinPoolCost :: HasCallStack => Gen Coin
genMinPoolCost = Coin <$> genInteger 10 50

-- | Generate a possible next Protocol version based on the previous version.
-- Increments the Major or Minor versions and possibly the Alt version.
genNextProtocolVersion :: EraPParams era => HasCallStack => PParams era -> Version -> Gen ProtVer
genNextProtocolVersion pp maxMajorPV = do
  QC.elements $ ProtVer m (n + 1) : [ProtVer m' 0 | Just m' <- [succVersion m], m' <= maxMajorPV]
  where
    ProtVer m n = pp ^. ppProtocolVersionL

genM :: Gen a -> Gen (StrictMaybe a)
genM gen = frequency [(1, SJust <$> gen), (2, pure SNothing)]

-- | This is only good in the Shelley Era, used to define the genShelleyEraPParamsUpdate method for (EraGen (ShelleyEra c))
genShelleyPParamsUpdate ::
  forall era.
  (ProtVerAtMost era 4, ProtVerAtMost era 6, EraPParams era) =>
  Constants ->
  PParams era ->
  Gen (PParamsUpdate era)
genShelleyPParamsUpdate c@Constants {maxMinFeeA, maxMinFeeB, maxMajorPV} pp = do
  -- TODO generate Maybe types so not all updates are full
  minFeeA <- genM $ genInteger 0 (unCoin maxMinFeeA)
  minFeeB <- genM $ genInteger 0 (unCoin maxMinFeeB)
  maxBBSize <- genM $ genNatural low hi
  maxTxSize <- genM $ genNatural low hi
  maxBHSize <- genM $ genNatural low hi
  keyDeposit <- genM $ genKeyDeposit
  poolDeposit <- genM $ genPoolDeposit
  eMax <- genM $ genEMax c
  nOpt <- genM genNOpt
  a0 <- genM genA0
  rho <- genM genRho
  tau <- genM genTau
  d <- genM genDecentralisationParam
  extraEntropy <- genM genExtraEntropy
  protocolVersion <- genM $ genNextProtocolVersion pp maxMajorPV
  minUTxOValue <- genM genMinUTxOValue
  minPoolCost <- genM genMinPoolCost
  pure $
    emptyPParamsUpdate
      & ppuMinFeeAL .~ fmap Coin minFeeA
      & ppuMinFeeBL .~ fmap Coin minFeeB
      & ppuMaxBBSizeL .~ maxBBSize
      & ppuMaxTxSizeL .~ maxTxSize
      & ppuMaxBHSizeL .~ maxBHSize
      & ppuKeyDepositL .~ keyDeposit
      & ppuPoolDepositL .~ poolDeposit
      & ppuEMaxL .~ eMax
      & ppuNOptL .~ nOpt
      & ppuA0L .~ a0
      & ppuRhoL .~ rho
      & ppuTauL .~ tau
      & ppuDL .~ d
      & ppuExtraEntropyL .~ extraEntropy
      & ppuProtocolVersionL .~ protocolVersion
      & ppuMinUTxOValueL .~ minUTxOValue
      & ppuMinPoolCostL .~ minPoolCost

-- | Generate a proposal for protocol parameter updates for all the given genesis keys.
-- Return an empty update if it is too late in the epoch for updates.
genPPUpdate ::
  forall era.
  EraGen era =>
  Constants ->
  PParams era ->
  [KeyHash 'Genesis (EraCrypto era)] ->
  Gen (ProposedPPUpdates era)
genPPUpdate constants pp genesisKeys = do
  pps <- genEraPParamsUpdate @era constants pp
  let ppUpdate = zip genesisKeys (repeat pps)
  pure $ ProposedPPUpdates . Map.fromList $ ppUpdate

-- | Generate an @Update (where all the given nodes participate)
genUpdateForNodes ::
  forall era.
  EraGen era =>
  Constants ->
  SlotNo ->
  EpochNo -> -- current epoch
  [KeyPair 'Genesis (EraCrypto era)] ->
  PParams era ->
  Gen (Maybe (Update era))
genUpdateForNodes c s e coreKeys pp =
  Just <$> (Update <$> genPPUpdate_ <*> pure e')
  where
    genesisKeys = hashKey . vKey <$> coreKeys
    genPPUpdate_ = genPPUpdate c pp genesisKeys
    e' = if tooLateInEpoch s then e + 1 else e

-- | Occasionally generate an update and return with the witness keys
genUpdate ::
  EraGen era =>
  Constants ->
  SlotNo ->
  [(GenesisKeyPair (EraCrypto era), AllIssuerKeys (EraCrypto era) 'GenesisDelegate)] ->
  Map (KeyHash 'GenesisDelegate (EraCrypto era)) (AllIssuerKeys (EraCrypto era) 'GenesisDelegate) ->
  PParams era ->
  (UTxOState era, CertState era) ->
  Gen (Maybe (Update era), [KeyPair 'Witness (EraCrypto era)])
genUpdate
  c@Constants {frequencyTxUpdates}
  s
  coreNodes
  genesisDelegatesByHash
  pp
  (_utxoSt, delegPoolSt) =
    do
      nodes <- take 5 <$> QC.shuffle coreNodes

      let e = epochFromSlotNo s
          GenDelegs genDelegs = dsGenDelegs (certDState delegPoolSt)
          genesisKeys = fst <$> nodes
          coreSigners =
            catMaybes $
              flip Map.lookup genesisDelegatesByHash . genDelegKeyHash <$> Map.elems genDelegs
          failedWitnessLookup = length coreSigners < Map.size genDelegs
      if failedWitnessLookup
        then -- discard
          pure (Nothing, [])
        else
          let wits = asWitness . aikCold <$> coreSigners
           in QC.frequency
                [
                  ( frequencyTxUpdates
                  , (,wits) <$> genUpdateForNodes c s e genesisKeys pp
                  )
                ,
                  ( 100 - frequencyTxUpdates
                  , pure (Nothing, [])
                  )
                ]
