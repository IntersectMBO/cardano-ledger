{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Test.Cardano.Ledger.Shelley.Generator.Update
  ( genPParams,
    genUpdate,
    genShelleyPParamsDelta,
    genM,
    genDecentralisationParam,
  )
where

import Cardano.Ledger.BaseTypes
  ( BoundedRational,
    NonNegativeInterval,
    Nonce (NeutralNonce),
    StrictMaybe (..),
    UnitInterval,
    mkNonceFromNumber,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (PParamsDelta)
import qualified Cardano.Ledger.Core as Core (PParams, PParamsDelta)
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Keys
  ( GenDelegPair (..),
    GenDelegs (..),
    KeyHash,
    KeyPair,
    KeyRole (..),
    asWitness,
    hashKey,
    vKey,
  )
import Cardano.Ledger.Shelley.API
  ( ProposedPPUpdates,
    Update,
  )
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    DState (..),
    UTxOState (..),
  )
import Cardano.Ledger.Shelley.PParams
  ( PParams,
    PParams' (..),
    PParamsUpdate,
    ProtVer (..),
    pattern ProposedPPUpdates,
    pattern Update,
  )
import Cardano.Ledger.Slot (EpochNo (EpochNo), SlotNo)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Ratio (Ratio, (%))
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Shelley.Generator.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.Core
  ( AllIssuerKeys (cold),
    genInteger,
    genNatural,
    genWord64,
    increasingProbabilityAt,
    tooLateInEpoch,
  )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import Test.Cardano.Ledger.Shelley.Utils
  ( GenesisKeyPair,
    epochFromSlotNo,
    unsafeBoundRational,
  )
import Test.QuickCheck (Gen, frequency)
import qualified Test.QuickCheck as QC

-- ====================================

genRationalInThousands :: HasCallStack => Integer -> Integer -> Gen Rational
genRationalInThousands lower upper =
  (% 1000) <$> genInteger lower upper

genIntervalInThousands :: (BoundedRational a, HasCallStack) => Integer -> Integer -> Gen a
genIntervalInThousands lower upper =
  unsafeBoundRational <$> genRationalInThousands lower upper

genPParams :: Constants -> Gen (PParams era)
genPParams c@(Constants {maxMinFeeA, maxMinFeeB}) =
  mkPParams <$> genNatural 0 maxMinFeeA -- _minfeeA
    <*> genNatural 0 maxMinFeeB -- _minfeeB
    <*> szGen -- (maxBBSize, maxBHSize, maxTxSize)
    <*> genKeyDeposit
    <*> genPoolDeposit
    <*> genEMax c
    <*> genNOpt
    <*> genA0
    <*> genRho
    <*> genTau
    <*> genDecentralisationParam
    <*> genExtraEntropy
    <*> genProtocolVersion
    <*> genMinUTxOValue
    <*> genMinPoolCost
  where
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
    [ (1, pure NeutralNonce),
      (1, mkNonceFromNumber <$> genWord64 1 123)
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

genProtocolVersion :: HasCallStack => Gen ProtVer
genProtocolVersion = ProtVer <$> genNatural 1 10 <*> genNatural 1 50

genMinUTxOValue :: HasCallStack => Gen Coin
genMinUTxOValue = Coin <$> genInteger 1 20

genMinPoolCost :: HasCallStack => Gen Coin
genMinPoolCost = Coin <$> genInteger 10 50

-- | Generate a possible next Protocol version based on the previous version.
-- Increments the Major or Minor versions and possibly the Alt version.
genNextProtocolVersion ::
  HasCallStack =>
  PParams era ->
  Gen ProtVer
genNextProtocolVersion pp = do
  QC.elements
    [ ProtVer (m + 1) 0,
      ProtVer m (n + 1)
    ]
  where
    ProtVer m n = _protocolVersion pp

genM :: Gen a -> Gen (StrictMaybe a)
genM gen = frequency [(1, SJust <$> gen), (2, pure SNothing)]

-- | This is only good in the Shelley Era, used to define the genEraPParamsDelta method for (EraGen (ShelleyEra c))
genShelleyPParamsDelta ::
  forall era.
  ( PParams era ~ Core.PParams era,
    Core.PParamsDelta era ~ PParamsUpdate era
  ) =>
  Constants ->
  PParams era ->
  Gen (PParamsDelta era)
genShelleyPParamsDelta (c@Constants {maxMinFeeA, maxMinFeeB}) pp = do
  -- TODO generate Maybe types so not all updates are full
  minFeeA <- genM $ genNatural 0 maxMinFeeA
  minFeeB <- genM $ genNatural 0 maxMinFeeB
  maxBBSize <- genM $ genNatural low hi
  maxTxSize <- genM $ genNatural low hi
  maxBHSize <- genM $ genNatural low hi
  keyDeposit <- genM $ genKeyDeposit
  poolDeposit <- genM $ genPoolDeposit
  eMax <- genM $ genEMax c
  nopt <- genM $ genNOpt
  a0 <- genM $ genA0
  rho <- genM $ genRho
  tau <- genM $ genTau
  d <- genM $ genDecentralisationParam
  extraEntropy <- genM $ genExtraEntropy
  protocolVersion <- genM $ genNextProtocolVersion pp
  minUTxOValue <- genM $ genMinUTxOValue
  minPoolCost <- genM $ genMinPoolCost
  pure
    ( PParams
        { _minfeeA = minFeeA,
          _minfeeB = minFeeB,
          _maxBBSize = maxBBSize,
          _maxTxSize = maxTxSize,
          _maxBHSize = maxBHSize,
          _keyDeposit = keyDeposit,
          _poolDeposit = poolDeposit,
          _eMax = eMax,
          _nOpt = nopt,
          _a0 = a0,
          _rho = rho,
          _tau = tau,
          _d = d,
          _extraEntropy = extraEntropy,
          _protocolVersion = protocolVersion,
          _minUTxOValue = minUTxOValue,
          _minPoolCost = minPoolCost
        }
    )

-- | Generate a proposal for protocol parameter updates for all the given genesis keys.
-- Return an empty update if it is too late in the epoch for updates.
genPPUpdate ::
  forall era.
  (EraGen era) =>
  Constants ->
  Core.PParams era ->
  [KeyHash 'Genesis (Crypto era)] ->
  Gen (ProposedPPUpdates era)
genPPUpdate constants pp genesisKeys = do
  pps <- genEraPParamsDelta @era constants pp
  let ppUpdate = zip genesisKeys (repeat pps)
  pure $ ProposedPPUpdates . Map.fromList $ ppUpdate

-- | Generate an @Update (where all the given nodes participate)
genUpdateForNodes ::
  forall era.
  (EraGen era) =>
  Constants ->
  SlotNo ->
  EpochNo -> -- current epoch
  [KeyPair 'Genesis (Crypto era)] ->
  Core.PParams era ->
  Gen (Maybe (Update era))
genUpdateForNodes c s e coreKeys pp =
  Just <$> (Update <$> genPPUpdate_ <*> pure e')
  where
    genesisKeys = hashKey . vKey <$> coreKeys
    genPPUpdate_ = genPPUpdate c pp genesisKeys
    e' = if tooLateInEpoch s then e + 1 else e

-- | Occasionally generate an update and return with the witness keys
genUpdate ::
  (EraGen era) =>
  Constants ->
  SlotNo ->
  [(GenesisKeyPair (Crypto era), AllIssuerKeys (Crypto era) 'GenesisDelegate)] ->
  Map (KeyHash 'GenesisDelegate (Crypto era)) (AllIssuerKeys (Crypto era) 'GenesisDelegate) ->
  Core.PParams era ->
  (UTxOState era, DPState (Crypto era)) ->
  Gen (Maybe (Update era), [KeyPair 'Witness (Crypto era)])
genUpdate
  c@(Constants {frequencyTxUpdates})
  s
  coreNodes
  genesisDelegatesByHash
  pp
  (_utxoSt, delegPoolSt) =
    do
      nodes <- take 5 <$> QC.shuffle coreNodes

      let e = epochFromSlotNo s
          (GenDelegs genDelegs) = (_genDelegs . _dstate) delegPoolSt
          genesisKeys = fst <$> nodes
          coreSigners = catMaybes $ (flip Map.lookup) genesisDelegatesByHash . genDelegKeyHash <$> Map.elems genDelegs
          failedWitnessLookup = length coreSigners < Map.size genDelegs
      if failedWitnessLookup
        then -- discard
          pure (Nothing, [])
        else
          let wits = asWitness . cold <$> coreSigners
           in QC.frequency
                [ ( frequencyTxUpdates,
                    (,wits) <$> genUpdateForNodes c s e genesisKeys pp
                  ),
                  ( 100 - frequencyTxUpdates,
                    pure (Nothing, [])
                  )
                ]
