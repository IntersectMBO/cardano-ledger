{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Shelley.Spec.Ledger.Generator.Update
  ( genPParams,
    genUpdate,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Ratio (Ratio, (%))
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.API
  ( ProposedPPUpdates,
    Update,
  )
import Shelley.Spec.Ledger.BaseTypes
  ( Nonce (NeutralNonce),
    StrictMaybe (..),
    UnitInterval,
    mkNonceFromNumber,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))

import Cardano.Ledger.Era (Crypto, Era)
import Shelley.Spec.Ledger.Keys
  ( GenDelegPair (..),
    GenDelegs (..),
    KeyHash,
    KeyPair,
    KeyRole (..),
    asWitness,
    hashKey,
    vKey,
  )
import Shelley.Spec.Ledger.LedgerState
  ( DPState (..),
    DState (..),
    UTxOState (..),
  )
import Shelley.Spec.Ledger.PParams
  ( PParams,
    PParams' (..),
    ProtVer (..),
    pattern ProposedPPUpdates,
    pattern Update,
  )
import Shelley.Spec.Ledger.Slot (EpochNo (EpochNo), SlotNo)
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core
  ( AllIssuerKeys (cold),
    genInteger,
    genNatural,
    genWord64,
    increasingProbabilityAt,
    tooLateInEpoch,
  )
import Test.Shelley.Spec.Ledger.Utils
  ( GenesisKeyPair,
    epochFromSlotNo,
    unsafeMkUnitInterval
  )

genRationalInThousands :: HasCallStack => Integer -> Integer -> Gen Rational
genRationalInThousands lower upper =
  (% 1000) <$> genInteger lower upper

genRatioWord64InThousands :: HasCallStack => Word64 -> Word64 -> Gen (Ratio Word64)
genRatioWord64InThousands lower upper =
  (% 1000) <$> genWord64 lower upper

genIntervalInThousands :: HasCallStack => Word64 -> Word64 -> Gen UnitInterval
genIntervalInThousands lower upper =
  unsafeMkUnitInterval <$> genRatioWord64InThousands lower upper

genPParams :: HasCallStack => Constants -> Gen (PParams era)
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
genA0 :: HasCallStack => Gen Rational
genA0 = genRationalInThousands 10 1000

-- | rho: 0.001-0.009
genRho :: HasCallStack => Gen UnitInterval
genRho = genIntervalInThousands 1 9

-- | tau: 0.1-0.3
genTau :: HasCallStack => Gen UnitInterval
genTau = genIntervalInThousands 100 300

genDecentralisationParam :: HasCallStack => Gen UnitInterval
genDecentralisationParam = unsafeMkUnitInterval <$> QC.elements [0.1, 0.2 .. 1]
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

-- | Generate a proposal for protocol parameter updates for all the given genesis keys.
-- Return an empty update if it is too late in the epoch for updates.
genPPUpdate ::
  HasCallStack =>
  Constants ->
  PParams era ->
  [KeyHash 'Genesis (Crypto era)] ->
  Gen (ProposedPPUpdates era)
genPPUpdate (c@Constants {maxMinFeeA, maxMinFeeB}) pp genesisKeys = do
  -- TODO generate Maybe tyes so not all updates are full
  minFeeA <- genNatural 0 maxMinFeeA
  minFeeB <- genNatural 0 maxMinFeeB
  maxBBSize <- genNatural low hi
  maxTxSize <- genNatural low hi
  maxBHSize <- genNatural low hi
  keyDeposit <- genKeyDeposit
  poolDeposit <- genPoolDeposit
  eMax <- genEMax c
  nopt <- genNOpt
  a0 <- genA0
  rho <- genRho
  tau <- genTau
  d <- genDecentralisationParam
  extraEntropy <- genExtraEntropy
  protocolVersion <- genNextProtocolVersion pp
  minUTxOValue <- genMinUTxOValue
  minPoolCost <- genMinPoolCost
  let pps =
        PParams
          { _minfeeA = SJust minFeeA,
            _minfeeB = SJust minFeeB,
            _maxBBSize = SJust maxBBSize,
            _maxTxSize = SJust maxTxSize,
            _maxBHSize = SJust maxBHSize,
            _keyDeposit = SJust keyDeposit,
            _poolDeposit = SJust poolDeposit,
            _eMax = SJust eMax,
            _nOpt = SJust nopt,
            _a0 = SJust a0,
            _rho = SJust rho,
            _tau = SJust tau,
            _d = SJust d,
            _extraEntropy = SJust extraEntropy,
            _protocolVersion = SJust protocolVersion,
            _minUTxOValue = SJust minUTxOValue,
            _minPoolCost = SJust minPoolCost
          }
  let ppUpdate = zip genesisKeys (repeat pps)
  pure $ ProposedPPUpdates . Map.fromList $ ppUpdate

-- | Generate an @Update (where all the given nodes participate)
genUpdateForNodes ::
  (HasCallStack, Era era) =>
  Constants ->
  SlotNo ->
  EpochNo -> -- current epoch
  [KeyPair 'Genesis (Crypto era)] ->
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
  (HasCallStack, Era era) =>
  Constants ->
  SlotNo ->
  [(GenesisKeyPair (Crypto era), AllIssuerKeys (Crypto era) 'GenesisDelegate)] ->
  Map (KeyHash 'GenesisDelegate (Crypto era)) (AllIssuerKeys (Crypto era) 'GenesisDelegate) ->
  PParams era ->
  (UTxOState era, DPState era) ->
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
