{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Test.Shelley.Spec.Ledger.Generator.Update
  ( genPParams,
    genUpdate,
  )
where

import Cardano.Crypto.Hash (HashAlgorithm)
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%), Ratio)
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes
  ( Nonce (NeutralNonce),
    StrictMaybe (..),
    UnitInterval,
    mkNonce,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Keys
  ( GenDelegPair (..),
    GenDelegs (..),
    KeyRole (..),
    asWitness,
    hashKey,
    vKey,
  )
import Shelley.Spec.Ledger.LedgerState (_dstate, _genDelegs)
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
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( DPState,
    GenesisKeyPair,
    KeyHash,
    KeyHash,
    KeyPair,
    ProposedPPUpdates,
    UTxOState,
    Update,
  )
import Test.Shelley.Spec.Ledger.Examples (unsafeMkUnitInterval)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core
  ( AllIssuerKeys (cold, hk),
    genInteger,
    genNatural,
    genWord64,
    increasingProbabilityAt,
    tooLateInEpoch,
  )
import Test.Shelley.Spec.Ledger.Utils (epochFromSlotNo)

genRationalInThousands :: HasCallStack => Integer -> Integer -> Gen Rational
genRationalInThousands lower upper =
  (% 1000) <$> genInteger lower upper

genRatioWord64InThousands :: HasCallStack => Word64 -> Word64 -> Gen (Ratio Word64)
genRatioWord64InThousands lower upper =
  (% 1000) <$> genWord64 lower upper

genIntervalInThousands :: HasCallStack => Word64 -> Word64 -> Gen UnitInterval
genIntervalInThousands lower upper =
  unsafeMkUnitInterval <$> genRatioWord64InThousands lower upper

genPParams :: HasCallStack => Constants -> Gen PParams
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
      (1, mkNonce <$> genNatural 1 123)
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
genMinUTxOValue = pure $ Coin 0
-- ^ ^ TODO generate nonzero minimum UTxO values
-- github issue #1544

genMinPoolCost :: HasCallStack => Gen Coin
genMinPoolCost = pure $ Coin 0
-- ^ ^ TODO generate nonzero minimum pool cost
-- github issue #1545

-- | Generate a possible next Protocol version based on the previous version.
-- Increments the Major or Minor versions and possibly the Alt version.
genNextProtocolVersion ::
  HasCallStack =>
  PParams ->
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
  SlotNo ->
  PParams ->
  [KeyHash h 'Genesis] ->
  Gen (ProposedPPUpdates h)
genPPUpdate (c@Constants {maxMinFeeA, maxMinFeeB}) s pp genesisKeys =
  if (tooLateInEpoch s)
    then pure (ProposedPPUpdates Map.empty)
    else do
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
      pure $
        (ProposedPPUpdates . Map.fromList) ppUpdate

-- | Generate an @Update (where all the given nodes participate)
-- with a 50% chance of having non-empty PPUpdates or AVUpdates
-- and a 25% chance of both being empty or non-empty
genUpdateForNodes ::
  (HasCallStack, HashAlgorithm h) =>
  Constants ->
  SlotNo ->
  EpochNo -> -- current epoch
  [GenesisKeyPair h] ->
  PParams ->
  Gen (Maybe (Update h))
genUpdateForNodes c s e coreKeys pp =
  Just <$> (Update <$> genPPUpdate_ <*> pure e)
  where
    genesisKeys = hashKey . vKey <$> coreKeys
    genPPUpdate_ = genPPUpdate c s pp genesisKeys

-- | Occasionally generate an update and return with the witness keys
genUpdate ::
  (HasCallStack, HashAlgorithm h) =>
  Constants ->
  SlotNo ->
  [(GenesisKeyPair h, AllIssuerKeys h 'GenesisDelegate)] ->
  [AllIssuerKeys h 'GenesisDelegate] ->
  PParams ->
  (UTxOState h, DPState h) ->
  Gen (Maybe (Update h), [KeyPair h 'RWitness])
genUpdate
  c@(Constants {frequencyTxUpdates})
  s
  coreKeyPairs
  genesisDelegateKeys
  pp
  (_utxoSt, delegPoolSt) =
    do
      nodes <- take 5 <$> QC.shuffle coreKeyPairs

      let e = epochFromSlotNo s
          (GenDelegs genDelegs) = (_genDelegs . _dstate) delegPoolSt
          genesisKeys = (fst . unzip) nodes
          updateWitnesses = asWitness . latestPoolColdKey genDelegs <$> nodes

      QC.frequency
        [ ( frequencyTxUpdates,
            (,updateWitnesses) <$> genUpdateForNodes c s e genesisKeys pp
          ),
          ( 100 - frequencyTxUpdates,
            pure (Nothing, [])
          )
        ]
    where
      latestPoolColdKey genDelegs_ (gkey, pkeys) =
        case Map.lookup (hashKey . vKey $ gkey) genDelegs_ of
          Nothing ->
            error "genUpdate: NoGenesisStaking"
          Just (GenDelegPair gkeyHash _) ->
            fromMaybe
              (cold pkeys)
              ( List.find (\aik -> hk aik == gkeyHash) genesisDelegateKeys
                  <&> cold
              )
