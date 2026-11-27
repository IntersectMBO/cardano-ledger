{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Dijkstra.Imp.EpochSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Conway.Governance (
  DRepPulser (..),
  DRepPulsingState (..),
  epochStateDRepPulsingStateL,
 )
import Cardano.Ledger.Conway.Rules (ConwayEpochEvent (..), ConwayNewEpochEvent (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.PParams (ppLeiosCommitteeSizeL)
import Cardano.Ledger.Dijkstra.Rules (maxKeyAgeEpochs)
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State
import Cardano.Ledger.Val ((<->))
import Control.State.Transition (Event)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Typeable (cast)
import qualified Data.VMap as VM
import Lens.Micro ((&), (.~))
import Lens.Micro.Mtl (use, (%=))
import Test.Cardano.Ledger.Core.Rational ((%!))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( DijkstraEraImp era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "POOLREAP" era) ~ Shelley.ShelleyPoolreapEvent era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "EPOCH" $ do
  describe "Stake pools that re-register" $ do
    it "adopt their new parameters at the next epoch boundary" $ do
      pps <- registerFreshPool
      delegator <- delegateBillion (sppId pps)
      newPps <- changeParams pps
      submitTx_ $ regPoolTx newPps
      expectActiveParams pps
      deposit <- poolDeposit (sppId pps)
      passEpoch
      expectActiveParams newPps
      poolDeposit (sppId newPps) `shouldReturn` deposit
      expectDelegatedToPool delegator (sppId newPps)
      Map.member (sppId pps) . psFutureStakePoolParams <$> getPState `shouldReturn` False
      expectKeysReleased pps
      expectKeysInUse newPps

    it "are seen with their new parameters by the DRep pulser started at the epoch boundary" $ do
      pps <- registerFreshPool
      newPps <- changeParams pps
      submitTx_ $ regPoolTx newPps
      passEpoch
      expectActiveParams newPps
      stakePools <- psStakePools <$> getPState
      getsNES (nesEsL . epochStateDRepPulsingStateL) >>= \case
        DRPulsing pulser -> dpStakePools pulser `shouldBe` stakePools
        DRComplete {} ->
          assertFailure "Expected the DRep pulser that was started at the epoch boundary"

    it
      "are in the mark snapshot with their new parameters at the next epoch boundary and the one after that" $ do
      -- Issue #6266: delayed to the epoch boundary after the next one.
      pps <- registerFreshPool
      _ <- delegateBillion (sppId pps)
      newPps <- changeParams pps
      submitTx_ $ regPoolTx newPps
      passEpoch
      expectMarkParams newPps
      passEpoch
      expectMarkParams newPps

    it "are paid the rewards for the blocks of the epoch after the next one with their new margin" $ do
      -- Issue #6266: a stake pool that raises its margin to 100% keeps all the rewards for the
      -- blocks of the epoch after the next one, because the mark snapshot taken at the next
      -- epoch boundary already has the new margin. The issue makes them use the old margin.
      --
      -- Rewards are paid out of the reserves in proportion to the stake in circulation, so both
      -- need to be realistic for the stake pool to earn more than its cost.
      modifyNES $ nesEsL . chainAccountStateL . casReservesL .~ Coin 20_000_000_000_000_000
      minPoolCost <- getsPParams ppMinPoolCostL
      -- Without a pledge the stake pool earns rewards without owners delegating to it.
      pps <-
        registerFreshPoolWith $ \p -> p {sppPledge = mempty, sppCost = minPoolCost, sppMargin = 0 %! 1}
      delegator <- delegateStakeOf (Coin 90_000_000_000_000) (sppId pps)
      passNEpochs 2
      submitTx_ $ regPoolTx pps {sppMargin = 1 %! 1}
      passNEpochs 2
      -- The stake pool makes blocks in the epoch after the next one ...
      replicateM_ 3 $
        withIssuerAndTxsInBlock_ (coerce (sppId pps)) $ do
          addr <- freshKeyAddr_
          sendCoinTo_ addr $ Coin 1_000_000_000
      -- ... and the rewards for them are paid out two epoch boundaries later.
      passNEpochs 2
      getBalance delegator `shouldReturn` mempty
      operatorRewards <- getAccountBalance (sppAccountAddress pps)
      operatorRewards `shouldSatisfy` (> mempty)

  describe "Stake pools that re-register with a new BLS key" $ do
    it "are seated with it in the Leios committee of the mark snapshot taken at the next epoch boundary" $ do
      -- Issue #6266: delayed to the epoch boundary after the next one, which also records it as
      -- registered one epoch later.
      modifyPParams $ ppLeiosCommitteeSizeL .~ 1_000
      pps <- registerFreshPool
      _ <- delegateBillion (sppId pps)
      passEpoch
      oldKey <- blsKeyOf pps
      isSeatedInMark oldKey `shouldReturn` True
      newPps <- changeParams pps
      newKey <- blsKeyOf newPps
      reRegisteredIn <- getsNES nesELL
      submitTx_ $ regPoolTx newPps
      passEpoch
      sps <- expectJust . Map.lookup (sppId pps) . psStakePools =<< getPState
      spsBlsKey sps `shouldBe` SJust (BlsKeyState newKey (succ reRegisteredIn))
      isSeatedInMark newKey `shouldReturn` True
      isSeatedInMark oldKey `shouldReturn` False
      -- The mark snapshot moves into the set position at the following epoch boundary, so the
      -- stake pool votes with the new key from the second epoch after its re-registration.
      passEpoch
      isSeatedInSet newKey `shouldReturn` True

    it "are seated with it for as long as its age allows" $ do
      -- Issue #6266: the key is recorded as registered one epoch later, so these epochs are
      -- delayed by one, without changing how many there are.
      modifyPParams $ ppLeiosCommitteeSizeL .~ 1_000
      -- A single KES period that lasts an epoch, plus the two epochs of activation delay.
      impGlobalsL %= \globals -> globals {maxKESEvo = 1, slotsPerKESPeriod = 4_320}
      globals <- use impGlobalsL
      curEpochNo <- getsNES nesELL
      maxKeyAgeEpochs globals curEpochNo `shouldBe` EpochInterval 3
      pps <- registerFreshPool
      _ <- delegateBillion (sppId pps)
      passEpoch
      newPps <- changeParams pps
      newKey <- blsKeyOf newPps
      submitTx_ $ regPoolTx newPps
      -- The key is recorded as registered at the next epoch boundary, which honours it in the
      -- committees of the mark snapshots taken there and at the epoch boundary after it ...
      passEpoch
      isSeatedInMark newKey `shouldReturn` True
      passEpoch
      isSeatedInMark newKey `shouldReturn` True
      -- ... but not in the one after those, even though the stake pool still has it.
      passEpoch
      isSeatedInMark newKey `shouldReturn` False
      expectActiveParams newPps

  describe "Stake pools that retire" $ do
    it "are left out of the mark snapshot taken at the epoch boundary they retire at" $ do
      pps <- registerFreshPool
      _ <- delegateBillion (sppId pps)
      passEpoch
      isInMarkSnapShot (sppId pps) `shouldReturn` True
      retirePool (sppId pps) (EpochInterval 1)
      passEpoch
      expectRetired (sppId pps)
      isInMarkSnapShot (sppId pps) `shouldReturn` False

  describe "Stake pools that re-register and retire in the same epoch" $ do
    it "retire with their new parameters at the next epoch boundary" $ do
      pps <- registerFreshPool
      delegator <- delegateBillion (sppId pps)
      passEpoch
      newPps <- changeParams pps
      submitTx_ $ regPoolTx newPps
      retirePool (sppId pps) (EpochInterval 1)
      deposit <- poolDeposit (sppId pps)
      retired <- passEpochRetiredPools
      expectRetired (sppId pps)
      expectKeysReleased pps
      expectKeysReleased newPps
      -- The deposit is refunded to the reward account of the new parameters.
      getAccountBalance (sppAccountAddress newPps) `shouldReturn` deposit
      getAccountBalance (sppAccountAddress pps) `shouldReturn` mempty
      Map.map (Map.map fromCompact) (Shelley.refundPools retired)
        `shouldBe` Map.singleton (accountCredential newPps) (Map.singleton (sppId pps) deposit)
      Shelley.unclaimedPools retired `shouldBe` mempty
      expectNotDelegatedToAnyPool delegator
      isInMarkSnapShot (sppId pps) `shouldReturn` False

    it "leave their deposit unclaimed when the reward account of their new parameters is not registered" $ do
      pps <- registerFreshPool
      passEpoch
      accountAddress <- freshUnregisteredAccount
      let newPps = pps {sppAccountAddress = accountAddress, sppMargin = 1 %! 2}
      submitTx_ $ regPoolTx newPps
      retirePool (sppId pps) (EpochInterval 1)
      deposit <- poolDeposit (sppId pps)
      deposited <- getDeposited
      retired <- passEpochRetiredPools
      expectRetired (sppId pps)
      -- The reward account of the active parameters does not get the deposit either.
      getAccountBalance (sppAccountAddress pps) `shouldReturn` mempty
      lookupAccountBalance accountAddress `shouldReturn` Nothing
      getDeposited `shouldReturn` (deposited <-> deposit)
      Shelley.refundPools retired `shouldBe` mempty
      Map.map (Map.map fromCompact) (Shelley.unclaimedPools retired)
        `shouldBe` Map.singleton (accountCredential newPps) (Map.singleton (sppId pps) deposit)

    it "adopt their new parameters first when they retire at a later epoch boundary" $ do
      pps <- registerFreshPool
      passEpoch
      newPps <- changeParams pps
      submitTx_ $ regPoolTx newPps
      retirePool (sppId pps) (EpochInterval 2)
      deposit <- poolDeposit (sppId pps)
      passEpoch
      expectActiveParams newPps
      Map.member (sppId pps) . psRetiring <$> getPState `shouldReturn` True
      expectKeysReleased pps
      expectKeysInUse newPps
      retired <- passEpochRetiredPools
      expectRetired (sppId pps)
      expectKeysReleased newPps
      getAccountBalance (sppAccountAddress newPps) `shouldReturn` deposit
      getAccountBalance (sppAccountAddress pps) `shouldReturn` mempty
      Map.map (Map.map fromCompact) (Shelley.refundPools retired)
        `shouldBe` Map.singleton (accountCredential newPps) (Map.singleton (sppId pps) deposit)

    it "stay registered with their new parameters when they re-register after scheduling to retire" $ do
      pps <- registerFreshPool
      retirePool (sppId pps) (EpochInterval 1)
      newPps <- changeParams pps
      submitTx_ $ regPoolTx newPps
      deposit <- poolDeposit (sppId pps)
      passEpoch
      expectActiveParams newPps
      Map.member (sppId pps) . psRetiring <$> getPState `shouldReturn` False
      poolDeposit (sppId pps) `shouldReturn` deposit
      getAccountBalance (sppAccountAddress newPps) `shouldReturn` mempty
      getAccountBalance (sppAccountAddress pps) `shouldReturn` mempty

  describe "Stake pools that re-register, retire, or both, at the same epoch boundary" $ do
    it "end up with the right parameters and keys" $ do
      reRegistering <- registerFreshPool
      retiring <- registerFreshPool
      reRegisteringAndRetiring <- registerFreshPool
      untouched <- registerFreshPool
      reRegistered <- changeParams reRegistering
      submitTx_ $ regPoolTx reRegistered
      retirePool (sppId retiring) (EpochInterval 1)
      reRegisteredAndRetired <- changeParams reRegisteringAndRetiring
      submitTx_ $ regPoolTx reRegisteredAndRetired
      retirePool (sppId reRegisteringAndRetiring) (EpochInterval 1)
      passEpoch
      expectActiveParams reRegistered
      expectActiveParams untouched
      expectRetired (sppId retiring)
      expectRetired (sppId reRegisteringAndRetiring)
      for_ [reRegistering, retiring, reRegisteringAndRetiring, reRegisteredAndRetired] expectKeysReleased
      for_ [reRegistered, untouched] expectKeysInUse
      ps <- getPState
      for_ [reRegistering, untouched] $ \pps ->
        Map.member (sppId pps) (psFutureStakePoolParams ps) `shouldBe` False

-- | Registers a fresh stake pool that has a BLS key.
registerFreshPool :: DijkstraEraImp era => ImpTestM era (StakePoolParams era)
registerFreshPool = registerFreshPoolWith id

-- | Registers a fresh stake pool that has a BLS key, with its parameters adjusted.
registerFreshPoolWith ::
  DijkstraEraImp era =>
  (StakePoolParams era -> StakePoolParams era) ->
  ImpTestM era (StakePoolParams era)
registerFreshPoolWith adjustParams = do
  blsKey <- freshBlsKey
  pps <- (\pps -> adjustParams pps {sppBlsKey = SJust blsKey}) <$> freshStakePool
  submitTx_ $ regPoolTx pps
  pure pps

-- | Changes all the parameters of a stake pool that the snapshots and the keys depend on.
changeParams :: DijkstraEraImp era => StakePoolParams era -> ImpTestM era (StakePoolParams era)
changeParams pps = do
  vrf <- freshKeyHashVRF
  blsKey <- freshBlsKey
  accountAddress <- registerAccountAddress
  owner <- freshKeyHash
  pure
    pps
      { sppVrf = vrf
      , sppBlsKey = SJust blsKey
      , sppPledge = sppPledge pps <> Coin 1
      , sppCost = sppCost pps <> Coin 1
      , sppMargin = 1 %! 2
      , sppAccountAddress = accountAddress
      , sppOwners = Set.insert owner (sppOwners pps)
      }

-- | Delegates some fresh stake to a stake pool.
delegateBillion :: DijkstraEraImp era => KeyHash StakePool -> ImpTestM era (Credential Staking)
delegateBillion = delegateStakeOf (Coin 1_000_000_000)

-- | Delegates the given amount of fresh stake to a stake pool.
delegateStakeOf ::
  DijkstraEraImp era => Coin -> KeyHash StakePool -> ImpTestM era (Credential Staking)
delegateStakeOf amount poolId = do
  cred <- KeyHashObj <$> freshKeyHash
  _ <- registerStakeCredential cred
  delegateStake cred poolId
  paymentKeyHash <- freshKeyHash @Payment
  sendCoinTo_ (mkAddr paymentKeyHash cred) amount
  pure cred

regPoolTx :: DijkstraEraImp era => StakePoolParams era -> Tx TopTx era
regPoolTx pps =
  mkBasicTx mkBasicTxBody
    & bodyTxL . certsTxBodyL .~ SSeq.singleton (RegPoolTxCert pps)

-- | Schedules a stake pool to retire the given number of epochs from now.
retirePool :: DijkstraEraImp era => KeyHash StakePool -> EpochInterval -> ImpTestM era ()
retirePool poolId retirementInterval = do
  curEpochNo <- getsNES nesELL
  submitTx_ $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.singleton (RetirePoolTxCert poolId (addEpochInterval curEpochNo retirementInterval))

-- | Crosses the epoch boundary, and returns the event of the stake pools that POOLREAP retired.
passEpochRetiredPools ::
  forall era.
  ( DijkstraEraImp era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "POOLREAP" era) ~ Shelley.ShelleyPoolreapEvent era
  ) =>
  ImpTestM era (Shelley.ShelleyPoolreapEvent era)
passEpochRetiredPools = do
  events <- impEventsFrom passEpoch
  case concatMap retiredPools events of
    [retired] -> pure retired
    retired -> assertFailure $ "Expected a single RetiredPools event, got " <> show (length retired)
  where
    retiredPools :: SomeSTSEvent era -> [Shelley.ShelleyPoolreapEvent era]
    retiredPools (SomeSTSEvent event) =
      case cast event :: Maybe (Shelley.ShelleyTickEvent era) of
        Just (Shelley.TickNewEpochEvent (EpochEvent (PoolReapEvent retired))) ->
          [retired]
        _ -> mempty

accountCredential :: StakePoolParams era -> Credential Staking
accountCredential = unAccountId . aaId . sppAccountAddress

blsKeyOf :: StakePoolParams era -> ImpTestM era BlsKey
blsKeyOf = expectJust . strictMaybeToMaybe . sppBlsKey

poolDeposit :: DijkstraEraImp era => KeyHash StakePool -> ImpTestM era Coin
poolDeposit poolId =
  fromCompact . spsDeposit <$> (expectJust . Map.lookup poolId . psStakePools =<< getPState)

getDeposited :: ImpTestM era Coin
getDeposited = getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosDepositedL

expectActiveParams :: DijkstraEraImp era => StakePoolParams era -> ImpTestM era ()
expectActiveParams pps = do
  sps <- expectJust . Map.lookup (sppId pps) . psStakePools =<< getPState
  stakePoolStateToStakePoolParams (aaNetworkId (sppAccountAddress pps)) (sppId pps) sps
    `shouldBe` pps

-- | The mark snapshot has the given parameters for the stake pool, both in its stake pool
-- snapshot and in its stake distribution.
expectMarkParams :: StakePoolParams era -> ImpTestM era ()
expectMarkParams pps = do
  spss <- expectJust =<< lookupMarkStakePoolSnapShot (sppId pps)
  spssVrf spss `shouldBe` sppVrf pps
  bksKey <$> spssBlsKey spss `shouldBe` sppBlsKey pps
  spssPledge spss `shouldBe` sppPledge pps
  spssCost spss `shouldBe` sppCost pps
  spssMargin spss `shouldBe` sppMargin pps
  spssAccountId spss `shouldBe` aaId (sppAccountAddress pps)
  mark <- getsNES $ nesEsL . esSnapshotsL . ssStakeMarkL
  individualPoolStakeVrf
    <$> Map.lookup (sppId pps) (unPoolDistr (msStakePoolDistr mark))
      `shouldBe` Just (sppVrf pps)

-- | The stake pool is neither registered, nor has future parameters, nor is retiring.
expectRetired :: DijkstraEraImp era => KeyHash StakePool -> ImpTestM era ()
expectRetired poolId = do
  ps <- getPState
  Map.member poolId (psStakePools ps) `shouldBe` False
  Map.member poolId (psFutureStakePoolParams ps) `shouldBe` False
  Map.member poolId (psRetiring ps) `shouldBe` False

-- | Exactly one stake pool uses the VRF key hash and the BLS key of these parameters.
expectKeysInUse :: DijkstraEraImp era => StakePoolParams era -> ImpTestM era ()
expectKeysInUse pps = do
  ps <- getPState
  unNonZero <$> Map.lookup (sppVrf pps) (psVRFKeyHashes ps) `shouldBe` Just 1
  for_ (sppBlsKey pps) $ \blsKey ->
    Set.member (hashBlsKey blsKey) (psBlsKeyHashes ps) `shouldBe` True

-- | No stake pool uses the VRF key hash or the BLS key of these parameters any more.
expectKeysReleased :: DijkstraEraImp era => StakePoolParams era -> ImpTestM era ()
expectKeysReleased pps = do
  ps <- getPState
  Map.member (sppVrf pps) (psVRFKeyHashes ps) `shouldBe` False
  for_ (sppBlsKey pps) $ \blsKey ->
    Set.member (hashBlsKey blsKey) (psBlsKeyHashes ps) `shouldBe` False

-- | The stake pool snapshot of a stake pool in the mark snapshot, if it is in there.
lookupMarkStakePoolSnapShot :: KeyHash StakePool -> ImpTestM era (Maybe StakePoolSnapShot)
lookupMarkStakePoolSnapShot poolId = do
  mark <- getsNES $ nesEsL . esSnapshotsL . ssStakeMarkL
  pure $ VM.lookup poolId $ ssStakePoolsSnapShot (msSnapShot mark)

-- | Whether a stake pool is in the mark snapshot, both in its stake pool snapshots and in its
-- stake distribution.
isInMarkSnapShot :: KeyHash StakePool -> ImpTestM era Bool
isInMarkSnapShot poolId = do
  mark <- getsNES $ nesEsL . esSnapshotsL . ssStakeMarkL
  inSnapShot <- isJust <$> lookupMarkStakePoolSnapShot poolId
  let inDistr = Map.member poolId (unPoolDistr (msStakePoolDistr mark))
  inSnapShot `shouldBe` inDistr
  pure inSnapShot

isSeatedInMark :: BlsKey -> ImpTestM era Bool
isSeatedInMark blsKey =
  isSeated blsKey <$> getsNES (nesEsL . esSnapshotsL . ssStakeMarkL . msLeiosCommitteeL)

isSeatedInSet :: BlsKey -> ImpTestM era Bool
isSeatedInSet blsKey =
  isSeated blsKey <$> getsNES (nesEsL . esSnapshotsL . ssStakeSetL . ssLeiosCommitteeL)

isSeated :: BlsKey -> LeiosCommittee -> Bool
isSeated blsKey = any ((== SJust (blsPubKey blsKey)) . seatVKey) . leiosCommitteeSeats
