{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.BenchmarkFunctions (
  ledgerSpendOneUTxO,
  ledgerSpendOneGivenUTxO,
  initUTxO, -- How to precompute env for the UTxO transactions
  ledgerEnv,
  ledgerRegisterStakeKeys,
  ledgerDeRegisterStakeKeys,
  ledgerRewardWithdrawals,
  ledgerStateWithNregisteredKeys, -- How to precompute env for the StakeKey transactions
  ledgerRegisterStakePools,
  ledgerReRegisterStakePools,
  ledgerRetireStakePools,
  ledgerStateWithNregisteredPools, -- How to precompute env for the Stake Pool transactions
  ledgerDelegateManyKeysOnePool,
  ledgerStateWithNkeysMpools, -- How to precompute env for the Stake Delegation transactions
) where

import Cardano.Ledger.Address (Addr, RewardAccount (..))
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  Network (..),
  StrictMaybe (..),
  TxIx,
  inject,
  mkTxIxPartial,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.PoolParams (
  PoolParams (..),
  ppCost,
  ppId,
  ppMargin,
  ppMetadata,
  ppOwners,
  ppPledge,
  ppRelays,
  ppRewardAccount,
  ppVrf,
 )
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..), ShelleyLEDGER)
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Shelley.TxBody (TxBody (ShelleyTxBody))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Slot (EpochNo (..), SlotNo (..))
import Cardano.Ledger.TxIn (TxIn (..), mkTxInPartial)
import Cardano.Protocol.Crypto (hashVerKeyVRF)
import Control.State.Transition.Extended (TRC (..), applySTS)
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Stack
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr, mkWitnessesVKey, vKey)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Generator.Core (
  VRFKeyPair (..),
  genesisCoins,
 )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Utils (
  RawSeed (..),
  mkKeyPair,
  mkKeyPair',
  mkVRFKeyPair,
  runShelleyBase,
  unsafeBoundRational,
 )

-- =========================================================

aliceStake :: KeyPair 'Staking
aliceStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 1)

alicePay :: KeyPair 'Payment
alicePay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 0)

aliceAddr :: Addr
aliceAddr = mkAddr alicePay aliceStake

-- ==========================================================

injcoins :: Integer -> [ShelleyTxOut ShelleyEra]
injcoins n = fmap (\_ -> ShelleyTxOut aliceAddr (inject $ Coin 100)) [0 .. n]

-- Cretae an initial UTxO set with n-many transaction outputs
initUTxO :: Integer -> UTxOState ShelleyEra
initUTxO n =
  UTxOState
    (genesisCoins genesisId (injcoins n))
    (Coin 0)
    (Coin 0)
    def
    mempty
    mempty

-- Protocal Parameters used for the benchmarknig tests.
-- Note that the fees and deposits are set to zero for
-- ease of creating transactions.
ppsBench :: (EraPParams era, ProtVerAtMost era 4, ProtVerAtMost era 6) => PParams era
ppsBench =
  emptyPParams
    & ppMaxBBSizeL .~ 50000
    & ppDL .~ unsafeBoundRational 0.5
    & ppEMaxL .~ EpochInterval 10000
    & ppKeyDepositL .~ Coin 0
    & ppMaxBHSizeL .~ 10000
    & ppMaxTxSizeL .~ 1000000000
    & ppMinFeeAL .~ Coin 0
    & ppMinFeeBL .~ Coin 0
    & ppMinUTxOValueL .~ Coin 10
    & ppPoolDepositL .~ Coin 0
    & ppRhoL .~ unsafeBoundRational 0.0021
    & ppTauL .~ unsafeBoundRational 0.2

ledgerEnv :: (EraPParams era, ProtVerAtMost era 4, ProtVerAtMost era 6) => LedgerEnv era
ledgerEnv = LedgerEnv (SlotNo 0) Nothing minBound ppsBench (ChainAccountState (Coin 0) (Coin 0))

testLEDGER ::
  LedgerState ShelleyEra ->
  Tx ShelleyEra ->
  LedgerEnv ShelleyEra ->
  ()
testLEDGER initSt tx env = do
  let st = runShelleyBase $ applySTS @(ShelleyLEDGER ShelleyEra) (TRC (env, initSt, tx))
  case st of
    Right _ -> ()
    Left e -> error $ show e

txbSpendOneUTxO :: TxBody ShelleyEra
txbSpendOneUTxO =
  ShelleyTxBody
    (Set.fromList [TxIn genesisId minBound])
    ( StrictSeq.fromList
        [ ShelleyTxOut aliceAddr (inject $ Coin 10)
        , ShelleyTxOut aliceAddr (inject $ Coin 89)
        ]
    )
    StrictSeq.empty
    (Withdrawals Map.empty)
    (Coin 1)
    (SlotNo 10)
    SNothing
    SNothing

txSpendOneUTxO :: Tx ShelleyEra
txSpendOneUTxO =
  mkBasicTx txbSpendOneUTxO
    & witsTxL
      .~ (mkBasicTxWits & addrTxWitsL .~ mkWitnessesVKey (hashAnnotated txbSpendOneUTxO) [asWitness alicePay])
    & auxDataTxL .~ SNothing

ledgerSpendOneUTxO :: Integer -> ()
ledgerSpendOneUTxO n = testLEDGER (initLedgerState n) txSpendOneUTxO ledgerEnv

ledgerSpendOneGivenUTxO :: UTxOState ShelleyEra -> ()
ledgerSpendOneGivenUTxO state = testLEDGER (LedgerState state def) txSpendOneUTxO ledgerEnv

-- ===========================================================================
--
-- Register a stake keys when there are a lot of registered stake keys
--

-- Create stake key pairs, corresponding to seeds
-- (RawSeed start 0 0 0 0) through (RawSeed end 0 0 0 0)
stakeKeys :: Word64 -> Word64 -> [KeyPair 'Staking]
stakeKeys start end = fmap (\w -> mkKeyPair' (RawSeed w 0 0 0 0)) [start .. end]

stakeKeyOne :: KeyPair 'Staking
stakeKeyOne = mkKeyPair' (RawSeed 1 0 0 0 0)

stakeKeyToCred :: KeyPair 'Staking -> Credential 'Staking
stakeKeyToCred = KeyHashObj . hashKey . vKey

firstStakeKeyCred :: Credential 'Staking
firstStakeKeyCred = stakeKeyToCred stakeKeyOne

-- Create stake key registration certificates
stakeKeyRegistrations :: [KeyPair 'Staking] -> StrictSeq (TxCert ShelleyEra)
stakeKeyRegistrations keys =
  StrictSeq.fromList $
    fmap (RegTxCert . KeyHashObj . hashKey . vKey) keys

-- Create a transaction body given a sequence of certificates.
-- It spends the genesis coin given by the index ix.
txbFromCerts :: TxIx -> StrictSeq (TxCert ShelleyEra) -> TxBody ShelleyEra
txbFromCerts ix regCerts =
  ShelleyTxBody
    (Set.fromList [TxIn genesisId ix])
    (StrictSeq.fromList [ShelleyTxOut aliceAddr (inject $ Coin 100)])
    regCerts
    (Withdrawals Map.empty)
    (Coin 0)
    (SlotNo 10)
    SNothing
    SNothing

makeSimpleTx ::
  TxBody ShelleyEra ->
  [KeyPair 'Witness] ->
  Tx ShelleyEra
makeSimpleTx txbody keysAddr =
  mkBasicTx mkBasicTxBody
    & bodyTxL .~ txbody
    & witsTxL .~ (mkBasicTxWits & addrTxWitsL .~ mkWitnessesVKey (hashAnnotated txbody) keysAddr)
    & auxDataTxL .~ SNothing

-- Create a transaction that registers stake credentials.
txRegStakeKeys :: TxIx -> [KeyPair 'Staking] -> Tx ShelleyEra
txRegStakeKeys ix keys =
  makeSimpleTx
    (txbFromCerts ix $ stakeKeyRegistrations keys)
    [asWitness alicePay]

initLedgerState :: Integer -> LedgerState ShelleyEra
initLedgerState n = LedgerState (initUTxO n) def

makeLEDGERState ::
  HasCallStack => LedgerState ShelleyEra -> Tx ShelleyEra -> LedgerState ShelleyEra
makeLEDGERState start tx =
  let st = applySTS @(ShelleyLEDGER ShelleyEra) (TRC (ledgerEnv, start, tx))
   in case runShelleyBase st of
        Right st' -> st'
        Left e -> error $ show e

-- Create a ledger state that has registered stake credentials that
-- are seeded with (RawSeed n 0 0 0 0) to (RawSeed m 0 0 0 0).
-- It is pre-populated with 2 genesis injcoins.
ledgerStateWithNregisteredKeys :: Word64 -> Word64 -> LedgerState ShelleyEra
ledgerStateWithNregisteredKeys n m =
  makeLEDGERState (initLedgerState 1) $ txRegStakeKeys minBound (stakeKeys n m)

-- ===========================================================
-- Stake Key Registration example

-- Given a ledger state, presumably created by ledgerStateWithNregisteredKeys n m,
-- so that keys (RawSeed n 0 0 0 0) through (RawSeed m 0 0 0 0) are already registered,
-- register new keys (RawSeed x 0 0 0 0) through (RawSeed y 0 0 0 0).
-- Note that [n, m] must be disjoint from [x, y].
ledgerRegisterStakeKeys :: Word64 -> Word64 -> LedgerState ShelleyEra -> ()
ledgerRegisterStakeKeys x y state =
  testLEDGER
    state
    (txRegStakeKeys (mkTxIxPartial 1) (stakeKeys x y))
    ledgerEnv

-- ===========================================================
-- Deregistration example

-- Create a transaction body that de-registers stake credentials,
-- corresponding to the keys seeded with (RawSeed x 0 0 0 0) to (RawSeed y 0 0 0 0)
txbDeRegStakeKey :: Word64 -> Word64 -> TxBody ShelleyEra
txbDeRegStakeKey x y =
  ShelleyTxBody
    (Set.fromList [mkTxInPartial genesisId 1])
    (StrictSeq.fromList [ShelleyTxOut aliceAddr (inject $ Coin 100)])
    ( StrictSeq.fromList $
        fmap (UnRegTxCert . stakeKeyToCred) (stakeKeys x y)
    )
    (Withdrawals Map.empty)
    (Coin 0)
    (SlotNo 10)
    SNothing
    SNothing

-- Create a transaction that deregisters stake credentials numbered x through y.
-- It spends the genesis coin indexed by 1.
txDeRegStakeKeys :: Word64 -> Word64 -> Tx ShelleyEra
txDeRegStakeKeys x y =
  makeSimpleTx
    (txbDeRegStakeKey x y)
    (asWitness alicePay : fmap asWitness (stakeKeys x y))

-- Given a ledger state, presumably created by ledgerStateWithNregisteredKeys n m,
-- so that keys (RawSeed n 0 0 0 0) through (RawSeed m 0 0 0 0) are already registered,
-- deregister keys (RawSeed x 0 0 0 0) through (RawSeed y 0 0 0 0).
-- Note that [x, y] must be contained in [n, m].
ledgerDeRegisterStakeKeys :: Word64 -> Word64 -> LedgerState ShelleyEra -> ()
ledgerDeRegisterStakeKeys x y state =
  testLEDGER
    state
    (txDeRegStakeKeys x y)
    ledgerEnv

-- ===========================================================
-- Reward Withdrawal example

-- Create a transaction body that withdrawals from reward accounts,
-- corresponding to the keys seeded with (RawSeed x 0 0 0 0) to (RawSeed y 0 0 0 0).
txbWithdrawals :: Word64 -> Word64 -> TxBody ShelleyEra
txbWithdrawals x y =
  ShelleyTxBody
    (Set.fromList [mkTxInPartial genesisId 1])
    (StrictSeq.fromList [ShelleyTxOut aliceAddr (inject $ Coin 100)])
    StrictSeq.empty
    ( Withdrawals $
        Map.fromList $
          fmap (\ks -> (RewardAccount Testnet (stakeKeyToCred ks), Coin 0)) (stakeKeys x y)
    )
    (Coin 0)
    (SlotNo 10)
    SNothing
    SNothing

-- Create a transaction that withdrawals from a reward accounts.
-- It spends the genesis coin indexed by 1.
txWithdrawals :: Word64 -> Word64 -> Tx ShelleyEra
txWithdrawals x y =
  makeSimpleTx
    (txbWithdrawals x y)
    (asWitness alicePay : fmap asWitness (stakeKeys x y))

-- Given a ledger state, presumably created by ledgerStateWithNregisteredKeys n m,
-- so that keys (RawSeed n 0 0 0 0) through (RawSeed m 0 0 0 0) are already registered,
-- make reward withdrawals for keys (RawSeed x 0 0 0 0) through (RawSeed y 0 0 0 0).
-- Note that [x, y] must be contained in [n, m].
ledgerRewardWithdrawals :: Word64 -> Word64 -> LedgerState ShelleyEra -> ()
ledgerRewardWithdrawals x y state = testLEDGER state (txWithdrawals x y) ledgerEnv

-- ===========================================================================
--
-- Register a stake pool when there are a lot of registered stake pool
--

-- Create stake pool key pairs, corresponding to seeds
-- (RawSeed start 0 0 0 0) through (RawSeed end 0 0 0 0)
poolColdKeys :: Word64 -> Word64 -> [KeyPair 'StakePool]
poolColdKeys start end = fmap (\w -> mkKeyPair' (RawSeed w 1 0 0 0)) [start .. end]

firstStakePool :: KeyPair 'StakePool
firstStakePool = mkKeyPair' (RawSeed 1 1 0 0 0)

mkPoolKeyHash :: KeyPair 'StakePool -> KeyHash 'StakePool
mkPoolKeyHash = hashKey . vKey

firstStakePoolKeyHash :: KeyHash 'StakePool
firstStakePoolKeyHash = mkPoolKeyHash firstStakePool

vrfKeyHash :: VRFVerKeyHash 'StakePoolVRF
vrfKeyHash = hashVerKeyVRF @MockCrypto . vrfVerKey . mkVRFKeyPair @MockCrypto $ RawSeed 0 0 0 0 0

mkPoolParameters :: KeyPair 'StakePool -> PoolParams
mkPoolParameters keys =
  PoolParams
    { ppId = hashKey (vKey keys)
    , ppVrf = vrfKeyHash
    , ppPledge = Coin 0
    , ppCost = Coin 0
    , ppMargin = unsafeBoundRational 0
    , ppRewardAccount = RewardAccount Testnet firstStakeKeyCred
    , ppOwners = Set.singleton $ hashKey (vKey stakeKeyOne)
    , ppRelays = StrictSeq.empty
    , ppMetadata = SNothing
    }

-- Create stake pool registration certs
poolRegCerts :: [KeyPair 'StakePool] -> StrictSeq (TxCert ShelleyEra)
poolRegCerts = StrictSeq.fromList . fmap (RegPoolTxCert . mkPoolParameters)

-- Create a transaction that registers stake pools.
txRegStakePools :: TxIx -> [KeyPair 'StakePool] -> Tx ShelleyEra
txRegStakePools ix keys =
  makeSimpleTx
    (txbFromCerts ix $ poolRegCerts keys)
    ([asWitness alicePay, asWitness stakeKeyOne] ++ fmap asWitness keys)

-- Create a ledger state that has n registered stake pools.
-- The keys are seeded with (RawSeed n 1 0 0 0) to (RawSeed m 1 0 0 0)
-- It is pre-populated with 2 genesis injcoins.
ledgerStateWithNregisteredPools :: Word64 -> Word64 -> LedgerState ShelleyEra
ledgerStateWithNregisteredPools n m =
  makeLEDGERState (initLedgerState 1) $ txRegStakePools minBound (poolColdKeys n m)

-- ===========================================================
-- Stake Pool Registration example

-- Given a ledger state, presumably created by ledgerStateWithNregisteredPools n m,
-- so that pool keys (RawSeed n 1 0 0 0) through (RawSeed m 1 0 0 0) are already registered,
-- register new pools (RawSeed x 0 0 0 0) through (RawSeed y 0 0 0 0).
-- Note that [n, m] must be disjoint from [x, y].
ledgerRegisterStakePools :: Word64 -> Word64 -> LedgerState ShelleyEra -> ()
ledgerRegisterStakePools x y state =
  testLEDGER
    state
    (txRegStakePools (mkTxIxPartial 1) (poolColdKeys x y))
    ledgerEnv

-- ===========================================================
-- Stake Pool Re-Registration/Update example

-- Given a ledger state, presumably created by ledgerStateWithNregisteredPools n m,
-- so that pool keys (RawSeed n 1 0 0 0) through (RawSeed m 1 0 0 0) are already registered,
-- re-register pools (RawSeed x 0 0 0 0) through (RawSeed y 0 0 0 0).
-- Note that [n, m] must be contained in [x, y].
ledgerReRegisterStakePools :: Word64 -> Word64 -> LedgerState ShelleyEra -> ()
ledgerReRegisterStakePools x y state =
  testLEDGER
    state
    (txRegStakePools (mkTxIxPartial 1) (poolColdKeys x y))
    ledgerEnv

-- ===========================================================
-- Stake Pool Retirement example

-- Create a transaction body that retires stake pools,
-- corresponding to the keys seeded with (RawSeed x 1 0 0 0) to (RawSeed y 1 0 0 0)
txbRetireStakePool :: Word64 -> Word64 -> TxBody ShelleyEra
txbRetireStakePool x y =
  ShelleyTxBody
    (Set.fromList [mkTxInPartial genesisId 1])
    (StrictSeq.fromList [ShelleyTxOut aliceAddr (inject $ Coin 100)])
    ( StrictSeq.fromList $
        fmap
          (\ks -> RetirePoolTxCert (mkPoolKeyHash ks) (EpochNo 1))
          (poolColdKeys x y)
    )
    (Withdrawals Map.empty)
    (Coin 0)
    (SlotNo 10)
    SNothing
    SNothing

-- Create a transaction that retires stake pools x through y.
-- It spends the genesis coin indexed by 1.
txRetireStakePool :: Word64 -> Word64 -> Tx ShelleyEra
txRetireStakePool x y =
  makeSimpleTx
    (txbRetireStakePool x y)
    (asWitness alicePay : fmap asWitness (poolColdKeys x y))

-- Given a ledger state, presumably created by ledgerStateWithNregisteredPools n m,
-- so that pool keys (RawSeed n 1 0 0 0) through (RawSeed m 1 0 0 0) are already registered,
-- retire pools (RawSeed x 0 0 0 0) through (RawSeed y 0 0 0 0).
-- Note that [n, m] must be contained in [x, y].
ledgerRetireStakePools :: Word64 -> Word64 -> LedgerState ShelleyEra -> ()
ledgerRetireStakePools x y state = testLEDGER state (txRetireStakePool x y) ledgerEnv

-- ===========================================================================
--
-- Delegate Stake Credentials when many stake keys and stake pools are registered.
--

-- Create a ledger state that has n registered stake keys and m stake pools.
-- The stake keys are seeded with (RawSeed 1 0 0 0 0) to (RawSeed n 0 0 0 0)
-- The stake pools are seeded with (RawSeed 1 1 0 0 0) to (RawSeed m 1 0 0 0)
-- It is pre-populated with 3 genesis injcoins.
ledgerStateWithNkeysMpools :: Word64 -> Word64 -> LedgerState ShelleyEra
ledgerStateWithNkeysMpools n m =
  makeLEDGERState
    (makeLEDGERState (initLedgerState 2) $ txRegStakeKeys minBound (stakeKeys 1 n))
    (txRegStakePools (mkTxIxPartial 1) (poolColdKeys 1 m))

-- Create a transaction body that delegates several keys to ONE stake pool,
-- corresponding to the keys seeded with (RawSeed n 0 0 0 0) to (RawSeed m 0 0 0 0)
txbDelegate :: Word64 -> Word64 -> TxBody ShelleyEra
txbDelegate n m =
  ShelleyTxBody
    (Set.fromList [mkTxInPartial genesisId 2])
    (StrictSeq.fromList [ShelleyTxOut aliceAddr (inject $ Coin 100)])
    ( StrictSeq.fromList $
        fmap
          (\ks -> DelegStakeTxCert (stakeKeyToCred ks) firstStakePoolKeyHash)
          (stakeKeys n m)
    )
    (Withdrawals Map.empty)
    (Coin 0)
    (SlotNo 10)
    SNothing
    SNothing

-- Create a transaction that delegates stake.
txDelegate :: Word64 -> Word64 -> Tx ShelleyEra
txDelegate n m =
  makeSimpleTx
    (txbDelegate n m)
    (asWitness alicePay : fmap asWitness (stakeKeys n m))

-- Given a ledger state, presumably created by ledgerStateWithNkeysMpools n m,
-- so that stake keys (RawSeed 1 0 0 0 0) through (RawSeed n 0 0 0 0) are already registered
-- and pool keys (RawSeed 1 1 0 0 0) through (RawSeed m 1 0 0 0) are already registered,
-- delegate stake keys (RawSeed x 0 0 0 0) through (RawSeed y 0 0 0 0) to ONE pool.
-- Note that [x, y] must be contained in [1, n].
ledgerDelegateManyKeysOnePool :: Word64 -> Word64 -> LedgerState ShelleyEra -> ()
ledgerDelegateManyKeysOnePool x y state = testLEDGER state (txDelegate x y) ledgerEnv
