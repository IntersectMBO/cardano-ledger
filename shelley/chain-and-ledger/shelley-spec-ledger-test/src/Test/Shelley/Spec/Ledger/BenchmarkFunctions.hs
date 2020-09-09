{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Shelley.Spec.Ledger.BenchmarkFunctions
  ( ledgerSpendOneUTxO,
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
    B, -- Crypto instance for Benchmarking
  )
where

import Cardano.Crypto.Hash.Blake2b (Blake2b_256)
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Era (Era (..))
import Control.State.Transition.Extended (TRC (..), applySTS)
import qualified Data.Map as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Address (Addr)
import Shelley.Spec.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.Delegation.Certificates (DelegCert (..))
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
import Shelley.Spec.Ledger.Keys
  ( Hash,
    KeyHash,
    KeyPair (..),
    KeyRole (..),
    VerKeyVRF,
    asWitness,
    hashKey,
    hashVerKeyVRF,
    vKey,
  )
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    DPState,
    UTxOState (..),
    emptyDPState,
    emptyPPUPState,
  )
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..), emptyPParams)
import Shelley.Spec.Ledger.STS.Ledger (LEDGER, LedgerEnv (..))
import Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo (..))
import Shelley.Spec.Ledger.Tx (Tx (..), WitnessSetHKD (..))
import Shelley.Spec.Ledger.TxBody
  ( DCert (..),
    Delegation (..),
    PoolCert (..),
    PoolParams (..),
    RewardAcnt (..),
    TxBody (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
    _poolCost,
    _poolMD,
    _poolMargin,
    _poolOwners,
    _poolPledge,
    _poolPubKey,
    _poolRAcnt,
    _poolRelays,
    _poolVrf,
  )
import Shelley.Spec.Ledger.UTxO (makeWitnessesVKey)
import qualified Test.Shelley.Spec.Ledger.ConcreteCryptoTypes as Original
  ( C_Crypto,
  )
import Test.Shelley.Spec.Ledger.Generator.Core
  ( genesisCoins,
    genesisId,
  )
import Test.Shelley.Spec.Ledger.Utils
  ( mkAddr,
    mkKeyPair,
    mkKeyPair',
    mkVRFKeyPair,
    runShelleyBase,
    unsafeMkUnitInterval,
  )

data B

instance Era B where
  type Crypto B = B_Crypto

data B_Crypto

instance Cardano.Ledger.Crypto.Crypto B_Crypto where
  type KES B_Crypto = KES Original.C_Crypto
  type VRF B_Crypto = VRF Original.C_Crypto
  type DSIGN B_Crypto = DSIGN Original.C_Crypto
  type HASH B_Crypto = Blake2b_256
  type ADDRHASH B_Crypto = Blake2b_256

-- =========================================================

aliceStake :: KeyPair 'Staking B
aliceStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 1)

alicePay :: KeyPair 'Payment B
alicePay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 0)

aliceAddr :: Addr B
aliceAddr = mkAddr (alicePay, aliceStake)

-- ==========================================================

coins :: Integer -> [TxOut B]
coins n = fmap (\_ -> TxOut aliceAddr (Coin 100)) [0 .. n]

-- Cretae an initial UTxO set with n-many transaction outputs
initUTxO :: Integer -> UTxOState B
initUTxO n =
  UTxOState
    (genesisCoins (coins n))
    (Coin 0)
    (Coin 0)
    emptyPPUPState

-- Protocal Parameters used for the benchmarknig tests.
-- Note that the fees and deposits are set to zero for
-- ease of creating transactions.
ppsBench :: PParams era
ppsBench =
  emptyPParams
    { _maxBBSize = 50000,
      _d = unsafeMkUnitInterval 0.5,
      _eMax = EpochNo 10000,
      _keyDeposit = Coin 0,
      _maxBHSize = 10000,
      _maxTxSize = 1000000000,
      _minfeeA = 0,
      _minfeeB = 0,
      _minUTxOValue = Coin 10,
      _poolDeposit = Coin 0,
      _rho = unsafeMkUnitInterval 0.0021,
      _tau = unsafeMkUnitInterval 0.2
    }

ledgerEnv :: LedgerEnv B
ledgerEnv = LedgerEnv (SlotNo 0) 0 ppsBench (AccountState (Coin 0) (Coin 0))

testLEDGER ::
  (UTxOState B, DPState B) ->
  Tx B ->
  LedgerEnv B ->
  ()
testLEDGER initSt tx env = do
  let st = runShelleyBase $ applySTS @(LEDGER B) (TRC (env, initSt, tx))
  case st of
    Right _ -> ()
    Left e -> error $ show e

txbSpendOneUTxO :: TxBody B
txbSpendOneUTxO =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.fromList [TxOut aliceAddr (Coin 10), TxOut aliceAddr (Coin 89)])
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 10)
    SNothing
    SNothing

txSpendOneUTxO :: Tx B
txSpendOneUTxO =
  Tx
    txbSpendOneUTxO
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated txbSpendOneUTxO) [asWitness alicePay]
      }
    SNothing

ledgerSpendOneUTxO :: Integer -> ()
ledgerSpendOneUTxO n = testLEDGER (initUTxO n, emptyDPState) txSpendOneUTxO ledgerEnv

ledgerSpendOneGivenUTxO :: UTxOState B -> ()
ledgerSpendOneGivenUTxO state = testLEDGER (state, emptyDPState) txSpendOneUTxO ledgerEnv

-- ===========================================================================
--
-- Register a stake keys when there are a lot of registered stake keys
--

-- Create stake key pairs, corresponding to seeds
-- (start, 0, 0, 0, 0) through (end, 0, 0, 0, 0)
stakeKeys :: Word64 -> Word64 -> [KeyPair 'Staking B]
stakeKeys start end = fmap (\w -> mkKeyPair' (w, 0, 0, 0, 0)) [start .. end]

stakeKeyOne :: KeyPair 'Staking B
stakeKeyOne = mkKeyPair' (1, 0, 0, 0, 0)

stakeKeyToCred :: KeyPair 'Staking B -> Credential 'Staking B
stakeKeyToCred = KeyHashObj . hashKey . vKey

firstStakeKeyCred :: Credential 'Staking B
firstStakeKeyCred = stakeKeyToCred stakeKeyOne

-- Create stake key registration certificates
stakeKeyRegistrations :: [KeyPair 'Staking B] -> StrictSeq (DCert B)
stakeKeyRegistrations keys =
  StrictSeq.fromList $
    fmap (DCertDeleg . RegKey . (KeyHashObj . hashKey . vKey)) keys

-- Create a transaction body given a sequence of certificates.
-- It spends the genesis coin given by the index ix.
txbFromCerts :: Natural -> StrictSeq (DCert B) -> TxBody B
txbFromCerts ix regCerts =
  TxBody
    (Set.fromList [TxIn genesisId ix])
    (StrictSeq.fromList [TxOut aliceAddr (Coin 100)])
    regCerts
    (Wdrl Map.empty)
    (Coin 0)
    (SlotNo 10)
    SNothing
    SNothing

makeSimpleTx ::
  TxBody B ->
  [KeyPair 'Witness B] ->
  Tx B
makeSimpleTx body keysAddr =
  Tx
    body
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated body) keysAddr
      }
    SNothing

-- Create a transaction that registers stake credentials.
txRegStakeKeys :: Natural -> [KeyPair 'Staking B] -> Tx B
txRegStakeKeys ix keys =
  makeSimpleTx
    (txbFromCerts ix $ stakeKeyRegistrations keys)
    [asWitness alicePay]

initLedgerState :: Integer -> (UTxOState B, DPState B)
initLedgerState n = (initUTxO n, emptyDPState)

makeLEDGERState ::
  (UTxOState B, DPState B) ->
  Tx B ->
  (UTxOState B, DPState B)
makeLEDGERState start tx =
  let st = applySTS @(LEDGER B) (TRC (ledgerEnv, start, tx))
   in case runShelleyBase st of
        Right st' -> st'
        Left e -> error $ show e

-- Create a ledger state that has registered stake credentials that
-- are seeded with (n, 0, 0, 0, 0) to (m, 0, 0, 0, 0).
-- It is pre-populated with 2 genesis coins.
ledgerStateWithNregisteredKeys ::
  Word64 -> Word64 -> (UTxOState B, DPState B)
ledgerStateWithNregisteredKeys n m =
  makeLEDGERState (initLedgerState 1) $ txRegStakeKeys 0 (stakeKeys n m)

-- ===========================================================
-- Stake Key Registration example

-- Given a ledger state, presumably created by ledgerStateWithNregisteredKeys n m,
-- so that keys (n, 0, 0, 0, 0) through (m, 0, 0, 0, 0) are already registered,
-- register new keys (x, 0, 0, 0, 0) through (y, 0, 0, 0, 0).
-- Note that [n, m] must be disjoint from [x, y].
ledgerRegisterStakeKeys :: Word64 -> Word64 -> (UTxOState B, DPState B) -> ()
ledgerRegisterStakeKeys x y state =
  testLEDGER
    state
    (txRegStakeKeys 1 (stakeKeys x y))
    ledgerEnv

-- ===========================================================
-- Deregistration example

-- Create a transaction body that de-registers stake credentials,
-- corresponding to the keys seeded with (x, 0, 0, 0, 0) to (y, 0, 0, 0, 0)
txbDeRegStakeKey :: Word64 -> Word64 -> TxBody B
txbDeRegStakeKey x y =
  TxBody
    (Set.fromList [TxIn genesisId 1])
    (StrictSeq.fromList [TxOut aliceAddr (Coin 100)])
    ( StrictSeq.fromList $
        fmap (DCertDeleg . DeRegKey . stakeKeyToCred) (stakeKeys x y)
    )
    (Wdrl Map.empty)
    (Coin 0)
    (SlotNo 10)
    SNothing
    SNothing

-- Create a transaction that deregisters stake credentials numbered x through y.
-- It spends the genesis coin indexed by 1.
txDeRegStakeKeys :: Word64 -> Word64 -> Tx B
txDeRegStakeKeys x y =
  makeSimpleTx
    (txbDeRegStakeKey x y)
    (asWitness alicePay : fmap asWitness (stakeKeys x y))

-- Given a ledger state, presumably created by ledgerStateWithNregisteredKeys n m,
-- so that keys (n, 0, 0, 0, 0) through (m, 0, 0, 0, 0) are already registered,
-- deregister keys (x, 0, 0, 0, 0) through (y, 0, 0, 0, 0).
-- Note that [x, y] must be contained in [n, m].
ledgerDeRegisterStakeKeys :: Word64 -> Word64 -> (UTxOState B, DPState B) -> ()
ledgerDeRegisterStakeKeys x y state =
  testLEDGER
    state
    (txDeRegStakeKeys x y)
    ledgerEnv

-- ===========================================================
-- Reward Withdrawal example

-- Create a transaction body that withdrawls from reward accounts,
-- corresponding to the keys seeded with (x, 0, 0, 0, 0) to (y, 0, 0, 0, 0).
txbWithdrawals :: Word64 -> Word64 -> TxBody B
txbWithdrawals x y =
  TxBody
    (Set.fromList [TxIn genesisId 1])
    (StrictSeq.fromList [TxOut aliceAddr (Coin 100)])
    StrictSeq.empty
    ( Wdrl $
        Map.fromList $
          fmap (\ks -> (RewardAcnt Testnet (stakeKeyToCred ks), Coin 0)) (stakeKeys x y)
    )
    (Coin 0)
    (SlotNo 10)
    SNothing
    SNothing

-- Create a transaction that withdrawls from a reward accounts.
-- It spends the genesis coin indexed by 1.
txWithdrawals :: Word64 -> Word64 -> Tx B
txWithdrawals x y =
  makeSimpleTx
    (txbWithdrawals x y)
    (asWitness alicePay : fmap asWitness (stakeKeys x y))

-- Given a ledger state, presumably created by ledgerStateWithNregisteredKeys n m,
-- so that keys (n, 0, 0, 0, 0) through (m, 0, 0, 0, 0) are already registered,
-- make reward withdrawls for keys (x, 0, 0, 0, 0) through (y, 0, 0, 0, 0).
-- Note that [x, y] must be contained in [n, m].
ledgerRewardWithdrawals :: Word64 -> Word64 -> (UTxOState B, DPState B) -> ()
ledgerRewardWithdrawals x y state = testLEDGER state (txWithdrawals x y) ledgerEnv

-- ===========================================================================
--
-- Register a stake pool when there are a lot of registered stake pool
--

-- Create stake pool key pairs, corresponding to seeds
-- (start, 0, 0, 0, 0) through (end, 0, 0, 0, 0)
poolColdKeys :: Word64 -> Word64 -> [KeyPair 'StakePool B]
poolColdKeys start end = fmap (\w -> mkKeyPair' (w, 1, 0, 0, 0)) [start .. end]

firstStakePool :: KeyPair 'StakePool B
firstStakePool = mkKeyPair' (1, 1, 0, 0, 0)

mkPoolKeyHash :: KeyPair 'StakePool B -> KeyHash 'StakePool B
mkPoolKeyHash = hashKey . vKey

firstStakePoolKeyHash :: KeyHash 'StakePool B
firstStakePoolKeyHash = mkPoolKeyHash firstStakePool

vrfKeyHash :: Hash B (VerKeyVRF B)
vrfKeyHash = hashVerKeyVRF . snd . mkVRFKeyPair $ (0, 0, 0, 0, 0)

mkPoolParameters :: KeyPair 'StakePool B -> PoolParams B
mkPoolParameters keys =
  PoolParams
    { _poolPubKey = (hashKey . vKey) keys,
      _poolVrf = vrfKeyHash,
      _poolPledge = Coin 0,
      _poolCost = Coin 0,
      _poolMargin = unsafeMkUnitInterval 0,
      _poolRAcnt = RewardAcnt Testnet firstStakeKeyCred,
      _poolOwners = Set.singleton $ (hashKey . vKey) stakeKeyOne,
      _poolRelays = StrictSeq.empty,
      _poolMD = SNothing
    }

-- Create stake pool registration certs
poolRegCerts :: [KeyPair 'StakePool B] -> StrictSeq (DCert B)
poolRegCerts = StrictSeq.fromList . fmap (DCertPool . RegPool . mkPoolParameters)

-- Create a transaction that registers stake pools.
txRegStakePools :: Natural -> [KeyPair 'StakePool B] -> Tx B
txRegStakePools ix keys =
  makeSimpleTx
    (txbFromCerts ix $ poolRegCerts keys)
    ([asWitness alicePay, asWitness stakeKeyOne] ++ fmap asWitness keys)

-- Create a ledger state that has n registered stake pools.
-- The keys are seeded with (n, 1, 0, 0, 0) to (m, 1, 0, 0, 0)
-- It is pre-populated with 2 genesis coins.
ledgerStateWithNregisteredPools :: Word64 -> Word64 -> (UTxOState B, DPState B)
ledgerStateWithNregisteredPools n m =
  makeLEDGERState (initLedgerState 1) $ txRegStakePools 0 (poolColdKeys n m)

-- ===========================================================
-- Stake Pool Registration example

-- Given a ledger state, presumably created by ledgerStateWithNregisteredPools n m,
-- so that pool keys (n, 1, 0, 0, 0) through (m, 1, 0, 0, 0) are already registered,
-- register new pools (x, 0, 0, 0, 0) through (y, 0, 0, 0, 0).
-- Note that [n, m] must be disjoint from [x, y].
ledgerRegisterStakePools :: Word64 -> Word64 -> (UTxOState B, DPState B) -> ()
ledgerRegisterStakePools x y state =
  testLEDGER
    state
    (txRegStakePools 1 (poolColdKeys x y))
    ledgerEnv

-- ===========================================================
-- Stake Pool Re-Registration/Update example

-- Given a ledger state, presumably created by ledgerStateWithNregisteredPools n m,
-- so that pool keys (n, 1, 0, 0, 0) through (m, 1, 0, 0, 0) are already registered,
-- re-register pools (x, 0, 0, 0, 0) through (y, 0, 0, 0, 0).
-- Note that [n, m] must be contained in [x, y].
ledgerReRegisterStakePools :: Word64 -> Word64 -> (UTxOState B, DPState B) -> ()
ledgerReRegisterStakePools x y state =
  testLEDGER
    state
    (txRegStakePools 1 (poolColdKeys x y))
    ledgerEnv

-- ===========================================================
-- Stake Pool Retirement example

-- Create a transaction body that retires stake pools,
-- corresponding to the keys seeded with (x, 1, 0, 0, 0) to (y, 1, 0, 0, 0)
txbRetireStakePool :: Word64 -> Word64 -> TxBody B
txbRetireStakePool x y =
  TxBody
    (Set.fromList [TxIn genesisId 1])
    (StrictSeq.fromList [TxOut aliceAddr (Coin 100)])
    ( StrictSeq.fromList $
        fmap
          (\ks -> DCertPool $ RetirePool (mkPoolKeyHash ks) (EpochNo 1))
          (poolColdKeys x y)
    )
    (Wdrl Map.empty)
    (Coin 0)
    (SlotNo 10)
    SNothing
    SNothing

-- Create a transaction that retires stake pools x through y.
-- It spends the genesis coin indexed by 1.
txRetireStakePool :: Word64 -> Word64 -> Tx B
txRetireStakePool x y =
  makeSimpleTx
    (txbRetireStakePool x y)
    (asWitness alicePay : fmap asWitness (poolColdKeys x y))

-- Given a ledger state, presumably created by ledgerStateWithNregisteredPools n m,
-- so that pool keys (n, 1, 0, 0, 0) through (m, 1, 0, 0, 0) are already registered,
-- retire pools (x, 0, 0, 0, 0) through (y, 0, 0, 0, 0).
-- Note that [n, m] must be contained in [x, y].
ledgerRetireStakePools :: Word64 -> Word64 -> (UTxOState B, DPState B) -> ()
ledgerRetireStakePools x y state = testLEDGER state (txRetireStakePool x y) ledgerEnv

-- ===========================================================================
--
-- Delegate Stake Credentials when many stake keys and stake pools are registered.
--

-- Create a ledger state that has n registered stake keys and m stake pools.
-- The stake keys are seeded with (1, 0, 0, 0, 0) to (n, 0, 0, 0, 0)
-- The stake pools are seeded with (1, 1, 0, 0, 0) to (m, 1, 0, 0, 0)
-- It is pre-populated with 3 genesis coins.
ledgerStateWithNkeysMpools :: Word64 -> Word64 -> (UTxOState B, DPState B)
ledgerStateWithNkeysMpools n m =
  makeLEDGERState
    (makeLEDGERState (initLedgerState 2) $ txRegStakeKeys 0 (stakeKeys 1 n))
    (txRegStakePools 1 (poolColdKeys 1 m))

-- Create a transaction body that delegates several keys to ONE stake pool,
-- corresponding to the keys seeded with (n, 0, 0, 0, 0) to (m, 0, 0, 0, 0)
txbDelegate :: Word64 -> Word64 -> TxBody B
txbDelegate n m =
  TxBody
    (Set.fromList [TxIn genesisId 2])
    (StrictSeq.fromList [TxOut aliceAddr (Coin 100)])
    ( StrictSeq.fromList $
        fmap
          (\ks -> DCertDeleg $ Delegate (Delegation (stakeKeyToCred ks) firstStakePoolKeyHash))
          (stakeKeys n m)
    )
    (Wdrl Map.empty)
    (Coin 0)
    (SlotNo 10)
    SNothing
    SNothing

-- Create a transaction that delegates stake.
txDelegate :: Word64 -> Word64 -> Tx B
txDelegate n m =
  makeSimpleTx
    (txbDelegate n m)
    (asWitness alicePay : fmap asWitness (stakeKeys n m))

-- Given a ledger state, presumably created by ledgerStateWithNkeysMpools n m,
-- so that stake keys (1, 0, 0, 0, 0) through (n, 0, 0, 0, 0) are already registered
-- and pool keys (1, 1, 0, 0, 0) through (m, 1, 0, 0, 0) are already registered,
-- delegate stake keys (x, 0, 0, 0, 0) through (y, 0, 0, 0, 0) to ONE pool.
-- Note that [x, y] must be contained in [1, n].
ledgerDelegateManyKeysOnePool ::
  Word64 -> Word64 -> (UTxOState B, DPState B) -> ()
ledgerDelegateManyKeysOnePool x y state = testLEDGER state (txDelegate x y) ledgerEnv
