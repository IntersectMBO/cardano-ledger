{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
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
  B, -- Era instance for Benchmarking
  B_Crypto, -- Crypto instance for Benchmarking
)
where

import Cardano.Crypto.Hash.Blake2b (Blake2b_256)
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
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Keys (
  Hash,
  KeyHash,
  KeyRole (..),
  VerKeyVRF,
  asWitness,
  hashKey,
  hashVerKeyVRF,
 )
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
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  LedgerState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..), ShelleyLEDGER)
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyTxBody (..))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits (addrWits)
import Cardano.Ledger.Slot (EpochNo (..), SlotNo (..))
import Cardano.Ledger.TxIn (TxIn (..), mkTxInPartial)
import Cardano.Protocol.TPraos.API (PraosCrypto)
import Control.State.Transition.Extended (TRC (..), applySTS)
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Stack
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr, mkWitnessesVKey, vKey)
import qualified Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes as Original (
  C_Crypto,
 )
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

-- ===============================================
-- A special Era to run the Benchmarks in

type B = ShelleyEra B_Crypto

data B_Crypto

instance Cardano.Ledger.Crypto.Crypto B_Crypto where
  type KES B_Crypto = KES Original.C_Crypto
  type VRF B_Crypto = VRF Original.C_Crypto
  type DSIGN B_Crypto = DSIGN Original.C_Crypto
  type HASH B_Crypto = Blake2b_256
  type ADDRHASH B_Crypto = Blake2b_256

instance PraosCrypto B_Crypto

-- =========================================================

aliceStake :: KeyPair 'Staking B_Crypto
aliceStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 1)

alicePay :: KeyPair 'Payment B_Crypto
alicePay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 0)

aliceAddr :: Addr B_Crypto
aliceAddr = mkAddr (alicePay, aliceStake)

-- ==========================================================

injcoins :: Integer -> [ShelleyTxOut B]
injcoins n = fmap (\_ -> ShelleyTxOut aliceAddr (inject $ Coin 100)) [0 .. n]

-- Cretae an initial UTxO set with n-many transaction outputs
initUTxO :: Integer -> UTxOState B
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
    & ppMaxBBSizeL
    .~ 50000
    & ppDL
    .~ unsafeBoundRational 0.5
    & ppEMaxL
    .~ EpochInterval 10000
    & ppKeyDepositL
    .~ Coin 0
    & ppMaxBHSizeL
    .~ 10000
    & ppMaxTxSizeL
    .~ 1000000000
    & ppMinFeeAL
    .~ Coin 0
    & ppMinFeeBL
    .~ Coin 0
    & ppMinUTxOValueL
    .~ Coin 10
    & ppPoolDepositL
    .~ Coin 0
    & ppRhoL
    .~ unsafeBoundRational 0.0021
    & ppTauL
    .~ unsafeBoundRational 0.2

ledgerEnv :: (EraPParams era, ProtVerAtMost era 4, ProtVerAtMost era 6) => LedgerEnv era
ledgerEnv = LedgerEnv (SlotNo 0) minBound ppsBench (AccountState (Coin 0) (Coin 0))

testLEDGER ::
  LedgerState B ->
  ShelleyTx B ->
  LedgerEnv B ->
  ()
testLEDGER initSt tx env = do
  let st = runShelleyBase $ applySTS @(ShelleyLEDGER B) (TRC (env, initSt, tx))
  case st of
    Right _ -> ()
    Left e -> error $ show e

txbSpendOneUTxO :: ShelleyTxBody B
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

txSpendOneUTxO :: ShelleyTx B
txSpendOneUTxO =
  ShelleyTx
    txbSpendOneUTxO
    mempty
      { addrWits = mkWitnessesVKey (hashAnnotated txbSpendOneUTxO) [asWitness alicePay]
      }
    SNothing

ledgerSpendOneUTxO :: Integer -> ()
ledgerSpendOneUTxO n = testLEDGER (initLedgerState n) txSpendOneUTxO ledgerEnv

ledgerSpendOneGivenUTxO :: UTxOState B -> ()
ledgerSpendOneGivenUTxO state = testLEDGER (LedgerState state def) txSpendOneUTxO ledgerEnv

-- ===========================================================================
--
-- Register a stake keys when there are a lot of registered stake keys
--

-- Create stake key pairs, corresponding to seeds
-- (RawSeed start 0 0 0 0) through (RawSeed end 0 0 0 0)
stakeKeys :: Word64 -> Word64 -> [KeyPair 'Staking B_Crypto]
stakeKeys start end = fmap (\w -> mkKeyPair' (RawSeed w 0 0 0 0)) [start .. end]

stakeKeyOne :: KeyPair 'Staking B_Crypto
stakeKeyOne = mkKeyPair' (RawSeed 1 0 0 0 0)

stakeKeyToCred :: KeyPair 'Staking B_Crypto -> Credential 'Staking B_Crypto
stakeKeyToCred = KeyHashObj . hashKey . vKey

firstStakeKeyCred :: Credential 'Staking B_Crypto
firstStakeKeyCred = stakeKeyToCred stakeKeyOne

-- Create stake key registration certificates
stakeKeyRegistrations :: [KeyPair 'Staking B_Crypto] -> StrictSeq (TxCert B)
stakeKeyRegistrations keys =
  StrictSeq.fromList $
    fmap (RegTxCert . KeyHashObj . hashKey . vKey) keys

-- Create a transaction body given a sequence of certificates.
-- It spends the genesis coin given by the index ix.
txbFromCerts :: TxIx -> StrictSeq (TxCert B) -> ShelleyTxBody B
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
  ShelleyTxBody B ->
  [KeyPair 'Witness B_Crypto] ->
  ShelleyTx B
makeSimpleTx txbody keysAddr =
  ShelleyTx
    txbody
    mempty
      { addrWits = mkWitnessesVKey (hashAnnotated txbody) keysAddr
      }
    SNothing

-- Create a transaction that registers stake credentials.
txRegStakeKeys :: TxIx -> [KeyPair 'Staking B_Crypto] -> ShelleyTx B
txRegStakeKeys ix keys =
  makeSimpleTx
    (txbFromCerts ix $ stakeKeyRegistrations keys)
    [asWitness alicePay]

initLedgerState :: Integer -> LedgerState B
initLedgerState n = LedgerState (initUTxO n) def

makeLEDGERState :: HasCallStack => LedgerState B -> ShelleyTx B -> LedgerState B
makeLEDGERState start tx =
  let st = applySTS @(ShelleyLEDGER B) (TRC (ledgerEnv, start, tx))
   in case runShelleyBase st of
        Right st' -> st'
        Left e -> error $ show e

-- Create a ledger state that has registered stake credentials that
-- are seeded with (RawSeed n 0 0 0 0) to (RawSeed m 0 0 0 0).
-- It is pre-populated with 2 genesis injcoins.
ledgerStateWithNregisteredKeys :: Word64 -> Word64 -> LedgerState B
ledgerStateWithNregisteredKeys n m =
  makeLEDGERState (initLedgerState 1) $ txRegStakeKeys minBound (stakeKeys n m)

-- ===========================================================
-- Stake Key Registration example

-- Given a ledger state, presumably created by ledgerStateWithNregisteredKeys n m,
-- so that keys (RawSeed n 0 0 0 0) through (RawSeed m 0 0 0 0) are already registered,
-- register new keys (RawSeed x 0 0 0 0) through (RawSeed y 0 0 0 0).
-- Note that [n, m] must be disjoint from [x, y].
ledgerRegisterStakeKeys :: Word64 -> Word64 -> LedgerState B -> ()
ledgerRegisterStakeKeys x y state =
  testLEDGER
    state
    (txRegStakeKeys (mkTxIxPartial 1) (stakeKeys x y))
    ledgerEnv

-- ===========================================================
-- Deregistration example

-- Create a transaction body that de-registers stake credentials,
-- corresponding to the keys seeded with (RawSeed x 0 0 0 0) to (RawSeed y 0 0 0 0)
txbDeRegStakeKey :: Word64 -> Word64 -> ShelleyTxBody B
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
txDeRegStakeKeys :: Word64 -> Word64 -> ShelleyTx B
txDeRegStakeKeys x y =
  makeSimpleTx
    (txbDeRegStakeKey x y)
    (asWitness alicePay : fmap asWitness (stakeKeys x y))

-- Given a ledger state, presumably created by ledgerStateWithNregisteredKeys n m,
-- so that keys (RawSeed n 0 0 0 0) through (RawSeed m 0 0 0 0) are already registered,
-- deregister keys (RawSeed x 0 0 0 0) through (RawSeed y 0 0 0 0).
-- Note that [x, y] must be contained in [n, m].
ledgerDeRegisterStakeKeys :: Word64 -> Word64 -> LedgerState B -> ()
ledgerDeRegisterStakeKeys x y state =
  testLEDGER
    state
    (txDeRegStakeKeys x y)
    ledgerEnv

-- ===========================================================
-- Reward Withdrawal example

-- Create a transaction body that withdrawals from reward accounts,
-- corresponding to the keys seeded with (RawSeed x 0 0 0 0) to (RawSeed y 0 0 0 0).
txbWithdrawals :: Word64 -> Word64 -> ShelleyTxBody B
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
txWithdrawals :: Word64 -> Word64 -> ShelleyTx B
txWithdrawals x y =
  makeSimpleTx
    (txbWithdrawals x y)
    (asWitness alicePay : fmap asWitness (stakeKeys x y))

-- Given a ledger state, presumably created by ledgerStateWithNregisteredKeys n m,
-- so that keys (RawSeed n 0 0 0 0) through (RawSeed m 0 0 0 0) are already registered,
-- make reward withdrawals for keys (RawSeed x 0 0 0 0) through (RawSeed y 0 0 0 0).
-- Note that [x, y] must be contained in [n, m].
ledgerRewardWithdrawals :: Word64 -> Word64 -> LedgerState B -> ()
ledgerRewardWithdrawals x y state = testLEDGER state (txWithdrawals x y) ledgerEnv

-- ===========================================================================
--
-- Register a stake pool when there are a lot of registered stake pool
--

-- Create stake pool key pairs, corresponding to seeds
-- (RawSeed start 0 0 0 0) through (RawSeed end 0 0 0 0)
poolColdKeys :: Word64 -> Word64 -> [KeyPair 'StakePool B_Crypto]
poolColdKeys start end = fmap (\w -> mkKeyPair' (RawSeed w 1 0 0 0)) [start .. end]

firstStakePool :: KeyPair 'StakePool B_Crypto
firstStakePool = mkKeyPair' (RawSeed 1 1 0 0 0)

mkPoolKeyHash :: KeyPair 'StakePool B_Crypto -> KeyHash 'StakePool B_Crypto
mkPoolKeyHash = hashKey . vKey

firstStakePoolKeyHash :: KeyHash 'StakePool B_Crypto
firstStakePoolKeyHash = mkPoolKeyHash firstStakePool

vrfKeyHash :: Hash B_Crypto (VerKeyVRF B_Crypto)
vrfKeyHash = hashVerKeyVRF . vrfVerKey . mkVRFKeyPair @B_Crypto $ RawSeed 0 0 0 0 0

mkPoolParameters :: KeyPair 'StakePool B_Crypto -> PoolParams B_Crypto
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
poolRegCerts :: [KeyPair 'StakePool B_Crypto] -> StrictSeq (TxCert B)
poolRegCerts = StrictSeq.fromList . fmap (RegPoolTxCert . mkPoolParameters)

-- Create a transaction that registers stake pools.
txRegStakePools :: TxIx -> [KeyPair 'StakePool B_Crypto] -> ShelleyTx B
txRegStakePools ix keys =
  makeSimpleTx
    (txbFromCerts ix $ poolRegCerts keys)
    ([asWitness alicePay, asWitness stakeKeyOne] ++ fmap asWitness keys)

-- Create a ledger state that has n registered stake pools.
-- The keys are seeded with (RawSeed n 1 0 0 0) to (RawSeed m 1 0 0 0)
-- It is pre-populated with 2 genesis injcoins.
ledgerStateWithNregisteredPools :: Word64 -> Word64 -> LedgerState B
ledgerStateWithNregisteredPools n m =
  makeLEDGERState (initLedgerState 1) $ txRegStakePools minBound (poolColdKeys n m)

-- ===========================================================
-- Stake Pool Registration example

-- Given a ledger state, presumably created by ledgerStateWithNregisteredPools n m,
-- so that pool keys (RawSeed n 1 0 0 0) through (RawSeed m 1 0 0 0) are already registered,
-- register new pools (RawSeed x 0 0 0 0) through (RawSeed y 0 0 0 0).
-- Note that [n, m] must be disjoint from [x, y].
ledgerRegisterStakePools :: Word64 -> Word64 -> LedgerState B -> ()
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
ledgerReRegisterStakePools :: Word64 -> Word64 -> LedgerState B -> ()
ledgerReRegisterStakePools x y state =
  testLEDGER
    state
    (txRegStakePools (mkTxIxPartial 1) (poolColdKeys x y))
    ledgerEnv

-- ===========================================================
-- Stake Pool Retirement example

-- Create a transaction body that retires stake pools,
-- corresponding to the keys seeded with (RawSeed x 1 0 0 0) to (RawSeed y 1 0 0 0)
txbRetireStakePool :: Word64 -> Word64 -> ShelleyTxBody B
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
txRetireStakePool :: Word64 -> Word64 -> ShelleyTx B
txRetireStakePool x y =
  makeSimpleTx
    (txbRetireStakePool x y)
    (asWitness alicePay : fmap asWitness (poolColdKeys x y))

-- Given a ledger state, presumably created by ledgerStateWithNregisteredPools n m,
-- so that pool keys (RawSeed n 1 0 0 0) through (RawSeed m 1 0 0 0) are already registered,
-- retire pools (RawSeed x 0 0 0 0) through (RawSeed y 0 0 0 0).
-- Note that [n, m] must be contained in [x, y].
ledgerRetireStakePools :: Word64 -> Word64 -> LedgerState B -> ()
ledgerRetireStakePools x y state = testLEDGER state (txRetireStakePool x y) ledgerEnv

-- ===========================================================================
--
-- Delegate Stake Credentials when many stake keys and stake pools are registered.
--

-- Create a ledger state that has n registered stake keys and m stake pools.
-- The stake keys are seeded with (RawSeed 1 0 0 0 0) to (RawSeed n 0 0 0 0)
-- The stake pools are seeded with (RawSeed 1 1 0 0 0) to (RawSeed m 1 0 0 0)
-- It is pre-populated with 3 genesis injcoins.
ledgerStateWithNkeysMpools :: Word64 -> Word64 -> LedgerState B
ledgerStateWithNkeysMpools n m =
  makeLEDGERState
    (makeLEDGERState (initLedgerState 2) $ txRegStakeKeys minBound (stakeKeys 1 n))
    (txRegStakePools (mkTxIxPartial 1) (poolColdKeys 1 m))

-- Create a transaction body that delegates several keys to ONE stake pool,
-- corresponding to the keys seeded with (RawSeed n 0 0 0 0) to (RawSeed m 0 0 0 0)
txbDelegate :: Word64 -> Word64 -> ShelleyTxBody B
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
txDelegate :: Word64 -> Word64 -> ShelleyTx B
txDelegate n m =
  makeSimpleTx
    (txbDelegate n m)
    (asWitness alicePay : fmap asWitness (stakeKeys n m))

-- Given a ledger state, presumably created by ledgerStateWithNkeysMpools n m,
-- so that stake keys (RawSeed 1 0 0 0 0) through (RawSeed n 0 0 0 0) are already registered
-- and pool keys (RawSeed 1 1 0 0 0) through (RawSeed m 1 0 0 0) are already registered,
-- delegate stake keys (RawSeed x 0 0 0 0) through (RawSeed y 0 0 0 0) to ONE pool.
-- Note that [x, y] must be contained in [1, n].
ledgerDelegateManyKeysOnePool :: Word64 -> Word64 -> LedgerState B -> ()
ledgerDelegateManyKeysOnePool x y state = testLEDGER state (txDelegate x y) ledgerEnv
