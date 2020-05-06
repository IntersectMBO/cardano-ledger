{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Examples.Examples
  ( CHAINExample(..)
  , ex1
  , ex2A
  , ex2B
  , ex2C
  , ex2Cbis
  , ex2Cter
  , ex2Cquater
  , ex2D
  , ex2E
  , ex2F
  , ex2G
  , ex2H
  , ex2I
  , ex2J
  , ex2K
  , ex2L
  , ex3A
  , ex3B
  , ex3C
  , ex4A
  , ex4B
  , ex5A
  , ex5B
  , ex5C
  , ex5D
  , ex5E
  , ex5F'
  , test5F
  -- key pairs and example addresses
  , alicePay
  , aliceStake
  , aliceAddr
  , bobPay
  , bobStake
  , bobAddr
  , carlPay
  , carlStake
  , carlAddr
  , dariaPay
  , dariaStake
  , dariaAddr
  , coreNodeSKG -- TODO remove
  -- blocks
  , blockEx1
  , blockEx2A
  , blockEx2B
  , blockEx2C
  , blockEx2D
  , blockEx2E
  , blockEx2F
  , blockEx2G
  , blockEx2H
  , blockEx2I
  , blockEx2J
  , blockEx2K
  , blockEx2L
  , blockEx3A
  , blockEx3B
  , blockEx3C
  , blockEx4A
  , blockEx4B
  , blockEx5A
  , blockEx5B
  , blockEx5F
  , blockEx5F'
  , blockEx5F''
  -- transactions
  , txEx2A
  , txEx2B
  , txEx2D
  , txEx2J
  , txEx2K
  , txEx3A
  , txEx3B
  , txEx4A
  , txEx5A
  , txEx5B
  , txEx5F
  , txEx5F'
  , txEx5F''
  -- helpers
  , unsafeMkUnitInterval
  )
where

import           Test.Tasty.HUnit (Assertion, assertBool, assertFailure)

import qualified Data.ByteString.Char8 as BS (pack)
import           Data.Coerce (coerce)
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, empty, fromList, insert, keysSet, member, singleton,
                     (!?))
import           Data.Maybe (fromJust, isJust, maybe)
import           Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import           Data.Word (Word64)
import           Numeric.Natural (Natural)

import           Cardano.Slotting.Slot (WithOrigin (..))
import           Control.State.Transition.Extended (PredicateFailure, TRC (..), applySTS)
import           Shelley.Spec.Ledger.Address (pattern Addr, mkRwdAcnt)
import           Shelley.Spec.Ledger.BaseTypes (Nonce (..), StrictMaybe (..), mkNonce, startRewards,
                     textToUrl, (⭒))
import           Shelley.Spec.Ledger.BlockChain (pattern HashHeader, LastAppliedBlock (..), bhHash,
                     bheader, hashHeaderToNonce)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Credential (pattern KeyHashObj, Ptr (..), pattern StakeRefPtr)
import           Shelley.Spec.Ledger.Delegation.Certificates (pattern DeRegKey, pattern Delegate,
                     pattern GenesisDelegCert, pattern MIRCert, pattern PoolDistr, pattern RegKey,
                     pattern RegPool, pattern RetirePool)
import           Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..), pattern SnapShot,
                     pattern SnapShots, pattern Stake, emptySnapShots, _feeSS, _pstakeGo,
                     _pstakeMark, _pstakeSet)
import           Shelley.Spec.Ledger.Keys (Hash, KeyRole (..), asWitness, coerceKeyRole, hash,
                     hashKey, vKey)
import           Shelley.Spec.Ledger.LedgerState (AccountState (..), pattern ActiveSlot,
                     pattern DPState, pattern EpochState, FutureGenDeleg (..), pattern LedgerState,
                     pattern NewEpochState, pattern RewardUpdate, pattern UTxOState, deltaF,
                     deltaR, deltaT, emptyDState, emptyPState, esAccountState, esLState, esPp,
                     genesisCoins, genesisId, nesEs, nonMyopic, overlaySchedule, rs,
                     _delegationState, _delegations, _dstate, _fGenDelegs, _genDelegs, _irwd,
                     _pParams, _ptrs, _reserves, _retiring, _rewards, _stPools, _stkCreds,
                     _treasury)
import           Shelley.Spec.Ledger.OCert (KESPeriod (..))
import           Shelley.Spec.Ledger.PParams (PParams, PParams' (PParams), PParamsUpdate,
                     pattern ProposedPPUpdates, pattern Update, emptyPPPUpdates, emptyPParams, _a0,
                     _d, _eMax, _extraEntropy, _keyDecayRate, _keyDeposit, _keyMinRefund,
                     _maxBBSize, _maxBHSize, _maxTxSize, _minfeeA, _minfeeB, _nOpt, _poolDecayRate,
                     _poolDeposit, _poolMinRefund, _protocolVersion, _rho, _tau)
import           Shelley.Spec.Ledger.Rewards (ApparentPerformance (..), pattern NonMyopic,
                     emptyNonMyopic, rewardPot)
import           Shelley.Spec.Ledger.Slot (BlockNo (..), Duration (..), EpochNo (..), SlotNo (..),
                     (+*))
import           Shelley.Spec.Ledger.STS.Bbody (pattern LedgersFailure)
import           Shelley.Spec.Ledger.STS.Chain (pattern BbodyFailure, pattern ChainState, chainNes,
                     initialShelleyState, totalAda)
import           Shelley.Spec.Ledger.STS.Deleg (pattern InsufficientForInstantaneousRewardsDELEG)
import           Shelley.Spec.Ledger.STS.Delegs (pattern DelplFailure)
import           Shelley.Spec.Ledger.STS.Delpl (pattern DelegFailure)
import           Shelley.Spec.Ledger.STS.Ledger (pattern DelegsFailure, pattern UtxowFailure)
import           Shelley.Spec.Ledger.STS.Ledgers (pattern LedgerFailure)
import           Shelley.Spec.Ledger.STS.Utxow (pattern MIRImpossibleInDecentralizedNetUTXOW,
                     pattern MIRInsufficientGenesisSigsUTXOW)
import           Shelley.Spec.Ledger.Tx (pattern Tx)
import           Shelley.Spec.Ledger.TxData (pattern DCertDeleg, pattern DCertGenesis,
                     pattern DCertMir, pattern DCertPool, pattern Delegation,
                     PoolMetaData (..), pattern PoolParams, pattern RewardAcnt,
                     pattern StakeCreds, pattern StakePools, pattern TxBody,
                     pattern TxIn, pattern TxOut, Wdrl (..), addStakeCreds, _poolCost, _poolMD,
                     _poolMDHash, _poolMDUrl, _poolMargin, _poolOwners, _poolPledge, _poolPubKey,
                     _poolRAcnt, _poolRelays, _poolVrf)
import qualified Shelley.Spec.Ledger.TxData as TxData (TxBody (..))
import           Shelley.Spec.Ledger.UTxO (pattern UTxO, balance, hashTxBody, makeWitnessesVKey, txid)

import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Addr, Block, CHAIN, ChainState,
                     ConcreteCrypto, Credential, DState, EpochState, pattern GenDelegs, HashHeader,
                     KeyHash, KeyPair, pattern KeyPair, LedgerState, NewEpochState, OBftSlot,
                     PState, PoolDistr, PoolParams, ProposedPPUpdates, RewardAcnt, SignKeyDSIGN,
                     SnapShot, SnapShots, Tx, TxBody, UTxO, UTxOState, Update, VKeyGenesis,
                     hashKeyVRF)
import           Test.Shelley.Spec.Ledger.Generator.Core (AllPoolKeys (..), NatNonce (..),
                     genesisAccountState, mkBlock, mkOCert, zero)
import           Test.Shelley.Spec.Ledger.Utils

data CHAINExample =
  CHAINExample { currentSlotNo    :: SlotNo       -- ^ Current slot
               , startState     :: ChainState -- ^ State to start testing with
               , newBlock       :: Block      -- ^ Block to run chain state transition system on
               , intendedResult :: Either [[PredicateFailure CHAIN]] ChainState
                  -- ^ type of fatal error, if failure expected and final chain state if success expected
               }

data MIRExample =
  MIRExample
  { mirStkCred :: Credential 'Staking
  , mirRewards :: Coin
  , target     :: Either [[PredicateFailure CHAIN]] ChainState
  } deriving (Show, Eq)

mkAllPoolKeys :: Word64 -> AllPoolKeys
mkAllPoolKeys w = AllPoolKeys (KeyPair vkCold skCold)
                              (mkVRFKeyPair (w, 0, 0, 0, 2))
                              [(KESPeriod 0, mkKESKeyPair (w, 0, 0, 0, 3))]
                  -- TODO mgudemann
                              (hashKey vkCold)
  where
    (skCold, vkCold) = mkKeyPair (w, 0, 0, 0, 1)

numCoreNodes :: Word64
numCoreNodes = 7

coreNodes :: [((SignKeyDSIGN, VKeyGenesis), AllPoolKeys)]
coreNodes = [(mkGenKey (x, 0, 0, 0, 0), mkAllPoolKeys x) | x <-[101..100+numCoreNodes]]

coreNodeSKG :: Int -> SignKeyDSIGN
coreNodeSKG = fst . fst . (coreNodes !!)

coreNodeVKG :: Int -> VKeyGenesis
coreNodeVKG = snd . fst . (coreNodes !!)

coreNodeKeys :: Int -> AllPoolKeys
coreNodeKeys = snd . (coreNodes !!)

genDelegs :: Map (KeyHash 'Genesis) (KeyHash 'GenesisDelegate)
genDelegs = Map.fromList
  [ ( hashKey $ snd gkey
    , coerceKeyRole . hashKey . vKey $ cold pkeys)
  | (gkey, pkeys) <- coreNodes]

alicePay :: KeyPair 'Payment
alicePay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (0, 0, 0, 0, 0)

aliceStake :: KeyPair 'Staking
aliceStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (1, 1, 1, 1, 1)

alicePool :: AllPoolKeys
alicePool = mkAllPoolKeys 1

aliceAddr :: Addr
aliceAddr = mkAddr (alicePay, aliceStake)

aliceSHK :: Credential 'Staking
aliceSHK = (KeyHashObj . hashKey . vKey) aliceStake

bobPay :: KeyPair 'Payment
bobPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (2, 2, 2, 2, 2)

bobStake :: KeyPair 'Staking
bobStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (3, 3, 3, 3, 3)

bobAddr :: Addr
bobAddr = mkAddr (bobPay, bobStake)

bobSHK :: Credential 'Staking
bobSHK = (KeyHashObj . hashKey . vKey) bobStake

aliceInitCoin :: Coin
aliceInitCoin = 10*1000*1000*1000*1000*1000

bobInitCoin :: Coin
bobInitCoin = 1*1000*1000*1000*1000*1000

alicePoolParams :: PoolParams
alicePoolParams =
  PoolParams
    { _poolPubKey = (hashKey . vKey . cold) alicePool
    , _poolVrf = hashKeyVRF . snd $ vrf alicePool
    , _poolPledge = Coin 1
    , _poolCost = Coin 5
    , _poolMargin = unsafeMkUnitInterval 0.1
    , _poolRAcnt = RewardAcnt aliceSHK
    , _poolOwners = Set.singleton $ (hashKey . vKey) aliceStake
    , _poolRelays = StrictSeq.empty
    , _poolMD = SJust $ PoolMetaData
                  { _poolMDUrl  = fromJust $ textToUrl "alice.pool"
                  , _poolMDHash = BS.pack "{}"
                  }
    }


-- | Helper Functions

-- |The first block of the Shelley era will point back to the last block of the Byron era.
-- For our purposes in this test we can bootstrap the chain by just coercing the value.
-- When this transition actually occurs, the consensus layer will do the work of making
-- sure that the hash gets translated across the fork
lastByronHeaderHash :: HashHeader
lastByronHeaderHash = HashHeader $ coerce (hash 0 :: Hash ConcreteCrypto Int)

nonce0 :: Nonce
nonce0 = hashHeaderToNonce lastByronHeaderHash

mkSeqNonce :: Natural -> Nonce
mkSeqNonce m = foldl' (\c x -> c ⭒ mkNonce x) nonce0 [1.. m]

carlPay :: KeyPair 'Payment
carlPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (4, 4, 4, 4, 4)

carlStake :: KeyPair 'Staking
carlStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (5, 5, 5, 5, 5)

carlAddr :: Addr
carlAddr = mkAddr (carlPay, carlStake)

carlSHK :: Credential 'Staking
carlSHK = (KeyHashObj . hashKey . vKey) carlStake


dariaPay :: KeyPair 'Payment
dariaPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (6, 6, 6, 6, 6)

dariaStake :: KeyPair 'Staking
dariaStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (7, 7, 7, 7, 7)

dariaAddr :: Addr
dariaAddr = mkAddr (dariaPay, dariaStake)

dariaSHK :: Credential 'Staking
dariaSHK = (KeyHashObj . hashKey . vKey) dariaStake

-- * Example 1 - apply CHAIN transition to an empty block

-- | Empty set of UTxOs. No coins to be spent.
utxostEx1 :: UTxOState
utxostEx1 = UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyPPPUpdates

dsEx1 :: DState
dsEx1 = emptyDState { _genDelegs = GenDelegs genDelegs }

oCertIssueNosEx1 :: Map (KeyHash 'BlockIssuer) Natural
oCertIssueNosEx1 = Map.fromList (fmap f (Map.elems genDelegs))
  where f vk = (coerceKeyRole vk, 0)

psEx1 :: PState
psEx1 = emptyPState

-- | Ledger state
lsEx1 :: LedgerState
lsEx1 = LedgerState utxostEx1 (DPState dsEx1 psEx1)

ppsEx1 :: PParams
ppsEx1 = emptyPParams { _maxBBSize       =       50000
                      , _maxBHSize       =       10000
                      , _maxTxSize       =       10000
                      , _eMax            = EpochNo 10000
                      , _keyDeposit      = Coin      7
                      , _poolDeposit     = Coin    250
                      , _d               = unsafeMkUnitInterval 0.5
                      , _tau             = unsafeMkUnitInterval 0.2
                      , _rho             = unsafeMkUnitInterval 0.0021
                      , _keyDecayRate    =                      0.002
                      , _keyMinRefund    = unsafeMkUnitInterval 0.5
                      , _poolDecayRate   =                      0.001
                      , _poolMinRefund   = unsafeMkUnitInterval 0.5
                      }

-- | Never decay.
ppsExNoDecay :: PParams
ppsExNoDecay = ppsEx1 { _keyDecayRate  = 0
                      , _poolDecayRate = 0 }

-- | Refund everything.
ppsExFullRefund :: PParams
ppsExFullRefund = ppsEx1 { _keyMinRefund  = unsafeMkUnitInterval 1
                         , _poolMinRefund = unsafeMkUnitInterval 1 }

-- | Decay instantly within one cycle.
ppsExInstantDecay :: PParams
ppsExInstantDecay = ppsEx1 { _keyDecayRate  = 1000
                           , _poolDecayRate = 1000 }


-- | Account with empty treasury.
acntEx1 :: AccountState
acntEx1 = genesisAccountState

-- | Epoch state with no snapshots.
esEx1 :: EpochState
esEx1 = EpochState acntEx1 emptySnapShots lsEx1 ppsEx1 ppsEx1 emptyNonMyopic

-- | Empty initial Shelley state with fake Byron hash and no blocks at all.
--   No blocks of Shelley have been processed yet.
initStEx1 :: ChainState
initStEx1 = initialShelleyState
  (At $ LastAppliedBlock (BlockNo 0) (SlotNo 0) lastByronHeaderHash)
  (EpochNo 0)
  (UTxO Map.empty)
  maxLLSupply
  genDelegs
  (Map.singleton (SlotNo 1) (ActiveSlot . hashKey $ coreNodeVKG 0))
  ppsEx1
  (hashHeaderToNonce lastByronHeaderHash)

-- | Null initial block. Just records the Byron hash, and contains no transactions.
blockEx1 :: Block
blockEx1 = mkBlock
             lastByronHeaderHash
             (coreNodeKeys 0)
             []
             (SlotNo 1)
             (BlockNo 1)
             (mkNonce 0)
             (NatNonce 1)
             zero
             0
             0
             (mkOCert (coreNodeKeys 0) 0 (KESPeriod 0))

-- | Expected chain state after successful processing of null block.
expectedStEx1 :: ChainState
expectedStEx1 = ChainState
  (NewEpochState
     (EpochNo 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     -- Note that blocks in the overlay schedule do not add to this count.
     esEx1
     SNothing
     (PoolDistr Map.empty)
     (Map.singleton (SlotNo 1) (ActiveSlot . hashKey $ coreNodeVKG 0)))
  oCertIssueNosEx1
  nonce0
  (nonce0 ⭒ mkNonce 1)
  (nonce0 ⭒ mkNonce 1)
  NeutralNonce
  (At $ LastAppliedBlock
    (BlockNo 1)
    (SlotNo 1)
    (bhHash . bheader $ blockEx1))

-- | Wraps example all together.
ex1 :: CHAINExample
ex1 = CHAINExample (SlotNo 1) initStEx1 blockEx1 (Right expectedStEx1)


-- * Example 2A - apply CHAIN transition to register stake keys and a pool

-- | Unspent transaction output for example 2A,
--   so that users actually have coins to spend.
utxoEx2A :: UTxO
utxoEx2A = genesisCoins
       [ TxOut aliceAddr aliceInitCoin
       , TxOut bobAddr   bobInitCoin
       ]

-- | Register a single pool with 255 coins of deposit
ppupEx2A :: ProposedPPUpdates
ppupEx2A = ProposedPPUpdates $ Map.singleton
             (hashKey $ coreNodeVKG 0) -- stake key
             (PParams
                { _minfeeA = SNothing
                , _minfeeB = SNothing
                , _maxBBSize = SNothing
                , _maxTxSize = SNothing
                , _maxBHSize = SNothing
                , _keyDeposit = SJust 255
                , _keyMinRefund = SNothing
                , _keyDecayRate = SNothing
                , _poolDeposit = SNothing
                , _poolMinRefund = SNothing
                , _poolDecayRate = SNothing
                , _eMax = SNothing
                , _nOpt = SNothing
                , _a0 = SNothing
                , _rho = SNothing
                , _tau = SNothing
                , _d = SNothing
                , _extraEntropy = SNothing
                , _protocolVersion = SNothing
                })

-- | Update proposal that just changes protocol parameters,
--   and does not change applications.
updateEx2A :: Update
updateEx2A = Update ppupEx2A (EpochNo 0)


aliceCoinEx2A :: Coin
aliceCoinEx2A = aliceInitCoin - (_poolDeposit ppsEx1) - 3 * (_keyDeposit ppsEx1) - 3

-- | Transaction body to be processed.
txbodyEx2A :: TxBody
txbodyEx2A = TxBody
           (Set.fromList [TxIn genesisId 0])
           (StrictSeq.fromList [TxOut aliceAddr aliceCoinEx2A])
           (StrictSeq.fromList ([ DCertDeleg (RegKey aliceSHK)
           , DCertDeleg (RegKey bobSHK)
           , DCertDeleg (RegKey carlSHK)
           , DCertPool (RegPool alicePoolParams)
           ] ++ [DCertMir (MIRCert (Map.fromList [ (carlSHK, 110)
                                                 , (dariaSHK, 99)]))]))
           (Wdrl Map.empty)
           (Coin 3)
           (SlotNo 10)
           (SJust updateEx2A)
           SNothing

txEx2A :: Tx
txEx2A = Tx
          txbodyEx2A
          (makeWitnessesVKey
            (hashTxBody txbodyEx2A)
            ( (asWitness <$> [alicePay, carlPay])
            <> (asWitness <$> [aliceStake])
            <> (asWitness <$> [cold alicePool, cold $ coreNodeKeys 0])
            )
            -- Note that Alice's stake key needs to sign this transaction
            -- since it is an owner of the stake pool being registered,
            -- and *not* because of the stake key registration.
                     `Set.union`
           makeWitnessesVKey (hashTxBody txbodyEx2A) [ KeyPair (coreNodeVKG 0) (coreNodeSKG 0)
             , KeyPair (coreNodeVKG 1) (coreNodeSKG 1)
             , KeyPair (coreNodeVKG 2) (coreNodeSKG 2)
             , KeyPair (coreNodeVKG 3) (coreNodeSKG 3)
             , KeyPair (coreNodeVKG 4) (coreNodeSKG 4)
             ])
          Map.empty
          SNothing

-- | Pointer address to address of Alice address.
alicePtrAddr :: Addr
alicePtrAddr = Addr (KeyHashObj . hashKey $ vKey alicePay) (StakeRefPtr $ Ptr (SlotNo 10) 0 0)

utxostEx2A :: UTxOState
utxostEx2A = UTxOState utxoEx2A (Coin 0) (Coin 0) emptyPPPUpdates

lsEx2A :: LedgerState
lsEx2A = LedgerState utxostEx2A (DPState dsEx1 psEx1)

acntEx2A :: AccountState
acntEx2A = AccountState
            { _treasury = Coin 0
            , _reserves = maxLLSupply - balance utxoEx2A
            }

esEx2A :: EpochState
esEx2A = EpochState acntEx2A emptySnapShots lsEx2A ppsEx1 ppsEx1 emptyNonMyopic

overlayEx2A :: Map SlotNo OBftSlot
overlayEx2A = runShelleyBase
  $ overlaySchedule
    (EpochNo 0)
    (Map.keysSet genDelegs)
    ppsEx1

initNesEx2A :: NewEpochState
initNesEx2A = NewEpochState
               (EpochNo 0)
               (BlocksMade Map.empty)
               (BlocksMade Map.empty)
               esEx2A
               SNothing
               (PoolDistr Map.empty)
               overlayEx2A


initStEx2A :: ChainState
initStEx2A = initialShelleyState
  (At $ LastAppliedBlock (BlockNo 0) (SlotNo 0) lastByronHeaderHash)
  (EpochNo 0)
  utxoEx2A
  (maxLLSupply - balance utxoEx2A)
  genDelegs
  overlayEx2A
  ppsEx1
  (hashHeaderToNonce lastByronHeaderHash)

blockEx2A :: Block
blockEx2A = mkBlock
             lastByronHeaderHash
             (coreNodeKeys 0)
             [txEx2A]
             (SlotNo 10)
             (BlockNo 1)
             (mkNonce 0)
             (NatNonce 1)
             zero
             0
             0
             (mkOCert (coreNodeKeys 0) 0 (KESPeriod 0))

dsEx2A :: DState
dsEx2A = dsEx1
          { _ptrs = Map.fromList [ (Ptr (SlotNo 10) 0 0, aliceSHK)
                                 , (Ptr (SlotNo 10) 0 1, bobSHK)
                                 , (Ptr (SlotNo 10) 0 2, carlSHK)]
          , _stkCreds = StakeCreds $ Map.fromList [ (aliceSHK, SlotNo 10)
                                                  , (bobSHK, SlotNo 10)
                                                  , (carlSHK, SlotNo 10)]
          , _rewards = Map.fromList [ (RewardAcnt aliceSHK, Coin 0)
                                    , (RewardAcnt bobSHK, Coin 0)
                                    , (RewardAcnt carlSHK, Coin 0)]
          , _irwd = Map.fromList [ (carlSHK, 110)
                                 , (dariaSHK, 99)]
          }

psEx2A :: PState
psEx2A = psEx1
          { _stPools = StakePools $ Map.singleton (hk alicePool) (SlotNo 10)
          , _pParams = Map.singleton (hk alicePool) alicePoolParams
          }

expectedLSEx2A :: LedgerState
expectedLSEx2A = LedgerState
               (UTxOState
                 (UTxO . Map.fromList $
                   [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
                   , (TxIn (txid txbodyEx2A) 0, TxOut aliceAddr aliceCoinEx2A)
                   ])
                 (Coin 271)
                 (Coin 3)
                 ppupEx2A)
               (DPState dsEx2A psEx2A)

blockEx2AHash :: HashHeader
blockEx2AHash = bhHash (bheader blockEx2A)

-- | Expected state after update is processed and STS applied.
expectedStEx2A :: ChainState
expectedStEx2A = ChainState
  (NewEpochState
     (EpochNo   0)
     (BlocksMade Map.empty) -- Still no blocks
     (BlocksMade Map.empty) -- Still no blocks
     (EpochState acntEx2A emptySnapShots expectedLSEx2A ppsEx1 ppsEx1 emptyNonMyopic)
     SNothing
     (PoolDistr Map.empty)
     overlayEx2A)
  -- Operational certificate issue numbers are now only updated during block
  -- header processing (in the OCERT rule). As such, we will not see the
  -- operational certificate issue number appear until the first time a block is
  -- issued using the corresponding hot key.
  oCertIssueNosEx1
  nonce0
  (nonce0 ⭒ mkNonce 1)
  (nonce0 ⭒ mkNonce 1)
  NeutralNonce
  (At $ LastAppliedBlock
    (BlockNo 1)
    (SlotNo 10)
    blockEx2AHash)

ex2A :: CHAINExample
ex2A = CHAINExample (SlotNo 10) initStEx2A blockEx2A (Right expectedStEx2A)


-- * Example 2B - process a block late enough in the epoch in order to create a reward update.

aliceCoinEx2BBase :: Coin
aliceCoinEx2BBase = 5*1000*1000*1000*1000*1000

aliceCoinEx2BPtr :: Coin
aliceCoinEx2BPtr = aliceCoinEx2A - (aliceCoinEx2BBase + 4)

-- | The transaction delegates Alice's and Bob's stake to Alice's pool.
--   Additionally, we split Alice's ADA between a base address and a pointer address.
txbodyEx2B :: TxBody
txbodyEx2B = TxBody
      { TxData._inputs   = Set.fromList [TxIn (txid txbodyEx2A) 0]
      , TxData._outputs  = StrictSeq.fromList [ TxOut aliceAddr    aliceCoinEx2BBase
                                              , TxOut alicePtrAddr aliceCoinEx2BPtr ]
      --  Delegation certificates
      , TxData._certs    =
        StrictSeq.fromList [ DCertDeleg (Delegate $ Delegation aliceSHK (hk alicePool))
                           , DCertDeleg (Delegate $ Delegation bobSHK   (hk alicePool))]
      , TxData._wdrls    = Wdrl Map.empty
      , TxData._txfee    = Coin 4
      , TxData._ttl      = SlotNo 90
      , TxData._txUpdate = SNothing
      , TxData._mdHash   = SNothing
      }

txEx2B :: Tx
txEx2B = Tx
          txbodyEx2B -- Body of the transaction
          (makeWitnessesVKey (hashTxBody txbodyEx2B)
            [asWitness alicePay, asWitness aliceStake, asWitness bobStake])
                     -- Witness verification key set
          Map.empty  -- Witness signature map
          SNothing

blockEx2B :: Block
blockEx2B = mkBlock
             blockEx2AHash    -- Hash of previous block
             (coreNodeKeys 3)
             [txEx2B]         -- Single transaction to record
             (SlotNo 90)      -- Current slot
             (BlockNo 2)
             (mkNonce 0)      -- Epoch nonce
             (NatNonce 2)     -- Block nonce
             zero             -- Praos leader value
             4                -- Period of KES (key evolving signature scheme)
             0
             (mkOCert (coreNodeKeys 3) 0 (KESPeriod 0))

blockEx2BHash :: HashHeader
blockEx2BHash = bhHash (bheader blockEx2B)

utxoEx2B :: UTxO
utxoEx2B = UTxO . Map.fromList $
                   [ (TxIn genesisId 1,         TxOut bobAddr      bobInitCoin)
                   , (TxIn (txid txbodyEx2B) 0, TxOut aliceAddr    aliceCoinEx2BBase)
                   , (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr aliceCoinEx2BPtr)
                   ]

-- | Both Alice and Bob delegate to the Alice pool
delegsEx2B :: Map (Credential 'Staking) (KeyHash 'StakePool)
delegsEx2B = Map.fromList
              [ (aliceSHK, hk alicePool)
              , (bobSHK,   hk alicePool)
              ]

carlMIR :: Coin
carlMIR = Coin 110

dariaMIR :: Coin
dariaMIR = Coin 99

dsEx2B :: DState
dsEx2B = dsEx2A { _delegations = delegsEx2B
                , _irwd = Map.fromList [ (carlSHK, carlMIR)
                                       , (dariaSHK, dariaMIR)] }

expectedLSEx2B :: LedgerState
expectedLSEx2B = LedgerState
               (UTxOState
                 utxoEx2B
                 (Coin 271)
                 (Coin 7)
                 ppupEx2A)
               (DPState dsEx2B psEx2A)

expectedStEx2Bgeneric :: PParams -> ChainState
expectedStEx2Bgeneric pp = ChainState
  -- New state of the epoch
  (NewEpochState
     (EpochNo 0)            -- First epoch
     (BlocksMade Map.empty) -- Blocks made before current
     (BlocksMade Map.empty) -- Blocks made before current
     (EpochState acntEx2A emptySnapShots expectedLSEx2B pp pp emptyNonMyopic)
                            -- Previous epoch state
     (SJust RewardUpdate { deltaT        = Coin 0
                         , deltaR        = Coin 0
                         , rs            = Map.empty
                         , deltaF        = Coin 0
                         , nonMyopic     = emptyNonMyopic
                         })  -- Update reward
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  nonce0
  (nonce0 ⭒ mkNonce 1 ⭒ mkNonce 2) -- Evolving nonce
  (nonce0 ⭒ mkNonce 1)             -- Candidate nonce
  NeutralNonce
  (At $ LastAppliedBlock
    (BlockNo 2)                    -- Current block no
    (SlotNo 90)                    -- Current slot
    blockEx2BHash)                 -- Hash header of the chain

-- | Expected state after transition
expectedStEx2B :: ChainState
expectedStEx2B = expectedStEx2Bgeneric ppsEx1

-- | Expected state after transition, variant with no decay
expectedStEx2Bbis :: ChainState
expectedStEx2Bbis = expectedStEx2Bgeneric ppsExNoDecay

-- | Expected state after transition, variant with full refund
expectedStEx2Bter :: ChainState
expectedStEx2Bter = expectedStEx2Bgeneric ppsExFullRefund

-- | Expected state after transtion, variant with instant decay
expectedStEx2Bquater :: ChainState
expectedStEx2Bquater = expectedStEx2Bgeneric ppsExInstantDecay

-- | Wrap plain example
ex2B :: CHAINExample
ex2B = CHAINExample (SlotNo 90) expectedStEx2A blockEx2B (Right expectedStEx2B)

-- | Example 2C - process an empty block in the next epoch
-- so that the (empty) reward update is applied and a stake snapshot is made.


blockEx2C :: Block
blockEx2C = mkBlock
             blockEx2BHash    -- Hash of previous block
             (coreNodeKeys 0)
             []               -- No transactions at all (empty block)
             (SlotNo 110)     -- Current slot
             (BlockNo 3)      -- Second block within the epoch
             (mkNonce 0)      -- Epoch nonce
             (NatNonce 3)     -- Block nonce
             zero             -- Praos leader value
             5                -- Period of KES (key evolving signature scheme)
             0
             (mkOCert (coreNodeKeys 0) 0 (KESPeriod 0))

epoch1OSchedEx2C :: Map SlotNo OBftSlot
epoch1OSchedEx2C = runShelleyBase $ overlaySchedule
                    (EpochNo 1)
                    (Map.keysSet genDelegs)
                    ppsEx1

-- | Snapshot of stakes for Alice and Bob
snapEx2C :: SnapShot
snapEx2C = SnapShot
  (Stake ( Map.fromList [ (aliceSHK, aliceCoinEx2BBase + aliceCoinEx2BPtr)
                        , (bobSHK, bobInitCoin)]))
  delegsEx2B
  (Map.singleton (hk alicePool) alicePoolParams)

-- | Make a snapshot for a given fee.
snapsEx2Cgeneric :: Coin -> SnapShots
snapsEx2Cgeneric feeSnapShot = emptySnapShots {
    _pstakeMark = snapEx2C -- snapshot of stake pools and parameters
  , _feeSS      = feeSnapShot
  }

-- | Snapshots with given fees.
snapsEx2C :: SnapShots
snapsEx2C = snapsEx2Cgeneric 21

snapsEx2Cbis :: SnapShots
snapsEx2Cbis = snapsEx2Cgeneric 7

snapsEx2Cter :: SnapShots
snapsEx2Cter = snapsEx2Cgeneric 7

snapsEx2Cquater :: SnapShots
snapsEx2Cquater = snapsEx2Cgeneric 144

expectedLSEx2Cgeneric :: Coin -> Coin -> LedgerState
expectedLSEx2Cgeneric lsDeposits lsFees =
  LedgerState
  (UTxOState
    utxoEx2B
    lsDeposits
    lsFees
    emptyPPPUpdates) -- Note that the ppup is gone now
  (DPState
    dsEx2B { _irwd     = Map.empty
           , _stkCreds = addStakeCreds carlSHK (SlotNo 10)   $ _stkCreds dsEx2B
           , _rewards  = Map.insert (mkRwdAcnt carlSHK) 110 $ _rewards dsEx2B
           }
    psEx2A)

expectedLSEx2C :: LedgerState
expectedLSEx2C = expectedLSEx2Cgeneric 257 21

expectedLSEx2Cbis :: LedgerState
expectedLSEx2Cbis = expectedLSEx2Cgeneric 271 7

expectedLSEx2Cter :: LedgerState
expectedLSEx2Cter = expectedLSEx2Cgeneric 271 7

expectedLSEx2Cquater :: LedgerState
expectedLSEx2Cquater = expectedLSEx2Cgeneric 134 144

blockEx2CHash :: HashHeader
blockEx2CHash = bhHash (bheader blockEx2C)

expectedStEx2Cgeneric :: SnapShots -> LedgerState -> PParams -> ChainState
expectedStEx2Cgeneric ss ls pp = ChainState
  (NewEpochState
     (EpochNo 1)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A { _reserves = _reserves acntEx2A - carlMIR } ss ls pp pp emptyNonMyopic)
     SNothing
     (PoolDistr Map.empty)
     epoch1OSchedEx2C)
  oCertIssueNosEx1
  (nonce0 ⭒ mkNonce 1)
  (mkSeqNonce 3)
  (mkSeqNonce 3)
  (hashHeaderToNonce blockEx2BHash)
  (At $ LastAppliedBlock
    (BlockNo 3)
    (SlotNo 110)
    blockEx2CHash)

-- ** Expected chain state after STS
expectedStEx2C :: ChainState
expectedStEx2C = expectedStEx2Cgeneric snapsEx2C expectedLSEx2C ppsEx1

expectedStEx2Cbis :: ChainState
expectedStEx2Cbis = expectedStEx2Cgeneric snapsEx2Cbis expectedLSEx2Cbis ppsExNoDecay

expectedStEx2Cter :: ChainState
expectedStEx2Cter =
  expectedStEx2Cgeneric snapsEx2Cter expectedLSEx2Cter ppsExFullRefund

expectedStEx2Cquater :: ChainState
expectedStEx2Cquater =
  expectedStEx2Cgeneric snapsEx2Cquater expectedLSEx2Cquater ppsExInstantDecay

-- | Example 2C with standard decay.
ex2C :: CHAINExample
ex2C = CHAINExample (SlotNo 110) expectedStEx2B blockEx2C (Right expectedStEx2C)

-- | Example 2C with no decay.
ex2Cbis :: CHAINExample
ex2Cbis = CHAINExample (SlotNo 110) expectedStEx2Bbis blockEx2C (Right expectedStEx2Cbis)

-- | Example 2C with full refund.
ex2Cter :: CHAINExample
ex2Cter = CHAINExample (SlotNo 110) expectedStEx2Bter blockEx2C (Right expectedStEx2Cter)

-- | Example 2C with instant decay.
ex2Cquater :: CHAINExample
ex2Cquater =
  CHAINExample (SlotNo 110) expectedStEx2Bquater blockEx2C (Right expectedStEx2Cquater)


-- | Example 2D - process a block late enough
-- in the epoch in order to create a second reward update, preparing the way for
-- the first non-empty pool distribution in this running example.
-- Additionally, in order to have the stake distribution change,
-- Carl delegates his stake.


-- | The transaction delegates Carl's stake to Alice's pool.

aliceCoinEx2DBase :: Coin
aliceCoinEx2DBase = aliceCoinEx2BBase - 5

txbodyEx2D :: TxBody
txbodyEx2D = TxBody
      { TxData._inputs   = Set.fromList [TxIn (txid txbodyEx2B) 0]
      , TxData._outputs  = StrictSeq.fromList [ TxOut aliceAddr aliceCoinEx2DBase ]
      , TxData._certs    =
        StrictSeq.fromList [ DCertDeleg (Delegate $ Delegation carlSHK (hk alicePool)) ]
      , TxData._wdrls    = Wdrl Map.empty
      , TxData._txfee    = Coin 5
      , TxData._ttl      = SlotNo 500
      , TxData._txUpdate = SNothing
      , TxData._mdHash   = SNothing
      }

txEx2D :: Tx
txEx2D = Tx
          txbodyEx2D
          (makeWitnessesVKey (hashTxBody txbodyEx2D) [asWitness alicePay, asWitness carlStake])
          Map.empty
          SNothing

blockEx2D :: Block
blockEx2D = mkBlock
             blockEx2CHash
             (coreNodeKeys 3)
             [txEx2D]
             (SlotNo 190)
             (BlockNo 4)
             (mkNonce 0 ⭒ mkNonce 1)
             (NatNonce 4)
             zero
             9
             0
             (mkOCert (coreNodeKeys 3) 0 (KESPeriod 0))

blockEx2DHash :: HashHeader
blockEx2DHash = bhHash (bheader blockEx2D)

utxoEx2D :: UTxO
utxoEx2D = UTxO . Map.fromList $
                   [ (TxIn genesisId 1,         TxOut bobAddr      bobInitCoin)
                   , (TxIn (txid txbodyEx2D) 0, TxOut aliceAddr    aliceCoinEx2DBase)
                   , (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr aliceCoinEx2BPtr)
                   ]

delegsEx2D :: Map (Credential 'Staking) (KeyHash 'StakePool)
delegsEx2D = Map.fromList
              [ (aliceSHK, hk alicePool)
              , (bobSHK,   hk alicePool)
              , (carlSHK,   hk alicePool)
              ]

dsEx2D :: DState
dsEx2D = (dsEx2C) { _delegations = delegsEx2D }
  where
    dsEx2C = (_dstate . _delegationState) expectedLSEx2C

expectedLSEx2D :: LedgerState
expectedLSEx2D = LedgerState
               (UTxOState
                 utxoEx2D
                 (Coin 257)
                 (Coin 26)
                 emptyPPPUpdates)
               (DPState dsEx2D psEx2A)

expectedStEx2D :: ChainState
expectedStEx2D = ChainState
  (NewEpochState
     (EpochNo 1)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState
        acntEx2A { _reserves = _reserves acntEx2A - carlMIR}
        snapsEx2C
        expectedLSEx2D
        ppsEx1
        ppsEx1
        emptyNonMyopic)
     (SJust RewardUpdate { deltaT        = Coin 21
                         , deltaR        = Coin 0
                         , rs            = Map.empty
                         , deltaF        = Coin (-21)
                         , nonMyopic     = emptyNonMyopic { rewardPot = Coin 17 }
                         })
     (PoolDistr Map.empty)
     epoch1OSchedEx2C)
  oCertIssueNosEx1
  (nonce0 ⭒ mkNonce 1)
  (mkSeqNonce 4)
  (mkSeqNonce 3)
  (hashHeaderToNonce blockEx2BHash)
  (At $ LastAppliedBlock
    (BlockNo 4)
    (SlotNo 190)
    blockEx2DHash)

ex2D :: CHAINExample
ex2D = CHAINExample (SlotNo 190) expectedStEx2C blockEx2D (Right expectedStEx2D)


-- | Example 2E - create the first non-empty pool distribution
-- by creating a block in the third epoch of this running example.


blockEx2E :: Block
blockEx2E = mkBlock
             blockEx2DHash
             (coreNodeKeys 3)
             []
             (SlotNo 220)
             (BlockNo 5)
             ((mkSeqNonce 3) ⭒ (hashHeaderToNonce blockEx2BHash))
             (NatNonce 5)
             zero
             11
             10
             (mkOCert (coreNodeKeys 3) 1 (KESPeriod 10))

epoch1OSchedEx2E :: Map SlotNo OBftSlot
epoch1OSchedEx2E = runShelleyBase $ overlaySchedule
                    (EpochNo 2)
                    (Map.keysSet genDelegs)
                    ppsEx1

snapEx2E :: SnapShot
snapEx2E = SnapShot
  (Stake ( Map.fromList [ (aliceSHK, aliceCoinEx2DBase + aliceCoinEx2BPtr)
                        , (carlSHK, carlMIR)
                        , (bobSHK, bobInitCoin)]))
  delegsEx2D
  (Map.singleton (hk alicePool) alicePoolParams)

snapsEx2E :: SnapShots
snapsEx2E = emptySnapShots { _pstakeMark = snapEx2E
                           , _pstakeSet = snapEx2C
                           , _feeSS = Coin 19
                           }

expectedLSEx2E :: LedgerState
expectedLSEx2E = LedgerState
               (UTxOState
                 utxoEx2D
                 (Coin 243)
                 (Coin 19)
                 emptyPPPUpdates)
               (DPState
                dsEx2D { _irwd = Map.empty
                       , _stkCreds = addStakeCreds carlSHK (SlotNo 10) $ _stkCreds dsEx2B
                       , _rewards = Map.insert (mkRwdAcnt carlSHK) 110 $ _rewards dsEx2B
                       }
                 psEx2A)

blockEx2EHash :: HashHeader
blockEx2EHash = bhHash (bheader blockEx2E)

acntEx2E :: AccountState
acntEx2E = AccountState
            { _treasury = Coin 21
            , _reserves = maxLLSupply - balance utxoEx2A - carlMIR
            }

oCertIssueNosEx2 :: Map (KeyHash 'BlockIssuer) Natural
oCertIssueNosEx2 =
  Map.insert
    (coerceKeyRole . hashKey $ vKey $ cold $ coreNodeKeys 3)
    1
    oCertIssueNosEx1

expectedStEx2E :: ChainState
expectedStEx2E = ChainState
  (NewEpochState
     (EpochNo 2)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2E snapsEx2E expectedLSEx2E ppsEx1 ppsEx1 emptyNonMyopic)
     SNothing
     (PoolDistr
       (Map.singleton
          (hk alicePool)
          (1, hashKeyVRF (snd $ vrf alicePool))))
     epoch1OSchedEx2E)
  oCertIssueNosEx2
  ((mkSeqNonce 3) ⭒ (hashHeaderToNonce blockEx2BHash))
  (mkSeqNonce 5)
  (mkSeqNonce 5)
  (hashHeaderToNonce blockEx2DHash)
  (At $ LastAppliedBlock
    (BlockNo 5)
    (SlotNo 220)
    blockEx2EHash)

ex2E :: CHAINExample
ex2E = CHAINExample (SlotNo 220) expectedStEx2D blockEx2E (Right expectedStEx2E)


-- | Example 2F - create a decentralized Praos block (ie one not in the overlay schedule)

oCertIssueNosEx2F :: Map (KeyHash 'BlockIssuer) Natural
oCertIssueNosEx2F = Map.insert (coerceKeyRole $ hk alicePool) 0 oCertIssueNosEx2

blockEx2F :: Block
blockEx2F = mkBlock
             blockEx2EHash
             alicePool
             []
             (SlotNo 295) -- odd slots open for decentralization in epoch1OSchedEx2E
             (BlockNo 6)
             ((mkSeqNonce 3) ⭒ (hashHeaderToNonce blockEx2BHash))
             (NatNonce 6)
             zero
             14
             14
             (mkOCert alicePool 0 (KESPeriod 14))

blockEx2FHash :: HashHeader
blockEx2FHash = bhHash (bheader blockEx2F)

pdEx2F :: PoolDistr
pdEx2F = PoolDistr $ Map.singleton (hk alicePool) (1, hashKeyVRF $ snd $ vrf alicePool)

expectedStEx2F :: ChainState
expectedStEx2F = ChainState
  (NewEpochState
     (EpochNo 2)
     (BlocksMade Map.empty)
     (BlocksMade $ Map.singleton (hk alicePool) 1)
     (EpochState acntEx2E snapsEx2E expectedLSEx2E ppsEx1 ppsEx1 emptyNonMyopic)
     (SJust RewardUpdate { deltaT        = Coin 19
                         , deltaR        = Coin 0
                         , rs            = Map.empty
                         , deltaF        = Coin (-19)
                         , nonMyopic     = emptyNonMyopic { rewardPot = Coin 16 }
                         })
     pdEx2F
     epoch1OSchedEx2E)
  oCertIssueNosEx2F
  ((mkSeqNonce 3) ⭒ (hashHeaderToNonce blockEx2BHash))
  (mkSeqNonce 6)
  (mkSeqNonce 5)
  (hashHeaderToNonce blockEx2DHash)
  (At $ LastAppliedBlock
    (BlockNo 6)
    (SlotNo 295)
    blockEx2FHash)

ex2F :: CHAINExample
ex2F = CHAINExample (SlotNo 295) expectedStEx2E blockEx2F (Right expectedStEx2F)


-- | Example 2G - create an empty block in the next epoch
-- to prepare the way for the first non-trivial reward update


blockEx2G :: Block
blockEx2G = mkBlock
             blockEx2FHash
             (coreNodeKeys 0)
             []
             (SlotNo 310)
             (BlockNo 7)
             (mkSeqNonce 5)
             (NatNonce 7)
             zero
             15
             15
             (mkOCert (coreNodeKeys 0) 1 (KESPeriod 15))

blockEx2GHash :: HashHeader
blockEx2GHash = bhHash (bheader blockEx2G)

epoch1OSchedEx2G :: Map SlotNo OBftSlot
epoch1OSchedEx2G = runShelleyBase $ overlaySchedule
                    (EpochNo 3)
                    (Map.keysSet genDelegs)
                    ppsEx1

snapsEx2G :: SnapShots
snapsEx2G = snapsEx2E { _pstakeMark = snapEx2E
                      , _pstakeSet = snapEx2E
                      , _pstakeGo = snapEx2C
                      , _feeSS = 10}

expectedLSEx2G :: LedgerState
expectedLSEx2G = LedgerState
               (UTxOState
                 utxoEx2D
                 (Coin 233)
                 (Coin 10)
                 emptyPPPUpdates)
               (DPState
                 dsEx2D
                 psEx2A)

oCertIssueNosEx2G :: Map (KeyHash 'BlockIssuer) Natural
oCertIssueNosEx2G =
  Map.insert
    (coerceKeyRole . hashKey $ vKey $ cold $ coreNodeKeys 0)
    1
    oCertIssueNosEx2F

acntEx2G :: AccountState
acntEx2G = acntEx2E { _treasury = Coin 40 }

expectedStEx2G :: ChainState
expectedStEx2G = ChainState
  (NewEpochState
     (EpochNo 3)
     (BlocksMade $ Map.singleton (hk alicePool) 1)
     (BlocksMade Map.empty)
     (EpochState acntEx2G snapsEx2G expectedLSEx2G ppsEx1 ppsEx1 emptyNonMyopic)
     SNothing
     pdEx2F
     epoch1OSchedEx2G)
  oCertIssueNosEx2G
  ((mkSeqNonce 5) ⭒ (hashHeaderToNonce blockEx2DHash))
  (mkSeqNonce 7)
  (mkSeqNonce 7)
  (hashHeaderToNonce blockEx2FHash)
  (At $ LastAppliedBlock
    (BlockNo 7)
    (SlotNo 310)
    blockEx2GHash)

ex2G :: CHAINExample
ex2G = CHAINExample (SlotNo 310) expectedStEx2F blockEx2G (Right expectedStEx2G)


-- | Example 2H - create the first non-trivial reward update


blockEx2H :: Block
blockEx2H = mkBlock
             blockEx2GHash
             (coreNodeKeys 3)
             []
             (SlotNo 390)
             (BlockNo 8)
             (mkSeqNonce 5)
             (NatNonce 8)
             zero
             19
             19
             (mkOCert (coreNodeKeys 3) 2 (KESPeriod 19))

blockEx2HHash :: HashHeader
blockEx2HHash = bhHash (bheader blockEx2H)

aliceRAcnt2H :: Coin
aliceRAcnt2H = Coin 23839338840

bobRAcnt2H :: Coin
bobRAcnt2H = Coin 2124297519

rewardsEx2H :: Map RewardAcnt Coin
rewardsEx2H = Map.fromList [ (RewardAcnt aliceSHK, aliceRAcnt2H)
                          , (RewardAcnt bobSHK, bobRAcnt2H) ]

oCertIssueNosEx2H :: Map (KeyHash 'BlockIssuer) Natural
oCertIssueNosEx2H =
  Map.insert
    (coerceKeyRole . hashKey $ vKey $ cold $ coreNodeKeys 3)
    2
    oCertIssueNosEx2G

alicePerfEx2H :: ApparentPerformance
alicePerfEx2H = ApparentPerformance (beta / sigma)
  where
    beta = 1 -- Alice produced the only decentralized block this epoch
    sigma = fromRational (fromIntegral stake % fromIntegral maxLLSupply)
    stake = aliceCoinEx2BBase + aliceCoinEx2BPtr + bobInitCoin

expectedStEx2H :: ChainState
expectedStEx2H = ChainState
  (NewEpochState
     (EpochNo 3)
     (BlocksMade $ Map.singleton (hk alicePool) 1)
     (BlocksMade Map.empty)
     (EpochState acntEx2G snapsEx2G expectedLSEx2G ppsEx1 ppsEx1 emptyNonMyopic)
     (SJust RewardUpdate { deltaT        = Coin 767369696984
                         , deltaR        = Coin (-793333333333)
                         , rs            = rewardsEx2H
                         , deltaF        = Coin (-10)
                         , nonMyopic     = NonMyopic
                             (Map.singleton (hk alicePool) alicePerfEx2H)
                             (Coin 634666666675)
                             snapEx2C
                         })
     pdEx2F
     epoch1OSchedEx2G)
  oCertIssueNosEx2H
  ((mkSeqNonce 5) ⭒ (hashHeaderToNonce blockEx2DHash))
  (mkSeqNonce 8)
  (mkSeqNonce 7)
  (hashHeaderToNonce blockEx2FHash)
  (At $ LastAppliedBlock
    (BlockNo 8)
    (SlotNo 390)
    blockEx2HHash)

ex2H :: CHAINExample
ex2H = CHAINExample (SlotNo 390) expectedStEx2G blockEx2H (Right expectedStEx2H)


-- | Example 2I - apply the first non-trivial reward update


blockEx2I :: Block
blockEx2I = mkBlock
              blockEx2HHash
              (coreNodeKeys 0)
              []
              (SlotNo 410)
              (BlockNo 9)
              (mkSeqNonce 7)
              (NatNonce 9)
              zero
              20
              20
              (mkOCert (coreNodeKeys 0) 2 (KESPeriod 20))

blockEx2IHash :: HashHeader
blockEx2IHash = bhHash (bheader blockEx2I)

epoch1OSchedEx2I :: Map SlotNo OBftSlot
epoch1OSchedEx2I = runShelleyBase $ overlaySchedule
                     (EpochNo 4)
                     (Map.keysSet genDelegs)
                     ppsEx1

acntEx2I :: AccountState
acntEx2I = AccountState
            { _treasury = Coin 767369697024
            , _reserves = Coin 33999206666666557
            }

dsEx2I :: DState
dsEx2I = dsEx2D { _rewards = Map.insert (mkRwdAcnt carlSHK) 110 rewardsEx2H }

expectedLSEx2I :: LedgerState
expectedLSEx2I = LedgerState
               (UTxOState
                 utxoEx2D
                 (Coin 224)
                 (Coin 9)
                 emptyPPPUpdates)
               (DPState dsEx2I psEx2A)

snapsEx2I :: SnapShots
snapsEx2I = snapsEx2G { _pstakeMark = SnapShot
                          (Stake ( Map.fromList
                            [ (bobSHK, bobInitCoin + bobRAcnt2H)
                            , (aliceSHK, aliceCoinEx2DBase + aliceCoinEx2BPtr + aliceRAcnt2H)
                            , (carlSHK, carlMIR) ]))
                          delegsEx2D
                          (Map.singleton (hk alicePool) alicePoolParams)
                        -- The stake snapshots have bigger values now, due to the new rewards
                      , _pstakeSet = snapEx2E
                      , _pstakeGo = snapEx2E
                      , _feeSS = Coin 9
                      }

oCertIssueNosEx2I :: Map (KeyHash 'BlockIssuer) Natural
oCertIssueNosEx2I =
  Map.insert
    (coerceKeyRole . hashKey $ vKey $ cold $ coreNodeKeys 0)
    2
    oCertIssueNosEx2H

expectedStEx2I :: ChainState
expectedStEx2I = ChainState
  (NewEpochState
     (EpochNo 4)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2I snapsEx2I expectedLSEx2I ppsEx1 ppsEx1 emptyNonMyopic)
     SNothing
     pdEx2F
     epoch1OSchedEx2I)
  oCertIssueNosEx2I
  ((mkSeqNonce 7) ⭒ (hashHeaderToNonce blockEx2FHash))
  (mkSeqNonce 9)
  (mkSeqNonce 9)
  (hashHeaderToNonce blockEx2HHash)
  (At $ LastAppliedBlock
    (BlockNo 9)
    (SlotNo 410)
    blockEx2IHash)

ex2I :: CHAINExample
ex2I = CHAINExample (SlotNo 410) expectedStEx2H blockEx2I (Right expectedStEx2I)


-- | Example 2J - drain reward account and de-register stake key

bobAda2J :: Coin
bobAda2J = bobRAcnt2H -- reward account
                   + bobInitCoin -- txin we will consume (must spend at least one)
                   + Coin 4 -- stake registration refund
                   - Coin 9 -- tx fee

txbodyEx2J :: TxBody
txbodyEx2J = TxBody
           (Set.fromList [TxIn genesisId 1])
           (StrictSeq.singleton $ TxOut bobAddr bobAda2J)
           (StrictSeq.fromList [DCertDeleg (DeRegKey bobSHK)])
           (Wdrl $ Map.singleton (RewardAcnt bobSHK) bobRAcnt2H)
           (Coin 9)
           (SlotNo 500)
           SNothing
           SNothing

txEx2J :: Tx
txEx2J = Tx
          txbodyEx2J
          (makeWitnessesVKey (hashTxBody txbodyEx2J) [asWitness bobPay, asWitness bobStake])
          Map.empty
          SNothing

blockEx2J :: Block
blockEx2J = mkBlock
              blockEx2IHash
              (coreNodeKeys 3)
              [txEx2J]
              (SlotNo 420)
              (BlockNo 10)
              (mkSeqNonce 7)
              (NatNonce 10)
              zero
              21
              19
              (mkOCert (coreNodeKeys 3) 2 (KESPeriod 19))

blockEx2JHash :: HashHeader
blockEx2JHash = bhHash (bheader blockEx2J)

utxoEx2J :: UTxO
utxoEx2J = UTxO . Map.fromList $
                   [ (TxIn (txid txbodyEx2J) 0, TxOut bobAddr bobAda2J)
                   , (TxIn (txid txbodyEx2D) 0, TxOut aliceAddr aliceCoinEx2DBase)
                   , (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr aliceCoinEx2BPtr)
                   ]

dsEx2J :: DState
dsEx2J = dsEx1
          { _ptrs = Map.fromList [ (Ptr (SlotNo 10) 0 0, aliceSHK)
                                 , (Ptr (SlotNo 10) 0 2, carlSHK)]
          , _stkCreds = StakeCreds $ Map.fromList [(aliceSHK, SlotNo 10), (carlSHK, SlotNo 10)]
          , _delegations = Map.fromList [(aliceSHK, hk alicePool), (carlSHK, hk alicePool)]
          , _rewards = Map.fromList [(RewardAcnt aliceSHK, aliceRAcnt2H), (RewardAcnt carlSHK, carlMIR)]
          }

expectedLSEx2J :: LedgerState
expectedLSEx2J = LedgerState
               (UTxOState
                 utxoEx2J
                 (Coin (219 - 4) + 5)
                 (Coin 18)
                 emptyPPPUpdates)
               (DPState dsEx2J psEx2A)

oCertIssueNosEx2J :: Map (KeyHash 'BlockIssuer) Natural
oCertIssueNosEx2J =
  Map.insert
    (coerceKeyRole . hashKey $ vKey $ cold $ coreNodeKeys 0)
    2
    oCertIssueNosEx2H

expectedStEx2J :: ChainState
expectedStEx2J = ChainState
  (NewEpochState
     (EpochNo 4)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2I snapsEx2I expectedLSEx2J ppsEx1 ppsEx1 emptyNonMyopic)
     SNothing
     pdEx2F
     epoch1OSchedEx2I)
  oCertIssueNosEx2J
  ((mkSeqNonce 7) ⭒ (hashHeaderToNonce blockEx2FHash))
  (mkSeqNonce 10)
  (mkSeqNonce 10)
  (hashHeaderToNonce blockEx2HHash)
  (At $ LastAppliedBlock
    (BlockNo 10)
    (SlotNo 420)
    blockEx2JHash)

ex2J :: CHAINExample
ex2J = CHAINExample (SlotNo 420) expectedStEx2I blockEx2J (Right expectedStEx2J)


-- | Example 2K - start stake pool retirement

aliceCoinEx2KPtr :: Coin
aliceCoinEx2KPtr = aliceCoinEx2DBase - 2

txbodyEx2K :: TxBody
txbodyEx2K = TxBody
           (Set.fromList [TxIn (txid txbodyEx2D) 0])
           (StrictSeq.singleton $ TxOut alicePtrAddr aliceCoinEx2KPtr)
           (StrictSeq.fromList [DCertPool (RetirePool (hk alicePool) (EpochNo 5))])
           (Wdrl Map.empty)
           (Coin 2)
           (SlotNo 500)
           SNothing
           SNothing

txEx2K :: Tx
txEx2K = Tx
          txbodyEx2K
          (makeWitnessesVKey (hashTxBody txbodyEx2K) [asWitness $ cold alicePool, asWitness alicePay])
          Map.empty
          SNothing

blockEx2K :: Block
blockEx2K = mkBlock
              blockEx2JHash
              (coreNodeKeys 3)
              [txEx2K]
              (SlotNo 490)
              (BlockNo 11)
              (mkSeqNonce 7)
              (NatNonce 11)
              zero
              24
              19
              (mkOCert (coreNodeKeys 3) 2 (KESPeriod 19))

blockEx2KHash :: HashHeader
blockEx2KHash = bhHash (bheader blockEx2K)

utxoEx2K :: UTxO
utxoEx2K = UTxO . Map.fromList $
                   [ (TxIn (txid txbodyEx2J) 0, TxOut bobAddr bobAda2J)
                   , (TxIn (txid txbodyEx2K) 0, TxOut alicePtrAddr aliceCoinEx2KPtr)
                   , (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr aliceCoinEx2BPtr)
                   ]

psEx2K :: PState
psEx2K = psEx2A { _retiring = Map.singleton (hk alicePool) (EpochNo 5) }

expectedLSEx2K :: LedgerState
expectedLSEx2K = LedgerState
               (UTxOState
                 utxoEx2K
                 (Coin 220)
                 (Coin 20)
                 emptyPPPUpdates)
               (DPState dsEx2J psEx2K)

expectedStEx2K :: ChainState
expectedStEx2K = ChainState
  (NewEpochState
     (EpochNo 4)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2I snapsEx2I expectedLSEx2K ppsEx1 ppsEx1 emptyNonMyopic)
     (SJust RewardUpdate { deltaT        = Coin 9
                         , deltaR        = Coin 0
                         , rs            = Map.empty
                         , deltaF        = Coin (-9)
                         , nonMyopic     = NonMyopic
                             (Map.singleton (hk alicePool) (ApparentPerformance 0))
                             (Coin 8)
                             snapEx2E
                         })
     pdEx2F
     epoch1OSchedEx2I)
  oCertIssueNosEx2J
  ((mkSeqNonce 7) ⭒ (hashHeaderToNonce blockEx2FHash))
  (mkSeqNonce 11)
  (mkSeqNonce 10)
  (hashHeaderToNonce blockEx2HHash)
  (At $ LastAppliedBlock
    (BlockNo 11)
    (SlotNo 490)
    blockEx2KHash)

ex2K :: CHAINExample
ex2K = CHAINExample (SlotNo 490) expectedStEx2J blockEx2K (Right expectedStEx2K)


-- | Example 2L - reap a stake pool


blockEx2L :: Block
blockEx2L = mkBlock
              blockEx2KHash
              (coreNodeKeys 0)
              []
              (SlotNo 510)
              (BlockNo 12)
              (mkSeqNonce 10)
              (NatNonce 12)
              zero
              25
              25
              (mkOCert (coreNodeKeys 0) 3 (KESPeriod 25))

blockEx2LHash :: HashHeader
blockEx2LHash = bhHash (bheader blockEx2L)

acntEx2L :: AccountState
acntEx2L = acntEx2I { _treasury =  _treasury acntEx2I --previous amount
                                  + Coin 9 } -- from the reward update

snapsEx2L :: SnapShots
snapsEx2L = SnapShots { _pstakeMark = SnapShot
                          (Stake (
                            Map.fromList [ (aliceSHK, aliceRAcnt2H + aliceCoinEx2BPtr + aliceCoinEx2KPtr)
                                         , (carlSHK, carlMIR)]))
                          (Map.fromList [ (aliceSHK, hk alicePool), (carlSHK, hk alicePool) ])
                          (Map.singleton (hk alicePool) alicePoolParams)
                      , _pstakeSet = _pstakeMark snapsEx2I
                      , _pstakeGo = _pstakeSet snapsEx2I
                      , _feeSS = Coin 22
                      }
dsEx2L :: DState
dsEx2L = dsEx1
          { _ptrs = Map.fromList [ (Ptr (SlotNo 10) 0 0, aliceSHK)
                                 , (Ptr (SlotNo 10) 0 2, carlSHK)
                                 ]
          , _stkCreds = StakeCreds $ Map.fromList [(aliceSHK, SlotNo 10), (carlSHK, SlotNo 10)]
          , _rewards = Map.fromList [ (RewardAcnt aliceSHK, aliceRAcnt2H + Coin 201)
                                    , (RewardAcnt carlSHK, carlMIR) ]
                       -- Note the pool cert refund of 201
          }

expectedLSEx2L :: LedgerState
expectedLSEx2L = LedgerState
               (UTxOState
                 utxoEx2K
                 (Coin 4 + 4)
                 (Coin 22)
                 emptyPPPUpdates)
               (DPState dsEx2L psEx1) -- Note the stake pool is reaped


oCertIssueNosEx2L :: Map (KeyHash 'BlockIssuer) Natural
oCertIssueNosEx2L =
  Map.insert (coerceKeyRole . hashKey $ vKey $ cold $ coreNodeKeys 0) 3 oCertIssueNosEx2J

expectedStEx2L :: ChainState
expectedStEx2L = ChainState
  (NewEpochState
     (EpochNo 5)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2L snapsEx2L expectedLSEx2L ppsEx1 ppsEx1 emptyNonMyopic)
     SNothing
     pdEx2F
     (runShelleyBase $ overlaySchedule (EpochNo 5) (Map.keysSet genDelegs) ppsEx1))
  oCertIssueNosEx2L
  ((mkSeqNonce 10) ⭒ (hashHeaderToNonce blockEx2HHash))
  (mkSeqNonce 12)
  (mkSeqNonce 12)
  (hashHeaderToNonce blockEx2KHash)
  (At $ LastAppliedBlock
    (BlockNo 12)
    (SlotNo 510)
    blockEx2LHash)

ex2L :: CHAINExample
ex2L = CHAINExample (SlotNo 510) expectedStEx2K blockEx2L (Right expectedStEx2L)


-- | Example 3A - Setting up for a successful protocol parameter update,
-- have three genesis keys vote on the same new parameters


ppVote3A :: PParamsUpdate
ppVote3A = PParams
             { _minfeeA = SNothing
             , _minfeeB = SNothing
             , _maxBBSize = SNothing
             , _maxTxSize = SNothing
             , _maxBHSize = SNothing
             , _keyDeposit = SNothing
             , _keyMinRefund = SNothing
             , _keyDecayRate = SNothing
             , _poolDeposit = SJust 200
             , _poolMinRefund = SNothing
             , _poolDecayRate = SNothing
             , _eMax = SNothing
             , _nOpt = SNothing
             , _a0 = SNothing
             , _rho = SNothing
             , _tau = SNothing
             , _d = SNothing
             , _extraEntropy = SJust (mkNonce 123)
             , _protocolVersion = SNothing
             }

ppupEx3A :: ProposedPPUpdates
ppupEx3A = ProposedPPUpdates $ Map.fromList
             [ (hashKey $ coreNodeVKG 0, ppVote3A)
             , (hashKey $ coreNodeVKG 3, ppVote3A)
             , (hashKey $ coreNodeVKG 4, ppVote3A)
                                   ]

updateEx3A :: Update
updateEx3A = Update ppupEx3A (EpochNo 0)

aliceCoinEx3A :: Coin
aliceCoinEx3A = aliceInitCoin - 1

txbodyEx3A :: TxBody
txbodyEx3A = TxBody
           (Set.fromList [TxIn genesisId 0])
           (StrictSeq.singleton $ TxOut aliceAddr aliceCoinEx3A)
           StrictSeq.empty
           (Wdrl Map.empty)
           (Coin 1)
           (SlotNo 10)
           (SJust updateEx3A)
           SNothing

txEx3A :: Tx
txEx3A = Tx
          txbodyEx3A
          (makeWitnessesVKey
            (hashTxBody txbodyEx3A)
            [ asWitness alicePay
            , asWitness . cold $ coreNodeKeys 0
            , asWitness . cold $ coreNodeKeys 3
            , asWitness . cold $ coreNodeKeys 4
            ])
          Map.empty
          SNothing

blockEx3A :: Block
blockEx3A = mkBlock
             lastByronHeaderHash
             (coreNodeKeys 0)
             [txEx3A]
             (SlotNo 10)
             (BlockNo 1)
             (mkNonce 0)
             (NatNonce 1)
             zero
             0
             0
             (mkOCert (coreNodeKeys 0) 0 (KESPeriod 0))

expectedLSEx3A :: LedgerState
expectedLSEx3A = LedgerState
               (UTxOState
                 (UTxO . Map.fromList $
                   [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
                   , (TxIn (txid txbodyEx3A) 0, TxOut aliceAddr aliceCoinEx3A)
                   ])
                 (Coin 0)
                 (Coin 1)
                 ppupEx3A)
               (DPState dsEx1 psEx1)

blockEx3AHash :: HashHeader
blockEx3AHash = bhHash (bheader blockEx3A)

expectedStEx3A :: ChainState
expectedStEx3A = ChainState
  (NewEpochState
     (EpochNo 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx3A ppsEx1 ppsEx1 emptyNonMyopic)
     SNothing
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  nonce0
  (nonce0 ⭒ mkNonce 1)
  (nonce0 ⭒ mkNonce 1)
  NeutralNonce
  (At $ LastAppliedBlock
    (BlockNo 1)
    (SlotNo 10)
    blockEx3AHash)

ex3A :: CHAINExample
ex3A = CHAINExample (SlotNo 10) initStEx2A blockEx3A (Right expectedStEx3A)


-- | Example 3B - Finish getting enough votes for the protocol parameter update.


ppupEx3B :: ProposedPPUpdates
ppupEx3B = ProposedPPUpdates $ Map.fromList
             [ (hashKey $ coreNodeVKG 1, ppVote3A)
             , (hashKey $ coreNodeVKG 5, ppVote3A)
             ]

updateEx3B :: Update
updateEx3B = Update ppupEx3B (EpochNo 0)

aliceCoinEx3B :: Coin
aliceCoinEx3B = aliceCoinEx3A - 1

txbodyEx3B :: TxBody
txbodyEx3B = TxBody
           (Set.fromList [TxIn (txid txbodyEx3A) 0])
           (StrictSeq.singleton $ TxOut aliceAddr aliceCoinEx3B)
           StrictSeq.empty
           (Wdrl Map.empty)
           (Coin 1)
           (SlotNo 31)
           (SJust updateEx3B)
           SNothing

txEx3B :: Tx
txEx3B = Tx
          txbodyEx3B
          (makeWitnessesVKey
            (hashTxBody txbodyEx3B)
            [ asWitness alicePay
            , asWitness . cold $ coreNodeKeys 1
            , asWitness . cold $ coreNodeKeys 5
            ])
          Map.empty
          SNothing

blockEx3B :: Block
blockEx3B = mkBlock
             blockEx3AHash
             (coreNodeKeys 3)
             [txEx3B]
             (SlotNo 20)
             (BlockNo 2)
             (mkNonce 0)
             (NatNonce 2)
             zero
             1
             0
             (mkOCert (coreNodeKeys 3) 0 (KESPeriod 0))

utxoEx3B :: UTxO
utxoEx3B = UTxO . Map.fromList $
             [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
             , (TxIn (txid txbodyEx3B) 0, TxOut aliceAddr aliceCoinEx3B)
             ]

ppupEx3B' :: ProposedPPUpdates
ppupEx3B' = ProposedPPUpdates $ Map.fromList $
  fmap (\n -> (hashKey $ coreNodeVKG n, ppVote3A)) [0, 1, 3, 4, 5]

expectedLSEx3B :: LedgerState
expectedLSEx3B = LedgerState
               (UTxOState
                 utxoEx3B
                 (Coin 0)
                 (Coin 2)
                 ppupEx3B')
               (DPState dsEx1 psEx1)

blockEx3BHash :: HashHeader
blockEx3BHash = bhHash (bheader blockEx3B)

expectedStEx3B :: ChainState
expectedStEx3B = ChainState
  (NewEpochState
     (EpochNo 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx3B ppsEx1 ppsEx1 emptyNonMyopic)
     SNothing
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  nonce0
  (mkSeqNonce 2)
  (mkSeqNonce 2)
  NeutralNonce
  (At $ LastAppliedBlock
    (BlockNo 2)
    (SlotNo 20)
    blockEx3BHash)

ex3B :: CHAINExample
ex3B = CHAINExample (SlotNo 20) expectedStEx3A blockEx3B (Right expectedStEx3B)


-- | Example 3C - Adopt protocol parameter update


blockEx3C :: Block
blockEx3C = mkBlock
             blockEx3BHash
             (coreNodeKeys 0)
             []
             (SlotNo 110)
             (BlockNo 3)
             (mkSeqNonce 2)
             (NatNonce 3)
             zero
             5
             0
             (mkOCert (coreNodeKeys 0) 0 (KESPeriod 0))

blockEx3CHash :: HashHeader
blockEx3CHash = bhHash (bheader blockEx3C)

overlayEx3C :: Map SlotNo OBftSlot
overlayEx3C = runShelleyBase $ overlaySchedule
                    (EpochNo 1)
                    (Map.keysSet genDelegs)
                    ppsEx1

snapsEx3C :: SnapShots
snapsEx3C = emptySnapShots { _feeSS = Coin 2 }

expectedLSEx3C :: LedgerState
expectedLSEx3C = LedgerState
               (UTxOState
                 utxoEx3B
                 (Coin 0)
                 (Coin 2)
                 emptyPPPUpdates)
               (DPState dsEx1 psEx1)

ppsEx3C :: PParams
ppsEx3C = ppsEx1 { _poolDeposit = Coin 200, _extraEntropy = mkNonce 123 }

expectedStEx3C :: ChainState
expectedStEx3C = ChainState
  (NewEpochState
     (EpochNo 1)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A snapsEx3C expectedLSEx3C ppsEx1 ppsEx3C emptyNonMyopic)
     SNothing
     (PoolDistr Map.empty)
     overlayEx3C)
  oCertIssueNosEx1
  (mkSeqNonce 2 ⭒ mkNonce 123)
  (mkSeqNonce 3)
  (mkSeqNonce 3)
  (hashHeaderToNonce blockEx3BHash)
  (At $ LastAppliedBlock
    (BlockNo 3)
    (SlotNo 110)
    blockEx3CHash)

ex3C :: CHAINExample
ex3C = CHAINExample (SlotNo 110) expectedStEx3B blockEx3C (Right expectedStEx3C)


-- | Example 4A - Genesis key delegation


newGenDelegate :: KeyPair 'GenesisDelegate
newGenDelegate  = KeyPair vkCold skCold
  where (skCold, vkCold) = mkKeyPair (108, 0, 0, 0, 1)

aliceCoinEx4A :: Coin
aliceCoinEx4A = aliceInitCoin - 1

txbodyEx4A :: TxBody
txbodyEx4A = TxBody
              (Set.fromList [TxIn genesisId 0])
              (StrictSeq.singleton $ TxOut aliceAddr aliceCoinEx4A)
              (StrictSeq.fromList [DCertGenesis (GenesisDelegCert
                                       (hashKey (coreNodeVKG 0))
                                       (hashKey (vKey newGenDelegate)))])
              (Wdrl Map.empty)
              (Coin 1)
              (SlotNo 10)
              SNothing
              SNothing

txEx4A :: Tx
txEx4A = Tx
           txbodyEx4A
           (makeWitnessesVKey (hashTxBody txbodyEx4A) [ alicePay ]
             `Set.union`
              makeWitnessesVKey (hashTxBody txbodyEx4A)
                [ KeyPair (coreNodeVKG 0) (coreNodeSKG 0) ])
           Map.empty
           SNothing

blockEx4A :: Block
blockEx4A = mkBlock
              lastByronHeaderHash
              (coreNodeKeys 0)
              [txEx4A]
              (SlotNo 10)
              (BlockNo 1)
              (mkNonce 0)
              (NatNonce 1)
              zero
              0
              0
              (mkOCert (coreNodeKeys 0) 0 (KESPeriod 0))

blockEx4AHash :: HashHeader
blockEx4AHash = bhHash (bheader blockEx4A)

dsEx4A :: DState
dsEx4A = dsEx1 { _fGenDelegs = Map.singleton
                          ( FutureGenDeleg (SlotNo 43) (hashKey $ coreNodeVKG 0) )
                          ( (hashKey . vKey) newGenDelegate ) }

utxoEx4A :: UTxO
utxoEx4A = UTxO . Map.fromList $
                    [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
                    , (TxIn (txid txbodyEx4A) 0, TxOut aliceAddr aliceCoinEx4A)
                    ]

expectedLSEx4A :: LedgerState
expectedLSEx4A = LedgerState
               (UTxOState
                 utxoEx4A
                 (Coin 0)
                 (Coin 1)
                 emptyPPPUpdates)
               (DPState dsEx4A psEx1)

expectedStEx4A :: ChainState
expectedStEx4A = ChainState
  (NewEpochState
     (EpochNo 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx4A ppsEx1 ppsEx1 emptyNonMyopic)
     SNothing
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  nonce0
  (nonce0 ⭒ mkNonce 1)
  (nonce0 ⭒ mkNonce 1)
  NeutralNonce
  (At $ LastAppliedBlock
    (BlockNo 1)
    (SlotNo 10)
    blockEx4AHash)

ex4A :: CHAINExample
ex4A = CHAINExample (SlotNo 10) initStEx2A blockEx4A (Right expectedStEx4A)


-- | Example 4B - New genesis key delegation updated from future delegations

blockEx4B :: Block
blockEx4B = mkBlock
             blockEx4AHash
             (coreNodeKeys 6)
             []
             (SlotNo 50)
             (BlockNo 2)
             (mkNonce 0)
             (NatNonce 2)
             zero
             2
             0
             (mkOCert (coreNodeKeys 6) 0 (KESPeriod 0))

blockEx4BHash :: HashHeader
blockEx4BHash = bhHash (bheader blockEx4B)

dsEx4B :: DState
dsEx4B = dsEx4A { _fGenDelegs = Map.empty
                , _genDelegs = GenDelegs $ Map.insert
                                 ((hashKey . coreNodeVKG) 0)
                                 ((hashKey . vKey) newGenDelegate)
                                 genDelegs }

expectedLSEx4B :: LedgerState
expectedLSEx4B = LedgerState
               (UTxOState
                 utxoEx4A
                 (Coin 0)
                 (Coin 1)
                 emptyPPPUpdates)
               (DPState dsEx4B psEx1)

expectedStEx4B :: ChainState
expectedStEx4B = ChainState
  (NewEpochState
     (EpochNo 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx4B ppsEx1 ppsEx1 emptyNonMyopic)
     (SJust RewardUpdate { deltaT        = Coin 0
                         , deltaR        = Coin 0
                         , rs            = Map.empty
                         , deltaF        = Coin 0
                         , nonMyopic     = emptyNonMyopic
                         })
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  nonce0
  (mkSeqNonce 2)
  (mkSeqNonce 2)
  NeutralNonce
  (At $ LastAppliedBlock
    (BlockNo 2)
    (SlotNo 50)
    blockEx4BHash)

ex4B :: CHAINExample
ex4B = CHAINExample (SlotNo 50) expectedStEx4A blockEx4B (Right expectedStEx4B)


-- | Example 5A - Genesis key delegation


ir :: Map (Credential 'Staking) Coin
ir = Map.fromList [(aliceSHK, Coin 100)]

aliceCoinEx5A :: Coin
aliceCoinEx5A = aliceInitCoin - 1

txbodyEx5A :: TxBody
txbodyEx5A = TxBody
              (Set.fromList [TxIn genesisId 0])
              (StrictSeq.singleton $ TxOut aliceAddr aliceCoinEx5A)
              (StrictSeq.fromList [DCertMir (MIRCert ir)])
              (Wdrl Map.empty)
              (Coin 1)
              (SlotNo 10)
              SNothing
              SNothing

txEx5A :: Tx
txEx5A = Tx
           txbodyEx5A
           (makeWitnessesVKey (hashTxBody txbodyEx5A) [ alicePay ]
             `Set.union` makeWitnessesVKey (hashTxBody txbodyEx5A)
             [ KeyPair (coreNodeVKG 0) (coreNodeSKG 0)
             , KeyPair (coreNodeVKG 1) (coreNodeSKG 1)
             , KeyPair (coreNodeVKG 2) (coreNodeSKG 2)
             , KeyPair (coreNodeVKG 3) (coreNodeSKG 3)
             , KeyPair (coreNodeVKG 4) (coreNodeSKG 4)
           ])
           Map.empty
           SNothing

blockEx5A :: Block
blockEx5A = mkBlock
              lastByronHeaderHash
              (coreNodeKeys 0)
              [txEx5A]
              (SlotNo 10)
              (BlockNo 1)
              (mkNonce 0)
              (NatNonce 1)
              zero
              0
              0
              (mkOCert (coreNodeKeys 0) 0 (KESPeriod 0))

blockEx5AHash :: HashHeader
blockEx5AHash = bhHash (bheader blockEx5A)

utxoEx5A :: UTxO
utxoEx5A = UTxO . Map.fromList $
                    [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
                    , (TxIn (txid txbodyEx5A) 0, TxOut aliceAddr aliceCoinEx5A)
                    ]

dsEx5A :: DState
dsEx5A = dsEx1 { _irwd = Map.fromList [(aliceSHK, Coin 100)] }

expectedLSEx5A :: LedgerState
expectedLSEx5A = LedgerState
               (UTxOState
                 utxoEx5A
                 (Coin 0)
                 (Coin 1)
                 emptyPPPUpdates)
               (DPState dsEx5A psEx1)

expectedStEx5A :: ChainState
expectedStEx5A = ChainState
  (NewEpochState
     (EpochNo 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx5A ppsEx1 ppsEx1 emptyNonMyopic)
     SNothing
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  nonce0
  (nonce0 ⭒ mkNonce 1)
  (nonce0 ⭒ mkNonce 1)
  NeutralNonce
  (At $ LastAppliedBlock
    (BlockNo 1)
    (SlotNo 10)
    blockEx5AHash)

ex5A :: CHAINExample
ex5A = CHAINExample (SlotNo 10) initStEx2A blockEx5A (Right expectedStEx5A)


-- | Example 5B - Instantaneous rewards with insufficient core node signatures

txEx5B :: Tx
txEx5B = Tx
           txbodyEx5A
           (makeWitnessesVKey (hashTxBody txbodyEx5A) [ alicePay ]
             `Set.union`
             makeWitnessesVKey (hashTxBody txbodyEx5A)
             [ KeyPair (coreNodeVKG 0) (coreNodeSKG 0)
             , KeyPair (coreNodeVKG 1) (coreNodeSKG 1)
             , KeyPair (coreNodeVKG 2) (coreNodeSKG 2)
             , KeyPair (coreNodeVKG 3) (coreNodeSKG 3)
           ])
           Map.empty
           SNothing

blockEx5B :: Block
blockEx5B = mkBlock
              lastByronHeaderHash
              (coreNodeKeys 0)
              [txEx5B]
              (SlotNo 10)
              (BlockNo 1)
              (mkNonce 0)
              (NatNonce 1)
              zero
              0
              0
              (mkOCert (coreNodeKeys 0) 0 (KESPeriod 0))

expectedStEx5B :: PredicateFailure CHAIN
expectedStEx5B = BbodyFailure (LedgersFailure (LedgerFailure (UtxowFailure MIRInsufficientGenesisSigsUTXOW)))

ex5B :: CHAINExample
ex5B = CHAINExample (SlotNo 10) initStEx2A blockEx5B (Left [[expectedStEx5B]])

-- | Example 5C - Instantaneous rewards in decentralized era

expectedStEx5C :: PredicateFailure CHAIN
expectedStEx5C = BbodyFailure (LedgersFailure (LedgerFailure (UtxowFailure MIRImpossibleInDecentralizedNetUTXOW)))

ex5C :: CHAINExample
ex5C =
  CHAINExample
   (SlotNo 10)
   (initStEx2A { chainNes = initNesEx2A { nesEs = esEx2A { esPp = ppsEx1 { _d = unsafeMkUnitInterval 0 }}}})
   blockEx5A
   (Left [[expectedStEx5C]])


-- | Example 5D - Instantaneous rewards in decentralized era and not enough core
-- signatures

ex5D :: CHAINExample
ex5D =
  CHAINExample
   (SlotNo 10)
   (initStEx2A { chainNes = initNesEx2A { nesEs = esEx2A { esPp = ppsEx1 { _d = unsafeMkUnitInterval 0 }}}})
   blockEx5B
   (Left [[expectedStEx5C, expectedStEx5B]])

-- | Example 5E - Instantaneous rewards that overrun the available reserves

ex5E :: CHAINExample
ex5E =
  CHAINExample
   (SlotNo 10)
   (initStEx2A { chainNes = initNesEx2A { nesEs = esEx2A { esAccountState = acntEx2A { _reserves = 99 }}}})
   blockEx5A
   (Left [[BbodyFailure
           (LedgersFailure
            (LedgerFailure
             (DelegsFailure
              (DelplFailure
               (DelegFailure InsufficientForInstantaneousRewardsDELEG)))))]])

-- | Example 5F - Apply instantaneous rewards at epoch boundary


-- | The first transaction adds the MIR certificate that transfers a value of
-- 100 to Alice.

aliceCoinEx5F :: Coin
aliceCoinEx5F = aliceInitCoin - (_keyDeposit ppsEx1) - 1

txbodyEx5F :: TxBody
txbodyEx5F = TxBody
              (Set.fromList [TxIn genesisId 0])
              (StrictSeq.singleton $ TxOut aliceAddr aliceCoinEx5F)
              (StrictSeq.fromList [DCertDeleg (RegKey aliceSHK), DCertMir (MIRCert ir)])
              (Wdrl Map.empty)
              (Coin 1)
              (SlotNo 99)
              SNothing
              SNothing

txEx5F :: Tx
txEx5F = Tx txbodyEx5F
            (makeWitnessesVKey (hashTxBody txbodyEx5F) [ asWitness alicePay, asWitness aliceStake ]
            `Set.union` makeWitnessesVKey (hashTxBody txbodyEx5F)
             [ KeyPair (coreNodeVKG 0) (coreNodeSKG 0)
             , KeyPair (coreNodeVKG 1) (coreNodeSKG 1)
             , KeyPair (coreNodeVKG 2) (coreNodeSKG 2)
             , KeyPair (coreNodeVKG 3) (coreNodeSKG 3)
             , KeyPair (coreNodeVKG 4) (coreNodeSKG 4)
             ])
            Map.empty
            SNothing

blockEx5F :: Block
blockEx5F = mkBlock
              lastByronHeaderHash
              (coreNodeKeys 0)
              [txEx5F]
              (SlotNo 10)
              (BlockNo 1)
              (mkNonce 0)
              (NatNonce 1)
              zero
              0
              0
              (mkOCert (coreNodeKeys 0) 0 (KESPeriod 0))

-- | The second transaction in the next epoch and at least `startRewards` slots
-- after the transaction carrying the MIR certificate, then creates the rewards
-- update that contains the transfer of `100` to Alice.

aliceCoinEx5F' :: Coin
aliceCoinEx5F' = aliceCoinEx5F - 1

txbodyEx5F' :: TxBody
txbodyEx5F' = TxBody
               (Set.fromList [TxIn (txid txbodyEx5F) 0])
               (StrictSeq.singleton $ TxOut aliceAddr aliceCoinEx5F')
               StrictSeq.empty
               (Wdrl Map.empty)
               (Coin 1)
               ((slotFromEpoch $ EpochNo 1)
                +* Duration (startRewards testGlobals) + SlotNo 7)
               SNothing
               SNothing

txEx5F' :: Tx
txEx5F' = Tx txbodyEx5F' (makeWitnessesVKey (hashTxBody txbodyEx5F') [ alicePay ]) Map.empty SNothing

blockEx5F' :: Block
blockEx5F' = mkBlock
              (bhHash (bheader blockEx5F))
              (coreNodeKeys 5)
              [txEx5F']
              ((slotFromEpoch $ EpochNo 1)
                +* Duration (startRewards testGlobals) + SlotNo 7)
              (BlockNo 2)
              (mkNonce 0)
              (NatNonce 1)
              zero
              7
              0
              (mkOCert (coreNodeKeys 5) 0 (KESPeriod 0))

-- | The third transaction in the next epoch applies the reward update to 1)
-- register a staking credential for Alice, 2) deducing the key deposit from the
-- 100 and to 3) create the reward account with an initial amount of 93.

aliceCoinEx5F'' :: Coin
aliceCoinEx5F'' = aliceCoinEx5F' - 1

txbodyEx5F'' :: TxBody
txbodyEx5F'' = TxBody
                (Set.fromList [TxIn (txid txbodyEx5F') 0])
                (StrictSeq.singleton $ TxOut aliceAddr aliceCoinEx5F'')
                StrictSeq.empty
                (Wdrl Map.empty)
                (Coin 1)
                ((slotFromEpoch $ EpochNo 2) + SlotNo 10)
                SNothing
                SNothing

txEx5F'' :: Tx
txEx5F'' = Tx txbodyEx5F'' (makeWitnessesVKey (hashTxBody txbodyEx5F'') [ alicePay ]) Map.empty SNothing

blockEx5F'' :: Block
blockEx5F'' = mkBlock
               (bhHash (bheader blockEx5F'))
               (coreNodeKeys 0)
               [txEx5F'']
               ((slotFromEpoch $ EpochNo 2) + SlotNo 10)
               (BlockNo 3)
               (mkNonce 0)
               (NatNonce 1)
               zero
               10
               10
               (mkOCert (coreNodeKeys 0) 0 (KESPeriod 10))

ex5F' :: Either [[PredicateFailure CHAIN]] ChainState
ex5F' = do
  nextState <- runShelleyBase $ applySTS @CHAIN (TRC (SlotNo 90, initStEx2A, blockEx5F))
  midState <-
    runShelleyBase $ applySTS @CHAIN
      (TRC (((slotFromEpoch $ EpochNo 1) + SlotNo 7) +* Duration (startRewards testGlobals)
           , nextState
           , blockEx5F')
      )
  finalState <-
    runShelleyBase $ applySTS @CHAIN (TRC (((slotFromEpoch $ EpochNo 2) + SlotNo 10), midState, blockEx5F''))

  pure finalState

-- | Tests that after getting instantaneous rewards, creating the update and
-- then applying the update, Alice's key is actually registered, the key deposit
-- value deducted and the remaining value credited as reward.
test5F :: Assertion
test5F = do
  case ex5F' of
    Left e -> assertFailure (show e)
    Right ex5FState -> do
      let getDState = _dstate . _delegationState . esLState . nesEs . chainNes
          ds = getDState ex5FState
          StakeCreds stkCreds = _stkCreds ds
          rews = _rewards ds
          rewEntry = rews Map.!? (mkRwdAcnt aliceSHK)
      assertBool "Alice's credential not in stkCreds" (aliceSHK `Map.member` stkCreds)
      assertBool "Alice's reward account does not exist" $ isJust rewEntry
      assertBool "Alice's rewards are wrong" $ maybe False (== Coin 100) rewEntry
      assertBool "Total amount of ADA is not preserved" $ maxLLSupply == totalAda ex5FState
