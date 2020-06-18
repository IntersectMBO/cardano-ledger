{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.Examples
  ( CHAINExample (..),
    ex1,
    ex2A,
    ex2B,
    ex2C,
    ex2D,
    ex2E,
    ex2F,
    ex2G,
    ex2H,
    ex2I,
    ex2J,
    ex2K,
    ex2L,
    ex3A,
    ex3B,
    ex3C,
    ex4A,
    ex4B,
    ex5AReserves,
    ex5ATreasury,
    ex5BReserves,
    ex5BTreasury,
    ex5CReserves,
    ex5CTreasury,
    ex5DReserves',
    ex5DTreasury',
    ex6A,
    ex6A',
    ex6BExpectedNES,
    ex6BExpectedNES',
    ex6BPoolParams,
    test5DReserves,
    test5DTreasury,
    ppsEx1,
    -- key pairs and example addresses
    alicePay,
    aliceStake,
    aliceAddr,
    bobPay,
    bobStake,
    bobAddr,
    carlPay,
    carlStake,
    carlAddr,
    dariaPay,
    dariaStake,
    dariaAddr,
    coreNodeSKG, -- TODO remove
    -- blocks
    blockEx1,
    blockEx2A,
    blockEx2B,
    blockEx2C,
    blockEx2D,
    blockEx2E,
    blockEx2F,
    blockEx2G,
    blockEx2H,
    blockEx2I,
    blockEx2J,
    blockEx2K,
    blockEx2L,
    blockEx3A,
    blockEx3B,
    blockEx3C,
    blockEx4A,
    blockEx4B,
    blockEx5A,
    blockEx5B,
    blockEx5D,
    -- transactions
    txEx2A,
    txEx2B,
    txEx2D,
    txEx2J,
    txEx2K,
    txEx3A,
    txEx3B,
    txEx4A,
    txEx5A,
    txEx5B,
    txEx5D,
    txEx5D',
    txEx5D'',
    -- helpers
    unsafeMkUnitInterval,
  )
where

import Cardano.Crypto.Hash (HashAlgorithm)
import qualified Cardano.Crypto.Hash as Monomorphic
import Cardano.Prelude (asks)
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.State.Transition.Extended (PredicateFailure, TRC (..), applySTS)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Coerce (coerce)
import Data.List (foldl')
import qualified Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust, maybe)
import Data.Proxy
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Address (mkRwdAcnt, pattern Addr)
import Shelley.Spec.Ledger.BaseTypes
  ( Globals (..),
    Network (..),
    Nonce (..),
    StrictMaybe (..),
    mkNonce,
    randomnessStabilisationWindow,
    textToUrl,
    (⭒),
  )
import Shelley.Spec.Ledger.BlockChain
  ( LastAppliedBlock (..),
    bhHash,
    bheader,
    hashHeaderToNonce,
    pattern HashHeader,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Ptr (..), pattern KeyHashObj, pattern StakeRefPtr)
import Shelley.Spec.Ledger.Delegation.Certificates
  ( pattern DeRegKey,
    pattern Delegate,
    pattern GenesisDelegCert,
    pattern MIRCert,
    pattern PoolDistr,
    pattern RegKey,
    pattern RegPool,
    pattern RetirePool,
  )
import Shelley.Spec.Ledger.EpochBoundary
  ( BlocksMade (..),
    _feeSS,
    _pstakeGo,
    _pstakeMark,
    _pstakeSet,
    _stake,
    emptySnapShots,
    unStake,
    pattern SnapShot,
    pattern SnapShots,
    pattern Stake,
  )
import Shelley.Spec.Ledger.Keys
  ( Hash,
    HashType (RegularHash),
    KeyRole (..),
    KeyRoleHashType,
    asWitness,
    coerceKeyRole,
    hash,
    hashKey,
    vKey,
  )
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    FutureGenDeleg (..),
    InstantaneousRewards (..),
    _delegationState,
    _delegations,
    _dstate,
    _fGenDelegs,
    _fPParams,
    _genDelegs,
    _irwd,
    _pParams,
    _ptrs,
    _reserves,
    _retiring,
    _rewards,
    _stPools,
    _stkCreds,
    _treasury,
    deltaF,
    deltaR,
    deltaT,
    emptyDState,
    emptyInstantaneousRewards,
    emptyPState,
    emptyRewardUpdate,
    esAccountState,
    esLState,
    genesisCoins,
    genesisId,
    nesEs,
    nonMyopic,
    overlaySchedule,
    rs,
    pattern ActiveSlot,
    pattern DPState,
    pattern EpochState,
    pattern LedgerState,
    pattern NewEpochState,
    pattern NonActiveSlot,
    pattern RewardUpdate,
    pattern UTxOState,
  )
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.PParams
  ( PParams,
    PParams' (..),
    PParamsUpdate,
    emptyPPPUpdates,
    emptyPParams,
    pattern ProposedPPUpdates,
    pattern Update,
  )
import Shelley.Spec.Ledger.Rewards
  ( Likelihood (..),
    emptyNonMyopic,
    leaderProbability,
    likelihood,
    rewardPotNM,
    pattern NonMyopic,
  )
import Shelley.Spec.Ledger.STS.Bbody (pattern LedgersFailure)
import Shelley.Spec.Ledger.STS.Chain
  ( chainCandidateNonce,
    chainNes,
    chainPrevEpochNonce,
    initialShelleyState,
    totalAda,
    pattern BbodyFailure,
    pattern ChainState,
  )
import Shelley.Spec.Ledger.STS.Deleg (pattern InsufficientForInstantaneousRewardsDELEG)
import Shelley.Spec.Ledger.STS.Delegs (pattern DelplFailure)
import Shelley.Spec.Ledger.STS.Delpl (pattern DelegFailure)
import Shelley.Spec.Ledger.STS.Ledger (pattern DelegsFailure, pattern UtxowFailure)
import Shelley.Spec.Ledger.STS.Ledgers (pattern LedgerFailure)
import Shelley.Spec.Ledger.STS.Utxow
  ( pattern MIRInsufficientGenesisSigsUTXOW,
  )
import Shelley.Spec.Ledger.Slot
  ( (+*),
    BlockNo (..),
    Duration (..),
    EpochNo (..),
    SlotNo (..),
    epochInfoSize,
  )
import Shelley.Spec.Ledger.Tx (WitnessSetHKD (..), pattern Tx)
import Shelley.Spec.Ledger.TxData
  ( MIRPot (..),
    PoolMetaData (..),
    Wdrl (..),
    _poolCost,
    _poolMD,
    _poolMDHash,
    _poolMDUrl,
    _poolMargin,
    _poolOwners,
    _poolPledge,
    _poolPubKey,
    _poolRAcnt,
    _poolRelays,
    _poolVrf,
    addStakeCreds,
    pattern DCertDeleg,
    pattern DCertGenesis,
    pattern DCertMir,
    pattern DCertPool,
    pattern Delegation,
    pattern PoolParams,
    pattern RewardAcnt,
    pattern StakeCreds,
    pattern StakePools,
    pattern TxBody,
    pattern TxIn,
    pattern TxOut,
  )
import qualified Shelley.Spec.Ledger.TxData as TxData (TxBody (..))
import Shelley.Spec.Ledger.UTxO (balance, hashTxBody, makeWitnessesVKey, txid, pattern UTxO)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( Addr,
    Block,
    CHAIN,
    ChainState,
    ConcreteCrypto,
    Credential,
    DState,
    EpochState,
    GenDelegPair,
    HashHeader,
    KeyHash,
    KeyPair,
    LedgerState,
    NewEpochState,
    NonMyopic,
    OBftSlot,
    PState,
    PoolDistr,
    PoolParams,
    ProposedPPUpdates,
    RewardAcnt,
    RewardUpdate,
    SignKeyDSIGN,
    SnapShot,
    SnapShots,
    Tx,
    TxBody,
    UTxO,
    UTxOState,
    Update,
    VKeyGenesis,
    VRFKeyHash,
    hashKeyVRF,
    pattern GenDelegPair,
    pattern GenDelegs,
    pattern KeyPair,
  )
import Test.Shelley.Spec.Ledger.Generator.Core
  ( AllIssuerKeys (..),
    NatNonce (..),
    genesisAccountState,
    mkBlock,
    mkOCert,
    zero,
  )
import Test.Shelley.Spec.Ledger.Utils
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure)

data CHAINExample h = CHAINExample
  { -- | State to start testing with
    startState :: ChainState h,
    -- | Block to run chain state transition system on
    newBlock :: Block h,
    -- | type of fatal error, if failure expected and final chain state if success expected
    intendedResult :: Either [[PredicateFailure (CHAIN h)]] (ChainState h)
  }

data MIRExample h = MIRExample
  { mirStkCred :: Credential h 'Staking,
    mirRewards :: Coin,
    target :: Either [[PredicateFailure (CHAIN h)]] (ChainState h)
  }
  deriving (Show, Eq)

mkAllIssuerKeys ::
  (HashAlgorithm h, KeyRoleHashType r ~ 'RegularHash) =>
  Word64 ->
  AllIssuerKeys h r
mkAllIssuerKeys w =
  AllIssuerKeys
    (KeyPair vkCold skCold)
    (mkVRFKeyPair (w, 0, 0, 0, 2))
    [(KESPeriod 0, mkKESKeyPair (w, 0, 0, 0, 3))]
    -- TODO mgudemann
    (hashKey vkCold)
  where
    (skCold, vkCold) = mkKeyPair (w, 0, 0, 0, 1)

numCoreNodes :: Word64
numCoreNodes = 7

coreNodes :: HashAlgorithm h => proxy h -> [((SignKeyDSIGN h, VKeyGenesis h), AllIssuerKeys h 'GenesisDelegate)]
coreNodes _ = [(mkGenKey (x, 0, 0, 0, 0), mkAllIssuerKeys x) | x <- [101 .. 100 + numCoreNodes]]

coreNodeSKG :: HashAlgorithm h => proxy h -> Int -> SignKeyDSIGN h
coreNodeSKG p = fst . fst . (coreNodes p !!)

coreNodeVKG :: forall h. HashAlgorithm h => Int -> VKeyGenesis h
coreNodeVKG = snd . fst . (coreNodes p !!)
  where
    p :: Proxy h
    p = Proxy

coreNodeKeys :: forall proxy h. HashAlgorithm h => proxy h -> Int -> AllIssuerKeys h 'GenesisDelegate
coreNodeKeys p = snd . (coreNodes p !!)

-- | Given the slot and an overlay schedule appropriate for this epoch, find the
-- correct core keys for the node with rights to issue a block in this slot.
coreNodeKeysForSlot ::
  forall h.
  (HasCallStack, HashAlgorithm h) =>
  Map SlotNo (OBftSlot h) ->
  Word64 ->
  AllIssuerKeys h 'GenesisDelegate
coreNodeKeysForSlot overlay slot = case Map.lookup (SlotNo slot) overlay of
  Nothing -> error $ "coreNodesForSlot: Cannot find keys for slot " <> show slot
  Just NonActiveSlot -> error $ "coreNodesForSlot: Non-active slot " <> show slot
  Just (ActiveSlot gkh) ->
    case Data.List.find (\((_, gk), _) -> hashKey gk == gkh) (coreNodes p) of
      Nothing -> error $ "coreNodesForSlot: Cannot find key hash in coreNodes: " <> show gkh
      Just ((_, _), ak) -> ak
  where
    p :: Proxy h
    p = Proxy

-- | Calculate the overlay schedule for a given epoch
overlayScheduleFor :: HashAlgorithm h => EpochNo -> Map SlotNo (OBftSlot h)
overlayScheduleFor e =
  runShelleyBase $
    overlaySchedule
      e
      (Map.keysSet genDelegs)
      ppsEx1

-- | Look up the correct core node to issue a block in the given slot, over any epoch
slotKeys :: (HasCallStack, HashAlgorithm h) => Word64 -> AllIssuerKeys h 'GenesisDelegate
slotKeys = coreNodeKeysForSlot fullOSched
  where
    fullOSched = Map.unions $ [overlayScheduleFor e | e <- [0 .. 10]]

genDelegs :: forall h. HashAlgorithm h => Map (KeyHash h 'Genesis) (GenDelegPair h)
genDelegs =
  Map.fromList
    [ ( hashKey $ snd gkey,
        ( GenDelegPair
            (coerceKeyRole . hashKey . vKey $ cold pkeys)
            (hashKeyVRF . snd . vrf $ pkeys)
        )
      )
      | (gkey, pkeys) <- coreNodes p
    ]
  where
    p :: Proxy h
    p = Proxy

alicePay :: KeyPair h 'Payment
alicePay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 0)

aliceStake :: KeyPair h 'Staking
aliceStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (1, 1, 1, 1, 1)

alicePool :: HashAlgorithm h => proxy h -> AllIssuerKeys h 'StakePool
alicePool _ = mkAllIssuerKeys 1

aliceAddr :: HashAlgorithm h => Addr h
aliceAddr = mkAddr (alicePay, aliceStake)

aliceSHK :: HashAlgorithm h => Credential h 'Staking
aliceSHK = (KeyHashObj . hashKey . vKey) aliceStake

bobPay :: KeyPair h 'Payment
bobPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (2, 2, 2, 2, 2)

bobStake :: KeyPair h 'Staking
bobStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (3, 3, 3, 3, 3)

bobAddr :: HashAlgorithm h => Addr h
bobAddr = mkAddr (bobPay, bobStake)

bobSHK :: HashAlgorithm h => Credential h 'Staking
bobSHK = (KeyHashObj . hashKey . vKey) bobStake

aliceInitCoin :: Coin
aliceInitCoin = 10 * 1000 * 1000 * 1000 * 1000 * 1000

bobInitCoin :: Coin
bobInitCoin = 1 * 1000 * 1000 * 1000 * 1000 * 1000

alicePoolParams :: forall h. HashAlgorithm h => PoolParams h
alicePoolParams =
  PoolParams
    { _poolPubKey = (hashKey . vKey . cold) (alicePool p),
      _poolVrf = hashKeyVRF . snd $ vrf (alicePool p),
      _poolPledge = Coin 1,
      _poolCost = Coin 5,
      _poolMargin = unsafeMkUnitInterval 0.1,
      _poolRAcnt = RewardAcnt Testnet aliceSHK,
      _poolOwners = Set.singleton $ (hashKey . vKey) aliceStake,
      _poolRelays = StrictSeq.empty,
      _poolMD =
        SJust $
          PoolMetaData
            { _poolMDUrl = fromJust $ textToUrl "alice.pool",
              _poolMDHash = BS.pack "{}"
            }
    }
  where
    p :: Proxy h
    p = Proxy

-- | Helper Functions

-- | The first block of the Shelley era will point back to the last block of the Byron era.
--  For our purposes in this test we can bootstrap the chain by just coercing the value.
--  When this transition actually occurs, the consensus layer will do the work of making
--  sure that the hash gets translated across the fork
lastByronHeaderHash :: forall proxy h. HashAlgorithm h => proxy h -> HashHeader h
lastByronHeaderHash _ = HashHeader $ coerce (hash 0 :: Hash (ConcreteCrypto h) Int)

nonce0 :: HashAlgorithm h => proxy h -> Nonce
nonce0 p = hashHeaderToNonce (lastByronHeaderHash p)

mkSeqNonce :: HashAlgorithm h => proxy h -> Natural -> Nonce
mkSeqNonce p m = foldl' (\c x -> c ⭒ mkNonce x) (nonce0 p) [1 .. m]

carlPay :: KeyPair h 'Payment
carlPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (4, 4, 4, 4, 4)

carlStake :: KeyPair h 'Staking
carlStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (5, 5, 5, 5, 5)

carlAddr :: HashAlgorithm h => Addr h
carlAddr = mkAddr (carlPay, carlStake)

carlSHK :: HashAlgorithm h => Credential h 'Staking
carlSHK = (KeyHashObj . hashKey . vKey) carlStake

dariaPay :: KeyPair h 'Payment
dariaPay = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (6, 6, 6, 6, 6)

dariaStake :: KeyPair h 'Staking
dariaStake = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (7, 7, 7, 7, 7)

dariaAddr :: HashAlgorithm h => Addr h
dariaAddr = mkAddr (dariaPay, dariaStake)

dariaSHK :: HashAlgorithm h => Credential h 'Staking
dariaSHK = (KeyHashObj . hashKey . vKey) dariaStake

-- * Example 1 - apply CHAIN transition to an empty block

-- | Empty set of UTxOs. No coins to be spent.
utxostEx1 :: UTxOState h
utxostEx1 = UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyPPPUpdates

dsEx1 :: HashAlgorithm h => DState h
dsEx1 = emptyDState {_genDelegs = GenDelegs genDelegs}

oCertIssueNosEx1 :: HashAlgorithm h => Map (KeyHash h 'BlockIssuer) Natural
oCertIssueNosEx1 = Map.fromList (fmap f (Map.elems genDelegs))
  where
    f (GenDelegPair vk _) = (coerceKeyRole vk, 0)

psEx1 :: PState h
psEx1 = emptyPState

-- | Ledger state
lsEx1 :: HashAlgorithm h => LedgerState h
lsEx1 = LedgerState utxostEx1 (DPState dsEx1 psEx1)

ppsEx1 :: PParams
ppsEx1 =
  emptyPParams
    { _maxBBSize = 50000,
      _maxBHSize = 10000,
      _maxTxSize = 10000,
      _eMax = EpochNo 10000,
      _keyDeposit = Coin 7,
      _poolDeposit = Coin 250,
      _d = unsafeMkUnitInterval 0.5,
      _tau = unsafeMkUnitInterval 0.2,
      _rho = unsafeMkUnitInterval 0.0021,
      _minUTxOValue = 100
    }

-- | Account with empty treasury.
acntEx1 :: AccountState
acntEx1 = genesisAccountState

-- | Epoch state with no snapshots.
esEx1 :: HashAlgorithm h => EpochState h
esEx1 = EpochState acntEx1 emptySnapShots lsEx1 ppsEx1 ppsEx1 emptyNonMyopic

initStEx1 :: forall h. HashAlgorithm h => ChainState h
initStEx1 =
  initialShelleyState
    (At $ LastAppliedBlock (BlockNo 0) (SlotNo 0) (lastByronHeaderHash p))
    (EpochNo 0)
    (UTxO Map.empty)
    maxLLSupply
    genDelegs
    (Map.singleton (SlotNo 1) (ActiveSlot . hashKey $ coreNodeVKG 0))
    ppsEx1
    (hashHeaderToNonce (lastByronHeaderHash p))
  where
    p :: Proxy h
    p = Proxy

-- | Null initial block. Just records the Byron hash, and contains no transactions.
blockEx1 :: forall h. HashAlgorithm h => Block h
blockEx1 =
  mkBlock
    (lastByronHeaderHash p)
    (coreNodeKeys p 0)
    []
    (SlotNo 1)
    (BlockNo 1)
    (nonce0 p)
    (NatNonce 1)
    zero
    0
    0
    (mkOCert (coreNodeKeys p 0) 0 (KESPeriod 0))
  where
    p :: Proxy h
    p = Proxy

-- | Expected chain state after successful processing of null block.
expectedStEx1 :: forall h. HashAlgorithm h => ChainState h
expectedStEx1 =
  ChainState
    ( NewEpochState
        (EpochNo 0)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        -- Note that blocks in the overlay schedule do not add to this count.
        esEx1
        SNothing
        (PoolDistr Map.empty)
        (Map.singleton (SlotNo 1) (ActiveSlot . hashKey $ coreNodeVKG 0))
    )
    oCertIssueNosEx1
    (nonce0 p)
    (nonce0 p ⭒ mkNonce 1)
    (nonce0 p ⭒ mkNonce 1)
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 1)
          (SlotNo 1)
          (bhHash . bheader $ blockEx1)
    )
  where
    p :: Proxy h
    p = Proxy

-- | Wraps example all together.
ex1 :: HashAlgorithm h => proxy h -> CHAINExample h
ex1 _ = CHAINExample initStEx1 blockEx1 (Right expectedStEx1)

-- * Example 2A - apply CHAIN transition to register stake keys and a pool

-- | Unspent transaction output for example 2A,
--   so that users actually have coins to spend.
utxoEx2A :: HashAlgorithm h => proxy h -> UTxO h
utxoEx2A _ =
  genesisCoins
    [ TxOut aliceAddr aliceInitCoin,
      TxOut bobAddr bobInitCoin
    ]

-- | Register a single pool with 255 coins of deposit
ppupEx2A :: HashAlgorithm h => ProposedPPUpdates h
ppupEx2A =
  ProposedPPUpdates $
    Map.singleton
      (hashKey $ coreNodeVKG 0) -- stake key
      ( PParams
          { _minfeeA = SNothing,
            _minfeeB = SNothing,
            _maxBBSize = SNothing,
            _maxTxSize = SNothing,
            _maxBHSize = SNothing,
            _keyDeposit = SJust 255,
            _poolDeposit = SNothing,
            _eMax = SNothing,
            _nOpt = SNothing,
            _a0 = SNothing,
            _rho = SNothing,
            _tau = SNothing,
            _d = SNothing,
            _extraEntropy = SNothing,
            _protocolVersion = SNothing,
            _minUTxOValue = SNothing,
            _minPoolCost = SNothing
          }
      )

-- | Update proposal that just changes protocol parameters,
--   and does not change applications.
updateEx2A :: HashAlgorithm h => Update h
updateEx2A = Update ppupEx2A (EpochNo 0)

aliceCoinEx2A :: Coin
aliceCoinEx2A = aliceInitCoin - (_poolDeposit ppsEx1) - 3 * (_keyDeposit ppsEx1) - 3

-- | Transaction body to be processed.
txbodyEx2A :: HashAlgorithm h => TxBody h
txbodyEx2A =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.fromList [TxOut aliceAddr aliceCoinEx2A])
    ( StrictSeq.fromList
        ( [ DCertDeleg (RegKey aliceSHK),
            DCertDeleg (RegKey bobSHK),
            DCertDeleg (RegKey carlSHK),
            DCertPool (RegPool alicePoolParams)
          ]
            ++ [ DCertMir
                   ( MIRCert
                       ReservesMIR
                       ( Map.fromList
                           [ (carlSHK, 110),
                             (dariaSHK, 99)
                           ]
                       )
                   )
               ]
        )
    )
    (Wdrl Map.empty)
    (Coin 3)
    (SlotNo 10)
    (SJust updateEx2A)
    SNothing

txEx2A :: forall h. HashAlgorithm h => Tx h
txEx2A =
  Tx
    txbodyEx2A
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashTxBody txbodyEx2A)
            ( (asWitness <$> [alicePay, carlPay])
                <> (asWitness <$> [aliceStake])
            ),
        -- Note that Alice's stake key needs to sign this transaction
        -- since it is an owner of the stake pool being registered,
        -- and *not* because of the stake key registration.
        regWits =
          makeWitnessesVKey
            (hashTxBody txbodyEx2A)
            ( [asWitness $ cold (alicePool p)]
                <> ( asWitness
                       <$> [ cold (coreNodeKeys p 0),
                             cold (coreNodeKeys p 1),
                             cold (coreNodeKeys p 2),
                             cold (coreNodeKeys p 3),
                             cold (coreNodeKeys p 4)
                           ]
                   )
            )
      }
    SNothing
  where
    p :: Proxy h
    p = Proxy

-- | Pointer address to address of Alice address.
alicePtrAddr :: HashAlgorithm h => Addr h
alicePtrAddr =
  Addr
    Testnet
    (KeyHashObj . hashKey $ vKey alicePay)
    (StakeRefPtr $ Ptr (SlotNo 10) 0 0)

acntEx2A :: HashAlgorithm h => Proxy h -> AccountState
acntEx2A p =
  AccountState
    { _treasury = Coin 0,
      _reserves = maxLLSupply - balance (utxoEx2A p)
    }

initStEx2A :: forall h. HashAlgorithm h => ChainState h
initStEx2A =
  initialShelleyState
    (At $ LastAppliedBlock (BlockNo 0) (SlotNo 0) (lastByronHeaderHash p))
    (EpochNo 0)
    (utxoEx2A p)
    (maxLLSupply - balance (utxoEx2A p))
    genDelegs
    (overlayScheduleFor (EpochNo 0))
    ppsEx1
    (hashHeaderToNonce (lastByronHeaderHash p))
  where
    p :: Proxy h
    p = Proxy

blockEx2A :: forall h. HashAlgorithm h => Block h
blockEx2A =
  mkBlock
    (lastByronHeaderHash p)
    (slotKeys 10)
    [txEx2A]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 p)
    (NatNonce 1)
    zero
    0
    0
    (mkOCert (slotKeys 10) 0 (KESPeriod 0))
  where
    p :: Proxy h
    p = Proxy

dsEx2A :: HashAlgorithm h => DState h
dsEx2A =
  dsEx1
    { _ptrs =
        Map.fromList
          [ (Ptr (SlotNo 10) 0 0, aliceSHK),
            (Ptr (SlotNo 10) 0 1, bobSHK),
            (Ptr (SlotNo 10) 0 2, carlSHK)
          ],
      _stkCreds =
        StakeCreds $
          Map.fromList
            [ (aliceSHK, SlotNo 10),
              (bobSHK, SlotNo 10),
              (carlSHK, SlotNo 10)
            ],
      _rewards =
        Map.fromList
          [ (RewardAcnt Testnet aliceSHK, Coin 0),
            (RewardAcnt Testnet bobSHK, Coin 0),
            (RewardAcnt Testnet carlSHK, Coin 0)
          ],
      _irwd =
        InstantaneousRewards
          { iRReserves =
              Map.fromList
                [ (carlSHK, 110),
                  (dariaSHK, 99)
                ],
            iRTreasury = Map.empty
          }
    }

psEx2A :: forall h. HashAlgorithm h => PState h
psEx2A =
  psEx1
    { _stPools = StakePools $ Map.singleton (hk (alicePool p)) (SlotNo 10),
      _pParams = Map.singleton (hk (alicePool p)) alicePoolParams
    }
  where
    p :: Proxy h
    p = Proxy

expectedLSEx2A :: HashAlgorithm h => LedgerState h
expectedLSEx2A =
  LedgerState
    ( UTxOState
        ( UTxO . Map.fromList $
            [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin),
              (TxIn (txid txbodyEx2A) 0, TxOut aliceAddr aliceCoinEx2A)
            ]
        )
        (Coin 271)
        (Coin 3)
        ppupEx2A
    )
    (DPState dsEx2A psEx2A)

blockEx2AHash :: HashAlgorithm h => HashHeader h
blockEx2AHash = bhHash (bheader blockEx2A)

-- | Expected state after update is processed and STS applied.
expectedStEx2A :: forall h. HashAlgorithm h => ChainState h
expectedStEx2A =
  ChainState
    ( NewEpochState
        (EpochNo 0)
        (BlocksMade Map.empty) -- Still no blocks
        (BlocksMade Map.empty) -- Still no blocks
        (EpochState (acntEx2A p) emptySnapShots expectedLSEx2A ppsEx1 ppsEx1 emptyNonMyopic)
        SNothing
        (PoolDistr Map.empty)
        (overlayScheduleFor (EpochNo 0))
    )
    -- Operational certificate issue numbers are now only updated during block
    -- header processing (in the OCERT rule). As such, we will not see the
    -- operational certificate issue number appear until the first time a block is
    -- issued using the corresponding hot key.
    oCertIssueNosEx1
    (nonce0 p)
    (nonce0 p ⭒ mkNonce 1)
    (nonce0 p ⭒ mkNonce 1)
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 1)
          (SlotNo 10)
          blockEx2AHash
    )
  where
    p :: Proxy h
    p = Proxy

ex2A :: HashAlgorithm h => proxy h -> CHAINExample h
ex2A _ = CHAINExample initStEx2A blockEx2A (Right expectedStEx2A)

-- * Example 2B - process a block late enough in the epoch in order to create a reward update.

aliceCoinEx2BBase :: Coin
aliceCoinEx2BBase = 5 * 1000 * 1000 * 1000 * 1000 * 1000

aliceCoinEx2BPtr :: Coin
aliceCoinEx2BPtr = aliceCoinEx2A - (aliceCoinEx2BBase + 4)

-- | The transaction delegates Alice's and Bob's stake to Alice's pool.
--   Additionally, we split Alice's ADA between a base address and a pointer address.
txbodyEx2B :: forall h. HashAlgorithm h => TxBody h
txbodyEx2B =
  TxBody
    { TxData._inputs = Set.fromList [TxIn (txid txbodyEx2A) 0],
      TxData._outputs =
        StrictSeq.fromList
          [ TxOut aliceAddr aliceCoinEx2BBase,
            TxOut alicePtrAddr aliceCoinEx2BPtr
          ],
      --  Delegation certificates
      TxData._certs =
        StrictSeq.fromList
          [ DCertDeleg (Delegate $ Delegation aliceSHK (hk (alicePool p))),
            DCertDeleg (Delegate $ Delegation bobSHK (hk (alicePool p)))
          ],
      TxData._wdrls = Wdrl Map.empty,
      TxData._txfee = Coin 4,
      TxData._ttl = SlotNo 90,
      TxData._txUpdate = SNothing,
      TxData._mdHash = SNothing
    }
  where
    p :: Proxy h
    p = Proxy

txEx2B :: HashAlgorithm h => Tx h
txEx2B =
  Tx
    txbodyEx2B -- Body of the transaction
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashTxBody txbodyEx2B)
            [asWitness alicePay, asWitness aliceStake, asWitness bobStake]
      }
    SNothing

blockEx2B :: forall h. HashAlgorithm h => Block h
blockEx2B =
  mkBlock
    blockEx2AHash -- Hash of previous block
    (slotKeys 90)
    [txEx2B] -- Single transaction to record
    (SlotNo 90) -- Current slot
    (BlockNo 2)
    (nonce0 p) -- Epoch nonce
    (NatNonce 2) -- Block nonce
    zero -- Praos leader value
    4 -- Period of KES (key evolving signature scheme)
    0
    (mkOCert (slotKeys 90) 0 (KESPeriod 0))
  where
    p :: Proxy h
    p = Proxy

blockEx2BHash :: HashAlgorithm h => proxy h -> HashHeader h
blockEx2BHash _ = bhHash (bheader blockEx2B)

utxoEx2B :: HashAlgorithm h => UTxO h
utxoEx2B =
  UTxO . Map.fromList $
    [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin),
      (TxIn (txid txbodyEx2B) 0, TxOut aliceAddr aliceCoinEx2BBase),
      (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr aliceCoinEx2BPtr)
    ]

-- | Both Alice and Bob delegate to the Alice pool
delegsEx2B :: forall h. HashAlgorithm h => Map (Credential h 'Staking) (KeyHash h 'StakePool)
delegsEx2B =
  Map.fromList
    [ (aliceSHK, hk (alicePool p)),
      (bobSHK, hk (alicePool p))
    ]
  where
    p :: Proxy h
    p = Proxy

carlMIR :: Coin
carlMIR = Coin 110

dariaMIR :: Coin
dariaMIR = Coin 99

dsEx2B :: HashAlgorithm h => DState h
dsEx2B =
  dsEx2A
    { _delegations = delegsEx2B,
      _irwd =
        InstantaneousRewards
          { iRReserves =
              Map.fromList
                [ (carlSHK, carlMIR),
                  (dariaSHK, dariaMIR)
                ],
            iRTreasury = Map.empty
          }
    }

expectedLSEx2B :: HashAlgorithm h => LedgerState h
expectedLSEx2B =
  LedgerState
    ( UTxOState
        utxoEx2B
        (Coin 271)
        (Coin 7)
        ppupEx2A
    )
    (DPState dsEx2B psEx2A)

-- | Expected state after transition
expectedStEx2B :: forall h. HashAlgorithm h => ChainState h
expectedStEx2B =
  ChainState
    ( NewEpochState
        (EpochNo 0) -- First epoch
        (BlocksMade Map.empty) -- Blocks made before current
        (BlocksMade Map.empty) -- Blocks made before current
        (EpochState (acntEx2A p) emptySnapShots expectedLSEx2B ppsEx1 ppsEx1 emptyNonMyopic)
        -- Previous epoch state
        (SJust emptyRewardUpdate)
        (PoolDistr Map.empty)
        (overlayScheduleFor (EpochNo 0))
    )
    oCertIssueNosEx1
    (nonce0 p)
    (nonce0 p ⭒ mkNonce 1 ⭒ mkNonce 2) -- Evolving nonce
    (nonce0 p ⭒ mkNonce 1) -- Candidate nonce
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 2) -- Current block no
          (SlotNo 90) -- Current slot
          (blockEx2BHash p) -- Hash header of the chain
    )
  where
    p :: Proxy h
    p = Proxy

ex2B :: HashAlgorithm h => proxy h -> CHAINExample h
ex2B _ = CHAINExample expectedStEx2A blockEx2B (Right expectedStEx2B)

-- | Example 2C - process an empty block in the next epoch
-- so that the (empty) reward update is applied and a stake snapshot is made.
blockEx2C :: forall h. HashAlgorithm h => Block h
blockEx2C =
  mkBlock
    (blockEx2BHash p) -- Hash of previous block
    (slotKeys 110)
    [] -- No transactions at all (empty block)
    (SlotNo 110) -- Current slot
    (BlockNo 3) -- Second block within the epoch
    (nonce0 p ⭒ mkNonce 1) -- Epoch nonce
    (NatNonce 3) -- Block nonce
    zero -- Praos leader value
    5 -- Period of KES (key evolving signature scheme)
    0
    (mkOCert (slotKeys 110) 0 (KESPeriod 0))
  where
    p :: Proxy h
    p = Proxy

-- | Snapshot of stakes for Alice and Bob
snapEx2C :: HashAlgorithm h => SnapShot h
snapEx2C =
  SnapShot
    ( Stake
        ( Map.fromList
            [ (aliceSHK, aliceCoinEx2BBase + aliceCoinEx2BPtr),
              (bobSHK, bobInitCoin)
            ]
        )
    )
    delegsEx2B
    (Map.singleton (hk (alicePool p)) alicePoolParams)
  where
    p :: Proxy h
    p = Proxy

-- | Snapshots with given fees.
snapsEx2C :: HashAlgorithm h => SnapShots h
snapsEx2C =
  emptySnapShots
    { _pstakeMark = snapEx2C, -- snapshot of stake pools and parameters
      _feeSS = 7
    }

expectedLSEx2C :: HashAlgorithm h => LedgerState h
expectedLSEx2C =
  LedgerState
    ( UTxOState
        utxoEx2B
        (Coin 271)
        (Coin 7)
        emptyPPPUpdates -- Note that the ppup is gone now
    )
    ( DPState
        dsEx2B
          { _irwd = emptyInstantaneousRewards,
            _stkCreds = addStakeCreds carlSHK (SlotNo 10) $ _stkCreds dsEx2B,
            _rewards = Map.insert (mkRwdAcnt Testnet carlSHK) 110 $ _rewards dsEx2B
          }
        psEx2A
    )

blockEx2CHash :: HashAlgorithm h => HashHeader h
blockEx2CHash = bhHash (bheader blockEx2C)

expectedStEx2Cgeneric :: forall h. HashAlgorithm h => SnapShots h -> LedgerState h -> PParams -> ChainState h
expectedStEx2Cgeneric ss ls pp =
  ChainState
    ( NewEpochState
        (EpochNo 1)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        (EpochState (acntEx2A p) {_reserves = _reserves (acntEx2A p) - carlMIR} ss ls pp pp emptyNonMyopic)
        SNothing
        (PoolDistr Map.empty)
        (overlayScheduleFor (EpochNo 1))
    )
    oCertIssueNosEx1
    (nonce0 p ⭒ mkNonce 1)
    (mkSeqNonce p 3)
    (mkSeqNonce p 3)
    (hashHeaderToNonce (blockEx2BHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 3)
          (SlotNo 110)
          blockEx2CHash
    )
  where
    p :: Proxy h
    p = Proxy

-- ** Expected chain state after STS

expectedStEx2C :: HashAlgorithm h => ChainState h
expectedStEx2C = expectedStEx2Cgeneric snapsEx2C expectedLSEx2C ppsEx1

ex2C :: HashAlgorithm h => proxy h -> CHAINExample h
ex2C _ = CHAINExample expectedStEx2B blockEx2C (Right expectedStEx2C)

-- | Example 2D - process a block late enough
-- in the epoch in order to create a second reward update, preparing the way for
-- the first non-empty pool distribution in this running example.
-- Additionally, in order to have the stake distribution change,
-- Carl delegates his stake.

-- | The transaction delegates Carl's stake to Alice's pool.
aliceCoinEx2DBase :: Coin
aliceCoinEx2DBase = aliceCoinEx2BBase - 5

txbodyEx2D :: forall h. HashAlgorithm h => TxBody h
txbodyEx2D =
  TxBody
    { TxData._inputs = Set.fromList [TxIn (txid txbodyEx2B) 0],
      TxData._outputs = StrictSeq.fromList [TxOut aliceAddr aliceCoinEx2DBase],
      TxData._certs =
        StrictSeq.fromList [DCertDeleg (Delegate $ Delegation carlSHK (hk (alicePool p)))],
      TxData._wdrls = Wdrl Map.empty,
      TxData._txfee = Coin 5,
      TxData._ttl = SlotNo 500,
      TxData._txUpdate = SNothing,
      TxData._mdHash = SNothing
    }
  where
    p :: Proxy h
    p = Proxy

txEx2D :: HashAlgorithm h => Tx h
txEx2D =
  Tx
    txbodyEx2D
    mempty
      { addrWits =
          makeWitnessesVKey (hashTxBody txbodyEx2D) [asWitness alicePay, asWitness carlStake]
      }
    SNothing

blockEx2D :: forall h. HashAlgorithm h => Block h
blockEx2D =
  mkBlock
    blockEx2CHash
    (slotKeys 190)
    [txEx2D]
    (SlotNo 190)
    (BlockNo 4)
    (nonce0 p ⭒ mkNonce 1)
    (NatNonce 4)
    zero
    9
    0
    (mkOCert (slotKeys 190) 0 (KESPeriod 0))
  where
    p :: Proxy h
    p = Proxy

blockEx2DHash :: HashAlgorithm h => proxy h -> HashHeader h
blockEx2DHash _ = bhHash (bheader blockEx2D)

utxoEx2D :: HashAlgorithm h => UTxO h
utxoEx2D =
  UTxO . Map.fromList $
    [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin),
      (TxIn (txid txbodyEx2D) 0, TxOut aliceAddr aliceCoinEx2DBase),
      (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr aliceCoinEx2BPtr)
    ]

delegsEx2D :: forall h. HashAlgorithm h => Map (Credential h 'Staking) (KeyHash h 'StakePool)
delegsEx2D =
  Map.fromList
    [ (aliceSHK, hk (alicePool p)),
      (bobSHK, hk (alicePool p)),
      (carlSHK, hk (alicePool p))
    ]
  where
    p :: Proxy h
    p = Proxy

dsEx2D :: HashAlgorithm h => DState h
dsEx2D = (dsEx2C) {_delegations = delegsEx2D}
  where
    dsEx2C = (_dstate . _delegationState) expectedLSEx2C

expectedLSEx2D :: HashAlgorithm h => LedgerState h
expectedLSEx2D =
  LedgerState
    ( UTxOState
        utxoEx2D
        (Coin 271)
        (Coin 12)
        emptyPPPUpdates
    )
    (DPState dsEx2D psEx2A)

expectedStEx2D :: forall h. HashAlgorithm h => ChainState h
expectedStEx2D =
  ChainState
    ( NewEpochState
        (EpochNo 1)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        ( EpochState
            (acntEx2A p) {_reserves = _reserves (acntEx2A p) - carlMIR}
            snapsEx2C
            expectedLSEx2D
            ppsEx1
            ppsEx1
            emptyNonMyopic
        )
        ( SJust
            RewardUpdate
              { deltaT = Coin 7,
                deltaR = Coin 0,
                rs = Map.empty,
                deltaF = Coin (-7),
                nonMyopic = emptyNonMyopic {rewardPotNM = Coin 6}
              }
        )
        (PoolDistr Map.empty)
        (overlayScheduleFor (EpochNo 1))
    )
    oCertIssueNosEx1
    (nonce0 p ⭒ mkNonce 1)
    (mkSeqNonce p 4)
    (mkSeqNonce p 3)
    (hashHeaderToNonce (blockEx2BHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 4)
          (SlotNo 190)
          (blockEx2DHash p)
    )
  where
    p :: Proxy h
    p = Proxy

ex2D :: HashAlgorithm h => proxy h -> CHAINExample h
ex2D _ = CHAINExample expectedStEx2C blockEx2D (Right expectedStEx2D)

-- | Example 2E - create the first non-empty pool distribution
-- by creating a block in the third epoch of this running example.
blockEx2E :: forall h. HashAlgorithm h => Block h
blockEx2E =
  mkBlock
    (blockEx2DHash p)
    (slotKeys 220)
    []
    (SlotNo 220)
    (BlockNo 5)
    ((mkSeqNonce p 3) ⭒ (hashHeaderToNonce (blockEx2BHash p)))
    (NatNonce 5)
    zero
    11
    10
    (mkOCert (slotKeys 220) 1 (KESPeriod 10))
  where
    p :: Proxy h
    p = Proxy

snapEx2E :: forall h. HashAlgorithm h => SnapShot h
snapEx2E =
  SnapShot
    ( Stake
        ( Map.fromList
            [ (aliceSHK, aliceCoinEx2DBase + aliceCoinEx2BPtr),
              (carlSHK, carlMIR),
              (bobSHK, bobInitCoin)
            ]
        )
    )
    delegsEx2D
    (Map.singleton (hk (alicePool p)) alicePoolParams)
  where
    p :: Proxy h
    p = Proxy

snapsEx2E :: HashAlgorithm h => proxy h -> SnapShots h
snapsEx2E _ =
  emptySnapShots
    { _pstakeMark = snapEx2E,
      _pstakeSet = snapEx2C,
      _feeSS = Coin 5
    }

expectedLSEx2E :: HashAlgorithm h => LedgerState h
expectedLSEx2E =
  LedgerState
    ( UTxOState
        utxoEx2D
        (Coin 271)
        (Coin 5)
        emptyPPPUpdates
    )
    ( DPState
        dsEx2D
          { _irwd = emptyInstantaneousRewards,
            _stkCreds = addStakeCreds carlSHK (SlotNo 10) $ _stkCreds dsEx2B,
            _rewards = Map.insert (mkRwdAcnt Testnet carlSHK) 110 $ _rewards dsEx2B
          }
        psEx2A
    )

blockEx2EHash :: HashAlgorithm h => HashHeader h
blockEx2EHash = bhHash (bheader blockEx2E)

acntEx2E :: HashAlgorithm h => proxy h -> AccountState
acntEx2E p =
  AccountState
    { _treasury = Coin 7,
      _reserves = maxLLSupply - balance (utxoEx2A p) - carlMIR
    }

oCertIssueNosEx2 :: HashAlgorithm h => Map (KeyHash h 'BlockIssuer) Natural
oCertIssueNosEx2 =
  Map.insert
    (coerceKeyRole . hashKey $ vKey $ cold $ slotKeys 220)
    1
    oCertIssueNosEx1

nonMyopicEx2E :: NonMyopic h
nonMyopicEx2E = emptyNonMyopic {rewardPotNM = Coin 6}

expectedStEx2E :: forall h. HashAlgorithm h => ChainState h
expectedStEx2E =
  ChainState
    ( NewEpochState
        (EpochNo 2)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        (EpochState (acntEx2E p) (snapsEx2E p) expectedLSEx2E ppsEx1 ppsEx1 nonMyopicEx2E)
        SNothing
        ( PoolDistr
            ( Map.singleton
                (hk (alicePool p))
                (1, hashKeyVRF (snd $ vrf (alicePool p)))
            )
        )
        (overlayScheduleFor (EpochNo 2))
    )
    oCertIssueNosEx2
    ((mkSeqNonce p 3) ⭒ (hashHeaderToNonce (blockEx2BHash p)))
    (mkSeqNonce p 5)
    (mkSeqNonce p 5)
    (hashHeaderToNonce (blockEx2DHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 5)
          (SlotNo 220)
          blockEx2EHash
    )
  where
    p :: Proxy h
    p = Proxy

ex2E :: HashAlgorithm h => proxy h -> CHAINExample h
ex2E _ = CHAINExample expectedStEx2D blockEx2E (Right expectedStEx2E)

-- | Example 2F - create a decentralized Praos block (ie one not in the overlay schedule)
oCertIssueNosEx2F :: forall h. HashAlgorithm h => Map (KeyHash h 'BlockIssuer) Natural
oCertIssueNosEx2F = Map.insert (coerceKeyRole $ hk (alicePool p)) 0 oCertIssueNosEx2
  where
    p :: Proxy h
    p = Proxy

blockEx2F :: forall h. HashAlgorithm h => Block h
blockEx2F =
  mkBlock
    blockEx2EHash
    (alicePool p)
    []
    (SlotNo 295) -- odd slots open for decentralization in epoch1OSchedEx2E
    (BlockNo 6)
    ((mkSeqNonce p 3) ⭒ (hashHeaderToNonce (blockEx2BHash p)))
    (NatNonce 6)
    zero
    14
    14
    (mkOCert (alicePool p) 0 (KESPeriod 14))
  where
    p :: Proxy h
    p = Proxy

blockEx2FHash :: HashAlgorithm h => proxy h -> HashHeader h
blockEx2FHash _ = bhHash (bheader blockEx2F)

pdEx2F :: forall h. HashAlgorithm h => PoolDistr h
pdEx2F = PoolDistr $ Map.singleton (hk (alicePool p)) (1, hashKeyVRF $ snd $ vrf (alicePool p))
  where
    p :: Proxy h
    p = Proxy

nonMyopicEx2F :: NonMyopic h
nonMyopicEx2F = emptyNonMyopic {rewardPotNM = Coin 4}

expectedStEx2F :: forall h. HashAlgorithm h => ChainState h
expectedStEx2F =
  ChainState
    ( NewEpochState
        (EpochNo 2)
        (BlocksMade Map.empty)
        (BlocksMade $ Map.singleton (hk (alicePool p)) 1)
        (EpochState (acntEx2E p) (snapsEx2E p) expectedLSEx2E ppsEx1 ppsEx1 nonMyopicEx2E)
        ( SJust
            RewardUpdate
              { deltaT = Coin 5,
                deltaR = Coin 0,
                rs = Map.empty,
                deltaF = Coin (-5),
                nonMyopic = nonMyopicEx2F
              }
        )
        pdEx2F
        (overlayScheduleFor (EpochNo 2))
    )
    oCertIssueNosEx2F
    ((mkSeqNonce p 3) ⭒ (hashHeaderToNonce (blockEx2BHash p)))
    (mkSeqNonce p 6)
    (mkSeqNonce p 5)
    (hashHeaderToNonce (blockEx2DHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 6)
          (SlotNo 295)
          (blockEx2FHash p)
    )
  where
    p :: Proxy h
    p = Proxy

ex2F :: HashAlgorithm h => proxy h -> CHAINExample h
ex2F _ = CHAINExample expectedStEx2E blockEx2F (Right expectedStEx2F)

-- | Example 2G - create an empty block in the next epoch
-- to prepare the way for the first non-trivial reward update
blockEx2G :: forall h. HashAlgorithm h => Block h
blockEx2G =
  mkBlock
    (blockEx2FHash p)
    (slotKeys 310)
    []
    (SlotNo 310)
    (BlockNo 7)
    ((mkSeqNonce p 5) ⭒ (hashHeaderToNonce (blockEx2DHash p)))
    (NatNonce 7)
    zero
    15
    15
    (mkOCert (slotKeys 310) 1 (KESPeriod 15))
  where
    p :: Proxy h
    p = Proxy

blockEx2GHash :: HashAlgorithm h => HashHeader h
blockEx2GHash = bhHash (bheader blockEx2G)

snapsEx2G :: HashAlgorithm h => proxy h -> SnapShots h
snapsEx2G p =
  (snapsEx2E p)
    { _pstakeMark = snapEx2E,
      _pstakeSet = snapEx2E,
      _pstakeGo = snapEx2C,
      _feeSS = 0
    }

expectedLSEx2G :: HashAlgorithm h => LedgerState h
expectedLSEx2G =
  LedgerState
    ( UTxOState
        utxoEx2D
        (Coin 271)
        (Coin 0)
        emptyPPPUpdates
    )
    ( DPState
        dsEx2D
        psEx2A
    )

oCertIssueNosEx2G :: HashAlgorithm h => Map (KeyHash h 'BlockIssuer) Natural
oCertIssueNosEx2G =
  Map.insert
    (coerceKeyRole . hashKey $ vKey $ cold $ slotKeys 310)
    1
    oCertIssueNosEx2F

acntEx2G :: HashAlgorithm h => proxy h -> AccountState
acntEx2G p = (acntEx2E p) {_treasury = Coin 12}

expectedStEx2G :: forall h. HashAlgorithm h => ChainState h
expectedStEx2G =
  ChainState
    ( NewEpochState
        (EpochNo 3)
        (BlocksMade $ Map.singleton (hk (alicePool p)) 1)
        (BlocksMade Map.empty)
        (EpochState (acntEx2G p) (snapsEx2G p) expectedLSEx2G ppsEx1 ppsEx1 nonMyopicEx2F)
        SNothing
        pdEx2F
        (overlayScheduleFor (EpochNo 3))
    )
    oCertIssueNosEx2G
    ((mkSeqNonce p 5) ⭒ (hashHeaderToNonce (blockEx2DHash p)))
    (mkSeqNonce p 7)
    (mkSeqNonce p 7)
    (hashHeaderToNonce (blockEx2FHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 7)
          (SlotNo 310)
          blockEx2GHash
    )
  where
    p :: Proxy h
    p = Proxy

ex2G :: HashAlgorithm h => proxy h -> CHAINExample h
ex2G _ = CHAINExample expectedStEx2F blockEx2G (Right expectedStEx2G)

-- | Example 2H - create the first non-trivial reward update
blockEx2H :: forall h. HashAlgorithm h => Block h
blockEx2H =
  mkBlock
    blockEx2GHash
    (slotKeys 390)
    []
    (SlotNo 390)
    (BlockNo 8)
    ((mkSeqNonce p 5) ⭒ (hashHeaderToNonce (blockEx2DHash p)))
    (NatNonce 8)
    zero
    19
    19
    (mkOCert (slotKeys 390) 2 (KESPeriod 19))
  where
    p :: Proxy h
    p = Proxy

blockEx2HHash :: HashAlgorithm h => proxy h -> HashHeader h
blockEx2HHash _ = bhHash (bheader blockEx2H)

aliceRAcnt2H :: Coin
aliceRAcnt2H = Coin 5827393939

bobRAcnt2H :: Coin
bobRAcnt2H = Coin 519272726

rewardsEx2H :: HashAlgorithm h => Map (RewardAcnt h) Coin
rewardsEx2H =
  Map.fromList
    [ (RewardAcnt Testnet aliceSHK, aliceRAcnt2H),
      (RewardAcnt Testnet bobSHK, bobRAcnt2H)
    ]

oCertIssueNosEx2H :: HashAlgorithm h => Map (KeyHash h 'BlockIssuer) Natural
oCertIssueNosEx2H =
  Map.insert
    (coerceKeyRole . hashKey $ vKey $ cold $ slotKeys 390)
    2
    oCertIssueNosEx2G

alicePerfEx2H :: forall h. HashAlgorithm h => Proxy h -> Likelihood
alicePerfEx2H p = likelihood blocks t slotsPerEpoch
  where
    slotsPerEpoch = runShelleyBase $ do
      ei <- asks epochInfo
      epochInfoSize ei 0
    blocks = 1
    t = leaderProbability f relativeStake (_d ppsEx1)
    stake = aliceCoinEx2BBase + aliceCoinEx2BPtr + bobInitCoin
    reserves = _reserves (acntEx2G p)
    relativeStake =
      fromRational (fromIntegral stake % (fromIntegral $ maxLLSupply - reserves))
    f = runShelleyBase (asks activeSlotCoeff)

deltaT2H :: Coin
deltaT2H = Coin 786986666668

deltaR2H :: Coin
deltaR2H = Coin (-793333333333)

nonMyopicEx2H :: forall h. HashAlgorithm h => NonMyopic h
nonMyopicEx2H =
  NonMyopic
    (Map.singleton (hk (alicePool p)) (alicePerfEx2H p))
    (Coin 634666666667)
    snapEx2C
  where
    p :: Proxy h
    p = Proxy

expectedStEx2H :: forall h. HashAlgorithm h => ChainState h
expectedStEx2H =
  ChainState
    ( NewEpochState
        (EpochNo 3)
        (BlocksMade $ Map.singleton (hk (alicePool p)) 1)
        (BlocksMade Map.empty)
        (EpochState (acntEx2G p) (snapsEx2G p) expectedLSEx2G ppsEx1 ppsEx1 nonMyopicEx2F)
        ( SJust
            RewardUpdate
              { deltaT = deltaT2H,
                deltaR = deltaR2H,
                rs = rewardsEx2H,
                deltaF = Coin 0,
                nonMyopic = nonMyopicEx2H
              }
        )
        pdEx2F
        (overlayScheduleFor (EpochNo 3))
    )
    oCertIssueNosEx2H
    ((mkSeqNonce p 5) ⭒ (hashHeaderToNonce (blockEx2DHash p)))
    (mkSeqNonce p 8)
    (mkSeqNonce p 7)
    (hashHeaderToNonce (blockEx2FHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 8)
          (SlotNo 390)
          (blockEx2HHash p)
    )
  where
    p :: Proxy h
    p = Proxy

ex2H :: HashAlgorithm h => proxy h -> CHAINExample h
ex2H _ = CHAINExample expectedStEx2G blockEx2H (Right expectedStEx2H)

-- | Example 2I - apply the first non-trivial reward update
blockEx2I :: forall h. HashAlgorithm h => Block h
blockEx2I =
  mkBlock
    (blockEx2HHash p)
    (slotKeys 410)
    []
    (SlotNo 410)
    (BlockNo 9)
    ((mkSeqNonce p 7) ⭒ (hashHeaderToNonce (blockEx2FHash p)))
    (NatNonce 9)
    zero
    20
    20
    (mkOCert (slotKeys 410) 2 (KESPeriod 20))
  where
    p :: Proxy h
    p = Proxy

blockEx2IHash :: HashAlgorithm h => HashHeader h
blockEx2IHash = bhHash (bheader blockEx2I)

acntEx2I :: HashAlgorithm h => proxy h -> AccountState
acntEx2I p =
  AccountState
    { _treasury = (_treasury (acntEx2G p)) + deltaT2H,
      _reserves = (_reserves (acntEx2G p)) + deltaR2H
    }

dsEx2I :: HashAlgorithm h => DState h
dsEx2I = dsEx2D {_rewards = Map.insert (mkRwdAcnt Testnet carlSHK) 110 rewardsEx2H}

expectedLSEx2I :: HashAlgorithm h => LedgerState h
expectedLSEx2I =
  LedgerState
    ( UTxOState
        utxoEx2D
        (Coin 271)
        (Coin 0)
        emptyPPPUpdates
    )
    (DPState dsEx2I psEx2A)

snapsEx2I :: forall h. HashAlgorithm h => Proxy h -> SnapShots h
snapsEx2I p =
  (snapsEx2G p)
    { _pstakeMark =
        SnapShot
          ( Stake
              ( Map.fromList
                  [ (bobSHK, bobInitCoin + bobRAcnt2H),
                    (aliceSHK, aliceCoinEx2DBase + aliceCoinEx2BPtr + aliceRAcnt2H),
                    (carlSHK, carlMIR)
                  ]
              )
          )
          delegsEx2D
          (Map.singleton (hk (alicePool p)) alicePoolParams),
      -- The stake snapshots have bigger values now, due to the new rewards
      _pstakeSet = snapEx2E,
      _pstakeGo = snapEx2E,
      _feeSS = Coin 0
    }

oCertIssueNosEx2I :: HashAlgorithm h => Map (KeyHash h 'BlockIssuer) Natural
oCertIssueNosEx2I =
  Map.insert
    (coerceKeyRole . hashKey $ vKey $ cold $ slotKeys 410)
    2
    oCertIssueNosEx2H

expectedStEx2I :: forall h. HashAlgorithm h => ChainState h
expectedStEx2I =
  ChainState
    ( NewEpochState
        (EpochNo 4)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        (EpochState (acntEx2I p) (snapsEx2I p) expectedLSEx2I ppsEx1 ppsEx1 nonMyopicEx2H)
        SNothing
        pdEx2F
        (overlayScheduleFor (EpochNo 4))
    )
    oCertIssueNosEx2I
    ((mkSeqNonce p 7) ⭒ (hashHeaderToNonce (blockEx2FHash p)))
    (mkSeqNonce p 9)
    (mkSeqNonce p 9)
    (hashHeaderToNonce (blockEx2HHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 9)
          (SlotNo 410)
          blockEx2IHash
    )
  where
    p :: Proxy h
    p = Proxy

ex2I :: HashAlgorithm h => proxy h -> CHAINExample h
ex2I _ = CHAINExample expectedStEx2H blockEx2I (Right expectedStEx2I)

-- | Example 2J - drain reward account and de-register stake key
bobAda2J :: Coin
bobAda2J =
  bobRAcnt2H -- reward account
    + bobInitCoin -- txin we will consume (must spend at least one)
    + Coin 7 -- stake registration refund
    - Coin 9 -- tx fee

txbodyEx2J :: HashAlgorithm h => TxBody h
txbodyEx2J =
  TxBody
    (Set.fromList [TxIn genesisId 1])
    (StrictSeq.singleton $ TxOut bobAddr bobAda2J)
    (StrictSeq.fromList [DCertDeleg (DeRegKey bobSHK)])
    (Wdrl $ Map.singleton (RewardAcnt Testnet bobSHK) bobRAcnt2H)
    (Coin 9)
    (SlotNo 500)
    SNothing
    SNothing

txEx2J :: HashAlgorithm h => Tx h
txEx2J =
  Tx
    txbodyEx2J
    mempty
      { addrWits =
          makeWitnessesVKey (hashTxBody txbodyEx2J) [asWitness bobPay, asWitness bobStake]
      }
    SNothing

blockEx2J :: forall h. HashAlgorithm h => Block h
blockEx2J =
  mkBlock
    blockEx2IHash
    (slotKeys 420)
    [txEx2J]
    (SlotNo 420)
    (BlockNo 10)
    ((mkSeqNonce p 7) ⭒ (hashHeaderToNonce (blockEx2FHash p)))
    (NatNonce 10)
    zero
    21
    19
    (mkOCert (slotKeys 420) 2 (KESPeriod 19))
  where
    p :: Proxy h
    p = Proxy

blockEx2JHash :: HashAlgorithm h => HashHeader h
blockEx2JHash = bhHash (bheader blockEx2J)

utxoEx2J :: HashAlgorithm h => UTxO h
utxoEx2J =
  UTxO . Map.fromList $
    [ (TxIn (txid txbodyEx2J) 0, TxOut bobAddr bobAda2J),
      (TxIn (txid txbodyEx2D) 0, TxOut aliceAddr aliceCoinEx2DBase),
      (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr aliceCoinEx2BPtr)
    ]

dsEx2J :: HashAlgorithm h => DState h
dsEx2J =
  dsEx1
    { _ptrs =
        Map.fromList
          [ (Ptr (SlotNo 10) 0 0, aliceSHK),
            (Ptr (SlotNo 10) 0 2, carlSHK)
          ],
      _stkCreds = StakeCreds $ Map.fromList [(aliceSHK, SlotNo 10), (carlSHK, SlotNo 10)],
      _delegations = Map.fromList [(aliceSHK, hk (alicePool p)), (carlSHK, hk (alicePool p))],
      _rewards = Map.fromList [(RewardAcnt Testnet aliceSHK, aliceRAcnt2H), (RewardAcnt Testnet carlSHK, carlMIR)]
    }
  where
    p :: Proxy h
    p = Proxy

expectedLSEx2J :: HashAlgorithm h => LedgerState h
expectedLSEx2J =
  LedgerState
    ( UTxOState
        utxoEx2J
        (Coin 264)
        (Coin 9)
        emptyPPPUpdates
    )
    (DPState dsEx2J psEx2A)

oCertIssueNosEx2J :: HashAlgorithm h => Map (KeyHash h 'BlockIssuer) Natural
oCertIssueNosEx2J =
  Map.insert
    (coerceKeyRole . hashKey $ vKey $ cold $ slotKeys 420)
    2
    oCertIssueNosEx2I

expectedStEx2J :: forall h. HashAlgorithm h => ChainState h
expectedStEx2J =
  ChainState
    ( NewEpochState
        (EpochNo 4)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        (EpochState (acntEx2I p) (snapsEx2I p) expectedLSEx2J ppsEx1 ppsEx1 nonMyopicEx2H)
        SNothing
        pdEx2F
        (overlayScheduleFor (EpochNo 4))
    )
    oCertIssueNosEx2J
    ((mkSeqNonce p 7) ⭒ (hashHeaderToNonce (blockEx2FHash p)))
    (mkSeqNonce p 10)
    (mkSeqNonce p 10)
    (hashHeaderToNonce (blockEx2HHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 10)
          (SlotNo 420)
          blockEx2JHash
    )
  where
    p :: Proxy h
    p = Proxy

ex2J :: HashAlgorithm h => proxy h -> CHAINExample h
ex2J _ = CHAINExample expectedStEx2I blockEx2J (Right expectedStEx2J)

-- | Example 2K - start stake pool retirement
aliceCoinEx2KPtr :: Coin
aliceCoinEx2KPtr = aliceCoinEx2DBase - 2

txbodyEx2K :: HashAlgorithm h => TxBody h
txbodyEx2K =
  TxBody
    (Set.fromList [TxIn (txid txbodyEx2D) 0])
    (StrictSeq.singleton $ TxOut alicePtrAddr aliceCoinEx2KPtr)
    (StrictSeq.fromList [DCertPool (RetirePool (hk (alicePool p)) (EpochNo 5))])
    (Wdrl Map.empty)
    (Coin 2)
    (SlotNo 500)
    SNothing
    SNothing
  where
    p :: Proxy h
    p = Proxy

txEx2K :: HashAlgorithm h => Tx h
txEx2K =
  Tx
    txbodyEx2K
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashTxBody txbodyEx2K)
            [asWitness alicePay],
        regWits =
          makeWitnessesVKey
            (hashTxBody txbodyEx2K)
            [asWitness $ cold (alicePool p)]
      }
    SNothing
  where
    p :: Proxy h
    p = Proxy

blockEx2K :: forall h. HashAlgorithm h => Block h
blockEx2K =
  mkBlock
    blockEx2JHash
    (slotKeys 490)
    [txEx2K]
    (SlotNo 490)
    (BlockNo 11)
    ((mkSeqNonce p 7) ⭒ (hashHeaderToNonce (blockEx2FHash p)))
    (NatNonce 11)
    zero
    24
    19
    (mkOCert (slotKeys 490) 2 (KESPeriod 19))
  where
    p :: Proxy h
    p = Proxy

blockEx2KHash :: HashAlgorithm h => proxy h -> HashHeader h
blockEx2KHash _ = bhHash (bheader blockEx2K)

utxoEx2K :: HashAlgorithm h => UTxO h
utxoEx2K =
  UTxO . Map.fromList $
    [ (TxIn (txid txbodyEx2J) 0, TxOut bobAddr bobAda2J),
      (TxIn (txid txbodyEx2K) 0, TxOut alicePtrAddr aliceCoinEx2KPtr),
      (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr aliceCoinEx2BPtr)
    ]

psEx2K :: HashAlgorithm h => PState h
psEx2K = psEx2A {_retiring = Map.singleton (hk (alicePool p)) (EpochNo 5)}
  where
    p :: Proxy h
    p = Proxy

expectedLSEx2K :: HashAlgorithm h => LedgerState h
expectedLSEx2K =
  LedgerState
    ( UTxOState
        utxoEx2K
        (Coin 264)
        (Coin 11)
        emptyPPPUpdates
    )
    (DPState dsEx2J psEx2K)

alicePerfEx2K :: forall h. HashAlgorithm h => Proxy h -> Likelihood
alicePerfEx2K p = (alicePerfEx2H p) <> epoch4Likelihood
  where
    epoch4Likelihood = likelihood blocks t slotsPerEpoch
    slotsPerEpoch = runShelleyBase $ do
      ei <- asks epochInfo
      epochInfoSize ei 0
    blocks = 0
    t = leaderProbability f relativeStake (_d ppsEx1)
    stake = sum . unStake . _stake . _pstakeSet $ (snapsEx2I p) -- everyone has delegated to Alice's Pool
    relativeStake = fromRational (fromIntegral stake % (fromIntegral $ supply))
    supply = maxLLSupply - _reserves (acntEx2I p)
    f = runShelleyBase (asks activeSlotCoeff)

nonMyopicEx2K :: forall h. HashAlgorithm h => NonMyopic h
nonMyopicEx2K =
  NonMyopic
    (Map.singleton (hk (alicePool p)) (alicePerfEx2K p))
    (Coin 0)
    snapEx2E
  where
    p :: Proxy h
    p = Proxy

expectedStEx2K :: forall h. HashAlgorithm h => ChainState h
expectedStEx2K =
  ChainState
    ( NewEpochState
        (EpochNo 4)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        (EpochState (acntEx2I p) (snapsEx2I p) expectedLSEx2K ppsEx1 ppsEx1 nonMyopicEx2H)
        ( SJust
            RewardUpdate
              { deltaT = Coin 0,
                deltaR = Coin 0,
                rs = Map.empty,
                deltaF = Coin 0,
                nonMyopic = nonMyopicEx2K
              }
        )
        pdEx2F
        (overlayScheduleFor (EpochNo 4))
    )
    oCertIssueNosEx2J
    ((mkSeqNonce p 7) ⭒ (hashHeaderToNonce (blockEx2FHash p)))
    (mkSeqNonce p 11)
    (mkSeqNonce p 10)
    (hashHeaderToNonce (blockEx2HHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 11)
          (SlotNo 490)
          (blockEx2KHash p)
    )
  where
    p :: Proxy h
    p = Proxy

ex2K :: HashAlgorithm h => proxy h -> CHAINExample h
ex2K _ = CHAINExample expectedStEx2J blockEx2K (Right expectedStEx2K)

-- | Example 2L - reap a stake pool
blockEx2L :: forall h. HashAlgorithm h => Block h
blockEx2L =
  mkBlock
    (blockEx2KHash p)
    (slotKeys 510)
    []
    (SlotNo 510)
    (BlockNo 12)
    ((mkSeqNonce p 10) ⭒ (hashHeaderToNonce (blockEx2HHash p)))
    (NatNonce 12)
    zero
    25
    25
    (mkOCert (slotKeys 510) 3 (KESPeriod 25))
  where
    p :: Proxy h
    p = Proxy

blockEx2LHash :: HashAlgorithm h => HashHeader h
blockEx2LHash = bhHash (bheader blockEx2L)

acntEx2L :: HashAlgorithm h => proxy h -> AccountState
acntEx2L p =
  (acntEx2I p)
    { _treasury =
        _treasury (acntEx2I p) --previous amount
    }

snapsEx2L :: HashAlgorithm h => SnapShots h
snapsEx2L =
  SnapShots
    { _pstakeMark =
        SnapShot
          ( Stake
              ( Map.fromList
                  [ (aliceSHK, aliceRAcnt2H + aliceCoinEx2BPtr + aliceCoinEx2KPtr),
                    (carlSHK, carlMIR)
                  ]
              )
          )
          (Map.fromList [(aliceSHK, hk (alicePool p)), (carlSHK, hk (alicePool p))])
          (Map.singleton (hk (alicePool p)) alicePoolParams),
      _pstakeSet = _pstakeMark (snapsEx2I p),
      _pstakeGo = _pstakeSet (snapsEx2I p),
      _feeSS = Coin 11
    }
  where
    p :: Proxy h
    p = Proxy

dsEx2L :: HashAlgorithm h => DState h
dsEx2L =
  dsEx1
    { _ptrs =
        Map.fromList
          [ (Ptr (SlotNo 10) 0 0, aliceSHK),
            (Ptr (SlotNo 10) 0 2, carlSHK)
          ],
      _stkCreds = StakeCreds $ Map.fromList [(aliceSHK, SlotNo 10), (carlSHK, SlotNo 10)],
      _rewards =
        Map.fromList
          [ (RewardAcnt Testnet aliceSHK, aliceRAcnt2H + Coin 250),
            (RewardAcnt Testnet carlSHK, carlMIR)
          ]
          -- Note the pool cert refund of 201
    }

expectedLSEx2L :: HashAlgorithm h => LedgerState h
expectedLSEx2L =
  LedgerState
    ( UTxOState
        utxoEx2K
        (Coin 14)
        (Coin 11)
        emptyPPPUpdates
    )
    (DPState dsEx2L psEx1) -- Note the stake pool is reaped

oCertIssueNosEx2L :: HashAlgorithm h => Map (KeyHash h 'BlockIssuer) Natural
oCertIssueNosEx2L =
  Map.insert (coerceKeyRole . hashKey $ vKey $ cold $ slotKeys 510) 3 oCertIssueNosEx2J

expectedStEx2L :: forall h. HashAlgorithm h => ChainState h
expectedStEx2L =
  ChainState
    ( NewEpochState
        (EpochNo 5)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        (EpochState (acntEx2L p) snapsEx2L expectedLSEx2L ppsEx1 ppsEx1 nonMyopicEx2K)
        SNothing
        pdEx2F
        (overlayScheduleFor (EpochNo 5))
    )
    oCertIssueNosEx2L
    ((mkSeqNonce p 10) ⭒ (hashHeaderToNonce (blockEx2HHash p)))
    (mkSeqNonce p 12)
    (mkSeqNonce p 12)
    (hashHeaderToNonce (blockEx2KHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 12)
          (SlotNo 510)
          blockEx2LHash
    )
  where
    p :: Proxy h
    p = Proxy

ex2L :: HashAlgorithm h => proxy h -> CHAINExample h
ex2L _ = CHAINExample expectedStEx2K blockEx2L (Right expectedStEx2L)

-- | Example 3A - Setting up for a successful protocol parameter update,
-- have three genesis keys vote on the same new parameters
ppVote3A :: PParamsUpdate
ppVote3A =
  PParams
    { _minfeeA = SNothing,
      _minfeeB = SNothing,
      _maxBBSize = SNothing,
      _maxTxSize = SNothing,
      _maxBHSize = SNothing,
      _keyDeposit = SNothing,
      _poolDeposit = SJust 200,
      _eMax = SNothing,
      _nOpt = SNothing,
      _a0 = SNothing,
      _rho = SNothing,
      _tau = SNothing,
      _d = SNothing,
      _extraEntropy = SJust (mkNonce 123),
      _protocolVersion = SNothing,
      _minUTxOValue = SNothing,
      _minPoolCost = SNothing
    }

ppupEx3A :: HashAlgorithm h => ProposedPPUpdates h
ppupEx3A =
  ProposedPPUpdates $
    Map.fromList
      [ (hashKey $ coreNodeVKG 0, ppVote3A),
        (hashKey $ coreNodeVKG 3, ppVote3A),
        (hashKey $ coreNodeVKG 4, ppVote3A)
      ]

updateEx3A :: HashAlgorithm h => Update h
updateEx3A = Update ppupEx3A (EpochNo 0)

aliceCoinEx3A :: Coin
aliceCoinEx3A = aliceInitCoin - 1

txbodyEx3A :: HashAlgorithm h => TxBody h
txbodyEx3A =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.singleton $ TxOut aliceAddr aliceCoinEx3A)
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 10)
    (SJust updateEx3A)
    SNothing

txEx3A :: HashAlgorithm h => Tx h
txEx3A =
  Tx
    txbodyEx3A
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashTxBody txbodyEx3A)
            [asWitness alicePay],
        regWits =
          makeWitnessesVKey
            (hashTxBody txbodyEx3A)
            [ asWitness . cold $ coreNodeKeys p 0,
              asWitness . cold $ coreNodeKeys p 3,
              asWitness . cold $ coreNodeKeys p 4
            ]
      }
    SNothing
  where
    p :: Proxy h
    p = Proxy

blockEx3A :: forall h. HashAlgorithm h => Block h
blockEx3A =
  mkBlock
    (lastByronHeaderHash p)
    (slotKeys 10)
    [txEx3A]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 p)
    (NatNonce 1)
    zero
    0
    0
    (mkOCert (slotKeys 10) 0 (KESPeriod 0))
  where
    p :: Proxy h
    p = Proxy

expectedLSEx3A :: HashAlgorithm h => LedgerState h
expectedLSEx3A =
  LedgerState
    ( UTxOState
        ( UTxO . Map.fromList $
            [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin),
              (TxIn (txid txbodyEx3A) 0, TxOut aliceAddr aliceCoinEx3A)
            ]
        )
        (Coin 0)
        (Coin 1)
        ppupEx3A
    )
    (DPState dsEx1 psEx1)

blockEx3AHash :: HashAlgorithm h => HashHeader h
blockEx3AHash = bhHash (bheader blockEx3A)

expectedStEx3A :: forall h. HashAlgorithm h => ChainState h
expectedStEx3A =
  ChainState
    ( NewEpochState
        (EpochNo 0)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        (EpochState (acntEx2A p) emptySnapShots expectedLSEx3A ppsEx1 ppsEx1 emptyNonMyopic)
        SNothing
        (PoolDistr Map.empty)
        (overlayScheduleFor (EpochNo 0))
    )
    oCertIssueNosEx1
    (nonce0 p)
    (nonce0 p ⭒ mkNonce 1)
    (nonce0 p ⭒ mkNonce 1)
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 1)
          (SlotNo 10)
          blockEx3AHash
    )
  where
    p :: Proxy h
    p = Proxy

ex3A :: HashAlgorithm h => proxy h -> CHAINExample h
ex3A _ = CHAINExample initStEx2A blockEx3A (Right expectedStEx3A)

-- | Example 3B - Finish getting enough votes for the protocol parameter update.
ppupEx3B :: HashAlgorithm h => ProposedPPUpdates h
ppupEx3B =
  ProposedPPUpdates $
    Map.fromList
      [ (hashKey $ coreNodeVKG 1, ppVote3A),
        (hashKey $ coreNodeVKG 5, ppVote3A)
      ]

updateEx3B :: HashAlgorithm h => Update h
updateEx3B = Update ppupEx3B (EpochNo 0)

aliceCoinEx3B :: Coin
aliceCoinEx3B = aliceCoinEx3A - 1

txbodyEx3B :: HashAlgorithm h => TxBody h
txbodyEx3B =
  TxBody
    (Set.fromList [TxIn (txid txbodyEx3A) 0])
    (StrictSeq.singleton $ TxOut aliceAddr aliceCoinEx3B)
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 31)
    (SJust updateEx3B)
    SNothing

txEx3B :: HashAlgorithm h => Tx h
txEx3B =
  Tx
    txbodyEx3B
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashTxBody txbodyEx3B)
            [asWitness alicePay],
        regWits =
          makeWitnessesVKey
            (hashTxBody txbodyEx3B)
            [ asWitness . cold $ coreNodeKeys p 1,
              asWitness . cold $ coreNodeKeys p 5
            ]
      }
    SNothing
  where
    p :: Proxy h
    p = Proxy

blockEx3B :: forall h. HashAlgorithm h => Block h
blockEx3B =
  mkBlock
    blockEx3AHash
    (slotKeys 20)
    [txEx3B]
    (SlotNo 20)
    (BlockNo 2)
    (nonce0 p)
    (NatNonce 2)
    zero
    1
    0
    (mkOCert (slotKeys 20) 0 (KESPeriod 0))
  where
    p :: Proxy h
    p = Proxy

utxoEx3B :: HashAlgorithm h => UTxO h
utxoEx3B =
  UTxO . Map.fromList $
    [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin),
      (TxIn (txid txbodyEx3B) 0, TxOut aliceAddr aliceCoinEx3B)
    ]

ppupEx3B' :: HashAlgorithm h => ProposedPPUpdates h
ppupEx3B' =
  ProposedPPUpdates $ Map.fromList $
    fmap (\n -> (hashKey $ coreNodeVKG n, ppVote3A)) [0, 1, 3, 4, 5]

expectedLSEx3B :: HashAlgorithm h => LedgerState h
expectedLSEx3B =
  LedgerState
    ( UTxOState
        utxoEx3B
        (Coin 0)
        (Coin 2)
        ppupEx3B'
    )
    (DPState dsEx1 psEx1)

blockEx3BHash :: HashAlgorithm h => proxy h -> HashHeader h
blockEx3BHash _ = bhHash (bheader blockEx3B)

expectedStEx3B :: forall h. HashAlgorithm h => ChainState h
expectedStEx3B =
  ChainState
    ( NewEpochState
        (EpochNo 0)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        (EpochState (acntEx2A p) emptySnapShots expectedLSEx3B ppsEx1 ppsEx1 emptyNonMyopic)
        SNothing
        (PoolDistr Map.empty)
        (overlayScheduleFor (EpochNo 0))
    )
    oCertIssueNosEx1
    (nonce0 p)
    (mkSeqNonce p 2)
    (mkSeqNonce p 2)
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 2)
          (SlotNo 20)
          (blockEx3BHash p)
    )
  where
    p :: Proxy h
    p = Proxy

ex3B :: HashAlgorithm h => proxy h -> CHAINExample h
ex3B _ = CHAINExample expectedStEx3A blockEx3B (Right expectedStEx3B)

-- | Example 3C - Adopt protocol parameter update
blockEx3C :: forall h. HashAlgorithm h => Block h
blockEx3C =
  mkBlock
    (blockEx3BHash p)
    (slotKeys 110)
    []
    (SlotNo 110)
    (BlockNo 3)
    (mkSeqNonce p 2 ⭒ mkNonce 123)
    (NatNonce 3)
    zero
    5
    0
    (mkOCert (slotKeys 110) 0 (KESPeriod 0))
  where
    p :: Proxy h
    p = Proxy

blockEx3CHash :: HashAlgorithm h => HashHeader h
blockEx3CHash = bhHash (bheader blockEx3C)

snapsEx3C :: SnapShots h
snapsEx3C = emptySnapShots {_feeSS = Coin 2}

expectedLSEx3C :: HashAlgorithm h => LedgerState h
expectedLSEx3C =
  LedgerState
    ( UTxOState
        utxoEx3B
        (Coin 0)
        (Coin 2)
        emptyPPPUpdates
    )
    (DPState dsEx1 psEx1)

ppsEx3C :: PParams
ppsEx3C = ppsEx1 {_poolDeposit = Coin 200, _extraEntropy = mkNonce 123}

expectedStEx3C :: forall h. HashAlgorithm h => ChainState h
expectedStEx3C =
  ChainState
    ( NewEpochState
        (EpochNo 1)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        (EpochState (acntEx2A p) snapsEx3C expectedLSEx3C ppsEx1 ppsEx3C emptyNonMyopic)
        SNothing
        (PoolDistr Map.empty)
        (overlayScheduleFor (EpochNo 1))
    )
    oCertIssueNosEx1
    (mkSeqNonce p 2 ⭒ mkNonce 123)
    (mkSeqNonce p 3)
    (mkSeqNonce p 3)
    (hashHeaderToNonce (blockEx3BHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 3)
          (SlotNo 110)
          blockEx3CHash
    )
  where
    p :: Proxy h
    p = Proxy

ex3C :: HashAlgorithm h => proxy h -> CHAINExample h
ex3C _ = CHAINExample expectedStEx3B blockEx3C (Right expectedStEx3C)

-- | Example 4A - Genesis key delegation
newGenDelegate :: KeyPair h 'GenesisDelegate
newGenDelegate = KeyPair vkCold skCold
  where
    (skCold, vkCold) = mkKeyPair (108, 0, 0, 0, 1)

newGenesisVrfKH :: HashAlgorithm h => VRFKeyHash h
newGenesisVrfKH = hashKeyVRF . snd $ mkVRFKeyPair (9, 8, 7, 6, 5)

aliceCoinEx4A :: Coin
aliceCoinEx4A = aliceInitCoin - 1

txbodyEx4A :: HashAlgorithm h => TxBody h
txbodyEx4A =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.singleton $ TxOut aliceAddr aliceCoinEx4A)
    ( StrictSeq.fromList
        [ DCertGenesis
            ( GenesisDelegCert
                (hashKey (coreNodeVKG 0))
                (hashKey (vKey newGenDelegate))
                newGenesisVrfKH
            )
        ]
    )
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 10)
    SNothing
    SNothing

txEx4A :: forall h. HashAlgorithm h => Tx h
txEx4A =
  Tx
    txbodyEx4A
    mempty
      { addrWits =
          makeWitnessesVKey (hashTxBody txbodyEx4A) [alicePay],
        regWits =
          makeWitnessesVKey
            (hashTxBody txbodyEx4A)
            [KeyPair (coreNodeVKG 0) (coreNodeSKG p 0)]
      }
    SNothing
  where
    p :: Proxy h
    p = Proxy

blockEx4A :: forall h. HashAlgorithm h => Block h
blockEx4A =
  mkBlock
    (lastByronHeaderHash p)
    (slotKeys 10)
    [txEx4A]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 p)
    (NatNonce 1)
    zero
    0
    0
    (mkOCert (slotKeys 10) 0 (KESPeriod 0))
  where
    p :: Proxy h
    p = Proxy

blockEx4AHash :: HashAlgorithm h => HashHeader h
blockEx4AHash = bhHash (bheader blockEx4A)

dsEx4A :: HashAlgorithm h => DState h
dsEx4A =
  dsEx1
    { _fGenDelegs =
        Map.singleton
          (FutureGenDeleg (SlotNo 43) (hashKey $ coreNodeVKG 0))
          (GenDelegPair (hashKey . vKey $ newGenDelegate) newGenesisVrfKH)
    }

utxoEx4A :: HashAlgorithm h => UTxO h
utxoEx4A =
  UTxO . Map.fromList $
    [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin),
      (TxIn (txid txbodyEx4A) 0, TxOut aliceAddr aliceCoinEx4A)
    ]

expectedLSEx4A :: HashAlgorithm h => LedgerState h
expectedLSEx4A =
  LedgerState
    ( UTxOState
        utxoEx4A
        (Coin 0)
        (Coin 1)
        emptyPPPUpdates
    )
    (DPState dsEx4A psEx1)

expectedStEx4A :: forall h. HashAlgorithm h => ChainState h
expectedStEx4A =
  ChainState
    ( NewEpochState
        (EpochNo 0)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        (EpochState (acntEx2A p) emptySnapShots expectedLSEx4A ppsEx1 ppsEx1 emptyNonMyopic)
        SNothing
        (PoolDistr Map.empty)
        (overlayScheduleFor (EpochNo 0))
    )
    oCertIssueNosEx1
    (nonce0 p)
    (nonce0 p ⭒ mkNonce 1)
    (nonce0 p ⭒ mkNonce 1)
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 1)
          (SlotNo 10)
          blockEx4AHash
    )
  where
    p :: Proxy h
    p = Proxy

ex4A :: HashAlgorithm h => proxy h -> CHAINExample h
ex4A _ = CHAINExample initStEx2A blockEx4A (Right expectedStEx4A)

-- | Example 4B - New genesis key delegation updated from future delegations
blockEx4B :: forall h. HashAlgorithm h => Block h
blockEx4B =
  mkBlock
    blockEx4AHash
    (slotKeys 50)
    []
    (SlotNo 50)
    (BlockNo 2)
    (nonce0 p)
    (NatNonce 2)
    zero
    2
    0
    (mkOCert (slotKeys 50) 0 (KESPeriod 0))
  where
    p :: Proxy h
    p = Proxy

blockEx4BHash :: HashAlgorithm h => HashHeader h
blockEx4BHash = bhHash (bheader blockEx4B)

dsEx4B :: HashAlgorithm h => DState h
dsEx4B =
  dsEx4A
    { _fGenDelegs = Map.empty,
      _genDelegs =
        GenDelegs $
          Map.insert
            ((hashKey . coreNodeVKG) 0)
            (GenDelegPair (hashKey . vKey $ newGenDelegate) newGenesisVrfKH)
            genDelegs
    }

expectedLSEx4B :: HashAlgorithm h => LedgerState h
expectedLSEx4B =
  LedgerState
    ( UTxOState
        utxoEx4A
        (Coin 0)
        (Coin 1)
        emptyPPPUpdates
    )
    (DPState dsEx4B psEx1)

expectedStEx4B :: forall h. HashAlgorithm h => ChainState h
expectedStEx4B =
  ChainState
    ( NewEpochState
        (EpochNo 0)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        (EpochState (acntEx2A p) emptySnapShots expectedLSEx4B ppsEx1 ppsEx1 emptyNonMyopic)
        (SJust emptyRewardUpdate)
        (PoolDistr Map.empty)
        (overlayScheduleFor (EpochNo 0))
    )
    oCertIssueNosEx1
    (nonce0 p)
    (mkSeqNonce p 2)
    (mkSeqNonce p 2)
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 2)
          (SlotNo 50)
          blockEx4BHash
    )
  where
    p :: Proxy h
    p = Proxy

ex4B :: HashAlgorithm h => proxy h -> CHAINExample h
ex4B _ = CHAINExample expectedStEx4A blockEx4B (Right expectedStEx4B)

-- | Example 5A - Genesis key delegation
ir :: HashAlgorithm h => Map (Credential h 'Staking) Coin
ir = Map.fromList [(aliceSHK, Coin 100)]

aliceCoinEx5A :: Coin
aliceCoinEx5A = aliceInitCoin - 1

txbodyEx5A :: HashAlgorithm h => MIRPot -> TxBody h
txbodyEx5A pot =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.singleton $ TxOut aliceAddr aliceCoinEx5A)
    (StrictSeq.fromList [DCertMir (MIRCert pot ir)])
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 10)
    SNothing
    SNothing

txEx5A :: HashAlgorithm h => MIRPot -> Tx h
txEx5A pot =
  Tx
    (txbodyEx5A pot)
    mempty
      { addrWits =
          makeWitnessesVKey (hashTxBody $ txbodyEx5A pot) [alicePay],
        regWits =
          makeWitnessesVKey
            (hashTxBody $ txbodyEx5A pot)
            ( asWitness
                <$> [ cold (coreNodeKeys p 0),
                      cold (coreNodeKeys p 1),
                      cold (coreNodeKeys p 2),
                      cold (coreNodeKeys p 3),
                      cold (coreNodeKeys p 4)
                    ]
            )
      }
    SNothing
  where
    p :: Proxy h
    p = Proxy

blockEx5A :: forall h. HashAlgorithm h => MIRPot -> Block h
blockEx5A pot =
  mkBlock
    (lastByronHeaderHash p)
    (slotKeys 10)
    [txEx5A pot]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 p)
    (NatNonce 1)
    zero
    0
    0
    (mkOCert (slotKeys 10) 0 (KESPeriod 0))
  where
    p :: Proxy h
    p = Proxy

blockEx5AHash :: HashAlgorithm h => MIRPot -> HashHeader h
blockEx5AHash pot = bhHash (bheader $ blockEx5A pot)

utxoEx5A :: HashAlgorithm h => MIRPot -> UTxO h
utxoEx5A pot =
  UTxO . Map.fromList $
    [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin),
      (TxIn (txid $ txbodyEx5A pot) 0, TxOut aliceAddr aliceCoinEx5A)
    ]

dsEx5A :: HashAlgorithm h => MIRPot -> DState h
dsEx5A pot = dsEx1 {_irwd = InstantaneousRewards {iRReserves = r, iRTreasury = t}}
  where
    (r, t) = case pot of
      ReservesMIR -> (Map.fromList [(aliceSHK, Coin 100)], Map.empty)
      TreasuryMIR -> (Map.empty, Map.fromList [(aliceSHK, Coin 100)])

expectedLSEx5A :: HashAlgorithm h => MIRPot -> LedgerState h
expectedLSEx5A pot =
  LedgerState
    ( UTxOState
        (utxoEx5A pot)
        (Coin 0)
        (Coin 1)
        emptyPPPUpdates
    )
    (DPState (dsEx5A pot) psEx1)

treasuryEx5A :: Coin
treasuryEx5A = Coin 1000

setChainStateAccountState :: AccountState -> ChainState h -> ChainState h
setChainStateAccountState as cs = cs {chainNes = (chainNes cs) {nesEs = es'}}
  where
    es' = (nesEs $ chainNes cs) {esAccountState = as}

initStEx5A :: forall h. HashAlgorithm h => ChainState h
initStEx5A =
  setChainStateAccountState
    ( AccountState
        { _treasury = 1000,
          _reserves = maxLLSupply - (1000 + balance (utxoEx2A p))
        }
    )
    initStEx2A
  where
    p :: Proxy h
    p = Proxy

acntEx5A :: HashAlgorithm h => Proxy h -> AccountState
acntEx5A p =
  AccountState
    { _treasury = treasuryEx5A,
      _reserves = maxLLSupply - (balance (utxoEx2A p) + treasuryEx5A)
    }

expectedStEx5A :: forall h. HashAlgorithm h => MIRPot -> ChainState h
expectedStEx5A pot =
  ChainState
    ( NewEpochState
        (EpochNo 0)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        (EpochState (acntEx5A p) emptySnapShots (expectedLSEx5A pot) ppsEx1 ppsEx1 emptyNonMyopic)
        SNothing
        (PoolDistr Map.empty)
        (overlayScheduleFor (EpochNo 0))
    )
    oCertIssueNosEx1
    (nonce0 p)
    (nonce0 p ⭒ mkNonce 1)
    (nonce0 p ⭒ mkNonce 1)
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 1)
          (SlotNo 10)
          (blockEx5AHash pot)
    )
  where
    p :: Proxy h
    p = Proxy

ex5A :: HashAlgorithm h => proxy h -> MIRPot -> CHAINExample h
ex5A _ pot = CHAINExample initStEx5A (blockEx5A pot) (Right $ expectedStEx5A pot)

ex5AReserves :: HashAlgorithm h => proxy h -> CHAINExample h
ex5AReserves p = ex5A p ReservesMIR

ex5ATreasury :: HashAlgorithm h => proxy h -> CHAINExample h
ex5ATreasury p = ex5A p TreasuryMIR

-- | Example 5B - Instantaneous rewards with insufficient core node signatures
txEx5B :: HashAlgorithm h => MIRPot -> Tx h
txEx5B pot =
  Tx
    (txbodyEx5A pot)
    ( mempty
        { addrWits =
            makeWitnessesVKey (hashTxBody $ txbodyEx5A pot) [alicePay],
          regWits =
            makeWitnessesVKey
              (hashTxBody $ txbodyEx5A pot)
              ( asWitness
                  <$> [ cold (coreNodeKeys p 0),
                        cold (coreNodeKeys p 1),
                        cold (coreNodeKeys p 2),
                        cold (coreNodeKeys p 3)
                      ]
              )
        }
    )
    SNothing
  where
    p :: Proxy h
    p = Proxy

blockEx5B :: forall h. HashAlgorithm h => MIRPot -> Block h
blockEx5B pot =
  mkBlock
    (lastByronHeaderHash p)
    (slotKeys 10)
    [txEx5B pot]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 p)
    (NatNonce 1)
    zero
    0
    0
    (mkOCert (slotKeys 10) 0 (KESPeriod 0))
  where
    p :: Proxy h
    p = Proxy

mirWitsEx5B :: HashAlgorithm h => Set (KeyHash h 'RWitness)
mirWitsEx5B = Set.fromList [asWitness . hk . coreNodeKeys p $ i | i <- [0 .. 3]]
  where
    p :: Proxy h
    p = Proxy

expectedStEx5B :: HashAlgorithm h => PredicateFailure (CHAIN h)
expectedStEx5B = BbodyFailure (LedgersFailure (LedgerFailure (UtxowFailure $ MIRInsufficientGenesisSigsUTXOW mirWitsEx5B)))

ex5B :: HashAlgorithm h => proxy h -> MIRPot -> CHAINExample h
ex5B _ pot = CHAINExample initStEx5A (blockEx5B pot) (Left [[expectedStEx5B]])

ex5BReserves :: HashAlgorithm h => proxy h -> CHAINExample h
ex5BReserves p = ex5B p ReservesMIR

ex5BTreasury :: HashAlgorithm h => proxy h -> CHAINExample h
ex5BTreasury p = ex5B p TreasuryMIR

-- | Example 5C - Instantaneous rewards that overrun the available reserves
initStEx5C :: HashAlgorithm h => ChainState h
initStEx5C =
  setChainStateAccountState
    (AccountState {_treasury = 99, _reserves = 99})
    initStEx2A

ex5C :: HashAlgorithm h => proxy h -> MIRPot -> CHAINExample h
ex5C _ pot =
  CHAINExample
    initStEx5C
    (blockEx5A pot)
    ( Left
        [ [ BbodyFailure
              ( LedgersFailure
                  ( LedgerFailure
                      ( DelegsFailure
                          ( DelplFailure
                              (DelegFailure $ InsufficientForInstantaneousRewardsDELEG pot (Coin 100) (Coin 99))
                          )
                      )
                  )
              )
          ]
        ]
    )

ex5CReserves :: HashAlgorithm h => proxy h -> CHAINExample h
ex5CReserves p = ex5C p ReservesMIR

ex5CTreasury :: HashAlgorithm h => proxy h -> CHAINExample h
ex5CTreasury p = ex5C p TreasuryMIR

-- | Example 5D - Apply instantaneous rewards at epoch boundary

-- | The first transaction adds the MIR certificate that transfers a value of
-- 100 to Alice.
aliceCoinEx5D :: Coin
aliceCoinEx5D = aliceInitCoin - (_keyDeposit ppsEx1) - 1

txbodyEx5D :: HashAlgorithm h => MIRPot -> TxBody h
txbodyEx5D pot =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.singleton $ TxOut aliceAddr aliceCoinEx5D)
    (StrictSeq.fromList [DCertDeleg (RegKey aliceSHK), DCertMir (MIRCert pot ir)])
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 99)
    SNothing
    SNothing

txEx5D :: HashAlgorithm h => MIRPot -> Tx h
txEx5D pot =
  Tx
    (txbodyEx5D pot)
    mempty
      { addrWits =
          makeWitnessesVKey (hashTxBody $ txbodyEx5D pot) [asWitness alicePay, asWitness aliceStake],
        regWits =
          makeWitnessesVKey
            (hashTxBody $ txbodyEx5D pot)
            ( asWitness
                <$> [ cold (coreNodeKeys p 0),
                      cold (coreNodeKeys p 1),
                      cold (coreNodeKeys p 2),
                      cold (coreNodeKeys p 3),
                      cold (coreNodeKeys p 4)
                    ]
            )
      }
    SNothing
  where
    p :: Proxy h
    p = Proxy

blockEx5D :: forall h. HashAlgorithm h => MIRPot -> Block h
blockEx5D pot =
  mkBlock
    (lastByronHeaderHash p)
    (slotKeys 10)
    [txEx5D pot]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 p)
    (NatNonce 1)
    zero
    0
    0
    (mkOCert (slotKeys 10) 0 (KESPeriod 0))
  where
    p :: Proxy h
    p = Proxy

-- | The second transaction in the next epoch and at least `randomnessStabilisationWindow` slots
-- after the transaction carrying the MIR certificate, then creates the rewards
-- update that contains the transfer of `100` to Alice.
aliceCoinEx5D' :: Coin
aliceCoinEx5D' = aliceCoinEx5D - 1

txbodyEx5D' :: HashAlgorithm h => MIRPot -> TxBody h
txbodyEx5D' pot =
  TxBody
    (Set.fromList [TxIn (txid $ txbodyEx5D pot) 0])
    (StrictSeq.singleton $ TxOut aliceAddr aliceCoinEx5D')
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    ( (slotFromEpoch $ EpochNo 1)
        +* Duration (randomnessStabilisationWindow testGlobals) + SlotNo 7
    )
    SNothing
    SNothing

txEx5D' :: HashAlgorithm h => MIRPot -> Tx h
txEx5D' pot =
  Tx
    (txbodyEx5D' pot)
    mempty
      { addrWits = makeWitnessesVKey (hashTxBody $ txbodyEx5D' pot) [alicePay]
      }
    SNothing

blockEx5D' :: forall h. HashAlgorithm h => MIRPot -> Block h
blockEx5D' pot =
  mkBlock
    (bhHash (bheader $ blockEx5D pot))
    (slotKeys s)
    [txEx5D' pot]
    (slot)
    (BlockNo 2)
    (mkSeqNonce p 1)
    (NatNonce 2)
    zero
    7
    0
    (mkOCert (slotKeys s) 0 (KESPeriod 0))
  where
    slot@(SlotNo s) =
      (slotFromEpoch $ EpochNo 1)
        +* Duration (randomnessStabilisationWindow testGlobals) + SlotNo 7
    p :: Proxy h
    p = Proxy

-- | The third transaction in the next epoch applies the reward update to 1)
-- register a staking credential for Alice, 2) deducing the key deposit from the
-- 100 and to 3) create the reward account with an initial amount of 93.
aliceCoinEx5D'' :: Coin
aliceCoinEx5D'' = aliceCoinEx5D' - 1

txbodyEx5D'' :: HashAlgorithm h => MIRPot -> TxBody h
txbodyEx5D'' pot =
  TxBody
    (Set.fromList [TxIn (txid $ txbodyEx5D' pot) 0])
    (StrictSeq.singleton $ TxOut aliceAddr aliceCoinEx5D'')
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    ((slotFromEpoch $ EpochNo 2) + SlotNo 10)
    SNothing
    SNothing

txEx5D'' :: HashAlgorithm h => MIRPot -> Tx h
txEx5D'' pot =
  Tx
    (txbodyEx5D'' pot)
    mempty {addrWits = makeWitnessesVKey (hashTxBody $ txbodyEx5D'' pot) [alicePay]}
    SNothing

blockEx5D'' :: HashAlgorithm h => MIRPot -> Nonce -> Block h
blockEx5D'' pot epochNonce =
  mkBlock
    (bhHash (bheader $ blockEx5D' pot))
    (slotKeys s)
    [txEx5D'' pot]
    (slot)
    (BlockNo 3)
    epochNonce
    (NatNonce 1)
    zero
    10
    10
    (mkOCert (slotKeys s) 0 (KESPeriod 10))
  where
    slot@(SlotNo s) = (slotFromEpoch $ EpochNo 2) + SlotNo 10

ex5D' :: forall proxy h. HashAlgorithm h => proxy h -> MIRPot -> Either [[PredicateFailure (CHAIN h)]] (ChainState h)
ex5D' _p pot = do
  nextState <- runShelleyBase $ applySTS @(CHAIN h) (TRC ((), initStEx5A, blockEx5D pot))
  midState <-
    runShelleyBase $
      applySTS @(CHAIN h) (TRC ((), nextState, blockEx5D' pot))
  let finalEpochNonce = (chainCandidateNonce midState) ⭒ (chainPrevEpochNonce midState)
  finalState <-
    runShelleyBase $ applySTS @(CHAIN h) (TRC ((), midState, blockEx5D'' pot finalEpochNonce))

  pure finalState

ex5DReserves' :: HashAlgorithm h => proxy h -> Either [[PredicateFailure (CHAIN h)]] (ChainState h)
ex5DReserves' p = ex5D' p ReservesMIR

ex5DTreasury' :: HashAlgorithm h => proxy h -> Either [[PredicateFailure (CHAIN h)]] (ChainState h)
ex5DTreasury' p = ex5D' p TreasuryMIR

-- | Tests that after getting instantaneous rewards, creating the update and
-- then applying the update, Alice's key is actually registered, the key deposit
-- value deducted and the remaining value credited as reward.
test5D :: HashAlgorithm h => proxy h -> MIRPot -> Assertion
test5D p pot = do
  case ex5D' p pot of
    Left e -> assertFailure (show e)
    Right ex5DState -> do
      let getDState = _dstate . _delegationState . esLState . nesEs . chainNes
          ds = getDState ex5DState
          StakeCreds stkCreds = _stkCreds ds
          rews = _rewards ds
          rewEntry = rews Map.!? (mkRwdAcnt Testnet aliceSHK)
      assertBool "Alice's credential not in stkCreds" (aliceSHK `Map.member` stkCreds)
      assertBool "Alice's reward account does not exist" $ isJust rewEntry
      assertBool "Alice's rewards are wrong" $ maybe False (== Coin 100) rewEntry
      assertBool "Total amount of ADA is not preserved" $ maxLLSupply == totalAda ex5DState

test5DReserves :: HashAlgorithm h => proxy h -> Assertion
test5DReserves p = test5D p ReservesMIR

test5DTreasury :: HashAlgorithm h => proxy h -> Assertion
test5DTreasury p = test5D p TreasuryMIR

-- * Example 6A - apply CHAIN transition to re-register a stake pool late in the epoch
-- This example continues on from example 2A.

feeEx6A :: Coin
feeEx6A = Coin 3

aliceCoinEx6A :: Coin
aliceCoinEx6A = aliceCoinEx2A - feeEx6A

alicePoolParams6A :: HashAlgorithm h => PoolParams h
alicePoolParams6A = alicePoolParams {_poolCost = Coin 500}

txbodyEx6A :: HashAlgorithm h => TxBody h
txbodyEx6A =
  TxBody
    (Set.fromList [TxIn (txid txbodyEx2A) 0])
    (StrictSeq.fromList [TxOut aliceAddr aliceCoinEx6A])
    ( StrictSeq.fromList
        ( [ DCertPool (RegPool alicePoolParams6A)
          ]
        )
    )
    (Wdrl Map.empty)
    feeEx6A
    (SlotNo 100)
    SNothing
    SNothing

txEx6A :: HashAlgorithm h => Tx h
txEx6A =
  Tx
    txbodyEx6A
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashTxBody txbodyEx6A)
            ( (asWitness <$> [alicePay])
                <> (asWitness <$> [aliceStake])
            ),
        regWits =
          makeWitnessesVKey
            (hashTxBody txbodyEx6A)
            ([asWitness $ cold (alicePool p)])
      }
    SNothing
  where
    p :: Proxy h
    p = Proxy

earlySlotEx6 :: Word64
earlySlotEx6 = 20

lateSlotEx6 :: Word64
lateSlotEx6 = 90

word64SlotToKesPeriodWord :: Word64 -> Word
word64SlotToKesPeriodWord slot =
  (fromIntegral $ toInteger slot) `div` (fromIntegral $ toInteger $ slotsPerKESPeriod testGlobals)

blockEx6A :: forall h. HashAlgorithm h => Word64 -> Block h
blockEx6A slot =
  mkBlock
    blockEx2AHash
    (slotKeys slot)
    [txEx6A]
    (SlotNo slot)
    (BlockNo 2)
    (nonce0 p)
    (NatNonce 2)
    zero
    (word64SlotToKesPeriodWord slot)
    0
    (mkOCert (slotKeys slot) 0 (KESPeriod 0))
  where
    p :: Proxy h
    p = Proxy

blockEx6AHash :: HashAlgorithm h => Word64 -> HashHeader h
blockEx6AHash slot = bhHash (bheader $ blockEx6A slot)

psEx6A :: HashAlgorithm h => PState h
psEx6A = psEx2A {_fPParams = Map.singleton (hk (alicePool p)) alicePoolParams6A}
  where
    p :: Proxy h
    p = Proxy

expectedLSEx6A :: HashAlgorithm h => LedgerState h
expectedLSEx6A =
  LedgerState
    ( UTxOState
        ( UTxO . Map.fromList $
            [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin),
              (TxIn (txid txbodyEx6A) 0, TxOut aliceAddr aliceCoinEx6A)
            ]
        )
        (Coin 271)
        (Coin 3 + feeEx6A)
        ppupEx2A
    )
    (DPState dsEx2A psEx6A)

rewardUpdateEx6A :: StrictMaybe (RewardUpdate h)
rewardUpdateEx6A = SNothing

rewardUpdateEx6A' :: StrictMaybe (RewardUpdate h)
rewardUpdateEx6A' = SJust emptyRewardUpdate

candidateNonceEx6A :: HashAlgorithm h => proxy h -> Nonce
candidateNonceEx6A p = nonce0 p ⭒ mkNonce 1 ⭒ mkNonce 2

candidateNonceEx6A' :: HashAlgorithm h => proxy h -> Nonce
candidateNonceEx6A' p = nonce0 p ⭒ mkNonce 1

expectedStEx6A :: forall h. HashAlgorithm h => Word64 -> StrictMaybe (RewardUpdate h) -> Nonce -> ChainState h
expectedStEx6A slot ru cn =
  ChainState
    ( NewEpochState
        (EpochNo 0)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        (EpochState (acntEx2A p) emptySnapShots expectedLSEx6A ppsEx1 ppsEx1 emptyNonMyopic)
        ru
        (PoolDistr Map.empty)
        (overlayScheduleFor (EpochNo 0))
    )
    oCertIssueNosEx1
    (nonce0 p)
    (nonce0 p ⭒ mkNonce 1 ⭒ mkNonce 2)
    cn
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 2)
          (SlotNo slot)
          (blockEx6AHash slot)
    )
  where
    p :: Proxy h
    p = Proxy

ex6A :: HashAlgorithm h => proxy h -> CHAINExample h
ex6A p =
  CHAINExample
    expectedStEx2A
    (blockEx6A earlySlotEx6)
    (Right $ expectedStEx6A earlySlotEx6 rewardUpdateEx6A (candidateNonceEx6A p))

ex6A' :: HashAlgorithm h => proxy h -> CHAINExample h
ex6A' p =
  CHAINExample
    expectedStEx2A
    (blockEx6A lateSlotEx6)
    (Right $ expectedStEx6A lateSlotEx6 rewardUpdateEx6A' (candidateNonceEx6A' p))

-- * Example 6B - If The TICK rule is applied to the NewEpochState
-- in expectedStEx6A, then the future pool parameters should be adopted

ex6BExpectedNES :: forall h. HashAlgorithm h => NewEpochState h
ex6BExpectedNES = chainNes (expectedStEx6A earlySlotEx6 rewardUpdateEx6A (candidateNonceEx6A p))
  where
    p :: Proxy h
    p = Proxy

ex6BExpectedNES' :: forall h. HashAlgorithm h => NewEpochState h
ex6BExpectedNES' = chainNes (expectedStEx6A lateSlotEx6 rewardUpdateEx6A' (candidateNonceEx6A' p))
  where
    p :: Proxy h
    p = Proxy

ex6BPoolParams :: HashAlgorithm h => Map (KeyHash h 'StakePool) (PoolParams h)
ex6BPoolParams = Map.singleton (hk (alicePool p)) alicePoolParams6A
  where
    p :: Proxy h
    p = Proxy
