{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Examples
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
  , ex4C
  , ex5A
  , ex5B
  , ex6A
  , ex6B
  , ex6C
  , ex6D
  , ex6E
  , test6F
  , maxLovelaceSupply
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
  -- helpers
  , unsafeMkUnitInterval
  )
where

import           Test.Tasty.HUnit (Assertion, assertBool, assertFailure)

import           Cardano.Crypto.Hash (ShortHash)
import           ConcreteCryptoTypes (AVUpdate, Addr, Applications, Block, CHAIN, ChainState,
                     Credential, DState, EpochState, GenKeyHash, HashHeader, KeyHash, KeyPair,
                     LedgerState, Mdt, NewEpochState, PPUpdate, PState, PoolDistr, PoolParams,
                     RewardAcnt, SKey, SnapShots, Stake, Tx, TxBody, UTxO, UTxOState, Update,
                     UpdateState, VKeyGenesis, hashKeyVRF)
import qualified Data.ByteString.Char8 as BS (pack)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, empty, fromList, insert, keysSet, member, singleton,
                     (!?))
import           Data.Maybe (isJust, maybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T (pack)
import           Data.Word (Word64)
import           Numeric.Natural (Natural)
import           Unsafe.Coerce (unsafeCoerce)

import           Address (mkRwdAcnt)
import           BaseTypes (Nonce (..), mkNonce, startRewards, (⭒))
import           BlockChain (pattern HashHeader, bhHash, bheader, hashHeaderToNonce)
import           Coin (Coin (..))
import           Control.State.Transition.Extended (PredicateFailure, TRC (..), applySTS)
import           Delegation.Certificates (pattern DeRegKey, pattern Delegate,
                     pattern GenesisDelegate, pattern MIRCert, pattern PoolDistr, pattern RegKey,
                     pattern RegPool, pattern RetirePool)
import           EpochBoundary (BlocksMade (..), pattern SnapShots, pattern Stake, emptySnapShots,
                     _feeSS, _poolsSS, _pstakeGo, _pstakeMark, _pstakeSet)
import           Generator.Core.QuickCheck (AllPoolKeys (..), NatNonce (..), genesisAccountState,
                     maxLovelaceSupply, mkBlock, mkOCert, zero)
import           Keys (pattern GenDelegs, Hash, pattern KeyPair, hash, hashKey, vKey)
import           LedgerState (AccountState (..), pattern DPState, pattern EpochState,
                     pattern LedgerState, pattern NewEpochState, pattern RewardUpdate,
                     pattern UTxOState, deltaF, deltaR, deltaT, emptyDState, emptyPState,
                     esAccountState, esLState, esPp, genesisCoins, genesisId, nesEs,
                     overlaySchedule, rs, _delegationState, _delegations, _dstate, _fGenDelegs,
                     _genDelegs, _irwd, _pParams, _ptrs, _reserves, _retiring, _rewards, _stPools,
                     _stkCreds, _treasury)
import           OCert (KESPeriod (..))
import           PParams (PParams (..), emptyPParams)
import           Slot (BlockNo (..), Duration (..), EpochNo (..), SlotNo (..), (+*))
import           STS.Bbody (pattern LedgersFailure)
import           STS.Chain (pattern BbodyFailure, pattern ChainState, chainNes, initialShelleyState,
                     totalAda)
import           STS.Deleg (pattern InsufficientForInstantaneousRewardsDELEG)
import           STS.Delegs (pattern DelplFailure)
import           STS.Delpl (pattern DelegFailure)
import           STS.Ledger (pattern DelegsFailure, pattern UtxowFailure)
import           STS.Ledgers (pattern LedgerFailure)
import           STS.Utxow (pattern MIRImpossibleInDecentralizedNetUTXOW,
                     pattern MIRInsufficientGenesisSigsUTXOW)
import           Test.Utils
import           Tx (pattern Tx)
import           TxData (pattern AddrPtr, pattern DCertDeleg, pattern DCertGenesis,
                     pattern DCertMir, pattern DCertPool, pattern Delegation, pattern KeyHashObj,
                     pattern PoolParams, Ptr (..), pattern RewardAcnt, pattern StakeCreds,
                     pattern StakePools, pattern TxBody, pattern TxIn, pattern TxOut, Wdrl (..),
                     addStakeCreds, _poolCost, _poolMargin, _poolOwners, _poolPledge, _poolPubKey,
                     _poolRAcnt, _poolVrf)
import qualified TxData (TxBody (..))
import           Updates (pattern AVUpdate, ApName (..), ApVer (..), pattern Applications,
                     InstallerHash (..), pattern Mdt, pattern PPUpdate, PParamsUpdate (..),
                     Ppm (..), SystemTag (..), pattern Update, pattern UpdateState, emptyUpdate,
                     emptyUpdateState)
import           UTxO (pattern UTxO, balance, makeGenWitnessesVKey, makeWitnessesVKey, txid)

data CHAINExample =
  CHAINExample { currentSlotNo    :: SlotNo       -- ^ Current slot
               , startState     :: ChainState -- ^ State to start testing with
               , newBlock       :: Block      -- ^ Block to run chain state transition system on
               , intendedResult :: (Either [[PredicateFailure CHAIN]] -- ^ type of fatal error, if failure expected
                                           ChainState                 --   and final chain state if success expected
                                   )
               }

data MIRExample =
  MIRExample
  { mirStkCred :: Credential
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

coreNodes :: [((SKey, VKeyGenesis), AllPoolKeys)]
coreNodes = [(mkGenKey (x, 0, 0, 0, 0), mkAllPoolKeys x) | x <-[101..100+numCoreNodes]]

coreNodeSKG :: Int -> SKey
coreNodeSKG = fst . fst . (coreNodes !!)

coreNodeVKG :: Int -> VKeyGenesis
coreNodeVKG = snd . fst . (coreNodes !!)

coreNodeKeys :: Int -> AllPoolKeys
coreNodeKeys = snd . (coreNodes !!)

genDelegs :: Map GenKeyHash KeyHash
genDelegs = Map.fromList [ (hashKey $ snd gkey, hashKey . vKey $ cold pkeys) | (gkey, pkeys) <- coreNodes]

-- | There are only two applications on test Byron blockchain:
byronApps :: Applications
byronApps = Applications $ Map.fromList
                            [ (ApName $ T.pack "Daedalus", (ApVer 16, Mdt Map.empty))
                            , (ApName $ T.pack "Yoroi", (ApVer 4, Mdt Map.empty))
                            ]

alicePay :: KeyPair
alicePay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (0, 0, 0, 0, 0)

aliceStake :: KeyPair
aliceStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (1, 1, 1, 1, 1)

alicePool :: AllPoolKeys
alicePool = mkAllPoolKeys 1

aliceAddr :: Addr
aliceAddr = mkAddr (alicePay, aliceStake)

aliceSHK :: Credential
aliceSHK = (KeyHashObj . hashKey . vKey) aliceStake

bobPay :: KeyPair
bobPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (2, 2, 2, 2, 2)

bobStake :: KeyPair
bobStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (3, 3, 3, 3, 3)

bobAddr :: Addr
bobAddr = mkAddr (bobPay, bobStake)

bobSHK :: Credential
bobSHK = (KeyHashObj . hashKey . vKey) bobStake

aliceInitCoin :: Coin
aliceInitCoin = 10000

bobInitCoin :: Coin
bobInitCoin = 1000

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
    }


-- | Helper Functions

-- |The first block of the Shelley era will point back to the last block of the Byron era.
-- For our purposes in this test we can bootstrap the chain by just coercing the value.
-- When this transition actually occurs, the consensus layer will do the work of making
-- sure that the hash gets translated across the fork
lastByronHeaderHash :: HashHeader
lastByronHeaderHash = HashHeader $ unsafeCoerce (hash 0 :: Hash ShortHash Int)

nonce0 :: Nonce
nonce0 = hashHeaderToNonce lastByronHeaderHash

mkSeqNonce :: Natural -> Nonce
mkSeqNonce m = foldl (\c x -> c ⭒ mkNonce x) nonce0 [1.. m]

carlPay :: KeyPair
carlPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (4, 4, 4, 4, 4)

carlStake :: KeyPair
carlStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (5, 5, 5, 5, 5)

carlAddr :: Addr
carlAddr = mkAddr (carlPay, carlStake)

carlSHK :: Credential
carlSHK = (KeyHashObj . hashKey . vKey) carlStake


dariaPay :: KeyPair
dariaPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (6, 6, 6, 6, 6)

dariaStake :: KeyPair
dariaStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (7, 7, 7, 7, 7)

dariaAddr :: Addr
dariaAddr = mkAddr (dariaPay, dariaStake)

dariaSHK :: Credential
dariaSHK = (KeyHashObj . hashKey . vKey) dariaStake

-- * Example 1 - apply CHAIN transition to an empty block

-- | Empty set of UTxOs. No coins to be spent.
utxostEx1 :: UTxOState
utxostEx1 = UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState

dsEx1 :: DState
dsEx1 = emptyDState { _genDelegs = GenDelegs genDelegs }

oCertIssueNosEx1 :: Map KeyHash Natural
oCertIssueNosEx1 = Map.fromList (fmap f (Map.elems genDelegs))
  where f vk = (vk, 0)

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
                      , _activeSlotCoeff = unsafeMkUnitInterval 0.9
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
esEx1 = EpochState acntEx1 emptySnapShots lsEx1 ppsEx1

-- | Empty initial Shelley state with fake Byron hash and no blocks at all.
--   No blocks of Shelley have been processed yet.
initStEx1 :: ChainState
initStEx1 = initialShelleyState
  (SlotNo 0)
  (BlockNo 0)
  (EpochNo 0)
  lastByronHeaderHash
  (UTxO Map.empty)
  maxLovelaceSupply
  genDelegs
  (Map.singleton (SlotNo 1) (Just . hashKey $ coreNodeVKG 0))
  (Applications Map.empty)
  ppsEx1

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
     Nothing
     (PoolDistr Map.empty)
     (Map.singleton (SlotNo 1) (Just . hashKey $ coreNodeVKG 0)))
  oCertIssueNosEx1
  nonce0
  (nonce0 ⭒ mkNonce 1)
  (nonce0 ⭒ mkNonce 1)
  NeutralNonce
  (bhHash (bheader blockEx1))
  (SlotNo 1)
  (BlockNo 1)

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
ppupEx2A :: PPUpdate
ppupEx2A = PPUpdate $ Map.singleton
                        (hashKey $ coreNodeVKG 0) -- stake key
                        (PParamsUpdate $ Set.singleton (PoolDeposit 255))

-- | Update proposal that just changes protocol parameters,
--   and does not change applications.
updateEx2A :: Update
updateEx2A = Update ppupEx2A (AVUpdate Map.empty) (Just $ EpochNo 0)

-- | Transaction body to be processed.
txbodyEx2A :: TxBody
txbodyEx2A = TxBody
           (Set.fromList [TxIn genesisId 0])
           (Seq.fromList [TxOut aliceAddr (Coin 9726)])
           (Seq.fromList ([ DCertDeleg (RegKey aliceSHK)
           , DCertDeleg (RegKey bobSHK)
           , DCertDeleg (RegKey carlSHK)
           , DCertPool (RegPool alicePoolParams)
           ] ++ [DCertMir (MIRCert (Map.fromList [ (carlSHK, 110)
                                                 , (dariaSHK, 99)]))]))
           (Wdrl Map.empty)
           (Coin 3)
           (SlotNo 10)
           updateEx2A
           Nothing

txEx2A :: Tx
txEx2A = Tx
          txbodyEx2A
          (makeWitnessesVKey
            txbodyEx2A
            [alicePay, carlPay, aliceStake, cold alicePool, cold $ coreNodeKeys 0]
            -- Note that Alice's stake key needs to sign this transaction
            -- since it is an owner of the stake pool being registered,
            -- and *not* because of the stake key registration.
                     `Set.union`
           makeGenWitnessesVKey txbodyEx2A [ KeyPair (coreNodeVKG 0) (coreNodeSKG 0)
             , KeyPair (coreNodeVKG 1) (coreNodeSKG 1)
             , KeyPair (coreNodeVKG 2) (coreNodeSKG 2)
             , KeyPair (coreNodeVKG 3) (coreNodeSKG 3)
             , KeyPair (coreNodeVKG 4) (coreNodeSKG 4)
             ])
          Map.empty
          Nothing

-- | Pointer address to address of Alice address.
alicePtrAddr :: Addr
alicePtrAddr = AddrPtr (KeyHashObj . hashKey $ vKey alicePay) (Ptr (SlotNo 10) 0 0)

usEx2A :: UpdateState
usEx2A = UpdateState (PPUpdate Map.empty) (AVUpdate Map.empty) Map.empty byronApps

utxostEx2A :: UTxOState
utxostEx2A = UTxOState utxoEx2A (Coin 0) (Coin 0) usEx2A

lsEx2A :: LedgerState
lsEx2A = LedgerState utxostEx2A (DPState dsEx1 psEx1)

acntEx2A :: AccountState
acntEx2A = AccountState
            { _treasury = Coin 0
            , _reserves = maxLovelaceSupply - balance utxoEx2A
            }

esEx2A :: EpochState
esEx2A = EpochState acntEx2A emptySnapShots lsEx2A ppsEx1

overlayEx2A :: Map SlotNo (Maybe GenKeyHash)
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
               Nothing
               (PoolDistr Map.empty)
               overlayEx2A


initStEx2A :: ChainState
initStEx2A = initialShelleyState
  (SlotNo 0)
  (BlockNo 0)
  (EpochNo 0)
  lastByronHeaderHash
  utxoEx2A
  (maxLovelaceSupply - balance utxoEx2A)
  genDelegs
  overlayEx2A
  byronApps
  ppsEx1

blockEx2A :: Block
blockEx2A = mkBlock
             lastByronHeaderHash
             (coreNodeKeys 2)
             [txEx2A]
             (SlotNo 10)
             (BlockNo 1)
             (mkNonce 0)
             (NatNonce 1)
             zero
             0
             0
             (mkOCert (coreNodeKeys 2) 0 (KESPeriod 0))

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

updateStEx2A :: UpdateState
updateStEx2A = UpdateState
  ppupEx2A
  (AVUpdate Map.empty)
  Map.empty
  byronApps

expectedLSEx2A :: LedgerState
expectedLSEx2A = LedgerState
               (UTxOState
                 (UTxO . Map.fromList $
                   [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
                   , (TxIn (txid txbodyEx2A) 0, TxOut aliceAddr (Coin 9726))
                   ])
                 (Coin 271)
                 (Coin 3)
                 updateStEx2A)
               (DPState dsEx2A psEx2A)

blockEx2AHash :: HashHeader
blockEx2AHash = bhHash (bheader blockEx2A)

-- | Expected state after update is processed and STS applied.
expectedStEx2A :: ChainState
expectedStEx2A = ChainState
  (NewEpochState
     (EpochNo   0)
     (BlocksMade Map.empty) -- ^ Still no blocks
     (BlocksMade Map.empty) -- ^ Still no blocks
     (EpochState acntEx2A emptySnapShots expectedLSEx2A ppsEx1)
     Nothing
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
  blockEx2AHash
  (SlotNo 10)
  (BlockNo 1)

ex2A :: CHAINExample
ex2A = CHAINExample (SlotNo 10) initStEx2A blockEx2A (Right expectedStEx2A)


-- * Example 2B - process a block late enough in the epoch in order to create a reward update.

-- | The transaction delegates Alice's and Bob's stake to Alice's pool.
--   Additionally, we split Alice's ADA between a base address and a pointer address.
txbodyEx2B :: TxBody
txbodyEx2B = TxBody
      { TxData._inputs   = Set.fromList [TxIn (txid txbodyEx2A) 0]
      , TxData._outputs  = Seq.fromList [ TxOut aliceAddr    (Coin 722)
                                        , TxOut alicePtrAddr (Coin 9000) ]
      -- | Delegation certificates
      , TxData._certs    =
        Seq.fromList [ DCertDeleg (Delegate $ Delegation aliceSHK (hk alicePool))
                     , DCertDeleg (Delegate $ Delegation bobSHK   (hk alicePool))]
      , TxData._wdrls    = Wdrl Map.empty
      , TxData._txfee    = Coin 4
      , TxData._ttl      = SlotNo 90
      , TxData._txUpdate = emptyUpdate
      , TxData._mdHash   = Nothing
      }

txEx2B :: Tx
txEx2B = Tx
          txbodyEx2B -- ^ Body of the transaction
          (makeWitnessesVKey txbodyEx2B [alicePay, aliceStake, bobStake])
                     -- ^ Witness verification key set
          Map.empty  -- ^ Witness signature map
          Nothing

blockEx2B :: Block
blockEx2B = mkBlock
             blockEx2AHash    -- ^ Hash of previous block
             (coreNodeKeys 5)
             [txEx2B]         -- ^ Single transaction to record
             (SlotNo 90)        -- ^ Current slot
             (BlockNo 2)
             (mkNonce 0)      -- ^ Epoch nonce
             (NatNonce 2)     -- ^ Block nonce
             zero             -- ^ Praos leader value
             4                -- ^ Period of KES (key evolving signature scheme)
             0
             (mkOCert (coreNodeKeys 5) 0 (KESPeriod 0))

blockEx2BHash :: HashHeader
blockEx2BHash = bhHash (bheader blockEx2B)

utxoEx2B :: UTxO
utxoEx2B = UTxO . Map.fromList $
                   [ (TxIn genesisId 1,         TxOut bobAddr      bobInitCoin) -- ^ Pay Bob from Genesis transaction
                   , (TxIn (txid txbodyEx2B) 0, TxOut aliceAddr    (Coin  722)) -- ^ Pay alice 722 coins from txEx2B
                   , (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr (Coin 9000)) -- ^ And reward Alice's pointer address with 9000 coins
                   ]

-- | Both Alice and Bob delegate to the Alice pool
delegsEx2B :: Map Credential KeyHash
delegsEx2B = Map.fromList
              [ (aliceSHK, hk alicePool)
              , (bobSHK,   hk alicePool)
              ]

dsEx2B :: DState
dsEx2B = dsEx2A { _delegations = delegsEx2B
                , _irwd = Map.fromList [ (carlSHK, Coin 110)
                                       , (dariaSHK, Coin 99)] }

expectedLSEx2B :: LedgerState
expectedLSEx2B = LedgerState
               (UTxOState
                 utxoEx2B
                 (Coin 271)
                 (Coin 7)
                 updateStEx2A)
               (DPState dsEx2B psEx2A)

expectedStEx2Bgeneric :: PParams -> ChainState
expectedStEx2Bgeneric pp = ChainState
  -- | New state of the epoch
  (NewEpochState
     (EpochNo   0)            -- ^ First epoch
     (BlocksMade Map.empty) -- ^ Blocks made before current
     (BlocksMade Map.empty) -- ^ Blocks made before current
     (EpochState acntEx2A emptySnapShots expectedLSEx2B pp)
                            -- ^ Previous epoch state
     (Just RewardUpdate { deltaT        = Coin 0
                        , deltaR        = Coin 0
                        , rs            = Map.empty
                        , deltaF        = Coin 0
                        })  -- ^ Update reward
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  nonce0
  (nonce0 ⭒ mkNonce 1 ⭒ mkNonce 2) -- ^ Evolving nonce
  (nonce0 ⭒ mkNonce 1)             -- ^ Candidate nonce
  NeutralNonce
  blockEx2BHash                    -- ^ Hash header of the chain
  (SlotNo 90)                      -- ^ Current slot
  (BlockNo 2)                      -- ^ Current block no

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
             blockEx2BHash    -- ^ Hash of previous block
             (coreNodeKeys 2)
             []               -- ^ No transactions at all (empty block)
             (SlotNo 110)       -- ^ Current slot
             (BlockNo 3)      -- ^ Second block within the epoch
             (mkNonce 0)      -- ^ Epoch nonce
             (NatNonce 3)     -- ^ Block nonce
             zero             -- ^ Praos leader value
             5                -- ^ Period of KES (key evolving signature scheme)
             0
             (mkOCert (coreNodeKeys 2) 0 (KESPeriod 0))

epoch1OSchedEx2C :: Map SlotNo (Maybe GenKeyHash)
epoch1OSchedEx2C = runShelleyBase $ overlaySchedule
                    (EpochNo 1)
                    (Map.keysSet genDelegs)
                    ppsEx1

-- | Snapshot of stakes for Alice and Bob
snapEx2C :: (Stake, Map Credential KeyHash)
snapEx2C = ( Stake ( Map.fromList [(aliceSHK, Coin 9722), (bobSHK, bobInitCoin)])
          , delegsEx2B )

-- | Make a snapshot for a given fee.
snapsEx2Cgeneric :: Coin -> SnapShots
snapsEx2Cgeneric feeSnapShot = emptySnapShots {
    _pstakeMark = snapEx2C -- ^ snapshot of stake pools and parameters
  , _poolsSS    = Map.singleton (hk alicePool) alicePoolParams -- ^ single pool of Alice
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
    usEx2A) -- Note that the ppup is gone now
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
     (EpochState acntEx2A { _reserves = _reserves acntEx2A - Coin 110 } ss ls pp)
     Nothing
     (PoolDistr Map.empty)
     epoch1OSchedEx2C)
  oCertIssueNosEx1
  (nonce0 ⭒ mkNonce 1)
  (mkSeqNonce 3)
  (mkSeqNonce 3)
  (hashHeaderToNonce blockEx2BHash)
  blockEx2CHash
  (SlotNo 110)
  (BlockNo 3)

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
txbodyEx2D :: TxBody
txbodyEx2D = TxBody
      { TxData._inputs   = Set.fromList [TxIn (txid txbodyEx2B) 0]
      , TxData._outputs  = Seq.fromList [ TxOut aliceAddr    (Coin 717) ]
      , TxData._certs    =
        Seq.fromList [ DCertDeleg (Delegate $ Delegation carlSHK (hk alicePool)) ]
      , TxData._wdrls    = Wdrl Map.empty
      , TxData._txfee    = Coin 5
      , TxData._ttl      = SlotNo 500
      , TxData._txUpdate = emptyUpdate
      , TxData._mdHash   = Nothing
      }

txEx2D :: Tx
txEx2D = Tx
          txbodyEx2D
          (makeWitnessesVKey txbodyEx2D [alicePay, carlStake])
          Map.empty
          Nothing

blockEx2D :: Block
blockEx2D = mkBlock
             blockEx2CHash
             (coreNodeKeys 5)
             [txEx2D]
             (SlotNo 190)
             (BlockNo 4)
             (mkNonce 0 ⭒ mkNonce 1)
             (NatNonce 4)
             zero
             9
             0
             (mkOCert (coreNodeKeys 5) 0 (KESPeriod 0))

blockEx2DHash :: HashHeader
blockEx2DHash = bhHash (bheader blockEx2D)

utxoEx2D :: UTxO
utxoEx2D = UTxO . Map.fromList $
                   [ (TxIn genesisId 1,         TxOut bobAddr      bobInitCoin)
                   , (TxIn (txid txbodyEx2D) 0, TxOut aliceAddr    (Coin  717))
                   , (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr (Coin 9000))
                   ]

delegsEx2D :: Map Credential KeyHash
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
                 usEx2A)
               (DPState dsEx2D psEx2A)

expectedStEx2D :: ChainState
expectedStEx2D = ChainState
  (NewEpochState
     (EpochNo 1)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A { _reserves = _reserves acntEx2A - Coin 110} snapsEx2C expectedLSEx2D ppsEx1)
     (Just RewardUpdate { deltaT        = Coin 21
                        , deltaR        = Coin 0
                        , rs            = Map.empty
                        , deltaF        = Coin (-21)
                        })
     (PoolDistr Map.empty)
     epoch1OSchedEx2C)
  oCertIssueNosEx1
  (nonce0 ⭒ mkNonce 1)
  (mkSeqNonce 4)
  (mkSeqNonce 3)
  (hashHeaderToNonce blockEx2BHash)
  blockEx2DHash
  (SlotNo 190)
  (BlockNo 4)

ex2D :: CHAINExample
ex2D = CHAINExample (SlotNo 190) expectedStEx2C blockEx2D (Right expectedStEx2D)


-- | Example 2E - create the first non-empty pool distribution
-- by creating a block in the third epoch of this running example.


blockEx2E :: Block
blockEx2E = mkBlock
             blockEx2DHash
             (coreNodeKeys 5)
             []
             (SlotNo 220)
             (BlockNo 5)
             ((mkSeqNonce 3) ⭒ (hashHeaderToNonce blockEx2BHash))
             (NatNonce 5)
             zero
             11
             10
             (mkOCert (coreNodeKeys 5) 1 (KESPeriod 10))

epoch1OSchedEx2E :: Map SlotNo (Maybe GenKeyHash)
epoch1OSchedEx2E = runShelleyBase $ overlaySchedule
                    (EpochNo 2)
                    (Map.keysSet genDelegs)
                    ppsEx1

snapEx2E :: (Stake, Map Credential KeyHash)
snapEx2E = ( Stake ( Map.fromList [ (aliceSHK, Coin 9717)
                                  , (carlSHK, Coin 110)
                                  , (bobSHK, bobInitCoin)])
          , delegsEx2D )

snapsEx2E :: SnapShots
snapsEx2E = emptySnapShots { _pstakeMark = snapEx2E
                           , _pstakeSet = snapEx2C
                           , _poolsSS = Map.singleton (hk alicePool) alicePoolParams
                           , _feeSS = Coin 19
                           }

expectedLSEx2E :: LedgerState
expectedLSEx2E = LedgerState
               (UTxOState
                 utxoEx2D
                 (Coin 243)
                 (Coin 19)
                 usEx2A)
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
            , _reserves = maxLovelaceSupply - balance utxoEx2A - (Coin 110)
            }

oCertIssueNosEx2 :: Map KeyHash Natural
oCertIssueNosEx2 =
  Map.insert (hashKey $ vKey $ cold $ coreNodeKeys 5) 1 oCertIssueNosEx1

expectedStEx2E :: ChainState
expectedStEx2E = ChainState
  (NewEpochState
     (EpochNo 2)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2E snapsEx2E expectedLSEx2E ppsEx1)
     Nothing
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
  blockEx2EHash
  (SlotNo 220)
  (BlockNo 5)

ex2E :: CHAINExample
ex2E = CHAINExample (SlotNo 220) expectedStEx2D blockEx2E (Right expectedStEx2E)


-- | Example 2F - create a decentralized Praos block (ie one not in the overlay schedule)

oCertIssueNosEx2F :: Map KeyHash Natural
oCertIssueNosEx2F = Map.insert (hk alicePool) 0 oCertIssueNosEx2

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
     (EpochState acntEx2E snapsEx2E expectedLSEx2E ppsEx1)
     (Just RewardUpdate { deltaT        = Coin 19
                        , deltaR        = Coin 0
                        , rs            = Map.empty
                        , deltaF        = Coin (-19)
                        })
     pdEx2F
     epoch1OSchedEx2E)
  oCertIssueNosEx2F
  ((mkSeqNonce 3) ⭒ (hashHeaderToNonce blockEx2BHash))
  (mkSeqNonce 6)
  (mkSeqNonce 5)
  (hashHeaderToNonce blockEx2DHash)
  blockEx2FHash
  (SlotNo 295)
  (BlockNo 6)

ex2F :: CHAINExample
ex2F = CHAINExample (SlotNo 295) expectedStEx2E blockEx2F (Right expectedStEx2F)


-- | Example 2G - create an empty block in the next epoch
-- to prepare the way for the first non-trivial reward update


blockEx2G :: Block
blockEx2G = mkBlock
             blockEx2FHash
             (coreNodeKeys 2)
             []
             (SlotNo 310)
             (BlockNo 7)
             (mkSeqNonce 5)
             (NatNonce 7)
             zero
             15
             15
             (mkOCert (coreNodeKeys 2) 1 (KESPeriod 15))

blockEx2GHash :: HashHeader
blockEx2GHash = bhHash (bheader blockEx2G)

epoch1OSchedEx2G :: Map SlotNo (Maybe GenKeyHash)
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
                 usEx2A)
               (DPState
                 dsEx2D
                 psEx2A)

oCertIssueNosEx2G :: Map KeyHash Natural
oCertIssueNosEx2G =
  Map.insert (hashKey $ vKey $ cold $ coreNodeKeys 2) 1 oCertIssueNosEx2F

acntEx2G :: AccountState
acntEx2G = acntEx2E { _treasury = Coin 40 }

expectedStEx2G :: ChainState
expectedStEx2G = ChainState
  (NewEpochState
     (EpochNo 3)
     (BlocksMade $ Map.singleton (hk alicePool) 1)
     (BlocksMade Map.empty)
     (EpochState acntEx2G snapsEx2G expectedLSEx2G ppsEx1)
     Nothing
     pdEx2F
     epoch1OSchedEx2G)
  oCertIssueNosEx2G
  ((mkSeqNonce 5) ⭒ (hashHeaderToNonce blockEx2DHash))
  (mkSeqNonce 7)
  (mkSeqNonce 7)
  (hashHeaderToNonce blockEx2FHash)
  blockEx2GHash
  (SlotNo 310)
  (BlockNo 7)

ex2G :: CHAINExample
ex2G = CHAINExample (SlotNo 310) expectedStEx2F blockEx2G (Right expectedStEx2G)


-- | Example 2H - create the first non-trivial reward update


blockEx2H :: Block
blockEx2H = mkBlock
             blockEx2GHash
             (coreNodeKeys 5)
             []
             (SlotNo 390)
             (BlockNo 8)
             (mkSeqNonce 5)
             (NatNonce 8)
             zero
             19
             19
             (mkOCert (coreNodeKeys 5) 2 (KESPeriod 19))

blockEx2HHash :: HashHeader
blockEx2HHash = bhHash (bheader blockEx2H)

aliceRAcnt2H :: Coin
aliceRAcnt2H = Coin 7694907666

bobRAcnt2H :: Coin
bobRAcnt2H = Coin 705092333

rewardsEx2H :: Map RewardAcnt Coin
rewardsEx2H = Map.fromList [ (RewardAcnt aliceSHK, aliceRAcnt2H)
                          , (RewardAcnt bobSHK, bobRAcnt2H) ]

oCertIssueNosEx2H :: Map KeyHash Natural
oCertIssueNosEx2H =
  Map.insert (hashKey $ vKey $ cold $ coreNodeKeys 5) 2 oCertIssueNosEx2G

expectedStEx2H :: ChainState
expectedStEx2H = ChainState
  (NewEpochState
     (EpochNo 3)
     (BlocksMade $ Map.singleton (hk alicePool) 1)
     (BlocksMade Map.empty)
     (EpochState acntEx2G snapsEx2G expectedLSEx2G ppsEx1)
     (Just RewardUpdate { deltaT        = Coin 1041600000010
                        , deltaR        = Coin (-1049999999999)
                        , rs            = rewardsEx2H
                        , deltaF        = Coin (-10)
                        })
     pdEx2F
     epoch1OSchedEx2G)
  oCertIssueNosEx2H
  ((mkSeqNonce 5) ⭒ (hashHeaderToNonce blockEx2DHash))
  (mkSeqNonce 8)
  (mkSeqNonce 7)
  (hashHeaderToNonce blockEx2FHash)
  blockEx2HHash
  (SlotNo 390)
  (BlockNo 8)

ex2H :: CHAINExample
ex2H = CHAINExample (SlotNo 390) expectedStEx2G blockEx2H (Right expectedStEx2H)


-- | Example 2I - apply the first non-trivial reward update


blockEx2I :: Block
blockEx2I = mkBlock
              blockEx2HHash
              (coreNodeKeys 2)
              []
              (SlotNo 410)
              (BlockNo 9)
              (mkSeqNonce 7)
              (NatNonce 9)
              zero
              20
              20
              (mkOCert (coreNodeKeys 2) 2 (KESPeriod 20))

blockEx2IHash :: HashHeader
blockEx2IHash = bhHash (bheader blockEx2I)

epoch1OSchedEx2I :: Map SlotNo (Maybe GenKeyHash)
epoch1OSchedEx2I = runShelleyBase $ overlaySchedule
                     (EpochNo 4)
                     (Map.keysSet genDelegs)
                     ppsEx1

acntEx2I :: AccountState
acntEx2I = AccountState
            { _treasury = Coin 1041600000050
            , _reserves = Coin 44998949999988891
            }

dsEx2I :: DState
dsEx2I = dsEx2D { _rewards = Map.insert (mkRwdAcnt carlSHK) 110 rewardsEx2H }

expectedLSEx2I :: LedgerState
expectedLSEx2I = LedgerState
               (UTxOState
                 utxoEx2D
                 (Coin 224)
                 (Coin 9)
                 usEx2A)
               (DPState dsEx2I psEx2A)

snapsEx2I :: SnapShots
snapsEx2I = snapsEx2G { _pstakeMark =
                          (Stake ( Map.fromList [ (bobSHK, Coin 1000 + bobRAcnt2H)
                                                , (aliceSHK, Coin 9717 + aliceRAcnt2H)
                                                , (carlSHK, Coin 110)])
                          , delegsEx2D )
                        -- The stake snapshots have bigger values now, due to the new rewards
                      , _pstakeSet = snapEx2E
                      , _pstakeGo = snapEx2E
                      , _feeSS = Coin 9
                      }

oCertIssueNosEx2I :: Map KeyHash Natural
oCertIssueNosEx2I =
  Map.insert (hashKey $ vKey $ cold $ coreNodeKeys 2) 2 oCertIssueNosEx2H

expectedStEx2I :: ChainState
expectedStEx2I = ChainState
  (NewEpochState
     (EpochNo 4)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2I snapsEx2I expectedLSEx2I ppsEx1)
     Nothing
     pdEx2F
     epoch1OSchedEx2I)
  oCertIssueNosEx2I
  ((mkSeqNonce 7) ⭒ (hashHeaderToNonce blockEx2FHash))
  (mkSeqNonce 9)
  (mkSeqNonce 9)
  (hashHeaderToNonce blockEx2HHash)
  blockEx2IHash
  (SlotNo 410)
  (BlockNo 9)

ex2I :: CHAINExample
ex2I = CHAINExample (SlotNo 410) expectedStEx2H blockEx2I (Right expectedStEx2I)


-- | Example 2J - drain reward account and de-register stake key

bobAda2J :: Coin
bobAda2J = bobRAcnt2H -- reward account
                   + Coin 1000 -- txin we will consume (must spend at least one)
                   + Coin 4 -- stake registration refund
                   - Coin 9 -- tx fee

txbodyEx2J :: TxBody
txbodyEx2J = TxBody
           (Set.fromList [TxIn genesisId 1]) --
           (Seq.singleton $ TxOut bobAddr bobAda2J)
           (Seq.fromList [DCertDeleg (DeRegKey bobSHK)])
           (Wdrl $ Map.singleton (RewardAcnt bobSHK) bobRAcnt2H)
           (Coin 9)
           (SlotNo 500)
           emptyUpdate
           Nothing

txEx2J :: Tx
txEx2J = Tx
          txbodyEx2J
          (makeWitnessesVKey txbodyEx2J [bobPay, bobStake])
          Map.empty
          Nothing

blockEx2J :: Block
blockEx2J = mkBlock
              blockEx2IHash
              (coreNodeKeys 5)
              [txEx2J]
              (SlotNo 420)
              (BlockNo 10)
              (mkSeqNonce 7)
              (NatNonce 10)
              zero
              21
              19
              (mkOCert (coreNodeKeys 5) 2 (KESPeriod 19))

blockEx2JHash :: HashHeader
blockEx2JHash = bhHash (bheader blockEx2J)

utxoEx2J :: UTxO
utxoEx2J = UTxO . Map.fromList $
                   [ (TxIn (txid txbodyEx2J) 0, TxOut bobAddr bobAda2J)
                   , (TxIn (txid txbodyEx2D) 0, TxOut aliceAddr (Coin 717))
                   , (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr (Coin 9000))
                   ]

dsEx2J :: DState
dsEx2J = dsEx1
          { _ptrs = Map.fromList [ (Ptr (SlotNo 10) 0 0, aliceSHK)
                                 , (Ptr (SlotNo 10) 0 2, carlSHK)]
          , _stkCreds = StakeCreds $ Map.fromList [(aliceSHK, SlotNo 10), (carlSHK, SlotNo 10)]
          , _delegations = Map.fromList [(aliceSHK, hk alicePool), (carlSHK, hk alicePool)]
          , _rewards = Map.fromList [(RewardAcnt aliceSHK, aliceRAcnt2H), (RewardAcnt carlSHK, Coin 110)]
          }

expectedLSEx2J :: LedgerState
expectedLSEx2J = LedgerState
               (UTxOState
                 utxoEx2J
                 (Coin (219 - 4) + 5)
                 (Coin 18)
                 usEx2A)
               (DPState dsEx2J psEx2A)

oCertIssueNosEx2J :: Map KeyHash Natural
oCertIssueNosEx2J =
  Map.insert (hashKey $ vKey $ cold $ coreNodeKeys 2) 2 oCertIssueNosEx2H

expectedStEx2J :: ChainState
expectedStEx2J = ChainState
  (NewEpochState
     (EpochNo 4)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2I snapsEx2I expectedLSEx2J ppsEx1)
     Nothing
     pdEx2F
     epoch1OSchedEx2I)
  oCertIssueNosEx2J
  ((mkSeqNonce 7) ⭒ (hashHeaderToNonce blockEx2FHash))
  (mkSeqNonce 10)
  (mkSeqNonce 10)
  (hashHeaderToNonce blockEx2HHash)
  blockEx2JHash
  (SlotNo 420)
  (BlockNo 10)

ex2J :: CHAINExample
ex2J = CHAINExample (SlotNo 420) expectedStEx2I blockEx2J (Right expectedStEx2J)


-- | Example 2K - start stake pool retirement


txbodyEx2K :: TxBody
txbodyEx2K = TxBody
           (Set.fromList [TxIn (txid txbodyEx2D) 0])
           (Seq.singleton $ TxOut alicePtrAddr 715)
           (Seq.fromList [DCertPool (RetirePool (hk alicePool) (EpochNo 5))])
           (Wdrl Map.empty)
           (Coin 2)
           (SlotNo 500)
           emptyUpdate
           Nothing

txEx2K :: Tx
txEx2K = Tx
          txbodyEx2K
          (makeWitnessesVKey txbodyEx2K [cold alicePool, alicePay])
          Map.empty
          Nothing

blockEx2K :: Block
blockEx2K = mkBlock
              blockEx2JHash
              (coreNodeKeys 5)
              [txEx2K]
              (SlotNo 490)
              (BlockNo 11)
              (mkSeqNonce 7)
              (NatNonce 11)
              zero
              24
              19
              (mkOCert (coreNodeKeys 5) 2 (KESPeriod 19))

blockEx2KHash :: HashHeader
blockEx2KHash = bhHash (bheader blockEx2K)

utxoEx2K :: UTxO
utxoEx2K = UTxO . Map.fromList $
                   [ (TxIn (txid txbodyEx2J) 0, TxOut bobAddr bobAda2J)
                   , (TxIn (txid txbodyEx2K) 0, TxOut alicePtrAddr (Coin 715))
                   , (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr (Coin 9000))
                   ]

psEx2K :: PState
psEx2K = psEx2A { _retiring = Map.singleton (hk alicePool) (EpochNo 5) }

expectedLSEx2K :: LedgerState
expectedLSEx2K = LedgerState
               (UTxOState
                 utxoEx2K
                 (Coin 220)
                 (Coin 20)
                 usEx2A)
               (DPState dsEx2J psEx2K)

expectedStEx2K :: ChainState
expectedStEx2K = ChainState
  (NewEpochState
     (EpochNo 4)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2I snapsEx2I expectedLSEx2K ppsEx1)
     (Just RewardUpdate { deltaT        = Coin 9
                        , deltaR        = Coin 0
                        , rs            = Map.empty
                        , deltaF        = Coin (-9)
                        })
     pdEx2F
     epoch1OSchedEx2I)
  oCertIssueNosEx2J
  ((mkSeqNonce 7) ⭒ (hashHeaderToNonce blockEx2FHash))
  (mkSeqNonce 11)
  (mkSeqNonce 10)
  (hashHeaderToNonce blockEx2HHash)
  blockEx2KHash
  (SlotNo 490)
  (BlockNo 11)

ex2K :: CHAINExample
ex2K = CHAINExample (SlotNo 490) expectedStEx2J blockEx2K (Right expectedStEx2K)


-- | Example 2L - reap a stake pool


blockEx2L :: Block
blockEx2L = mkBlock
              blockEx2KHash
              (coreNodeKeys 2)
              []
              (SlotNo 510)
              (BlockNo 12)
              (mkSeqNonce 10)
              (NatNonce 12)
              zero
              25
              25
              (mkOCert (coreNodeKeys 2) 3 (KESPeriod 25))

blockEx2LHash :: HashHeader
blockEx2LHash = bhHash (bheader blockEx2L)

acntEx2L :: AccountState
acntEx2L = acntEx2I { _treasury =  _treasury acntEx2I --previous amount
                                  + Coin 9 } -- from the reward update

snapsEx2L :: SnapShots
snapsEx2L = SnapShots { _pstakeMark =
                          (Stake (
                            Map.fromList [ (aliceSHK, aliceRAcnt2H + 9000 + 715)
                                         , (carlSHK, 110)])
                          , Map.fromList [ (aliceSHK, hk alicePool), (carlSHK, hk alicePool) ])
                      , _pstakeSet = _pstakeMark snapsEx2I
                      , _pstakeGo = _pstakeSet snapsEx2I
                      , _poolsSS = Map.singleton (hk alicePool) alicePoolParams
                      , _feeSS = Coin 22
                      }
dsEx2L :: DState
dsEx2L = dsEx1
          { _ptrs = Map.fromList [ (Ptr (SlotNo 10) 0 0, aliceSHK)
                                 , (Ptr (SlotNo 10) 0 2, carlSHK)
                                 ]
          , _stkCreds = StakeCreds $ Map.fromList [(aliceSHK, SlotNo 10), (carlSHK, SlotNo 10)]
          , _rewards = Map.fromList [ (RewardAcnt aliceSHK, aliceRAcnt2H + Coin 201)
                                    , (RewardAcnt carlSHK, Coin 110)]
                       -- Note the pool cert refund of 201
          }

expectedLSEx2L :: LedgerState
expectedLSEx2L = LedgerState
               (UTxOState
                 utxoEx2K
                 (Coin 4 + 4)
                 (Coin 22)
                 usEx2A)
               (DPState dsEx2L psEx1) -- Note the stake pool is reaped


oCertIssueNosEx2L :: Map KeyHash Natural
oCertIssueNosEx2L =
  Map.insert (hashKey $ vKey $ cold $ coreNodeKeys 2) 3 oCertIssueNosEx2J

expectedStEx2L :: ChainState
expectedStEx2L = ChainState
  (NewEpochState
     (EpochNo 5)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2L snapsEx2L expectedLSEx2L ppsEx1)
     Nothing
     pdEx2F
     (runShelleyBase $ overlaySchedule (EpochNo 5) (Map.keysSet genDelegs) ppsEx1))
  oCertIssueNosEx2L
  ((mkSeqNonce 10) ⭒ (hashHeaderToNonce blockEx2HHash))
  (mkSeqNonce 12)
  (mkSeqNonce 12)
  (hashHeaderToNonce blockEx2KHash)
  blockEx2LHash
  (SlotNo 510)
  (BlockNo 12)

ex2L :: CHAINExample
ex2L = CHAINExample (SlotNo 510) expectedStEx2K blockEx2L (Right expectedStEx2L)


-- | Example 3A - Setting up for a successful protocol parameter update,
-- have three genesis keys vote on the same new parameters


ppVote3A :: PParamsUpdate
ppVote3A = PParamsUpdate $ Set.fromList [ExtraEntropy (mkNonce 123), PoolDeposit 200]

ppupEx3A :: PPUpdate
ppupEx3A = PPUpdate $ Map.fromList [ (hashKey $ coreNodeVKG 0, ppVote3A)
                                   , (hashKey $ coreNodeVKG 3, ppVote3A)
                                   , (hashKey $ coreNodeVKG 4, ppVote3A)
                                   ]

updateEx3A :: Update
updateEx3A = Update ppupEx3A (AVUpdate Map.empty) (Just $ EpochNo 0)

txbodyEx3A :: TxBody
txbodyEx3A = TxBody
           (Set.fromList [TxIn genesisId 0])
           (Seq.singleton $ TxOut aliceAddr (Coin 9999))
           Seq.empty
           (Wdrl Map.empty)
           (Coin 1)
           (SlotNo 10)
           updateEx3A
           Nothing

txEx3A :: Tx
txEx3A = Tx
          txbodyEx3A
          (makeWitnessesVKey
            txbodyEx3A
            [ alicePay
            , cold $ coreNodeKeys 0
            , cold $ coreNodeKeys 3
            , cold $ coreNodeKeys 4
            ])
          Map.empty
          Nothing

blockEx3A :: Block
blockEx3A = mkBlock
             lastByronHeaderHash
             (coreNodeKeys 2)
             [txEx3A]
             (SlotNo 10)
             (BlockNo 1)
             (mkNonce 0)
             (NatNonce 1)
             zero
             0
             0
             (mkOCert (coreNodeKeys 2) 0 (KESPeriod 0))

updateStEx3A :: UpdateState
updateStEx3A = UpdateState
  ppupEx3A
  (AVUpdate Map.empty)
  Map.empty
  byronApps

expectedLSEx3A :: LedgerState
expectedLSEx3A = LedgerState
               (UTxOState
                 (UTxO . Map.fromList $
                   [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
                   , (TxIn (txid txbodyEx3A) 0, TxOut aliceAddr (Coin 9999))
                   ])
                 (Coin 0)
                 (Coin 1)
                 updateStEx3A)
               (DPState dsEx1 psEx1)

blockEx3AHash :: HashHeader
blockEx3AHash = bhHash (bheader blockEx3A)

expectedStEx3A :: ChainState
expectedStEx3A = ChainState
  (NewEpochState
     (EpochNo 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx3A ppsEx1)
     Nothing
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  nonce0
  (nonce0 ⭒ mkNonce 1)
  (nonce0 ⭒ mkNonce 1)
  NeutralNonce
  blockEx3AHash
  (SlotNo 10)
  (BlockNo 1)

ex3A :: CHAINExample
ex3A = CHAINExample (SlotNo 10) initStEx2A blockEx3A (Right expectedStEx3A)


-- | Example 3B - Finish getting enough votes for the protocol parameter update.


ppupEx3B :: PPUpdate
ppupEx3B = PPUpdate $ Map.fromList [ (hashKey $ coreNodeVKG 1, ppVote3A)
                                   , (hashKey $ coreNodeVKG 5, ppVote3A)
                                   ]

updateEx3B :: Update
updateEx3B = Update ppupEx3B (AVUpdate Map.empty) (Just $ EpochNo 0)

txbodyEx3B :: TxBody
txbodyEx3B = TxBody
           (Set.fromList [TxIn (txid txbodyEx3A) 0])
           (Seq.singleton $ TxOut aliceAddr (Coin 9998))
           Seq.empty
           (Wdrl Map.empty)
           (Coin 1)
           (SlotNo 31)
           updateEx3B
           Nothing

txEx3B :: Tx
txEx3B = Tx
          txbodyEx3B
          (makeWitnessesVKey
            txbodyEx3B
            [ alicePay
            , cold $ coreNodeKeys 1
            , cold $ coreNodeKeys 5
            ])
          Map.empty
          Nothing

blockEx3B :: Block
blockEx3B = mkBlock
             blockEx3AHash
             (coreNodeKeys 5)
             [txEx3B]
             (SlotNo 20)
             (BlockNo 2)
             (mkNonce 0)
             (NatNonce 2)
             zero
             1
             0
             (mkOCert (coreNodeKeys 5) 0 (KESPeriod 0))

updateStEx3B :: UpdateState
updateStEx3B = UpdateState
  (PPUpdate $ Map.fromList $ fmap (\n -> (hashKey $ coreNodeVKG n, ppVote3A))
    [0, 1, 3, 4, 5])
  (AVUpdate Map.empty)
  Map.empty
  byronApps

utxoEx3B :: UTxO
utxoEx3B = UTxO . Map.fromList $
             [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
             , (TxIn (txid txbodyEx3B) 0, TxOut aliceAddr (Coin 9998))
             ]

expectedLSEx3B :: LedgerState
expectedLSEx3B = LedgerState
               (UTxOState
                 utxoEx3B
                 (Coin 0)
                 (Coin 2)
                 updateStEx3B)
               (DPState dsEx1 psEx1)

blockEx3BHash :: HashHeader
blockEx3BHash = bhHash (bheader blockEx3B)

expectedStEx3B :: ChainState
expectedStEx3B = ChainState
  (NewEpochState
     (EpochNo 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx3B ppsEx1)
     Nothing
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  nonce0
  (mkSeqNonce 2)
  (mkSeqNonce 2)
  NeutralNonce
  blockEx3BHash
  (SlotNo 20)
  (BlockNo 2)

ex3B :: CHAINExample
ex3B = CHAINExample (SlotNo 20) expectedStEx3A blockEx3B (Right expectedStEx3B)


-- | Example 3C - Adopt protocol parameter update


blockEx3C :: Block
blockEx3C = mkBlock
             blockEx3BHash
             (coreNodeKeys 2)
             []
             (SlotNo 110)
             (BlockNo 3)
             (mkSeqNonce 2)
             (NatNonce 3)
             zero
             5
             0
             (mkOCert (coreNodeKeys 2) 0 (KESPeriod 0))

blockEx3CHash :: HashHeader
blockEx3CHash = bhHash (bheader blockEx3C)

overlayEx3C :: Map SlotNo (Maybe GenKeyHash)
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
                 usEx2A)
               (DPState dsEx1 psEx1)

ppsEx3C :: PParams
ppsEx3C = ppsEx1 { _poolDeposit = Coin 200, _extraEntropy = mkNonce 123 }

expectedStEx3C :: ChainState
expectedStEx3C = ChainState
  (NewEpochState
     (EpochNo 1)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A snapsEx3C expectedLSEx3C ppsEx3C)
     Nothing
     (PoolDistr Map.empty)
     overlayEx3C)
  oCertIssueNosEx1
  (mkSeqNonce 2 ⭒ mkNonce 123)
  (mkSeqNonce 3)
  (mkSeqNonce 3)
  (hashHeaderToNonce blockEx3BHash)
  blockEx3CHash
  (SlotNo 110)
  (BlockNo 3)

ex3C :: CHAINExample
ex3C = CHAINExample (SlotNo 110) expectedStEx3B blockEx3C (Right expectedStEx3C)


-- | Example 4A - Setting up for a successful application version update,
-- have three genesis keys vote on the same new version


daedalusMDEx4A :: Mdt
daedalusMDEx4A = Mdt $ Map.singleton
                              (SystemTag $ T.pack "DOS")
                              (InstallerHash $ hash $ BS.pack "ABC")

appsEx4A :: Applications
appsEx4A = Applications $ Map.singleton
                            (ApName $ T.pack "Daedalus")
                            (ApVer 17, daedalusMDEx4A)

avupEx4A :: AVUpdate
avupEx4A = AVUpdate $ Map.fromList [ (hashKey $ coreNodeVKG 0, appsEx4A)
                                   , (hashKey $ coreNodeVKG 3, appsEx4A)
                                   , (hashKey $ coreNodeVKG 4, appsEx4A)
                                   ]

updateEx4A :: Update
updateEx4A = Update (PPUpdate Map.empty) avupEx4A Nothing

txbodyEx4A :: TxBody
txbodyEx4A = TxBody
           (Set.fromList [TxIn genesisId 0])
           (Seq.singleton $ TxOut aliceAddr (Coin 9999))
           Seq.empty
           (Wdrl Map.empty)
           (Coin 1)
           (SlotNo 10)
           updateEx4A
           Nothing

txEx4A :: Tx
txEx4A = Tx
          txbodyEx4A
          (makeWitnessesVKey
            txbodyEx4A
            [ alicePay
            , cold $ coreNodeKeys 0
            , cold $ coreNodeKeys 3
            , cold $ coreNodeKeys 4
            ])
          Map.empty
          Nothing

blockEx4A :: Block
blockEx4A = mkBlock
             lastByronHeaderHash
             (coreNodeKeys 2)
             [txEx4A]
             (SlotNo 10)
             (BlockNo 1)
             (mkNonce 0)
             (NatNonce 1)
             zero
             0
             0
             (mkOCert (coreNodeKeys 2) 0 (KESPeriod 0))

updateStEx4A :: UpdateState
updateStEx4A = UpdateState
  (PPUpdate Map.empty)
  avupEx4A
  Map.empty
  byronApps

expectedLSEx4A :: LedgerState
expectedLSEx4A = LedgerState
               (UTxOState
                 (UTxO . Map.fromList $
                   [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
                   , (TxIn (txid txbodyEx4A) 0, TxOut aliceAddr (Coin 9999))
                   ])
                 (Coin 0)
                 (Coin 1)
                 updateStEx4A)
               (DPState dsEx1 psEx1)

blockEx4AHash :: HashHeader
blockEx4AHash = bhHash (bheader blockEx4A)

expectedStEx4A :: ChainState
expectedStEx4A = ChainState
  (NewEpochState
     (EpochNo 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx4A ppsEx1)
     Nothing
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  nonce0
  (nonce0 ⭒ mkNonce 1)
  (nonce0 ⭒ mkNonce 1)
  NeutralNonce
  blockEx4AHash
  (SlotNo 10)
  (BlockNo 1)

ex4A :: CHAINExample
ex4A = CHAINExample (SlotNo 10) initStEx2A blockEx4A (Right expectedStEx4A)


-- | Example 4B - Finish getting enough votes for the application version update.

avupEx4B :: AVUpdate
avupEx4B = AVUpdate $ Map.fromList [ (hashKey $ coreNodeVKG 1, appsEx4A)
                                   , (hashKey $ coreNodeVKG 5, appsEx4A)
                                   ]

updateEx4B :: Update
updateEx4B = Update (PPUpdate Map.empty) avupEx4B Nothing

txbodyEx4B :: TxBody
txbodyEx4B = TxBody
           (Set.fromList [TxIn (txid txbodyEx4A) 0])
           (Seq.singleton $ TxOut aliceAddr (Coin 9998))
           Seq.empty
           (Wdrl Map.empty)
           (Coin 1)
           (SlotNo 31)
           updateEx4B
           Nothing

txEx4B :: Tx
txEx4B = Tx
          txbodyEx4B
          (makeWitnessesVKey
            txbodyEx4B
            [ alicePay
            , cold $ coreNodeKeys 1
            , cold $ coreNodeKeys 5
            ])
          Map.empty
          Nothing

blockEx4B :: Block
blockEx4B = mkBlock
             blockEx4AHash
             (coreNodeKeys 5)
             [txEx4B]
             (SlotNo 20)
             (BlockNo 2)
             (mkNonce 0)
             (NatNonce 2)
             zero
             1
             0
             (mkOCert (coreNodeKeys 5) 0 (KESPeriod 0))

updateStEx4B :: UpdateState
updateStEx4B = UpdateState
  (PPUpdate Map.empty)
  (AVUpdate Map.empty)
  (Map.singleton (SlotNo 53) appsEx4A)
  byronApps

utxoEx4B :: UTxO
utxoEx4B = UTxO . Map.fromList $
             [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
             , (TxIn (txid txbodyEx4B) 0, TxOut aliceAddr (Coin 9998))
             ]

expectedLSEx4B :: LedgerState
expectedLSEx4B = LedgerState
               (UTxOState
                 utxoEx4B
                 (Coin 0)
                 (Coin 2)
                 updateStEx4B)
               (DPState dsEx1 psEx1)

blockEx4BHash :: HashHeader
blockEx4BHash = bhHash (bheader blockEx4B)

expectedStEx4B :: ChainState
expectedStEx4B = ChainState
  (NewEpochState
     (EpochNo 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx4B ppsEx1)
     Nothing
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  nonce0
  (mkSeqNonce 2)
  (mkSeqNonce 2)
  NeutralNonce
  blockEx4BHash
  (SlotNo 20)
  (BlockNo 2)

ex4B :: CHAINExample
ex4B = CHAINExample (SlotNo 20) expectedStEx4A blockEx4B (Right expectedStEx4B)


-- | Example 4C - Adopt application version update


blockEx4C :: Block
blockEx4C = mkBlock
             blockEx4BHash
             (coreNodeKeys 3)
             []
             (SlotNo 60)
             (BlockNo 3)
             (mkNonce 0)
             (NatNonce 3)
             zero
             3
             0
             (mkOCert (coreNodeKeys 3) 0 (KESPeriod 0))

updateStEx4C :: UpdateState
updateStEx4C = UpdateState
  (PPUpdate Map.empty)
  (AVUpdate Map.empty)
  Map.empty
  (Applications $ Map.fromList
                     [ (ApName $ T.pack "Daedalus", (ApVer 17, daedalusMDEx4A))
                     , (ApName $ T.pack "Yoroi", (ApVer 4, Mdt Map.empty))
                     ])

expectedLSEx4C :: LedgerState
expectedLSEx4C = LedgerState
               (UTxOState
                 utxoEx4B
                 (Coin 0)
                 (Coin 2)
                 updateStEx4C)
               (DPState dsEx1 psEx1)

blockEx4CHash :: HashHeader
blockEx4CHash = bhHash (bheader blockEx4C)

expectedStEx4C :: ChainState
expectedStEx4C = ChainState
  (NewEpochState
     (EpochNo 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx4C ppsEx1)
     (Just RewardUpdate { deltaT        = Coin 0
                        , deltaR        = Coin 0
                        , rs            = Map.empty
                        , deltaF        = Coin 0
                        })
      (PoolDistr Map.empty)
      overlayEx2A)
  oCertIssueNosEx1
  nonce0
  (mkSeqNonce 3)
  (mkSeqNonce 3)
  NeutralNonce
  blockEx4CHash
  (SlotNo 60)
  (BlockNo 3)


ex4C :: CHAINExample
ex4C = CHAINExample (SlotNo 60) expectedStEx4B blockEx4C (Right expectedStEx4C)


-- | Example 5A - Genesis key delegation


newGenDelegate :: KeyPair
newGenDelegate  = KeyPair vkCold skCold
  where (skCold, vkCold) = mkKeyPair (108, 0, 0, 0, 1)

txbodyEx5A :: TxBody
txbodyEx5A = TxBody
              (Set.fromList [TxIn genesisId 0])
              (Seq.singleton $ TxOut aliceAddr (Coin 9999))
              (Seq.fromList [DCertGenesis (GenesisDelegate
                                       ( (hashKey . coreNodeVKG) 0
                                       , (hashKey . vKey) newGenDelegate))])
              (Wdrl Map.empty)
              (Coin 1)
              (SlotNo 10)
              emptyUpdate
              Nothing

txEx5A :: Tx
txEx5A = Tx
           txbodyEx5A
           (makeWitnessesVKey txbodyEx5A [ alicePay ] `Set.union` makeGenWitnessesVKey txbodyEx5A [ KeyPair (coreNodeVKG 0) (coreNodeSKG 0) ])
           Map.empty
           Nothing

blockEx5A :: Block
blockEx5A = mkBlock
              lastByronHeaderHash
              (coreNodeKeys 2)
              [txEx5A]
              (SlotNo 10)
              (BlockNo 1)
              (mkNonce 0)
              (NatNonce 1)
              zero
              0
              0
              (mkOCert (coreNodeKeys 2) 0 (KESPeriod 0))

blockEx5AHash :: HashHeader
blockEx5AHash = bhHash (bheader blockEx5A)

dsEx5A :: DState
dsEx5A = dsEx1 { _fGenDelegs = Map.singleton
                          ( SlotNo 43, hashKey $ coreNodeVKG 0 )
                          ( (hashKey . vKey) newGenDelegate ) }

utxoEx5A :: UTxO
utxoEx5A = UTxO . Map.fromList $
                    [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
                    , (TxIn (txid txbodyEx5A) 0, TxOut aliceAddr (Coin 9999))
                    ]

expectedLSEx5A :: LedgerState
expectedLSEx5A = LedgerState
               (UTxOState
                 utxoEx5A
                 (Coin 0)
                 (Coin 1)
                 (UpdateState (PPUpdate Map.empty) (AVUpdate Map.empty) Map.empty byronApps))
               (DPState dsEx5A psEx1)

expectedStEx5A :: ChainState
expectedStEx5A = ChainState
  (NewEpochState
     (EpochNo 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx5A ppsEx1)
     Nothing
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  nonce0
  (nonce0 ⭒ mkNonce 1)
  (nonce0 ⭒ mkNonce 1)
  NeutralNonce
  blockEx5AHash
  (SlotNo 10)
  (BlockNo 1)

ex5A :: CHAINExample
ex5A = CHAINExample (SlotNo 10) initStEx2A blockEx5A (Right expectedStEx5A)


-- | Example 5B - New genesis key delegation updated from future delegations

blockEx5B :: Block
blockEx5B = mkBlock
             blockEx5AHash
             (coreNodeKeys 1)
             []
             (SlotNo 50)
             (BlockNo 2)
             (mkNonce 0)
             (NatNonce 2)
             zero
             2
             0
             (mkOCert (coreNodeKeys 1) 0 (KESPeriod 0))

blockEx5BHash :: HashHeader
blockEx5BHash = bhHash (bheader blockEx5B)

dsEx5B :: DState
dsEx5B = dsEx5A { _fGenDelegs = Map.empty
                , _genDelegs = GenDelegs $ Map.insert
                                 ((hashKey . coreNodeVKG) 0)
                                 ((hashKey . vKey) newGenDelegate)
                                 genDelegs }

expectedLSEx5B :: LedgerState
expectedLSEx5B = LedgerState
               (UTxOState
                 utxoEx5A
                 (Coin 0)
                 (Coin 1)
                 (UpdateState (PPUpdate Map.empty) (AVUpdate Map.empty) Map.empty byronApps))
               (DPState dsEx5B psEx1)

expectedStEx5B :: ChainState
expectedStEx5B = ChainState
  (NewEpochState
     (EpochNo 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx5B ppsEx1)
     (Just RewardUpdate { deltaT        = Coin 0
                        , deltaR        = Coin 0
                        , rs            = Map.empty
                        , deltaF        = Coin 0
                        })
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  nonce0
  (mkSeqNonce 2)
  (mkSeqNonce 2)
  NeutralNonce
  blockEx5BHash
  (SlotNo 50)
  (BlockNo 2)

ex5B :: CHAINExample
ex5B = CHAINExample (SlotNo 50) expectedStEx5A blockEx5B (Right expectedStEx5B)


-- | Example 6A - Genesis key delegation


ir :: Map Credential Coin
ir = Map.fromList [(aliceSHK, Coin 100)]

txbodyEx6A :: TxBody
txbodyEx6A = TxBody
              (Set.fromList [TxIn genesisId 0])
              (Seq.singleton $ TxOut aliceAddr (Coin 9999))
              (Seq.fromList [DCertMir (MIRCert ir)])
              (Wdrl Map.empty)
              (Coin 1)
              (SlotNo 10)
              emptyUpdate
              Nothing

txEx6A :: Tx
txEx6A = Tx
           txbodyEx6A
           (makeWitnessesVKey txbodyEx6A [ alicePay ] `Set.union` makeGenWitnessesVKey txbodyEx6A
             [ KeyPair (coreNodeVKG 0) (coreNodeSKG 0)
             , KeyPair (coreNodeVKG 1) (coreNodeSKG 1)
             , KeyPair (coreNodeVKG 2) (coreNodeSKG 2)
             , KeyPair (coreNodeVKG 3) (coreNodeSKG 3)
             , KeyPair (coreNodeVKG 4) (coreNodeSKG 4)
           ])
           Map.empty
           Nothing

blockEx6A :: Block
blockEx6A = mkBlock
              lastByronHeaderHash
              (coreNodeKeys 2)
              [txEx6A]
              (SlotNo 10)
              (BlockNo 1)
              (mkNonce 0)
              (NatNonce 1)
              zero
              0
              0
              (mkOCert (coreNodeKeys 2) 0 (KESPeriod 0))

blockEx6AHash :: HashHeader
blockEx6AHash = bhHash (bheader blockEx6A)

utxoEx6A :: UTxO
utxoEx6A = UTxO . Map.fromList $
                    [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
                    , (TxIn (txid txbodyEx6A) 0, TxOut aliceAddr (Coin 9999))
                    ]

dsEx6A :: DState
dsEx6A = dsEx1 { _irwd = Map.fromList [(aliceSHK, Coin 100)] }

expectedLSEx6A :: LedgerState
expectedLSEx6A = LedgerState
               (UTxOState
                 utxoEx6A
                 (Coin 0)
                 (Coin 1)
                 (UpdateState (PPUpdate Map.empty) (AVUpdate Map.empty) Map.empty byronApps))
               (DPState dsEx6A psEx1)

expectedStEx6A :: ChainState
expectedStEx6A = ChainState
  (NewEpochState
     (EpochNo 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx6A ppsEx1)
     Nothing
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  nonce0
  (nonce0 ⭒ mkNonce 1)
  (nonce0 ⭒ mkNonce 1)
  NeutralNonce
  blockEx6AHash
  (SlotNo 10)
  (BlockNo 1)

ex6A :: CHAINExample
ex6A = CHAINExample (SlotNo 10) initStEx2A blockEx6A (Right expectedStEx6A)


-- | Example 6B - Instantaneous rewards with insufficient core node signatures

txEx6B :: Tx
txEx6B = Tx
           txbodyEx6A
           (makeWitnessesVKey txbodyEx6A [ alicePay ] `Set.union` makeGenWitnessesVKey txbodyEx6A
             [ KeyPair (coreNodeVKG 0) (coreNodeSKG 0)
             , KeyPair (coreNodeVKG 1) (coreNodeSKG 1)
             , KeyPair (coreNodeVKG 2) (coreNodeSKG 2)
             , KeyPair (coreNodeVKG 3) (coreNodeSKG 3)
           ])
           Map.empty
           Nothing

blockEx6B :: Block
blockEx6B = mkBlock
              lastByronHeaderHash
              (coreNodeKeys 2)
              [txEx6B]
              (SlotNo 10)
              (BlockNo 1)
              (mkNonce 0)
              (NatNonce 1)
              zero
              0
              0
              (mkOCert (coreNodeKeys 2) 0 (KESPeriod 0))

expectedStEx6B :: PredicateFailure CHAIN
expectedStEx6B = BbodyFailure (LedgersFailure (LedgerFailure (UtxowFailure MIRInsufficientGenesisSigsUTXOW)))

ex6B :: CHAINExample
ex6B = CHAINExample (SlotNo 10) initStEx2A blockEx6B (Left [[expectedStEx6B]])

-- | Example 6C - Instantaneous rewards in decentralized era

expectedStEx6C :: PredicateFailure CHAIN
expectedStEx6C = BbodyFailure (LedgersFailure (LedgerFailure (UtxowFailure MIRImpossibleInDecentralizedNetUTXOW)))

ex6C :: CHAINExample
ex6C =
  CHAINExample
   (SlotNo 10)
   (initStEx2A { chainNes = initNesEx2A { nesEs = esEx2A { esPp = ppsEx1 { _d = unsafeMkUnitInterval 0 }}}})
   blockEx6A
   (Left [[expectedStEx6C]])


-- | Example 6D - Instantaneous rewards in decentralized era and not enough core
-- signatures

ex6D :: CHAINExample
ex6D =
  CHAINExample
   (SlotNo 10)
   (initStEx2A { chainNes = initNesEx2A { nesEs = esEx2A { esPp = ppsEx1 { _d = unsafeMkUnitInterval 0 }}}})
   blockEx6B
   (Left [[expectedStEx6C, expectedStEx6B]])

-- | Example 6E - Instantaneous rewards that overrun the available reserves

ex6E :: CHAINExample
ex6E =
  CHAINExample
   (SlotNo 10)
   (initStEx2A { chainNes = initNesEx2A { nesEs = esEx2A { esAccountState = acntEx2A { _reserves = 99 }}}})
   blockEx6A
   (Left [[BbodyFailure
           (LedgersFailure
            (LedgerFailure
             (DelegsFailure
              (DelplFailure
               (DelegFailure InsufficientForInstantaneousRewardsDELEG)))))]])

-- | Example 6F - Apply instantaneous rewards at epoch boundary


-- | The first transaction adds the MIR certificate that transfers a value of
-- 100 to Alice.

txbodyEx6F :: TxBody
txbodyEx6F = TxBody
              (Set.fromList [TxIn genesisId 0])
              (Seq.singleton $ TxOut aliceAddr (Coin 9992))
              (Seq.fromList [DCertDeleg (RegKey aliceSHK), DCertMir (MIRCert ir)])
              (Wdrl Map.empty)
              (Coin 1)
              (SlotNo 99)
              emptyUpdate
              Nothing

txEx6F :: Tx
txEx6F = Tx txbodyEx6F
            (makeWitnessesVKey txbodyEx6F [ alicePay, aliceStake ]  `Set.union` makeGenWitnessesVKey txbodyEx6F
             [ KeyPair (coreNodeVKG 0) (coreNodeSKG 0)
             , KeyPair (coreNodeVKG 1) (coreNodeSKG 1)
             , KeyPair (coreNodeVKG 2) (coreNodeSKG 2)
             , KeyPair (coreNodeVKG 3) (coreNodeSKG 3)
             , KeyPair (coreNodeVKG 4) (coreNodeSKG 4)
             ])
            Map.empty
            Nothing

blockEx6F :: Block
blockEx6F = mkBlock
              lastByronHeaderHash
              (coreNodeKeys 2)
              [txEx6F]
              (SlotNo 10)
              (BlockNo 1)
              (mkNonce 0)
              (NatNonce 1)
              zero
              0
              0
              (mkOCert (coreNodeKeys 2) 0 (KESPeriod 0))

-- | The second transaction in the next epoch and at least `startRewards` slots
-- after the transaction carrying the MIR certificate, then creates the rewards
-- update that contains the transfer of `100` to Alice.

txbodyEx6F' :: TxBody
txbodyEx6F' = TxBody
               (Set.fromList [TxIn (txid txbodyEx6F) 0])
               (Seq.singleton $ TxOut aliceAddr (Coin 9991))
               Seq.empty
               (Wdrl Map.empty)
               (Coin 1)
               ((slotFromEpoch $ EpochNo 1)
                +* Duration (startRewards testGlobals) + SlotNo 7)
               emptyUpdate
               Nothing

txEx6F' :: Tx
txEx6F' = Tx txbodyEx6F' (makeWitnessesVKey txbodyEx6F' [ alicePay ]) Map.empty Nothing

blockEx6F' :: Block
blockEx6F' = mkBlock
              (bhHash (bheader blockEx6F))
              (coreNodeKeys 0)
              [txEx6F']
              ((slotFromEpoch $ EpochNo 1)
                +* Duration (startRewards testGlobals) + SlotNo 7)
              (BlockNo 2)
              (mkNonce 0)
              (NatNonce 1)
              zero
              7
              0
              (mkOCert (coreNodeKeys 0) 0 (KESPeriod 0))

-- | The third transaction in the next epoch applies the reward update to 1)
-- register a staking credential for Alice, 2) deducing the key deposit from the
-- 100 and to 3) create the reward account with an initial amount of 93.

txbodyEx6F'' :: TxBody
txbodyEx6F'' = TxBody
                (Set.fromList [TxIn (txid txbodyEx6F') 0])
                (Seq.singleton $ TxOut aliceAddr (Coin 9990))
                Seq.empty
                (Wdrl Map.empty)
                (Coin 1)
                ((slotFromEpoch $ EpochNo 2) + SlotNo 10)
                emptyUpdate
                Nothing

txEx6F'' :: Tx
txEx6F'' = Tx txbodyEx6F'' (makeWitnessesVKey txbodyEx6F'' [ alicePay ]) Map.empty Nothing

blockEx6F'' :: Block
blockEx6F'' = mkBlock
               (bhHash (bheader blockEx6F'))
               (coreNodeKeys 2)
               [txEx6F'']
               ((slotFromEpoch $ EpochNo 2) + SlotNo 10)
               (BlockNo 3)
               (mkNonce 0)
               (NatNonce 1)
               zero
               10
               10
               (mkOCert (coreNodeKeys 2) 0 (KESPeriod 10))

ex6F' :: Either [[PredicateFailure CHAIN]] ChainState
ex6F' = do
  nextState <- runShelleyBase $ applySTS @CHAIN (TRC (SlotNo 90, initStEx2A, blockEx6F))
  midState <-
    runShelleyBase $ applySTS @CHAIN
      (TRC (((slotFromEpoch $ EpochNo 1) + SlotNo 7) +* Duration (startRewards testGlobals)
           , nextState
           , blockEx6F')
      )
  finalState <-
    runShelleyBase $ applySTS @CHAIN (TRC (((slotFromEpoch $ EpochNo 2) + SlotNo 10), midState, blockEx6F''))

  pure finalState

-- | Tests that after getting instantaneous rewards, creating the update and
-- then applying the update, Alice's key is actually registered, the key deposit
-- value deducted and the remaining value credited as reward.
test6F :: Assertion
test6F = do
  case ex6F' of
    Left e -> assertFailure (show e)
    Right ex6FState -> do
      let getDState = _dstate . _delegationState . esLState . nesEs . chainNes
          ds = getDState ex6FState
          StakeCreds stkCreds = _stkCreds ds
          rews = _rewards ds
          rewEntry = rews Map.!? (mkRwdAcnt aliceSHK)
      assertBool "Alice's credential not in stkCreds" (aliceSHK `Map.member` stkCreds)
      assertBool "Alice's reward account does not exist" $ isJust rewEntry
      assertBool "Alice's rewards are wrong" $ maybe False (== Coin 100) rewEntry
      assertBool "Total amount of ADA is not preserved" $ maxLovelaceSupply == totalAda ex6FState
