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

import           Cardano.Binary (ToCBOR)
import           Cardano.Crypto.Hash (ShortHash)
import           Cardano.Crypto.KES (deriveVerKeyKES, genKeyKES)
import           Cardano.Crypto.VRF (evalCertified)
import           Cardano.Crypto.VRF.Fake (WithResult (..))
import           Crypto.Random (drgNewTest, withDRG)
import           Data.ByteString.Char8 (pack)
import           Data.Coerce (coerce)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, empty, fromList, insert, keysSet, member, singleton,
                     (!?))
import           Data.Maybe (isJust, maybe)
import           Data.Ratio ((%))
import           Data.Sequence (empty, fromList)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           MockTypes (AVUpdate, Addr, Applications, Block, CHAIN, CertifiedVRF, ChainState,
                     Credential, DState, EpochState, GenKeyHash, HashHeader, KeyHash, KeyPair,
                     LedgerState, Mdt, NewEpochState, PPUpdate, PState, PoolDistr, PoolParams,
                     RewardAcnt, SKey, SKeyES, SignKeyVRF, SnapShots, Stake, Tx, TxBody, UTxO,
                     UTxOState, Update, UpdateState, VKeyES, VKeyGenesis, VerKeyVRF, hashKeyVRF)
import           Numeric.Natural (Natural)
import           Unsafe.Coerce (unsafeCoerce)

import           Address (mkRwdAcnt)
import           BaseTypes (Nonce (..), UnitInterval, intervalValue, mkNonce, (⭒))
import           BlockChain (pattern BHBody, pattern BHeader, pattern Block, pattern HashHeader,
                     ProtVer (..), TxSeq (..), bBodySize, bhHash, bhbHash, bheader, mkSeed,
                     seedEta, seedL, startRewards)
import           Coin (Coin (..))
import           Delegation.Certificates (pattern DeRegKey, pattern Delegate,
                     pattern GenesisDelegate, pattern InstantaneousRewards, pattern PoolDistr,
                     pattern RegKey, pattern RegPool, pattern RetirePool)
import           EpochBoundary (BlocksMade (..), pattern SnapShots, pattern Stake, emptySnapShots,
                     _feeSS, _poolsSS, _pstakeGo, _pstakeMark, _pstakeSet)
import           Keys (pattern GenDelegs, Hash, pattern KeyPair, pattern SKeyES, pattern VKeyES,
                     hash, hashKey, sKey, sign, signKES, vKey)
import           LedgerState (AccountState (..), pattern DPState, pattern EpochState,
                     pattern LedgerState, pattern NewEpochState, pattern RewardUpdate,
                     pattern UTxOState, deltaF, deltaR, deltaT, emptyDState, emptyPState,
                     esAccountState, esLState, esPp, genesisCoins, genesisId, nesEs,
                     overlaySchedule, rs, updateIRwd, _delegationState, _delegations, _dstate,
                     _fGenDelegs, _genDelegs, _irwd, _pParams, _ptrs, _reserves, _retiring,
                     _rewards, _stPools, _stkCreds, _treasury)
import           OCert (KESPeriod (..), pattern OCert)
import           PParams (PParams (..), emptyPParams)
import           Slot (BlockNo (..), Epoch (..), Slot (..), slotFromEpoch, (+*))
import           STS.Bbody (pattern LedgersFailure)
import           STS.Chain (pattern BbodyFailure, pattern ChainState, chainNes, totalAda)
import           STS.Deleg (pattern InsufficientForInstantaneousRewardsDELEG)
import           STS.Delegs (pattern DelplFailure)
import           STS.Delpl (pattern DelegFailure)
import           STS.Ledger (pattern DelegsFailure, pattern UtxowFailure)
import           STS.Ledgers (pattern LedgerFailure)
import           STS.Utxow (pattern MIRImpossibleInDecentralizedNetUTXOW,
                     pattern MIRInsufficientGenesisSigsUTXOW)
import           Test.Utils
import           TxData (pattern AddrPtr, pattern Delegation, pattern KeyHashObj,
                     pattern PoolParams, Ptr (..), pattern RewardAcnt, pattern StakeCreds,
                     pattern StakePools, pattern Tx, pattern TxBody, pattern TxIn, pattern TxOut,
                     addStakeCreds, _poolCost, _poolMargin, _poolOwners, _poolPledge, _poolPubKey,
                     _poolRAcnt, _poolVrf)
import qualified TxData (TxBody (..))
import           Updates (pattern AVUpdate, ApName (..), ApVer (..), pattern Applications,
                     InstallerHash (..), pattern Mdt, pattern PPUpdate, Ppm (..), SystemTag (..),
                     pattern Update, pattern UpdateState, emptyUpdate, emptyUpdateState)
import           UTxO (pattern UTxO, balance, makeGenWitnessesVKey, makeWitnessesVKey, txid)

import           Control.State.Transition (PredicateFailure, TRC (..), applySTS)

data CHAINExample =
  CHAINExample { currentSlot    :: Slot       -- ^ Current slot
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

mkCertifiedVRF
  :: ToCBOR a
  => WithResult a
  -> SignKeyVRF
  -> CertifiedVRF a
mkCertifiedVRF a sk = fst . withDRG (drgNewTest seed) $
    coerce <$> evalCertified () a sk
  where
    seed = (4,0,0,0,1)

-- | For testing purposes, generate a deterministic KES key pair given a seed.
mkKESKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> (SKeyES, VKeyES)
mkKESKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyKES 90
  return (SKeyES sk, VKeyES $ deriveVerKeyKES sk)

data AllPoolKeys = AllPoolKeys
  { cold :: KeyPair
  , vrf :: (SignKeyVRF, VerKeyVRF)
  , hot :: (SKeyES, VKeyES)
  , hk  :: KeyHash
  } deriving (Show)

mkAllPoolKeys :: Word64 -> AllPoolKeys
mkAllPoolKeys w = AllPoolKeys (KeyPair vkCold skCold)
                              (mkVRFKeyPair (w, 0, 0, 0, 2))
                              (mkKESKeyPair (w, 0, 0, 0, 3))
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
                            [ (ApName $ pack "Daedalus", (ApVer 16, Mdt Map.empty))
                            , (ApName $ pack "Yoroi", (ApVer 4, Mdt Map.empty))
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

mkSeqNonce :: Natural -> Nonce
mkSeqNonce m = foldl (\c x -> c ⭒ mkNonce x) NeutralNonce [0.. m]

-- | We provide our own nonces to 'mkBlock', which we then wish to recover as
-- the output of the VRF functions. In general, however, we just derive them
-- from a natural. Since the nonce is a hash, we do not want to recover it to
-- find a preimage. In testing, therefore, we just wrap the raw natural, which
-- we then encode into the fake VRF implementation.
newtype NatNonce = NatNonce Natural

-- | Try to map the unit interval to a natural number. We don't care whether
-- this is surjective. But it should be right inverse to `fromNatural` - that
-- is, one should be able to recover the `UnitInterval` value used here.
unitIntervalToNatural :: UnitInterval -> Natural
unitIntervalToNatural = floor . ((10000 % 1) *) . intervalValue

mkBlock
  :: HashHeader   -- ^ Hash of previous block
  -> AllPoolKeys  -- ^ All keys in the stake pool
  -> [Tx]         -- ^ Transactions to record
  -> Slot         -- ^ Current slot
  -> BlockNo      -- ^ Block number/chain length/chain "difficulty"
  -> Nonce        -- ^ Epoch nonce
  -> NatNonce     -- ^ Block nonce
  -> UnitInterval -- ^ Praos leader value
  -> Natural      -- ^ Period of KES (key evolving signature scheme)
  -> Block
mkBlock prev pkeys txns s blockNo enonce (NatNonce bnonce) l kesPeriod =
  let
    (shot, vhot) = hot pkeys
    nonceNonce = mkSeed seedEta s enonce prev
    leaderNonce = mkSeed seedL s enonce prev
    bhb = BHBody
            prev
            (vKey $ cold pkeys)
            (snd $ vrf pkeys)
            s
            blockNo
            (coerce $ mkCertifiedVRF (WithResult nonceNonce bnonce) (fst $ vrf pkeys))
            (coerce $ mkCertifiedVRF (WithResult leaderNonce $ unitIntervalToNatural l) (fst $ vrf pkeys))
            (fromIntegral $ bBodySize $ (TxSeq . fromList) txns)
            (bhbHash $ TxSeq $ fromList txns)
            (OCert
              vhot
              (vKey $ cold pkeys)
              0
              (KESPeriod 0)
              (sign (sKey $ cold pkeys) (vhot, 0, KESPeriod 0))
            )
            (ProtVer 0 0 0)
    bh = BHeader bhb (Keys.signKES shot bhb kesPeriod)
  in
    Block bh (TxSeq $ fromList txns)

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
lsEx1 = LedgerState utxostEx1 (DPState dsEx1 psEx1) 0

ppsEx1 :: PParams
ppsEx1 = emptyPParams { _maxBBSize       =       50000
                      , _maxBHSize       =       10000
                      , _maxTxSize       =       10000
                      , _eMax            = Epoch 10000
                      , _keyDeposit      = Coin      7
                      , _poolDeposit     = Coin    250
                      , _d               = unsafeMkUnitInterval 0.5
                      , _activeSlotCoeff = unsafeMkUnitInterval 0.1
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
acntEx1 = AccountState
            { _treasury = Coin 0
            , _reserves = maxLovelaceSupply
            }

-- | Epoch state with no snapshots.
esEx1 :: EpochState
esEx1 = EpochState acntEx1 emptySnapShots lsEx1 ppsEx1

-- |The first block of the Shelley era will point back to the last block of the Byron era.
-- For our purposes in this test we can bootstrap the chain by just coercing the value.
-- When this transition actually occurs, the consensus layer will do the work of making
-- sure that the hash gets translated across the fork
lastByronHeaderHash :: HashHeader
lastByronHeaderHash = HashHeader $ unsafeCoerce (hash 0 :: Hash ShortHash Int)

-- | Empty initial Shelley state with fake Byron hash and no blocks at all.
--   No blocks of Shelley have been processed yet.
initStEx1 :: ChainState
initStEx1 = ChainState
  (NewEpochState
     (Epoch 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     esEx1
     Nothing
     (PoolDistr Map.empty)
     (Map.singleton (Slot 1) (Just . hashKey $ coreNodeVKG 0)))
    -- The overlay schedule has one entry, setting Core Node 1 to slot 1.
  oCertIssueNosEx1
  (mkNonce 0)
  (mkNonce 0)
  (mkNonce 0)
  lastByronHeaderHash
  (Slot 0)

zero :: UnitInterval
zero = unsafeMkUnitInterval 0

-- | Null initial block. Just records the Byron hash, and contains no transactions.
blockEx1 :: Block
blockEx1 = mkBlock
             lastByronHeaderHash
             (coreNodeKeys 0)
             []
             (Slot 1)
             (BlockNo 1)
             (mkNonce 0)
             (NatNonce 1)
             zero
             0

-- | Expected chain state after successful processing of null block.
expectedStEx1 :: ChainState
expectedStEx1 = ChainState
  (NewEpochState
     (Epoch 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     -- Note that blocks in the overlay schedule do not add to this count.
     esEx1
     Nothing
     (PoolDistr Map.empty)
     (Map.singleton (Slot 1) (Just . hashKey $ coreNodeVKG 0)))
  oCertIssueNosEx1
  (mkNonce 0)
  (mkNonce 0 ⭒ mkNonce 1)
  (mkNonce 0 ⭒ mkNonce 1)
  (bhHash (bheader blockEx1))
  (Slot 1)

-- | Wraps example all together.
ex1 :: CHAINExample
ex1 = CHAINExample (Slot 1) initStEx1 blockEx1 (Right expectedStEx1)


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
                        (Set.singleton (PoolDeposit 255))

-- | Update proposal that just changes protocol parameters,
--   and does not change applications.
updateEx2A :: Update
updateEx2A = Update ppupEx2A (AVUpdate Map.empty)

-- | Transaction body to be processed.
txbodyEx2A :: TxBody
txbodyEx2A = TxBody
           (Set.fromList [TxIn genesisId 0])
           [TxOut aliceAddr (Coin 9726)]
           (fromList ([ RegKey aliceSHK
           , RegKey bobSHK
           , RegKey carlSHK
           , RegPool alicePoolParams
           ] ++ [InstantaneousRewards (Map.fromList [ (carlSHK, 110)
                                                    , (dariaSHK, 99)])]))
           Map.empty
           (Coin 3)
           (Slot 10)
           updateEx2A

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

-- | Pointer address to address of Alice address.
alicePtrAddr :: Addr
alicePtrAddr = AddrPtr (KeyHashObj . hashKey $ vKey alicePay) (Ptr (Slot 10) 0 0)

usEx2A :: UpdateState
usEx2A = UpdateState (PPUpdate Map.empty) (AVUpdate Map.empty) Map.empty byronApps

utxostEx2A :: UTxOState
utxostEx2A = UTxOState utxoEx2A (Coin 0) (Coin 0) usEx2A

lsEx2A :: LedgerState
lsEx2A = LedgerState utxostEx2A (DPState dsEx1 psEx1) 0

maxLovelaceSupply :: Coin
maxLovelaceSupply = Coin 45*1000*1000*1000*1000*1000

acntEx2A :: AccountState
acntEx2A = AccountState
            { _treasury = Coin 0
            , _reserves = maxLovelaceSupply - balance utxoEx2A
            }

esEx2A :: EpochState
esEx2A = EpochState acntEx2A emptySnapShots lsEx2A ppsEx1

overlayEx2A :: Map Slot (Maybe GenKeyHash)
overlayEx2A = overlaySchedule
                    (Epoch 0)
                    (Map.keysSet genDelegs)
                    ppsEx1

initNesEx2A :: NewEpochState
initNesEx2A = NewEpochState
               (Epoch 0)
               (BlocksMade Map.empty)
               (BlocksMade Map.empty)
               esEx2A
               Nothing
               (PoolDistr Map.empty)
               overlayEx2A


initStEx2A :: ChainState
initStEx2A = ChainState
  initNesEx2A
  oCertIssueNosEx1
  (mkNonce 0)
  (mkNonce 0)
  (mkNonce 0)
  lastByronHeaderHash
  (Slot 0)

blockEx2A :: Block
blockEx2A = mkBlock
             lastByronHeaderHash
             (coreNodeKeys 6)
             [txEx2A]
             (Slot 10)
             (BlockNo 10)
             (mkNonce 0)
             (NatNonce 1)
             zero
             0

dsEx2A :: DState
dsEx2A = dsEx1
          { _ptrs = Map.fromList [ (Ptr (Slot 10) 0 0, aliceSHK)
                                 , (Ptr (Slot 10) 0 1, bobSHK)
                                 , (Ptr (Slot 10) 0 2, carlSHK)]
          , _stkCreds = StakeCreds $ Map.fromList [ (aliceSHK, Slot 10)
                                                  , (bobSHK, Slot 10)
                                                  , (carlSHK, Slot 10)]
          , _rewards = Map.fromList [ (RewardAcnt aliceSHK, Coin 0)
                                    , (RewardAcnt bobSHK, Coin 0)
                                    , (RewardAcnt carlSHK, Coin 0)]
          , _irwd = Map.fromList [ (carlSHK, 110)
                                 , (dariaSHK, 99)]
          }

psEx2A :: PState
psEx2A = psEx1
          { _stPools = StakePools $ Map.singleton (hk alicePool) (Slot 10)
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
               0

blockEx2AHash :: HashHeader
blockEx2AHash = bhHash (bheader blockEx2A)

-- | Expected state after update is processed and STS applied.
expectedStEx2A :: ChainState
expectedStEx2A = ChainState
  (NewEpochState
     (Epoch   0)
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
  (mkNonce 0)
  (mkNonce 0 ⭒ mkNonce 1)
  (mkNonce 0 ⭒ mkNonce 1)
  blockEx2AHash
  (Slot 10)

ex2A :: CHAINExample
ex2A = CHAINExample (Slot 10) initStEx2A blockEx2A (Right expectedStEx2A)


-- * Example 2B - process a block late enough in the epoch in order to create a reward update.

-- | The transaction delegates Alice's and Bob's stake to Alice's pool.
--   Additionally, we split Alice's ADA between a base address and a pointer address.
txbodyEx2B :: TxBody
txbodyEx2B = TxBody
      { TxData._inputs   = Set.fromList [TxIn (txid txbodyEx2A) 0]
      , TxData._outputs  = [ TxOut aliceAddr    (Coin 722)
                           , TxOut alicePtrAddr (Coin 9000) ]
      -- | Delegation certificates
      , TxData._certs    = fromList [ Delegate $ Delegation aliceSHK (hk alicePool)
                                    , Delegate $ Delegation bobSHK   (hk alicePool)
                                    ]
      , TxData._wdrls    = Map.empty
      , TxData._txfee    = Coin 4
      , TxData._ttl      = Slot 90
      , TxData._txUpdate = emptyUpdate
      }

txEx2B :: Tx
txEx2B = Tx
          txbodyEx2B -- ^ Body of the transaction
          (makeWitnessesVKey txbodyEx2B [alicePay, aliceStake, bobStake, cold $ coreNodeKeys 0])
                     -- ^ Witness verification key set
          Map.empty  -- ^ Witness signature map

blockEx2B :: Block
blockEx2B = mkBlock
             blockEx2AHash    -- ^ Hash of previous block
             (coreNodeKeys 3) -- ^ Third genesis node
             [txEx2B]         -- ^ Single transaction to record
             (Slot 90)        -- ^ Current slot
             (BlockNo 2)
             (mkNonce 0)      -- ^ Epoch nonce
             (NatNonce 2)     -- ^ Block nonce
             zero             -- ^ Praos leader value
             1                -- ^ Period of KES (key evolving signature scheme)

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
               0

expectedStEx2Bgeneric :: PParams -> ChainState
expectedStEx2Bgeneric pp = ChainState
  -- | New state of the epoch
  (NewEpochState
     (Epoch   0)            -- ^ First epoch
     (BlocksMade Map.empty) -- ^ Blocks made before current
     (BlocksMade Map.empty) -- ^ Blocks made before current
     (EpochState acntEx2A emptySnapShots expectedLSEx2B pp)
                            -- ^ Previous epoch state
     (Just RewardUpdate { deltaT        = Coin 0
                        , deltaR        = Coin (-110)
                        , rs            = Map.empty
                        , deltaF        = Coin 0
                        , updateIRwd    = Map.fromList [(carlSHK, 110)]
                        })  -- ^ Update reward
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  (mkNonce 0)
  (mkNonce 0 ⭒ mkNonce 1 ⭒ mkNonce 2) -- ^ Evolving nonce
  (mkNonce 0 ⭒ mkNonce 1)             -- ^ Candidate nonce
  blockEx2BHash                       -- ^ Hash header of the chain
  (Slot 90)                           -- ^ Current slot

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
ex2B = CHAINExample (Slot 90) expectedStEx2A blockEx2B (Right expectedStEx2B)

-- | Example 2C - process an empty block in the next epoch
-- so that the (empty) reward update is applied and a stake snapshot is made.


blockEx2C :: Block
blockEx2C = mkBlock
             blockEx2BHash    -- ^ Hash of previous block
             (coreNodeKeys 6) -- ^ Sixth genesis node
             []               -- ^ No transactions at all (empty block)
             (Slot 110)       -- ^ Current slot
             (BlockNo 2)      -- ^ Second block within the epoch
             (mkNonce 0)      -- ^ Epoch nonce
             (NatNonce 3)     -- ^ Block nonce
             zero             -- ^ Praos leader value
             1                -- ^ Period of KES (key evolving signature scheme)

epoch1OSchedEx2C :: Map Slot (Maybe GenKeyHash)
epoch1OSchedEx2C = overlaySchedule
                    (Epoch 1)
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
           , _stkCreds = addStakeCreds carlSHK (Slot 10)   $ _stkCreds dsEx2B
           , _rewards  = Map.insert (mkRwdAcnt carlSHK) 110 $ _rewards dsEx2B
           }
    psEx2A)
  0

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
     (Epoch 1)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A { _reserves = _reserves acntEx2A - Coin 110 } ss ls pp)
     Nothing
     (PoolDistr Map.empty)
     epoch1OSchedEx2C)
  oCertIssueNosEx1
  (mkNonce 0 ⭒ mkNonce 1)
  (mkSeqNonce 3)
  (mkSeqNonce 3)
  blockEx2CHash
  (Slot 110)

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
ex2C = CHAINExample (Slot 110) expectedStEx2B blockEx2C (Right expectedStEx2C)

-- | Example 2C with no decay.
ex2Cbis :: CHAINExample
ex2Cbis = CHAINExample (Slot 110) expectedStEx2Bbis blockEx2C (Right expectedStEx2Cbis)

-- | Example 2C with full refund.
ex2Cter :: CHAINExample
ex2Cter = CHAINExample (Slot 110) expectedStEx2Bter blockEx2C (Right expectedStEx2Cter)

-- | Example 2C with instant decay.
ex2Cquater :: CHAINExample
ex2Cquater =
  CHAINExample (Slot 110) expectedStEx2Bquater blockEx2C (Right expectedStEx2Cquater)


-- | Example 2D - process an empty block late enough
-- in the epoch in order to create a second reward update, preparing the way for
-- the first non-empty pool distribution in this running example.


blockEx2D :: Block
blockEx2D = mkBlock
             blockEx2CHash
             (coreNodeKeys 3)
             []
             (Slot 190)
             (BlockNo 2)
             (mkNonce 0 ⭒ mkNonce 1)
             (NatNonce 4)
             zero
             2

blockEx2DHash :: HashHeader
blockEx2DHash = bhHash (bheader blockEx2D)

expectedStEx2D :: ChainState
expectedStEx2D = ChainState
  (NewEpochState
     (Epoch 1)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A { _reserves = _reserves acntEx2A - Coin 110} snapsEx2C expectedLSEx2C ppsEx1)
     (Just RewardUpdate { deltaT        = Coin 21
                        , deltaR        = Coin 0
                        , rs            = Map.empty
                        , deltaF        = Coin (-21)
                        , updateIRwd    = Map.empty
                        })
     (PoolDistr Map.empty)
     epoch1OSchedEx2C)
  oCertIssueNosEx1
  (mkNonce 0 ⭒ mkNonce 1)
  (mkSeqNonce 4)
  (mkSeqNonce 3)
  blockEx2DHash
  (Slot 190)

ex2D :: CHAINExample
ex2D = CHAINExample (Slot 190) expectedStEx2C blockEx2D (Right expectedStEx2D)


-- | Example 2E - create the first non-empty pool distribution
-- by creating a block in the third epoch of this running example.


blockEx2E :: Block
blockEx2E = mkBlock
             blockEx2DHash
             (coreNodeKeys 3)
             []
             (Slot 220)
             (BlockNo 2)
             (mkSeqNonce 3)
             (NatNonce 5)
             zero
             2

epoch1OSchedEx2E :: Map Slot (Maybe GenKeyHash)
epoch1OSchedEx2E = overlaySchedule
                    (Epoch 2)
                    (Map.keysSet genDelegs)
                    ppsEx1

snapsEx2E :: SnapShots
snapsEx2E = emptySnapShots { _pstakeMark = snapEx2C
                           , _pstakeSet = snapEx2C
                           , _poolsSS = Map.singleton (hk alicePool) alicePoolParams
                           , _feeSS = Coin 14
                           }

expectedLSEx2E :: LedgerState
expectedLSEx2E = LedgerState
               (UTxOState
                 utxoEx2B
                 (Coin 243)
                 (Coin 14)
                 usEx2A)
               (DPState
                dsEx2B { _irwd = Map.empty
                       , _stkCreds = addStakeCreds carlSHK (Slot 10) $ _stkCreds dsEx2B
                       , _rewards = Map.insert (mkRwdAcnt carlSHK) 110 $ _rewards dsEx2B
                       }
                 psEx2A)
               0

blockEx2EHash :: HashHeader
blockEx2EHash = bhHash (bheader blockEx2E)

acntEx2E :: AccountState
acntEx2E = AccountState
            { _treasury = Coin 21
            , _reserves = maxLovelaceSupply - balance utxoEx2A - (Coin 110)
            }

expectedStEx2E :: ChainState
expectedStEx2E = ChainState
  (NewEpochState
     (Epoch 2)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2E snapsEx2E expectedLSEx2E ppsEx1)
     Nothing
     (PoolDistr
       (Map.singleton
          (hk alicePool)
          (1, hashKeyVRF (snd $ vrf alicePool))))
     epoch1OSchedEx2E)
  oCertIssueNosEx1
  (mkSeqNonce 3)
  (mkSeqNonce 5)
  (mkSeqNonce 5)
  blockEx2EHash
  (Slot 220)

ex2E :: CHAINExample
ex2E = CHAINExample (Slot 220) expectedStEx2D blockEx2E (Right expectedStEx2E)


-- | Example 2F - create a decentralized Praos block (ie one not in the overlay schedule)

oCertIssueNosEx2F :: Map KeyHash Natural
oCertIssueNosEx2F = Map.insert (hk alicePool) 0 oCertIssueNosEx1

blockEx2F :: Block
blockEx2F = mkBlock
             blockEx2EHash
             alicePool
             []
             (Slot 295) -- odd slots open for decentralization in epoch1OSchedEx2E
             (BlockNo 2)
             (mkSeqNonce 3)
             (NatNonce 6)
             zero
             3

blockEx2FHash :: HashHeader
blockEx2FHash = bhHash (bheader blockEx2F)

pdEx2F :: PoolDistr
pdEx2F = PoolDistr $ Map.singleton (hk alicePool) (1, hashKeyVRF $ snd $ vrf alicePool)

expectedStEx2F :: ChainState
expectedStEx2F = ChainState
  (NewEpochState
     (Epoch 2)
     (BlocksMade Map.empty)
     (BlocksMade $ Map.singleton (hk alicePool) 1)
     (EpochState acntEx2E snapsEx2E expectedLSEx2E ppsEx1)
     (Just RewardUpdate { deltaT        = Coin 14
                        , deltaR        = Coin 0
                        , rs            = Map.empty
                        , deltaF        = Coin (-14)
                        , updateIRwd    = Map.empty
                        })
     pdEx2F
     epoch1OSchedEx2E)
  oCertIssueNosEx2F
  (mkSeqNonce 3)
  (mkSeqNonce 6)
  (mkSeqNonce 5)
  blockEx2FHash
  (Slot 295)

ex2F :: CHAINExample
ex2F = CHAINExample (Slot 295) expectedStEx2E blockEx2F (Right expectedStEx2F)


-- | Example 2G - create an empty block in the next epoch
-- to prepare the way for the first non-trivial reward update


blockEx2G :: Block
blockEx2G = mkBlock
             blockEx2FHash
             (coreNodeKeys 6)
             []
             (Slot 310)
             (BlockNo 2)
             (mkSeqNonce 5)
             (NatNonce 7)
             zero
             3

blockEx2GHash :: HashHeader
blockEx2GHash = bhHash (bheader blockEx2G)

epoch1OSchedEx2G :: Map Slot (Maybe GenKeyHash)
epoch1OSchedEx2G = overlaySchedule
                    (Epoch 3)
                    (Map.keysSet genDelegs)
                    ppsEx1

snapsEx2G :: SnapShots
snapsEx2G = snapsEx2E { _pstakeGo = snapEx2C
                      , _feeSS = 10}

expectedLSEx2G :: LedgerState
expectedLSEx2G = LedgerState
               (UTxOState
                 utxoEx2B
                 (Coin 233)
                 (Coin 10)
                 usEx2A)
               (DPState
                 dsEx2B { _irwd = Map.empty
                        , _stkCreds = addStakeCreds carlSHK (Slot 10) $ _stkCreds dsEx2B
                        , _rewards = Map.insert (mkRwdAcnt carlSHK) 110 $ _rewards dsEx2B
                        }
                 psEx2A)
               0

expectedStEx2G :: ChainState
expectedStEx2G = ChainState
  (NewEpochState
     (Epoch 3)
     (BlocksMade $ Map.singleton (hk alicePool) 1)
     (BlocksMade Map.empty)
     (EpochState (acntEx2E { _treasury = 35}) snapsEx2G expectedLSEx2G ppsEx1)
     Nothing
     pdEx2F
     epoch1OSchedEx2G)
  oCertIssueNosEx2F
  (mkSeqNonce 5)
  (mkSeqNonce 7)
  (mkSeqNonce 7)
  blockEx2GHash
  (Slot 310)

ex2G :: CHAINExample
ex2G = CHAINExample (Slot 310) expectedStEx2F blockEx2G (Right expectedStEx2G)


-- | Example 2H - create the first non-trivial reward update


blockEx2H :: Block
blockEx2H = mkBlock
             blockEx2GHash
             (coreNodeKeys 3)
             []
             (Slot 390)
             (BlockNo 2)
             (mkSeqNonce 5)
             (NatNonce 8)
             zero
             4

blockEx2HHash :: HashHeader
blockEx2HHash = bhHash (bheader blockEx2H)

aliceRAcnt2H :: Coin
aliceRAcnt2H = Coin 69254168998

bobRAcnt2H :: Coin
bobRAcnt2H = Coin 6345831001

rewardsEx2H :: Map RewardAcnt Coin
rewardsEx2H = Map.fromList [ (RewardAcnt aliceSHK, aliceRAcnt2H)
                          , (RewardAcnt bobSHK, bobRAcnt2H) ]

expectedStEx2H :: ChainState
expectedStEx2H = ChainState
  (NewEpochState
     (Epoch 3)
     (BlocksMade $ Map.singleton (hk alicePool) 1)
     (BlocksMade Map.empty)
     (EpochState (acntEx2E { _treasury = Coin 35 }) snapsEx2G expectedLSEx2G ppsEx1)
     (Just RewardUpdate { deltaT        = Coin 9374400000008
                        , deltaR        = Coin (-9449999999997)
                        , rs            = rewardsEx2H
                        , deltaF        = Coin (-10)
                        , updateIRwd    = Map.empty
                        })
     pdEx2F
     epoch1OSchedEx2G)
  oCertIssueNosEx2F
  (mkSeqNonce 5)
  (mkSeqNonce 8)
  (mkSeqNonce 7)
  blockEx2HHash
  (Slot 390)

ex2H :: CHAINExample
ex2H = CHAINExample (Slot 390) expectedStEx2G blockEx2H (Right expectedStEx2H)


-- | Example 2I - apply the first non-trivial reward update


blockEx2I :: Block
blockEx2I = mkBlock
              blockEx2HHash
              (coreNodeKeys 6)
              []
              (Slot 410)
              (BlockNo 2)
              (mkSeqNonce 7)
              (NatNonce 9)
              zero
              4

blockEx2IHash :: HashHeader
blockEx2IHash = bhHash (bheader blockEx2I)

epoch1OSchedEx2I :: Map Slot (Maybe GenKeyHash)
epoch1OSchedEx2I = overlaySchedule
                     (Epoch 4)
                     (Map.keysSet genDelegs)
                     ppsEx1

acntEx2I :: AccountState
acntEx2I = AccountState
            { _treasury = Coin 9374400000043
            , _reserves = Coin 44990549999988893
            }

dsEx2I :: DState
dsEx2I = dsEx2B { _irwd = Map.empty
                , _stkCreds = addStakeCreds carlSHK (Slot 10) $ _stkCreds dsEx2B
                , _rewards = Map.insert (mkRwdAcnt carlSHK) 110 rewardsEx2H
                }

expectedLSEx2I :: LedgerState
expectedLSEx2I = LedgerState
               (UTxOState
                 utxoEx2B
                 (Coin 224)
                 (Coin 9)
                 usEx2A)
               (DPState dsEx2I psEx2A)
               0

snapsEx2I :: SnapShots
snapsEx2I = snapsEx2G { _pstakeMark =
                          (Stake ( Map.fromList [ (bobSHK, Coin 1000 + bobRAcnt2H)
                                                , (aliceSHK, Coin 9722 + aliceRAcnt2H)])
                          , delegsEx2B )
                        -- The stake snapshots have bigger values now, due to the new rewards
                      , _feeSS = Coin 9
                      }

expectedStEx2I :: ChainState
expectedStEx2I = ChainState
  (NewEpochState
     (Epoch 4)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2I snapsEx2I expectedLSEx2I ppsEx1)
     Nothing
     pdEx2F
     epoch1OSchedEx2I)
  oCertIssueNosEx2F
  (mkSeqNonce 7)
  (mkSeqNonce 9)
  (mkSeqNonce 9)
  blockEx2IHash
  (Slot 410)

ex2I :: CHAINExample
ex2I = CHAINExample (Slot 410) expectedStEx2H blockEx2I (Right expectedStEx2I)


-- | Example 2J - drain reward account and de-register stake key

bobAda2J :: Coin
bobAda2J = bobRAcnt2H -- reward account
                   + Coin 1000 -- txin we will consume (must spend at least one)
                   + Coin 4 -- stake registration refund
                   - Coin 9 -- tx fee

txbodyEx2J :: TxBody
txbodyEx2J = TxBody
           (Set.fromList [TxIn genesisId 1]) --
           [TxOut bobAddr bobAda2J]
           (fromList [DeRegKey bobSHK])
           (Map.singleton (RewardAcnt bobSHK) bobRAcnt2H)
           (Coin 9)
           (Slot 500)
           emptyUpdate

txEx2J :: Tx
txEx2J = Tx
          txbodyEx2J
          (makeWitnessesVKey txbodyEx2J [bobPay, bobStake])
          Map.empty

blockEx2J :: Block
blockEx2J = mkBlock
              blockEx2IHash
              (coreNodeKeys 3)
              [txEx2J]
              (Slot 420)
              (BlockNo 2)
              (mkSeqNonce 7)
              (NatNonce 10)
              zero
              4

blockEx2JHash :: HashHeader
blockEx2JHash = bhHash (bheader blockEx2J)

utxoEx2J :: UTxO
utxoEx2J = UTxO . Map.fromList $
                   [ (TxIn (txid txbodyEx2J) 0, TxOut bobAddr bobAda2J)
                   , (TxIn (txid txbodyEx2B) 0, TxOut aliceAddr (Coin 722))
                   , (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr (Coin 9000))
                   ]

dsEx2J :: DState
dsEx2J = dsEx1
          { _ptrs = Map.fromList [ (Ptr (Slot 10) 0 0, aliceSHK)
                                 , (Ptr (Slot 10) 0 2, carlSHK)]
          , _stkCreds = StakeCreds $ Map.fromList [(aliceSHK, Slot 10), (carlSHK, Slot 10)]
          , _delegations = Map.singleton aliceSHK (hk alicePool)
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
               0

expectedStEx2J :: ChainState
expectedStEx2J = ChainState
  (NewEpochState
     (Epoch 4)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2I snapsEx2I expectedLSEx2J ppsEx1)
     Nothing
     pdEx2F
     epoch1OSchedEx2I)
  oCertIssueNosEx2F
  (mkSeqNonce 7)
  (mkSeqNonce 10)
  (mkSeqNonce 10)
  blockEx2JHash
  (Slot 420)

ex2J :: CHAINExample
ex2J = CHAINExample (Slot 420) expectedStEx2I blockEx2J (Right expectedStEx2J)


-- | Example 2K - start stake pool retirement


txbodyEx2K :: TxBody
txbodyEx2K = TxBody
           (Set.fromList [TxIn (txid txbodyEx2B) 0])
           [TxOut alicePtrAddr 720]
           (fromList [RetirePool (hk alicePool) (Epoch 5)])
           Map.empty
           (Coin 2)
           (Slot 500)
           emptyUpdate

txEx2K :: Tx
txEx2K = Tx
          txbodyEx2K
          (makeWitnessesVKey txbodyEx2K [cold alicePool, alicePay])
          Map.empty

blockEx2K :: Block
blockEx2K = mkBlock
              blockEx2JHash
              (coreNodeKeys 3)
              [txEx2K]
              (Slot 490)
              (BlockNo 2)
              (mkSeqNonce 7)
              (NatNonce 11)
              zero
              5

blockEx2KHash :: HashHeader
blockEx2KHash = bhHash (bheader blockEx2K)

utxoEx2K :: UTxO
utxoEx2K = UTxO . Map.fromList $
                   [ (TxIn (txid txbodyEx2J) 0, TxOut bobAddr bobAda2J)
                   , (TxIn (txid txbodyEx2K) 0, TxOut alicePtrAddr (Coin 720))
                   , (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr (Coin 9000))
                   ]

psEx2K :: PState
psEx2K = psEx2A { _retiring = Map.singleton (hk alicePool) (Epoch 5) }

expectedLSEx2K :: LedgerState
expectedLSEx2K = LedgerState
               (UTxOState
                 utxoEx2K
                 (Coin 220)
                 (Coin 20)
                 usEx2A)
               (DPState dsEx2J psEx2K)
               0


expectedStEx2K :: ChainState
expectedStEx2K = ChainState
  (NewEpochState
     (Epoch 4)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2I snapsEx2I expectedLSEx2K ppsEx1)
     (Just RewardUpdate { deltaT        = Coin 9
                        , deltaR        = Coin 0
                        , rs            = Map.empty
                        , deltaF        = Coin (-9)
                        , updateIRwd    = Map.empty
                        })
     pdEx2F
     epoch1OSchedEx2I)
  oCertIssueNosEx2F
  (mkSeqNonce 7)
  (mkSeqNonce 11)
  (mkSeqNonce 10)
  blockEx2KHash
  (Slot 490)

ex2K :: CHAINExample
ex2K = CHAINExample (Slot 490) expectedStEx2J blockEx2K (Right expectedStEx2K)


-- | Example 2L - reap a stake pool


blockEx2L :: Block
blockEx2L = mkBlock
              blockEx2KHash
              (coreNodeKeys 6)
              []
              (Slot 510)
              (BlockNo 2)
              (mkSeqNonce 10)
              (NatNonce 12)
              zero
              5

blockEx2LHash :: HashHeader
blockEx2LHash = bhHash (bheader blockEx2L)

acntEx2L :: AccountState
acntEx2L = acntEx2I { _treasury =  _treasury acntEx2I --previous amount
                                  + Coin 9 } -- from the reward update

snapsEx2L :: SnapShots
snapsEx2L = SnapShots { _pstakeMark =
                          (Stake ( Map.fromList [ (aliceSHK, aliceRAcnt2H + 9000 + 720) ])
                          , Map.singleton aliceSHK (hk alicePool))
                      , _pstakeSet = _pstakeMark snapsEx2I
                      , _pstakeGo = _pstakeSet snapsEx2I
                      , _poolsSS = Map.singleton (hk alicePool) alicePoolParams
                      , _feeSS = Coin 22
                      }
dsEx2L :: DState
dsEx2L = dsEx1
          { _ptrs = Map.fromList [ (Ptr (Slot 10) 0 0, aliceSHK)
                                 , (Ptr (Slot 10) 0 2, carlSHK)
                                 ]
          , _stkCreds = StakeCreds $ Map.fromList [(aliceSHK, Slot 10), (carlSHK, Slot 10)]
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
               0

expectedStEx2L :: ChainState
expectedStEx2L = ChainState
  (NewEpochState
     (Epoch 5)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2L snapsEx2L expectedLSEx2L ppsEx1)
     Nothing
     pdEx2F
     (overlaySchedule (Epoch 5) (Map.keysSet genDelegs) ppsEx1))
  oCertIssueNosEx2F
  (mkSeqNonce 10)
  (mkSeqNonce 12)
  (mkSeqNonce 12)
  blockEx2LHash
  (Slot 510)

ex2L :: CHAINExample
ex2L = CHAINExample (Slot 510) expectedStEx2K blockEx2L (Right expectedStEx2L)


-- | Example 3A - Setting up for a successful protocol parameter update,
-- have three genesis keys vote on the same new parameters


ppVote3A :: Set Ppm
ppVote3A = Set.fromList [ExtraEntropy (mkNonce 123), PoolDeposit 200]

ppupEx3A :: PPUpdate
ppupEx3A = PPUpdate $ Map.fromList [ (hashKey $ coreNodeVKG 0, ppVote3A)
                                   , (hashKey $ coreNodeVKG 3, ppVote3A)
                                   , (hashKey $ coreNodeVKG 4, ppVote3A)
                                   ]

updateEx3A :: Update
updateEx3A = Update ppupEx3A (AVUpdate Map.empty)

txbodyEx3A :: TxBody
txbodyEx3A = TxBody
           (Set.fromList [TxIn genesisId 0])
           [TxOut aliceAddr (Coin 9999)]
           Data.Sequence.empty
           Map.empty
           (Coin 1)
           (Slot 10)
           updateEx3A

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

blockEx3A :: Block
blockEx3A = mkBlock
             lastByronHeaderHash
             (coreNodeKeys 6)
             [txEx3A]
             (Slot 10)
             (BlockNo 2)
             (mkNonce 0)
             (NatNonce 1)
             zero
             0

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
               0

blockEx3AHash :: HashHeader
blockEx3AHash = bhHash (bheader blockEx3A)

expectedStEx3A :: ChainState
expectedStEx3A = ChainState
  (NewEpochState
     (Epoch 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx3A ppsEx1)
     Nothing
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  (mkNonce 0)
  (mkNonce 0 ⭒ mkNonce 1)
  (mkNonce 0 ⭒ mkNonce 1)
  blockEx3AHash
  (Slot 10)

ex3A :: CHAINExample
ex3A = CHAINExample (Slot 10) initStEx2A blockEx3A (Right expectedStEx3A)


-- | Example 3B - Finish getting enough votes for the protocol parameter update.


ppupEx3B :: PPUpdate
ppupEx3B = PPUpdate $ Map.fromList [ (hashKey $ coreNodeVKG 1, ppVote3A)
                                   , (hashKey $ coreNodeVKG 5, ppVote3A)
                                   ]

updateEx3B :: Update
updateEx3B = Update ppupEx3B (AVUpdate Map.empty)

txbodyEx3B :: TxBody
txbodyEx3B = TxBody
           (Set.fromList [TxIn (txid txbodyEx3A) 0])
           [TxOut aliceAddr (Coin 9998)]
           Data.Sequence.empty
           Map.empty
           (Coin 1)
           (Slot 31)
           updateEx3B

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

blockEx3B :: Block
blockEx3B = mkBlock
             blockEx3AHash
             (coreNodeKeys 3)
             [txEx3B]
             (Slot 20)
             (BlockNo 2)
             (mkNonce 0)
             (NatNonce 2)
             zero
             0

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
               0

blockEx3BHash :: HashHeader
blockEx3BHash = bhHash (bheader blockEx3B)

expectedStEx3B :: ChainState
expectedStEx3B = ChainState
  (NewEpochState
     (Epoch 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx3B ppsEx1)
     Nothing
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  (mkNonce 0)
  (mkSeqNonce 2)
  (mkSeqNonce 2)
  blockEx3BHash
  (Slot 20)

ex3B :: CHAINExample
ex3B = CHAINExample (Slot 20) expectedStEx3A blockEx3B (Right expectedStEx3B)


-- | Example 3C - Adopt protocol parameter update


blockEx3C :: Block
blockEx3C = mkBlock
             blockEx3BHash
             (coreNodeKeys 6)
             []
             (Slot 110)
             (BlockNo 2)
             (mkSeqNonce 2)
             (NatNonce 3)
             zero
             1

blockEx3CHash :: HashHeader
blockEx3CHash = bhHash (bheader blockEx3C)

overlayEx3C :: Map Slot (Maybe GenKeyHash)
overlayEx3C = overlaySchedule
                    (Epoch 1)
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
               0

ppsEx3C :: PParams
ppsEx3C = ppsEx1 { _poolDeposit = Coin 200, _extraEntropy = mkNonce 123 }

expectedStEx3C :: ChainState
expectedStEx3C = ChainState
  (NewEpochState
     (Epoch 1)
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
  blockEx3CHash
  (Slot 110)

ex3C :: CHAINExample
ex3C = CHAINExample (Slot 110) expectedStEx3B blockEx3C (Right expectedStEx3C)


-- | Example 4A - Setting up for a successful application version update,
-- have three genesis keys vote on the same new version


daedalusMDEx4A :: Mdt
daedalusMDEx4A = Mdt $ Map.singleton
                              (SystemTag $ pack "DOS")
                              (InstallerHash $ hash $ pack "ABC")

appsEx4A :: Applications
appsEx4A = Applications $ Map.singleton
                            (ApName $ pack "Daedalus")
                            (ApVer 17, daedalusMDEx4A)

avupEx4A :: AVUpdate
avupEx4A = AVUpdate $ Map.fromList [ (hashKey $ coreNodeVKG 0, appsEx4A)
                                   , (hashKey $ coreNodeVKG 3, appsEx4A)
                                   , (hashKey $ coreNodeVKG 4, appsEx4A)
                                   ]

updateEx4A :: Update
updateEx4A = Update (PPUpdate Map.empty) avupEx4A

txbodyEx4A :: TxBody
txbodyEx4A = TxBody
           (Set.fromList [TxIn genesisId 0])
           [TxOut aliceAddr (Coin 9999)]
           Data.Sequence.empty
           Map.empty
           (Coin 1)
           (Slot 10)
           updateEx4A

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

blockEx4A :: Block
blockEx4A = mkBlock
             lastByronHeaderHash
             (coreNodeKeys 6)
             [txEx4A]
             (Slot 10)
             (BlockNo 2)
             (mkNonce 0)
             (NatNonce 1)
             zero
             0

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
               0

blockEx4AHash :: HashHeader
blockEx4AHash = bhHash (bheader blockEx4A)

expectedStEx4A :: ChainState
expectedStEx4A = ChainState
  (NewEpochState
     (Epoch 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx4A ppsEx1)
     Nothing
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  (mkNonce 0)
  (mkNonce 0 ⭒ mkNonce 1)
  (mkNonce 0 ⭒ mkNonce 1)
  blockEx4AHash
  (Slot 10)

ex4A :: CHAINExample
ex4A = CHAINExample (Slot 10) initStEx2A blockEx4A (Right expectedStEx4A)


-- | Example 4B - Finish getting enough votes for the application version update.

avupEx4B :: AVUpdate
avupEx4B = AVUpdate $ Map.fromList [ (hashKey $ coreNodeVKG 1, appsEx4A)
                                   , (hashKey $ coreNodeVKG 5, appsEx4A)
                                   ]

updateEx4B :: Update
updateEx4B = Update (PPUpdate Map.empty) avupEx4B

txbodyEx4B :: TxBody
txbodyEx4B = TxBody
           (Set.fromList [TxIn (txid txbodyEx4A) 0])
           [TxOut aliceAddr (Coin 9998)]
           Data.Sequence.empty
           Map.empty
           (Coin 1)
           (Slot 31)
           updateEx4B

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

blockEx4B :: Block
blockEx4B = mkBlock
             blockEx4AHash
             (coreNodeKeys 3)
             [txEx4B]
             (Slot 20)
             (BlockNo 2)
             (mkNonce 0)
             (NatNonce 2)
             zero
             0

updateStEx4B :: UpdateState
updateStEx4B = UpdateState
  (PPUpdate Map.empty)
  (AVUpdate Map.empty)
  (Map.singleton (Slot 53) appsEx4A)
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
               0

blockEx4BHash :: HashHeader
blockEx4BHash = bhHash (bheader blockEx4B)

expectedStEx4B :: ChainState
expectedStEx4B = ChainState
  (NewEpochState
     (Epoch 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx4B ppsEx1)
     Nothing
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  (mkNonce 0)
  (mkSeqNonce 2)
  (mkSeqNonce 2)
  blockEx4BHash
  (Slot 20)

ex4B :: CHAINExample
ex4B = CHAINExample (Slot 20) expectedStEx4A blockEx4B (Right expectedStEx4B)


-- | Example 4C - Adopt application version update


blockEx4C :: Block
blockEx4C = mkBlock
             blockEx4BHash
             (coreNodeKeys 0)
             []
             (Slot 60)
             (BlockNo 2)
             (mkNonce 0)
             (NatNonce 3)
             zero
             0

updateStEx4C :: UpdateState
updateStEx4C = UpdateState
  (PPUpdate Map.empty)
  (AVUpdate Map.empty)
  Map.empty
  (Applications $ Map.fromList
                     [ (ApName $ pack "Daedalus", (ApVer 17, daedalusMDEx4A))
                     , (ApName $ pack "Yoroi", (ApVer 4, Mdt Map.empty))
                     ])

expectedLSEx4C :: LedgerState
expectedLSEx4C = LedgerState
               (UTxOState
                 utxoEx4B
                 (Coin 0)
                 (Coin 2)
                 updateStEx4C)
               (DPState dsEx1 psEx1)
               0

blockEx4CHash :: HashHeader
blockEx4CHash = bhHash (bheader blockEx4C)

expectedStEx4C :: ChainState
expectedStEx4C = ChainState
  (NewEpochState
     (Epoch 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx4C ppsEx1)
     (Just RewardUpdate { deltaT        = Coin 0
                        , deltaR        = Coin 0
                        , rs            = Map.empty
                        , deltaF        = Coin 0
                        , updateIRwd    = Map.empty
                        })
      (PoolDistr Map.empty)
      overlayEx2A)
  oCertIssueNosEx1
  (mkNonce 0)
  (mkSeqNonce 3)
  (mkSeqNonce 3)
  blockEx4CHash
  (Slot 60)


ex4C :: CHAINExample
ex4C = CHAINExample (Slot 60) expectedStEx4B blockEx4C (Right expectedStEx4C)


-- | Example 5A - Genesis key delegation


newGenDelegate :: KeyPair
newGenDelegate  = KeyPair vkCold skCold
  where (skCold, vkCold) = mkKeyPair (108, 0, 0, 0, 1)

txbodyEx5A :: TxBody
txbodyEx5A = TxBody
              (Set.fromList [TxIn genesisId 0])
              [TxOut aliceAddr (Coin 9999)]
              (fromList [GenesisDelegate
                          ( (hashKey . coreNodeVKG) 0
                          , (hashKey . vKey) newGenDelegate)])
              Map.empty
              (Coin 1)
              (Slot 10)
              emptyUpdate

txEx5A :: Tx
txEx5A = Tx
           txbodyEx5A
           (makeWitnessesVKey txbodyEx5A [ alicePay ] `Set.union` makeGenWitnessesVKey txbodyEx5A [ KeyPair (coreNodeVKG 0) (coreNodeSKG 0) ])
           Map.empty

blockEx5A :: Block
blockEx5A = mkBlock
              lastByronHeaderHash
              (coreNodeKeys 6)
              [txEx5A]
              (Slot 10)
              (BlockNo 2)
              (mkNonce 0)
              (NatNonce 1)
              zero
              0

blockEx5AHash :: HashHeader
blockEx5AHash = bhHash (bheader blockEx5A)

dsEx5A :: DState
dsEx5A = dsEx1 { _fGenDelegs = Map.singleton
                          ( Slot 43, hashKey $ coreNodeVKG 0 )
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
               0

expectedStEx5A :: ChainState
expectedStEx5A = ChainState
  (NewEpochState
     (Epoch 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx5A ppsEx1)
     Nothing
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  (mkNonce 0)
  (mkNonce 0 ⭒ mkNonce 1)
  (mkNonce 0 ⭒ mkNonce 1)
  blockEx5AHash
  (Slot 10)

ex5A :: CHAINExample
ex5A = CHAINExample (Slot 10) initStEx2A blockEx5A (Right expectedStEx5A)


-- | Example 5B - New genesis key delegation updated from future delegations

blockEx5B :: Block
blockEx5B = mkBlock
             blockEx5AHash
             (coreNodeKeys 2)
             []
             (Slot 50)
             (BlockNo 2)
             (mkNonce 0)
             (NatNonce 2)
             zero
             0

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
               0

expectedStEx5B :: ChainState
expectedStEx5B = ChainState
  (NewEpochState
     (Epoch 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx5B ppsEx1)
     (Just RewardUpdate { deltaT        = Coin 0
                        , deltaR        = Coin 0
                        , rs            = Map.empty
                        , deltaF        = Coin 0
                        , updateIRwd    = Map.empty
                        })
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  (mkNonce 0)
  (mkSeqNonce 2)
  (mkSeqNonce 2)
  blockEx5BHash
  (Slot 50)

ex5B :: CHAINExample
ex5B = CHAINExample (Slot 50) expectedStEx5A blockEx5B (Right expectedStEx5B)


-- | Example 6A - Genesis key delegation


ir :: Map Credential Coin
ir = Map.fromList [(aliceSHK, Coin 100)]

txbodyEx6A :: TxBody
txbodyEx6A = TxBody
              (Set.fromList [TxIn genesisId 0])
              [TxOut aliceAddr (Coin 9999)]
              (fromList [InstantaneousRewards ir])
              Map.empty
              (Coin 1)
              (Slot 10)
              emptyUpdate

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

blockEx6A :: Block
blockEx6A = mkBlock
              lastByronHeaderHash
              (coreNodeKeys 6)
              [txEx6A]
              (Slot 10)
              (BlockNo 1)
              (mkNonce 0)
              (NatNonce 1)
              zero
              0

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
               0

expectedStEx6A :: ChainState
expectedStEx6A = ChainState
  (NewEpochState
     (Epoch 0)
     (BlocksMade Map.empty)
     (BlocksMade Map.empty)
     (EpochState acntEx2A emptySnapShots expectedLSEx6A ppsEx1)
     Nothing
     (PoolDistr Map.empty)
     overlayEx2A)
  oCertIssueNosEx1
  (mkNonce 0)
  (mkNonce 0 ⭒ mkNonce 1)
  (mkNonce 0 ⭒ mkNonce 1)
  blockEx6AHash
  (Slot 10)

ex6A :: CHAINExample
ex6A = CHAINExample (Slot 10) initStEx2A blockEx6A (Right expectedStEx6A)


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

blockEx6B :: Block
blockEx6B = mkBlock
              lastByronHeaderHash
              (coreNodeKeys 6)
              [txEx6B]
              (Slot 10)
              (BlockNo 1)
              (mkNonce 0)
              (NatNonce 1)
              zero
              0

expectedStEx6B :: PredicateFailure CHAIN
expectedStEx6B = BbodyFailure (LedgersFailure (LedgerFailure (UtxowFailure MIRInsufficientGenesisSigsUTXOW)))

ex6B :: CHAINExample
ex6B = CHAINExample (Slot 10) initStEx2A blockEx6B (Left [[expectedStEx6B]])

-- | Example 6C - Instantaneous rewards in decentralized era

expectedStEx6C :: PredicateFailure CHAIN
expectedStEx6C = BbodyFailure (LedgersFailure (LedgerFailure (UtxowFailure MIRImpossibleInDecentralizedNetUTXOW)))

ex6C :: CHAINExample
ex6C =
  CHAINExample
   (Slot 10)
   (initStEx2A { chainNes = initNesEx2A { nesEs = esEx2A { esPp = ppsEx1 { _d = unsafeMkUnitInterval 0 }}}})
   blockEx6A
   (Left [[expectedStEx6C]])


-- | Example 6D - Instantaneous rewards in decentralized era and not enough core
-- signatures

ex6D :: CHAINExample
ex6D =
  CHAINExample
   (Slot 10)
   (initStEx2A { chainNes = initNesEx2A { nesEs = esEx2A { esPp = ppsEx1 { _d = unsafeMkUnitInterval 0 }}}})
   blockEx6B
   (Left [[expectedStEx6C, expectedStEx6B]])

-- | Example 6E - Instantaneous rewards that overrun the available reserves

ex6E :: CHAINExample
ex6E =
  CHAINExample
   (Slot 10)
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
              [TxOut aliceAddr (Coin 9992)]
              (fromList [RegKey aliceSHK, InstantaneousRewards ir])
              Map.empty
              (Coin 1)
              (Slot 99)
              emptyUpdate

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

blockEx6F :: Block
blockEx6F = mkBlock
              lastByronHeaderHash
              (coreNodeKeys 6)
              [txEx6F]
              (Slot 10)
              (BlockNo 1)
              (mkNonce 0)
              (NatNonce 1)
              zero
              0

-- | The second transaction in the next epoch and at least `startRewards` slots
-- after the transaction carrying the MIR certificate, then creates the rewards
-- update that contains the transfer of `100` to Alice.

txbodyEx6F' :: TxBody
txbodyEx6F' = TxBody
               (Set.fromList [TxIn (txid txbodyEx6F) 0])
               [TxOut aliceAddr (Coin 9991)]
               empty
               Map.empty
               (Coin 1)
               ((slotFromEpoch $ Epoch 1) +* startRewards + Slot 7)
               emptyUpdate

txEx6F' :: Tx
txEx6F' = Tx txbodyEx6F' (makeWitnessesVKey txbodyEx6F' [ alicePay ]) Map.empty

blockEx6F' :: Block
blockEx6F' = mkBlock
              (bhHash (bheader blockEx6F))
              (coreNodeKeys 1)
              [txEx6F']
              ((slotFromEpoch $ Epoch 1) +* startRewards + Slot 7)
              (BlockNo 1)
              (mkNonce 0)
              (NatNonce 1)
              zero
              1

-- | The third transaction in the next epoch applies the reward update to 1)
-- register a staking credential for Alice, 2) deducing the key deposit from the
-- 100 and to 3) create the reward account with an initial amount of 93.

txbodyEx6F'' :: TxBody
txbodyEx6F'' = TxBody
                (Set.fromList [TxIn (txid txbodyEx6F') 0])
                [TxOut aliceAddr (Coin 9990)]
                empty
                Map.empty
                (Coin 1)
                ((slotFromEpoch $ Epoch 2) + Slot 10)
                emptyUpdate

txEx6F'' :: Tx
txEx6F'' = Tx txbodyEx6F'' (makeWitnessesVKey txbodyEx6F'' [ alicePay ]) Map.empty

blockEx6F'' :: Block
blockEx6F'' = mkBlock
               (bhHash (bheader blockEx6F'))
               (coreNodeKeys 6)
               [txEx6F'']
               ((slotFromEpoch $ Epoch 2) + Slot 10)
               (BlockNo 1)
               (mkNonce 0)
               (NatNonce 1)
               zero
               2

ex6F' :: Either [[PredicateFailure CHAIN]] ChainState
ex6F' = do
  nextState <- applySTS @CHAIN (TRC (Slot 90, initStEx2A, blockEx6F))
  midState <-
    applySTS @CHAIN (TRC (((slotFromEpoch $ Epoch 1) + Slot 7) +* startRewards, nextState, blockEx6F'))
  finalState <-
    applySTS @CHAIN (TRC (((slotFromEpoch $ Epoch 2) + Slot 10), midState, blockEx6F''))

  pure finalState

-- | Tests that after getting instantaneous rewards, creating the update and
-- then applying the update, Alice's key is actually registered, the key deposit
-- value deducted and the remaining value credited as reward.
test6F :: Assertion
test6F = do
  case ex6F' of
    Left _ -> assertFailure "encountered error state"
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
