{-# LANGUAGE PatternSynonyms #-}

module Examples
  ( CHAINExample(..)
  , ex1
  , ex2
  , ex3
  , ex4
  , ex5
  , ex6
  , ex7
  , ex8
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
  )
where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, fromList, singleton)
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Word (Word64)

import           Cardano.Crypto.DSIGN (deriveVerKeyDSIGN, genKeyDSIGN)
import           Cardano.Crypto.KES (deriveVerKeyKES, genKeyKES)
import           Crypto.Random (drgNewTest, withDRG)
import           MockTypes (Addr, Block, Credential, DState, EpochState, HashHeader, KeyHash,
                     KeyPair, LedgerState, NewEpochState, PState, PoolDistr, PoolParams, SKey,
                     SKeyES, SnapShots, Tx, TxBody, UTxO, UTxOState, VKey, VKeyES, VKeyGenesis)
import           Numeric.Natural (Natural)

import           BaseTypes (Seed (..), UnitInterval, mkUnitInterval, seedOp)
import           BlockChain (pattern BHBody, pattern BHeader, pattern Block, pattern Proof,
                     ProtVer (..), bBodySize, bhHash, bhbHash, bheader, slotToSeed)
import           Coin (Coin (..))
import           Delegation.Certificates (pattern Delegate, pattern PoolDistr, pattern RegKey,
                     pattern RegPool)
import           EpochBoundary (BlocksMade (..), pattern Stake, emptySnapShots, _feeSS, _poolsSS,
                     _pstakeMark, _pstakeSet)
import           Keys (pattern Dms, pattern KeyPair, pattern SKey, pattern SKeyES, pattern VKey,
                     pattern VKeyES, pattern VKeyGenesis, hashKey, sKey, sign, signKES, vKey)
import           LedgerState (AccountState (..), pattern DPState, pattern EpochState,
                     pattern LedgerState, pattern NewEpochState, pattern RewardUpdate,
                     pattern UTxOState, deltaF, deltaR, deltaT, emptyAccount, emptyDState,
                     emptyPState, genesisCoins, genesisId, overlaySchedule, rs, _cCounters,
                     _delegations, _dms, _pParams, _ptrs, _reserves, _rewards, _stKeys, _stPools,
                     _treasury)
import           OCert (KESPeriod (..), pattern OCert)
import           PParams (PParams (..), emptyPParams)
import           Slot (Epoch (..), Slot (..))
import           TxData (pattern AddrBase, pattern Delegation, pattern KeyHashObj,
                     pattern PoolParams, Ptr (..), pattern RewardAcnt, pattern StakeKeys,
                     pattern StakePools, pattern Tx, pattern TxBody, pattern TxIn, pattern TxOut,
                     _poolCost, _poolMargin, _poolOwners, _poolPledge, _poolPubKey, _poolRAcnt,
                     _poolVrf)
import           Updates (emptyUpdate, emptyUpdateState)
import           UTxO (pattern UTxO, makeWitnessesVKey, txid)


type ChainState = (NewEpochState, Seed, Seed, Maybe HashHeader, Slot)

data CHAINExample = CHAINExample Slot ChainState Block ChainState


-- | Set up keys for all the actors in the examples.


mkKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> (SKey, VKey)
mkKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyDSIGN
  return (SKey sk, VKey $ deriveVerKeyDSIGN sk)

-- | For testing purposes, generate a deterministic KES key pair given a seed.
mkKESKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> (SKeyES, VKeyES)
mkKESKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyKES 90
  return (SKeyES sk, VKeyES $ deriveVerKeyKES sk)

mkAddr :: (KeyPair, KeyPair) -> Addr
mkAddr (payKey, stakeKey) =
  AddrBase (KeyHashObj . hashKey $ vKey payKey) (KeyHashObj . hashKey $ vKey stakeKey)

gerolamoVKG :: VKeyGenesis
gerolamoVKG = VKeyGenesis 1501 :: VKeyGenesis

gerolamoCold :: KeyPair
gerolamoCold = KeyPair 1 1

gerolamoVRF :: KeyPair
gerolamoVRF = KeyPair 11 11

gerolamoHot :: (SKeyES, VKeyES)
gerolamoHot = mkKESKeyPair (0, 0, 0, 0, 0)

alicePay :: KeyPair
alicePay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (0, 0, 0, 0, 0)

aliceStake :: KeyPair
aliceStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (1, 1, 1, 1, 1)

aliceOperator :: KeyPair
aliceOperator = KeyPair vk sk
  where (sk, vk) = mkKeyPair (10, 10, 10, 10, 10)

aliceVRF :: KeyPair
aliceVRF = KeyPair vk sk
  where (sk, vk) = mkKeyPair (20, 20, 20, 20, 20)

aliceOperatorHK :: KeyHash
aliceOperatorHK = hashKey $ vKey aliceOperator

aliceHot :: (SKeyES, VKeyES)
aliceHot = mkKESKeyPair (0, 0, 0, 0, 1)

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

aliceInitCoin :: Coin
aliceInitCoin = 10000

bobInitCoin :: Coin
bobInitCoin = 1000

alicePoolParams :: PoolParams
alicePoolParams =
  PoolParams
    { _poolPubKey = vKey aliceOperator
    , _poolVrf = hashKey $ vKey aliceVRF
    , _poolPledge = Coin 1
    , _poolCost = Coin 5
    , _poolMargin = unsafeMkUnitInterval 0.1
    , _poolRAcnt = RewardAcnt aliceSHK
    , _poolOwners = Set.singleton $ (hashKey . vKey) aliceStake
    }


-- | Helper Functions

mkBlock :: Maybe HashHeader -> KeyPair -> KeyPair -> (SKeyES, VKeyES) -> [Tx] -> Slot
  -> Seed -> Seed -> UnitInterval -> Natural -> Block
mkBlock prev cold vrf (shot, vhot) txns s enonce bnonce l kesPeriod =
  let
    nonceSeed = (enonce `seedOp` (slotToSeed s)) `seedOp` SeedEta
    leaderSeed = (enonce `seedOp` (slotToSeed s)) `seedOp` SeedL
    bhb = BHBody
            prev
            (vKey cold)
            (vKey vrf)
            s
            bnonce
            (Proof (vKey vrf) nonceSeed bnonce)
            l
            (Proof (vKey vrf) leaderSeed l)
            (fromIntegral $ bBodySize txns)
            (bhbHash [txEx2])
            (OCert
              vhot
              (vKey cold)
              0
              (KESPeriod 0)
              (sign (sKey cold) (vhot, 0, KESPeriod 0))
            )
            (ProtVer 0 0 0)
    bh = BHeader bhb (Keys.signKES shot bhb kesPeriod)
  in
    Block bh txns

unsafeMkUnitInterval :: Rational -> UnitInterval
unsafeMkUnitInterval r =
  fromMaybe (error "could not construct unit interval") $ mkUnitInterval r

carlPay :: KeyPair
carlPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (4, 4, 4, 4, 4)

carlStake :: KeyPair
carlStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (5, 5, 5, 5, 5)

carlAddr :: Addr
carlAddr = mkAddr (carlPay, carlStake)


dariaPay :: KeyPair
dariaPay = KeyPair vk sk
  where (sk, vk) = mkKeyPair (6, 6, 6, 6, 6)

dariaStake :: KeyPair
dariaStake = KeyPair vk sk
  where (sk, vk) = mkKeyPair (7, 7, 7, 7, 7)

dariaAddr :: Addr
dariaAddr = mkAddr (dariaPay, dariaStake)

-- | Example 1 - apply CHAIN transition to an empty block


utxostEx1 :: UTxOState
utxostEx1 = UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState

dsEx1 :: DState
dsEx1 = emptyDState { _dms = Dms (Map.singleton gerolamoVKG (vKey gerolamoCold)) }

psEx1 :: PState
psEx1 = emptyPState { _cCounters = Map.singleton (hashKey $ vKey gerolamoCold) 0}

lsEx1 :: LedgerState
lsEx1 = LedgerState utxostEx1 (DPState dsEx1 psEx1) 0

ppsEx1 :: PParams
ppsEx1 = emptyPParams { _maxBBSize = 10000
                   , _maxBHSize = 10000
                   , _keyDeposit = Coin 7
                   , _poolDeposit = Coin 250
                   , _d = unsafeMkUnitInterval 0.5
                   , _activeSlotCoeff = unsafeMkUnitInterval 0.1 }

esEx1 :: EpochState
esEx1 = EpochState emptyAccount emptySnapShots lsEx1 ppsEx1

initStEx1 :: ChainState
initStEx1 =
  ( NewEpochState
      (Epoch 0)
      (Nonce 0)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      esEx1
      Nothing
      (PoolDistr Map.empty)
      (Map.singleton (Slot 1) (Just gerolamoVKG))
      -- The overlay schedule has one entry, setting Gerolamo to slot 1.
  , Nonce 0
  , Nonce 0
  , Nothing
  , Slot 0
  )

zero :: UnitInterval
zero = unsafeMkUnitInterval 0

blockEx1 :: Block
blockEx1 = mkBlock Nothing gerolamoCold gerolamoVRF gerolamoHot [] slot1 (Nonce 0) (Nonce 1) zero 0

expectedStEx1 :: ChainState
expectedStEx1 =
  ( NewEpochState
      (Epoch 0)
      (Nonce 0)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      -- Note that blocks in the overlay schedule do not add to this count.
      esEx1
      Nothing
      (PoolDistr Map.empty)
      (Map.singleton (Slot 1) (Just gerolamoVKG))
  , SeedOp (Nonce 0) (Nonce 1)
  , SeedOp (Nonce 0) (Nonce 1)
  , Just (bhHash (bheader blockEx1))
  , Slot 1
  )

slot1 :: Slot
slot1 = Slot 1

ex1 :: CHAINExample
ex1 = CHAINExample slot1 initStEx1 blockEx1 expectedStEx1


-- | Example 2 - apply CHAIN transition to register a stake key and a pool


utxoEx2 :: UTxO
utxoEx2 = genesisCoins
       [ TxOut aliceAddr aliceInitCoin
       , TxOut bobAddr bobInitCoin]

txbodyEx2 :: TxBody
txbodyEx2 = TxBody
           (Set.fromList [TxIn genesisId 0])
           [TxOut aliceAddr (Coin 9740)]
           [ RegKey aliceSHK
           , RegPool alicePoolParams
           ]
           Map.empty
           (Coin 3)
           (Slot 10)
           emptyUpdate

txEx2 :: Tx
txEx2 = Tx txbodyEx2 (makeWitnessesVKey txbodyEx2 [alicePay, aliceStake, aliceOperator]) Map.empty

utxostEx2 :: UTxOState
utxostEx2 = UTxOState utxoEx2 (Coin 0) (Coin 0) emptyUpdateState

lsEx2 :: LedgerState
lsEx2 = LedgerState utxostEx2 (DPState dsEx1 psEx1) 0

esEx2 :: EpochState
esEx2 = EpochState emptyAccount emptySnapShots lsEx2 ppsEx1

overlayEx2 :: Map Slot (Maybe VKeyGenesis)
overlayEx2 =
  Map.fromList [ (Slot 1, Just gerolamoVKG)
                , (Slot 89, Just gerolamoVKG)
                ]

initStEx2 :: ChainState
initStEx2 =
  ( NewEpochState
      (Epoch 0)
      (Nonce 0)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      esEx2
      Nothing
      (PoolDistr Map.empty)
      overlayEx2
  , Nonce 0
  , Nonce 0
  , Nothing
  , Slot 0
  )

blockEx2 :: Block
blockEx2 = mkBlock
             Nothing
             gerolamoCold
             gerolamoVRF
             gerolamoHot
             [txEx2]
             slot1
             (Nonce 0)
             (Nonce 1)
             zero
             0

dsEx2 :: DState
dsEx2 = dsEx1
          { _ptrs = Map.singleton (Ptr (Slot 1) 0 1) aliceSHK
          , _stKeys = StakeKeys $ Map.singleton aliceSHK (Slot 1)
          , _rewards = Map.singleton (RewardAcnt aliceSHK) (Coin 0)
          }

psEx2 :: PState
psEx2 = psEx1
          { _stPools = StakePools $ Map.singleton aliceOperatorHK (Slot 1)
          , _pParams = Map.singleton aliceOperatorHK alicePoolParams
          , _cCounters = Map.fromList [ (hashKey $ vKey gerolamoCold, 0)
                                      , (aliceOperatorHK, 0)
                                      ]
          }

expectedLSEx2 :: LedgerState
expectedLSEx2 = LedgerState
               (UTxOState
                 (UTxO . Map.fromList $
                   [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
                   , (TxIn (txid txbodyEx2) 0, TxOut aliceAddr (Coin 9740))
                   ])
                 (Coin 257)
                 (Coin 3)
                 emptyUpdateState)
               (DPState dsEx2 psEx2)
               0

blockEx2Hash :: Maybe HashHeader
blockEx2Hash = Just (bhHash (bheader blockEx2))

expectedStEx2 :: ChainState
expectedStEx2 =
  ( NewEpochState
      (Epoch 0)
      (Nonce 0)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState emptyAccount emptySnapShots expectedLSEx2 ppsEx1)
      Nothing
      (PoolDistr Map.empty)
      overlayEx2
  , SeedOp (Nonce 0) (Nonce 1)
  , SeedOp (Nonce 0) (Nonce 1)
  , blockEx2Hash
  , Slot 1
  )

ex2 :: CHAINExample
ex2 = CHAINExample slot1 initStEx2 blockEx2 expectedStEx2


-- | Example 3 - continuing on after example 2, process a block late enough
-- in the epoch in order to create a reward update.
-- The block delegates Alice's stake to Alice's pool.

txbodyEx3 :: TxBody
txbodyEx3 = TxBody
           (Set.fromList [TxIn (txid txbodyEx2) 0])
           [TxOut aliceAddr (Coin 9736)]
           [ Delegate $ Delegation aliceSHK aliceOperatorHK ]
           Map.empty
           (Coin 4)
           (Slot 99)
           emptyUpdate

txEx3 :: Tx
txEx3 = Tx txbodyEx3 (makeWitnessesVKey txbodyEx3 [alicePay, aliceStake]) Map.empty

blockEx3 :: Block
blockEx3 = mkBlock
             blockEx2Hash
             gerolamoCold
             gerolamoVRF
             gerolamoHot
             [txEx3]
             (Slot 89)
             (Nonce 0)
             (Nonce 2)
             zero
             0

blockEx3Hash :: Maybe HashHeader
blockEx3Hash = Just (bhHash (bheader blockEx3))

utxoEx3 :: UTxO
utxoEx3 = UTxO . Map.fromList $
                   [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
                   , (TxIn (txid txbodyEx3) 0, TxOut aliceAddr (Coin 9736))
                   ]

dsEx3 :: DState
dsEx3 = dsEx2 { _delegations = Map.singleton aliceSHK aliceOperatorHK }

expectedLSEx3 :: LedgerState
expectedLSEx3 = LedgerState
               (UTxOState
                 utxoEx3
                 (Coin 257)
                 (Coin 7)
                 emptyUpdateState)
               (DPState dsEx3 psEx2)
               0

expectedStEx3 :: ChainState
expectedStEx3 =
  ( NewEpochState
      (Epoch 0)
      (Nonce 0)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState emptyAccount emptySnapShots expectedLSEx3 ppsEx1)
      (Just RewardUpdate { deltaT = Coin 0
                         , deltaR = Coin 0
                         , rs     = Map.empty
                         , deltaF = Coin 0
                         })
      (PoolDistr Map.empty)
      overlayEx2
  , SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 2)
  , SeedOp (Nonce 0) (Nonce 1)
  , blockEx3Hash
  , Slot 89
  )

ex3 :: CHAINExample
ex3 = CHAINExample (Slot 89) expectedStEx2 blockEx3 expectedStEx3


-- | Example 4 - continuing on after example 3, process an empty block in the next epoch
-- so that the (empty) reward update is applied and a stake snapshot is made.


blockEx4 :: Block
blockEx4 = mkBlock
             blockEx3Hash
             gerolamoCold
             gerolamoVRF
             gerolamoHot
             []
             (Slot 110)
             (Nonce 0)
             (Nonce 88)
             zero
             1

-- | This overlay schedule creates BFT slots on the even slot
-- with Gerolamo assigned to the multiples of ten.
epoch1OSchedEx4 :: Map Slot (Maybe VKeyGenesis)
epoch1OSchedEx4 = overlaySchedule
                    (Epoch 1)
                    (Set.singleton gerolamoVKG)
                    (SeedOp (Nonce 0) (Nonce 1))
                    ppsEx1

snapsEx4 :: SnapShots
snapsEx4 = emptySnapShots { _pstakeMark =
                              (Stake
                                 ( Map.singleton aliceSHK (Coin 9736))
                                 , Map.singleton aliceSHK aliceOperatorHK )
                          , _poolsSS = Map.singleton aliceOperatorHK alicePoolParams
                          , _feeSS = Coin 264
                          }

expectedLSEx4 :: LedgerState
expectedLSEx4 = LedgerState
               (UTxOState
                 utxoEx3
                 (Coin 0)   -- TODO check that both deposits really decayed completely
                 (Coin 264) -- TODO shouldn't this pot have moved to the treasury?
                 emptyUpdateState)
               (DPState dsEx3 psEx2)
               0

blockEx4Hash :: Maybe HashHeader
blockEx4Hash = Just (bhHash (bheader blockEx4))

expectedStEx4 :: ChainState
expectedStEx4 =
  ( NewEpochState
      (Epoch 1)
      (SeedOp (Nonce 0) (Nonce 1))
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState emptyAccount snapsEx4 expectedLSEx4 ppsEx1)
      Nothing
      (PoolDistr Map.empty)
      epoch1OSchedEx4
  , SeedOp (SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 2)) (Nonce 88)
  , SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 88)
  , blockEx4Hash
  , Slot 110
  )

ex4 :: CHAINExample
ex4 = CHAINExample (Slot 110) expectedStEx3 blockEx4 expectedStEx4


-- | Example 5 - continuing on after example 4, process an empty block late enough
-- in the epoch in order to create a second reward update, preparing the way for
-- the first non-empty pool distribution in this running example.


blockEx5 :: Block
blockEx5 = mkBlock
             blockEx4Hash
             gerolamoCold
             gerolamoVRF
             gerolamoHot
             []
             (Slot 190)
             (Nonce 0)
             (Nonce 13)
             zero
             2

blockEx5Hash :: Maybe HashHeader
blockEx5Hash = Just (bhHash (bheader blockEx5))

expectedStEx5 :: ChainState
expectedStEx5 =
  ( NewEpochState
      (Epoch 1)
      (SeedOp (Nonce 0) (Nonce 1))
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState emptyAccount snapsEx4 expectedLSEx4 ppsEx1)
      (Just RewardUpdate { deltaT = Coin 264
                         , deltaR = Coin 0
                         , rs     = Map.empty
                         , deltaF = Coin (-264)
                         })
      (PoolDistr Map.empty)
      epoch1OSchedEx4
  , SeedOp (SeedOp (SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 2)) (Nonce 88)) (Nonce 13)
  , SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 88)
  , blockEx5Hash
  , Slot 190
  )

ex5 :: CHAINExample
ex5 = CHAINExample (Slot 190) expectedStEx4 blockEx5 expectedStEx5


-- | Example 6 - continuing on after example 5, create the first non-empty pool distribution
-- by creating a block in the third epoch of this running example.


blockEx6 :: Block
blockEx6 = mkBlock
             blockEx5Hash
             gerolamoCold
             gerolamoVRF
             gerolamoHot
             []
             (Slot 210)
             (SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 88))
             (Nonce 987)
             zero
             2

-- | This overlay schedule creates BFT slots on the even slot
-- with Gerolamo assigned to the multiples of ten.
epoch1OSchedEx6 :: Map Slot (Maybe VKeyGenesis)
epoch1OSchedEx6 = overlaySchedule
                    (Epoch 2)
                    (Set.singleton gerolamoVKG)
                    (SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 88))
                    ppsEx1

snapsEx6 :: SnapShots
snapsEx6 = emptySnapShots { _pstakeMark =
                              (Stake
                                 ( Map.singleton aliceSHK (Coin 9736))
                                 , Map.singleton aliceSHK aliceOperatorHK )
                          , _pstakeSet =
                              (Stake
                                 ( Map.singleton aliceSHK (Coin 9736))
                                 , Map.singleton aliceSHK aliceOperatorHK )
                          , _poolsSS = Map.singleton aliceOperatorHK alicePoolParams
                          , _feeSS = Coin 0
                          }

expectedLSEx6 :: LedgerState
expectedLSEx6 = LedgerState
               (UTxOState
                 utxoEx3
                 (Coin 0)
                 (Coin 0)
                 emptyUpdateState)
               (DPState dsEx3 psEx2)
               0

blockEx6Hash :: Maybe HashHeader
blockEx6Hash = Just (bhHash (bheader blockEx6))

acntEx6 :: AccountState
acntEx6 = AccountState
            { _treasury = Coin 264
            , _reserves = Coin 0
            }

expectedStEx6 :: ChainState
expectedStEx6 =
  ( NewEpochState
      (Epoch 2)
      (SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 88))
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx6 snapsEx6 expectedLSEx6 ppsEx1)
      Nothing
      (PoolDistr
        (Map.singleton
           aliceOperatorHK
           (1, hashKey (vKey aliceVRF))))
      epoch1OSchedEx6
  , SeedOp
      (SeedOp (SeedOp (SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 2)) (Nonce 88)) (Nonce 13))
      (Nonce 987)
  , SeedOp (SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 88)) (Nonce 987)
  , blockEx6Hash
  , Slot 210
  )

ex6 :: CHAINExample
ex6 = CHAINExample (Slot 210) expectedStEx5 blockEx6 expectedStEx6


--  | Example 7 - create a decentralized Praos block (ie one not in the overlay schedule)


blockEx7 :: Block
blockEx7 = mkBlock
             blockEx6Hash
             aliceOperator
             aliceVRF
             aliceHot
             []
             (Slot 215) -- slot 15 is not even, and hence open for decentralization in epoch1OSchedEx6
             (SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 88))
             (Nonce 100)
             zero
             2

blockEx7Hash :: Maybe HashHeader
blockEx7Hash = Just (bhHash (bheader blockEx7))

pdEx7 :: PoolDistr
pdEx7 = PoolDistr $ Map.singleton aliceOperatorHK (1, hashKey (vKey aliceVRF))

expectedStEx7 :: ChainState
expectedStEx7 =
  ( NewEpochState
      (Epoch 2)
      (SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 88))
      (BlocksMade Map.empty)
      (BlocksMade $ Map.singleton aliceOperatorHK 1)
      (EpochState acntEx6 snapsEx6 expectedLSEx6 ppsEx1)
      Nothing
      pdEx7
      epoch1OSchedEx6
  , SeedOp
      (SeedOp
        (SeedOp (SeedOp (SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 2)) (Nonce 88)) (Nonce 13))
        (Nonce 987))
      (Nonce 100)
  , SeedOp (SeedOp (SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 88)) (Nonce 987)) (Nonce 100)
  , blockEx7Hash
  , Slot 215
  )

ex7 :: CHAINExample
ex7 = CHAINExample (Slot 215) expectedStEx6 blockEx7 expectedStEx7


--  | Example 8 - create the first non-trivial reward update by processing an
--  empty block late in the epoch.


blockEx8 :: Block
blockEx8 = mkBlock
             blockEx7Hash
             gerolamoCold
             gerolamoVRF
             gerolamoHot
             []
             (Slot 290)
             (SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 88))
             (Nonce 888)
             zero
             3

blockEx8Hash :: Maybe HashHeader
blockEx8Hash = Just (bhHash (bheader blockEx8))

expectedStEx8 :: ChainState
expectedStEx8 =
  ( NewEpochState
      (Epoch 2)
      (SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 88))
      (BlocksMade Map.empty)
      (BlocksMade $ Map.singleton aliceOperatorHK 1)
      (EpochState acntEx6 snapsEx6 expectedLSEx6 ppsEx1)
      (Just RewardUpdate { deltaT = Coin 0
                         , deltaR = Coin 0
                         , rs     = Map.empty
                         , deltaF = Coin 0
                         })
      pdEx7
      epoch1OSchedEx6
  , SeedOp
      (SeedOp
        (SeedOp
          (SeedOp (SeedOp (SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 2)) (Nonce 88)) (Nonce 13))
          (Nonce 987))
        (Nonce 100))
      (Nonce 888)
  , SeedOp
      (SeedOp (SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 88)) (Nonce 987))
      (Nonce 100)
  , blockEx8Hash
  , Slot 290
  )

ex8 :: CHAINExample
ex8 = CHAINExample (Slot 290) expectedStEx7 blockEx8 expectedStEx8
