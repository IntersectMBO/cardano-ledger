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
  , ex9
  , ex10
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
import qualified Data.Map.Strict as Map (elems, empty, fromList, insert, keysSet, singleton)
import           Data.Maybe (fromMaybe)
import           Data.Sequence (fromList)
import qualified Data.Set as Set
import           Data.Word (Word64)

import           Cardano.Crypto.DSIGN (deriveVerKeyDSIGN, genKeyDSIGN)
import           Cardano.Crypto.KES (deriveVerKeyKES, genKeyKES)
import           Crypto.Random (drgNewTest, withDRG)
import           MockTypes (AVUpdate, Addr, Block, Credential, DState, EpochState, HashHeader,
                     KeyHash, KeyPair, LedgerState, NewEpochState, PPUpdate, PState, PoolDistr,
                     PoolParams, RewardAcnt, SKey, SKeyES, SnapShots, Stake, Tx, TxBody, UTxO,
                     UTxOState, Update, VKey, VKeyES, VKeyGenesis)
import           Numeric.Natural (Natural)

import           BaseTypes (Seed (..), UnitInterval, mkUnitInterval, (⭒))
import           BlockChain (pattern BHBody, pattern BHeader, pattern Block, pattern Proof,
                     ProtVer (..), TxSeq (..), bBodySize, bhHash, bhbHash, bheader, slotToSeed)
import           Coin (Coin (..))
import           Delegation.Certificates (pattern Delegate, pattern PoolDistr, pattern RegKey,
                     pattern RegPool)
import           EpochBoundary (BlocksMade (..), pattern Stake, emptySnapShots, _feeSS, _poolsSS,
                     _pstakeGo, _pstakeMark, _pstakeSet)
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
import           Updates (pattern AVUpdate, Applications (..), pattern PPUpdate, Ppm (..),
                     pattern Update, emptyUpdate, emptyUpdateState)
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
gerolamoCold = KeyPair vk sk
  where (sk, vk) = mkKeyPair (1501, 0, 0, 0, 0)

gerolamoVRF :: KeyPair
gerolamoVRF = KeyPair vk sk
  where (sk, vk) = mkKeyPair (1501, 0, 0, 0, 1)

gerolamoHot :: (SKeyES, VKeyES)
gerolamoHot = mkKESKeyPair (0, 0, 0, 0, 0)

lodovicoVKG :: VKeyGenesis
lodovicoVKG = VKeyGenesis 1521 :: VKeyGenesis

lodovicoCold :: KeyPair
lodovicoCold = KeyPair vk sk
  where (sk, vk) = mkKeyPair (1521, 0, 0, 0, 0)

lodovicoVRF :: KeyPair
lodovicoVRF = KeyPair vk sk
  where (sk, vk) = mkKeyPair (1521, 0, 0, 0, 1)

lodovicoHot :: (SKeyES, VKeyES)
lodovicoHot = mkKESKeyPair (1, 0, 0, 0, 0)

nicoloVKG :: VKeyGenesis
nicoloVKG = VKeyGenesis 1499 :: VKeyGenesis

nicoloCold :: KeyPair
nicoloCold = KeyPair vk sk
  where (sk, vk) = mkKeyPair (1499, 0, 0, 0, 0)

nicoloVRF :: KeyPair
nicoloVRF = KeyPair vk sk
  where (sk, vk) = mkKeyPair (1499, 0, 0, 0, 1)

nicoloHot :: (SKeyES, VKeyES)
nicoloHot = mkKESKeyPair (1, 0, 0, 0, 0)

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

bobSHK :: Credential
bobSHK = (KeyHashObj . hashKey . vKey) bobStake

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

mkSeqNonce :: Natural -> Seed
mkSeqNonce m = foldl (\c x -> c ⭒ Nonce x) NeutralSeed [0..toInteger m]

mkBlock :: Maybe HashHeader -> KeyPair -> KeyPair -> (SKeyES, VKeyES) -> [Tx] -> Slot
  -> Seed -> Seed -> UnitInterval -> Natural -> Block
mkBlock prev cold vrf (shot, vhot) txns s enonce bnonce l kesPeriod =
  let
    nonceSeed = (enonce ⭒ slotToSeed s) ⭒ SeedEta
    leaderSeed = (enonce ⭒ slotToSeed s) ⭒ SeedL
    bhb = BHBody
            prev
            (vKey cold)
            (vKey vrf)
            s
            bnonce
            (Proof (vKey vrf) nonceSeed bnonce)
            l

            (Proof (vKey vrf) leaderSeed l)
            (fromIntegral $ bBodySize $ (TxSeq . fromList) txns)
            (bhbHash $ TxSeq $ fromList [txEx2])
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
    Block bh (TxSeq $ fromList txns)

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

genesisDelegations :: Map VKeyGenesis VKey
genesisDelegations = Map.fromList [ (gerolamoVKG, vKey gerolamoCold)
                                  , (lodovicoVKG, vKey lodovicoCold)
                                  , (nicoloVKG,   vKey nicoloCold) ]

dsEx1 :: DState
dsEx1 = emptyDState { _dms = Dms genesisDelegations }

psEx1 :: PState
psEx1 = emptyPState { _cCounters = Map.fromList (fmap f (Map.elems genesisDelegations)) }
  where f vk = (hashKey vk, 0)

lsEx1 :: LedgerState
lsEx1 = LedgerState utxostEx1 (DPState dsEx1 psEx1) 0

ppsEx1 :: PParams
ppsEx1 = emptyPParams { _maxBBSize = 10000
                   , _maxBHSize = 10000
                   , _keyDeposit = Coin 7
                   , _poolDeposit = Coin 250
                   , _d = unsafeMkUnitInterval 0.5
                   , _activeSlotCoeff = unsafeMkUnitInterval 0.1
                   , _tau = unsafeMkUnitInterval 0.2
                   , _rho = unsafeMkUnitInterval 0.0021
                   }

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
blockEx1 = mkBlock
             Nothing
             gerolamoCold
             gerolamoVRF
             gerolamoHot
             []
             (Slot 1)
             (Nonce 0)
             (Nonce 1)
             zero
             0

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
  , Nonce 0 ⭒ Nonce 1
  , Nonce 0 ⭒ Nonce 1
  , Just (bhHash (bheader blockEx1))
  , Slot 1
  )

ex1 :: CHAINExample
ex1 = CHAINExample (Slot 1) initStEx1 blockEx1 expectedStEx1


-- | Example 2 - apply CHAIN transition to register a stake keys and a pool


utxoEx2 :: UTxO
utxoEx2 = genesisCoins
       [ TxOut aliceAddr aliceInitCoin
       , TxOut bobAddr bobInitCoin]

ppupEx2 :: PPUpdate
ppupEx2 = PPUpdate $ Map.singleton gerolamoVKG (Set.singleton (PoolDeposit 255))

updateEx2 :: Update
updateEx2 = Update ppupEx2 (AVUpdate Map.empty)

txbodyEx2 :: TxBody
txbodyEx2 = TxBody
           (Set.fromList [TxIn genesisId 0])
           [TxOut aliceAddr (Coin 9733)]
           (fromList [ RegKey aliceSHK
           , RegKey bobSHK
           , RegPool alicePoolParams
           ])
           Map.empty
           (Coin 3)
           (Slot 10)
           updateEx2

txEx2 :: Tx
txEx2 = Tx
          txbodyEx2
          (makeWitnessesVKey
            txbodyEx2
            [alicePay, aliceStake, bobStake, aliceOperator, gerolamoCold])
          Map.empty

utxostEx2 :: UTxOState
utxostEx2 = UTxOState utxoEx2 (Coin 0) (Coin 0) emptyUpdateState

lsEx2 :: LedgerState
lsEx2 = LedgerState utxostEx2 (DPState dsEx1 psEx1) 0

acntEx2 :: AccountState
acntEx2 = AccountState
            { _treasury = Coin 0
            , _reserves = Coin 45*1000*1000*1000*1000*1000
            }

esEx2 :: EpochState
esEx2 = EpochState acntEx2 emptySnapShots lsEx2 ppsEx1


-- | This overlay schedule creates BFT slots on the even slot
-- with Gerolamo assigned to the multiples of ten.
overlayEx2 :: Map Slot (Maybe VKeyGenesis)
overlayEx2 = overlaySchedule
                    (Epoch 0)
                    (Map.keysSet genesisDelegations)
                    NeutralSeed
                    ppsEx1

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
             (Slot 10)
             (Nonce 0)
             (Nonce 1)
             zero
             0

dsEx2 :: DState
dsEx2 = dsEx1
          { _ptrs = Map.fromList [ (Ptr (Slot 10) 0 0, aliceSHK)
                                 , (Ptr (Slot 10) 0 1, bobSHK) ]
          , _stKeys = StakeKeys $ Map.fromList [ (aliceSHK, Slot 10)
                                               , (bobSHK, Slot 10) ]
          , _rewards = Map.fromList [ (RewardAcnt aliceSHK, Coin 0)
                                    , (RewardAcnt bobSHK, Coin 0) ]
          }

psEx2 :: PState
psEx2 = psEx1
          { _stPools = StakePools $ Map.singleton aliceOperatorHK (Slot 10)
          , _pParams = Map.singleton aliceOperatorHK alicePoolParams
          , _cCounters = Map.insert aliceOperatorHK 0 (_cCounters psEx1)
          }

updateStEx2 :: ( PPUpdate
               , AVUpdate
               , Map Slot Applications
               , Applications)
updateStEx2 =
  ( ppupEx2
  , AVUpdate Map.empty
  , Map.empty
  , Applications Map.empty)

expectedLSEx2 :: LedgerState
expectedLSEx2 = LedgerState
               (UTxOState
                 (UTxO . Map.fromList $
                   [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
                   , (TxIn (txid txbodyEx2) 0, TxOut aliceAddr (Coin 9733))
                   ])
                 (Coin 264)
                 (Coin 3)
                 updateStEx2)
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
      (EpochState acntEx2 emptySnapShots expectedLSEx2 ppsEx1)
      Nothing
      (PoolDistr Map.empty)
      overlayEx2
  , Nonce 0 ⭒ Nonce 1
  , Nonce 0 ⭒ Nonce 1
  , blockEx2Hash
  , Slot 10
  )

ex2 :: CHAINExample
ex2 = CHAINExample (Slot 10) initStEx2 blockEx2 expectedStEx2


-- | Example 3 - continuing on after example 2, process a block late enough
-- in the epoch in order to create a reward update.
-- The block delegates Alice's and Bob's stake to Alice's pool.

txbodyEx3 :: TxBody
txbodyEx3 = TxBody
           (Set.fromList [TxIn (txid txbodyEx2) 0])
           [TxOut aliceAddr (Coin 9729)]
           (fromList [ Delegate $ Delegation aliceSHK aliceOperatorHK
           , Delegate $ Delegation bobSHK aliceOperatorHK
           ])
           Map.empty
           (Coin 4)
           (Slot 99)
           emptyUpdate

txEx3 :: Tx
txEx3 = Tx
          txbodyEx3
          (makeWitnessesVKey txbodyEx3 [alicePay, aliceStake, bobStake, gerolamoCold])
          Map.empty

blockEx3 :: Block
blockEx3 = mkBlock
             blockEx2Hash
             nicoloCold
             nicoloVRF
             nicoloHot
             [txEx3]
             (Slot 90)
             (Nonce 0)
             (Nonce 2)
             zero
             1

blockEx3Hash :: Maybe HashHeader
blockEx3Hash = Just (bhHash (bheader blockEx3))

utxoEx3 :: UTxO
utxoEx3 = UTxO . Map.fromList $
                   [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
                   , (TxIn (txid txbodyEx3) 0, TxOut aliceAddr (Coin 9729))
                   ]

delegsEx3 :: Map Credential KeyHash
delegsEx3 = Map.fromList
              [ (aliceSHK, aliceOperatorHK)
              , (bobSHK, aliceOperatorHK)
              ]

dsEx3 :: DState
dsEx3 = dsEx2 { _delegations = delegsEx3 }

expectedLSEx3 :: LedgerState
expectedLSEx3 = LedgerState
               (UTxOState
                 utxoEx3
                 (Coin 264)
                 (Coin 7)
                 updateStEx2)
               (DPState dsEx3 psEx2)
               0

expectedStEx3 :: ChainState
expectedStEx3 =
  ( NewEpochState
      (Epoch 0)
      (Nonce 0)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx2 emptySnapShots expectedLSEx3 ppsEx1)
      (Just RewardUpdate { deltaT = Coin 0
                         , deltaR = Coin 0
                         , rs     = Map.empty
                         , deltaF = Coin 0
                         })
      (PoolDistr Map.empty)
      overlayEx2
  , Nonce 0 ⭒ Nonce 1 ⭒ Nonce 2
  , Nonce 0 ⭒ Nonce 1
  , blockEx3Hash
  , Slot 90
  )

ex3 :: CHAINExample
ex3 = CHAINExample (Slot 90) expectedStEx2 blockEx3 expectedStEx3


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
             (Nonce 3)
             zero
             1

epoch1OSchedEx4 :: Map Slot (Maybe VKeyGenesis)
epoch1OSchedEx4 = overlaySchedule
                    (Epoch 1)
                    (Map.keysSet genesisDelegations)
                    (Nonce 0 ⭒ Nonce 1)
                    ppsEx1

snapEx4 :: (Stake, Map Credential KeyHash)
snapEx4 = ( Stake ( Map.fromList [(aliceSHK, Coin 9729), (bobSHK, bobInitCoin)])
          , delegsEx3 )

snapsEx4 :: SnapShots
snapsEx4 = emptySnapShots { _pstakeMark = snapEx4
                          , _poolsSS = Map.singleton aliceOperatorHK alicePoolParams
                          , _feeSS = Coin 271
                          }

expectedLSEx4 :: LedgerState
expectedLSEx4 = LedgerState
               (UTxOState
                 utxoEx3
                 (Coin 0)   -- TODO check that both deposits really decayed completely
                 (Coin 271) -- TODO shouldn't this pot have moved to the treasury?
                 emptyUpdateState) -- Note that the ppup is gone now
               (DPState dsEx3 psEx2)
               0

blockEx4Hash :: Maybe HashHeader
blockEx4Hash = Just (bhHash (bheader blockEx4))

expectedStEx4 :: ChainState
expectedStEx4 =
  ( NewEpochState
      (Epoch 1)
      (Nonce 0 ⭒ Nonce 1)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx2 snapsEx4 expectedLSEx4 ppsEx1)
      Nothing
      (PoolDistr Map.empty)
      epoch1OSchedEx4
  , mkSeqNonce 3
  , mkSeqNonce 3
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
             nicoloCold
             nicoloVRF
             nicoloHot
             []
             (Slot 190)
             (Nonce 0 ⭒ Nonce 1)
             (Nonce 4)
             zero
             2

blockEx5Hash :: Maybe HashHeader
blockEx5Hash = Just (bhHash (bheader blockEx5))

expectedStEx5 :: ChainState
expectedStEx5 =
  ( NewEpochState
      (Epoch 1)
      (Nonce 0 ⭒ Nonce 1)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx2 snapsEx4 expectedLSEx4 ppsEx1)
      (Just RewardUpdate { deltaT = Coin 271
                         , deltaR = Coin 0
                         , rs     = Map.empty
                         , deltaF = Coin (-271)
                         })
      (PoolDistr Map.empty)
      epoch1OSchedEx4
  , mkSeqNonce 4
  , mkSeqNonce 3
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
             lodovicoCold
             lodovicoVRF
             lodovicoHot
             []
             (Slot 220)
             (mkSeqNonce 3)
             (Nonce 5)
             zero
             2

-- | This overlay schedule creates BFT slots on the even slot
-- with Gerolamo assigned to the multiples of ten.
epoch1OSchedEx6 :: Map Slot (Maybe VKeyGenesis)
epoch1OSchedEx6 = overlaySchedule
                    (Epoch 2)
                    (Map.keysSet genesisDelegations)
                    (mkSeqNonce 3)
                    ppsEx1

snapsEx6 :: SnapShots
snapsEx6 = emptySnapShots { _pstakeMark = snapEx4
                          , _pstakeSet = snapEx4
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
            { _treasury = Coin 271
            , _reserves = Coin 45*1000*1000*1000*1000*1000
            }

expectedStEx6 :: ChainState
expectedStEx6 =
  ( NewEpochState
      (Epoch 2)
      (mkSeqNonce 3)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx6 snapsEx6 expectedLSEx6 ppsEx1)
      Nothing
      (PoolDistr
        (Map.singleton
           aliceOperatorHK
           (1, hashKey (vKey aliceVRF))))
      epoch1OSchedEx6
  , mkSeqNonce 5
  , mkSeqNonce 5
  , blockEx6Hash
  , Slot 220
  )

ex6 :: CHAINExample
ex6 = CHAINExample (Slot 220) expectedStEx5 blockEx6 expectedStEx6


-- | Example 7 - continuing on after example 6, create a decentralized Praos block
-- (ie one not in the overlay schedule)


blockEx7 :: Block
blockEx7 = mkBlock
             blockEx6Hash
             aliceOperator
             aliceVRF
             aliceHot
             []
             (Slot 295) -- odd slots open for decentralization in epoch1OSchedEx6
             (mkSeqNonce 3)
             (Nonce 6)
             zero
             3

blockEx7Hash :: Maybe HashHeader
blockEx7Hash = Just (bhHash (bheader blockEx7))

pdEx7 :: PoolDistr
pdEx7 = PoolDistr $ Map.singleton aliceOperatorHK (1, hashKey (vKey aliceVRF))

expectedStEx7 :: ChainState
expectedStEx7 =
  ( NewEpochState
      (Epoch 2)
      (mkSeqNonce 3)
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
  , mkSeqNonce 6
  , mkSeqNonce 5
  , blockEx7Hash
  , Slot 295
  )

ex7 :: CHAINExample
ex7 = CHAINExample (Slot 295) expectedStEx6 blockEx7 expectedStEx7


-- | Example 8 - continuing on after example 7, create an empty block in the next epoch
-- to prepare the way for the first non-trivial reward update


blockEx8 :: Block
blockEx8 = mkBlock
             blockEx7Hash
             gerolamoCold
             gerolamoVRF
             gerolamoHot
             []
             (Slot 310)
             (mkSeqNonce 5)
             (Nonce 7)
             zero
             3

blockEx8Hash :: Maybe HashHeader
blockEx8Hash = Just (bhHash (bheader blockEx8))

epoch1OSchedEx8 :: Map Slot (Maybe VKeyGenesis)
epoch1OSchedEx8 = overlaySchedule
                    (Epoch 3)
                    (Map.keysSet genesisDelegations)
                    (mkSeqNonce 5)
                    ppsEx1

snapsEx8 :: SnapShots
snapsEx8 = snapsEx6 { _pstakeGo = snapEx4 }

expectedStEx8 :: ChainState
expectedStEx8 =
  ( NewEpochState
      (Epoch 3)
      (mkSeqNonce 5)
      (BlocksMade $ Map.singleton aliceOperatorHK 1)
      (BlocksMade Map.empty)
      (EpochState acntEx6 snapsEx8 expectedLSEx6 ppsEx1)
      Nothing
      pdEx7
      epoch1OSchedEx8
  , mkSeqNonce 7
  , mkSeqNonce 7
  , blockEx8Hash
  , Slot 310
  )

ex8 :: CHAINExample
ex8 = CHAINExample (Slot 310) expectedStEx7 blockEx8 expectedStEx8


-- | Example 9 - continuing on after example 8, create the first non-trivial reward update


blockEx9 :: Block
blockEx9 = mkBlock
             blockEx8Hash
             nicoloCold
             nicoloVRF
             nicoloHot
             []
             (Slot 390)
             (mkSeqNonce 5)
             (Nonce 8)
             zero
             4

blockEx9Hash :: Maybe HashHeader
blockEx9Hash = Just (bhHash (bheader blockEx9))

rewardsEx9 :: Map RewardAcnt Coin
rewardsEx9 = Map.fromList [ (RewardAcnt aliceSHK, Coin 82593524514)
                          , (RewardAcnt bobSHK, Coin 730001159951) ]

expectedStEx9 :: ChainState
expectedStEx9 =
  ( NewEpochState
      (Epoch 3)
      (mkSeqNonce 5)
      (BlocksMade $ Map.singleton aliceOperatorHK 1)
      (BlocksMade Map.empty)
      (EpochState acntEx6 snapsEx8 expectedLSEx6 ppsEx1)
      (Just RewardUpdate { deltaT = Coin 8637405315535
                         , deltaR = Coin (-9450000000000)
                         , rs = rewardsEx9
                         , deltaF = Coin 0
                         })
      pdEx7
      epoch1OSchedEx8
  , mkSeqNonce 8
  , mkSeqNonce 7
  , blockEx9Hash
  , Slot 390
  )

ex9 :: CHAINExample
ex9 = CHAINExample (Slot 390) expectedStEx8 blockEx9 expectedStEx9


-- | Example 10 - continuing on after example 9, apply the first non-trivial reward update


blockEx10 :: Block
blockEx10 = mkBlock
              blockEx9Hash
              gerolamoCold
              gerolamoVRF
              gerolamoHot
              []
              (Slot 410)
              (mkSeqNonce 7)
              (Nonce 9)
              zero
              4

blockEx10Hash :: Maybe HashHeader
blockEx10Hash = Just (bhHash (bheader blockEx10))

epoch1OSchedEx10 :: Map Slot (Maybe VKeyGenesis)
epoch1OSchedEx10 = overlaySchedule
                     (Epoch 4)
                     (Map.keysSet genesisDelegations)
                     (mkSeqNonce 7)
                     ppsEx1

acntEx10 :: AccountState
acntEx10 = AccountState
            { _treasury = Coin 8637405315806
            , _reserves = Coin 44990550000000000
            }

dsEx10 :: DState
dsEx10 = dsEx3 { _rewards = rewardsEx9 }

expectedLSEx10 :: LedgerState
expectedLSEx10 = LedgerState
               (UTxOState
                 utxoEx3
                 (Coin 0)
                 (Coin 0)
                 emptyUpdateState)
               (DPState dsEx10 psEx2)
               0

expectedStEx10 :: ChainState
expectedStEx10 =
  ( NewEpochState
      (Epoch 4)
      (mkSeqNonce 7)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx10 snapsEx8 expectedLSEx10 ppsEx1)
      Nothing
      pdEx7
      epoch1OSchedEx10
  , mkSeqNonce 9
  , mkSeqNonce 9
  , blockEx10Hash
  , Slot 410
  )

ex10 :: CHAINExample
ex10 = CHAINExample (Slot 410) expectedStEx9 blockEx10 expectedStEx10
