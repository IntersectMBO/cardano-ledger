{-# LANGUAGE PatternSynonyms #-}

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
  , ex3A
  , ex3B
  , ex3C
  , ex4A
  , ex4B
  , ex4C
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

import           Data.ByteString.Char8 (pack)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, empty, fromList, insert, keysSet, singleton)
import           Data.Maybe (fromMaybe)
import           Data.Sequence (empty, fromList)
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
import           Updates (pattern AVUpdate, ApName (..), ApVer (..), Applications (..),
                     Metadata (..), pattern PPUpdate, Ppm (..), pattern Update, emptyUpdate,
                     emptyUpdateState, updatePPup)
import           UTxO (pattern UTxO, makeWitnessesVKey, txid)


type ChainState = (NewEpochState, Seed, Seed, Maybe HashHeader, Slot)

data CHAINExample = CHAINExample Slot ChainState Block ChainState


-- | Set up keys for all the actors in the examples.


mkKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> (SKey, VKey)
mkKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyDSIGN
  return (SKey sk, VKey $ deriveVerKeyDSIGN sk)

mkVKGen :: (Word64, Word64, Word64, Word64, Word64) -> VKeyGenesis
mkVKGen seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyDSIGN
  return $ VKeyGenesis $ deriveVerKeyDSIGN sk

-- | For testing purposes, generate a deterministic KES key pair given a seed.
mkKESKeyPair :: (Word64, Word64, Word64, Word64, Word64) -> (SKeyES, VKeyES)
mkKESKeyPair seed = fst . withDRG (drgNewTest seed) $ do
  sk <- genKeyKES 90
  return (SKeyES sk, VKeyES $ deriveVerKeyKES sk)

mkAddr :: (KeyPair, KeyPair) -> Addr
mkAddr (payKey, stakeKey) =
  AddrBase (KeyHashObj . hashKey $ vKey payKey) (KeyHashObj . hashKey $ vKey stakeKey)

data AllPoolKeys = AllPoolKeys
  { cold :: KeyPair
  , vrf :: KeyPair
  , hot :: (SKeyES, VKeyES)
  , hk  :: KeyHash
  } deriving (Show, Eq)

mkAllPoolKeys :: Word64 -> AllPoolKeys
mkAllPoolKeys w = AllPoolKeys (KeyPair vkCold skCold)
                              (KeyPair vkVrf skVrf)
                              (mkKESKeyPair (w, 0, 0, 0, 3))
                              (hashKey vkCold)
  where
    (skCold, vkCold) = mkKeyPair (w, 0, 0, 0, 1)
    (skVrf, vkVrf) = mkKeyPair (w, 0, 0, 0, 2)

numCoreNodes :: Word64
numCoreNodes = 7

coreNodes :: [(VKeyGenesis, AllPoolKeys)]
coreNodes = [(mkVKGen (x, 0, 0, 0, 0), mkAllPoolKeys x) | x <-[101..100+numCoreNodes]]

coreNodeVKG :: Int -> VKeyGenesis
coreNodeVKG = fst . (coreNodes !!)

coreNodeKeys :: Int -> AllPoolKeys
coreNodeKeys = snd . (coreNodes !!)

dms :: Map VKeyGenesis VKey
dms = Map.fromList [ (gkey, vKey $ cold pkeys) | (gkey, pkeys) <- coreNodes]

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
    { _poolPubKey = vKey $ cold alicePool
    , _poolVrf = hashKey $ vKey $ vrf alicePool
    , _poolPledge = Coin 1
    , _poolCost = Coin 5
    , _poolMargin = unsafeMkUnitInterval 0.1
    , _poolRAcnt = RewardAcnt aliceSHK
    , _poolOwners = Set.singleton $ (hashKey . vKey) aliceStake
    }


-- | Helper Functions

mkSeqNonce :: Natural -> Seed
mkSeqNonce m = foldl (\c x -> c ⭒ Nonce x) NeutralSeed [0..toInteger m]

mkBlock :: Maybe HashHeader -> AllPoolKeys -> [Tx] -> Slot
  -> Seed -> Seed -> UnitInterval -> Natural -> Block
mkBlock prev pkeys txns s enonce bnonce l kesPeriod =
  let
    (shot, vhot) = hot pkeys
    nonceSeed = (enonce ⭒ slotToSeed s) ⭒ SeedEta
    leaderSeed = (enonce ⭒ slotToSeed s) ⭒ SeedL
    bhb = BHBody
            prev
            (vKey $ cold pkeys)
            (vKey $ vrf pkeys)
            s
            bnonce
            (Proof (vKey $ vrf pkeys) nonceSeed bnonce)
            l
            (Proof (vKey $ vrf pkeys) leaderSeed l)
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
dsEx1 = emptyDState { _dms = Dms dms }

psEx1 :: PState
psEx1 = emptyPState { _cCounters = Map.fromList (fmap f (Map.elems dms)) }
  where f vk = (hashKey vk, 0)

lsEx1 :: LedgerState
lsEx1 = LedgerState utxostEx1 (DPState dsEx1 psEx1) 0

ppsEx1 :: PParams
ppsEx1 = emptyPParams { _maxBBSize = 50000
                   , _maxBHSize = 10000
                   , _maxTxSize = 10000
                   , _keyDeposit = Coin 7
                   , _poolDeposit = Coin 250
                   , _d = unsafeMkUnitInterval 0.5
                   , _activeSlotCoeff = unsafeMkUnitInterval 0.1
                   , _tau = unsafeMkUnitInterval 0.2
                   , _rho = unsafeMkUnitInterval 0.0021
                   , _keyDecayRate = 0.002
                   , _keyMinRefund = unsafeMkUnitInterval 0.5
                   , _poolDecayRate = 0.001
                   , _poolMinRefund = unsafeMkUnitInterval 0.5
                   }

ppsExNoDecay :: PParams
ppsExNoDecay = ppsEx1 { _keyDecayRate = 0
                      , _poolDecayRate = 0 }

ppsExFullRefund :: PParams
ppsExFullRefund = ppsEx1 { _keyMinRefund = unsafeMkUnitInterval 1
                         , _poolMinRefund = unsafeMkUnitInterval 1 }

ppsExInstantDecay :: PParams
ppsExInstantDecay = ppsEx1 { _keyDecayRate = 1000
                           , _poolDecayRate = 1000 }


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
      (Map.singleton (Slot 1) (Just $ coreNodeVKG 0))
      -- The overlay schedule has one entry, setting Core Node 1 to slot 1.
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
             (coreNodeKeys 0)
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
      (Map.singleton (Slot 1) (Just $ coreNodeVKG 0))
  , Nonce 0 ⭒ Nonce 1
  , Nonce 0 ⭒ Nonce 1
  , Just (bhHash (bheader blockEx1))
  , Slot 1
  )

ex1 :: CHAINExample
ex1 = CHAINExample (Slot 1) initStEx1 blockEx1 expectedStEx1


-- | Example 2A - apply CHAIN transition to register stake keys and a pool


utxoEx2A :: UTxO
utxoEx2A = genesisCoins
       [ TxOut aliceAddr aliceInitCoin
       , TxOut bobAddr bobInitCoin]

ppupEx2A :: PPUpdate
ppupEx2A = PPUpdate $ Map.singleton (coreNodeVKG 0) (Set.singleton (PoolDeposit 255))

updateEx2A :: Update
updateEx2A = Update ppupEx2A (AVUpdate Map.empty)

txbodyEx2A :: TxBody
txbodyEx2A = TxBody
           (Set.fromList [TxIn genesisId 0])
           [TxOut aliceAddr (Coin 9733)]
           (fromList [ RegKey aliceSHK
           , RegKey bobSHK
           , RegPool alicePoolParams
           ])
           Map.empty
           (Coin 3)
           (Slot 10)
           updateEx2A

txEx2A :: Tx
txEx2A = Tx
          txbodyEx2A
          (makeWitnessesVKey
            txbodyEx2A
            [alicePay, aliceStake, bobStake, cold alicePool, cold $ coreNodeKeys 0])
          Map.empty

utxostEx2A :: UTxOState
utxostEx2A = UTxOState utxoEx2A (Coin 0) (Coin 0) emptyUpdateState

lsEx2A :: LedgerState
lsEx2A = LedgerState utxostEx2A (DPState dsEx1 psEx1) 0

acntEx2A :: AccountState
acntEx2A = AccountState
            { _treasury = Coin 0
            , _reserves = Coin 45*1000*1000*1000*1000*1000
            }

esEx2A :: EpochState
esEx2A = EpochState acntEx2A emptySnapShots lsEx2A ppsEx1


overlayEx2A :: Map Slot (Maybe VKeyGenesis)
overlayEx2A = overlaySchedule
                    (Epoch 0)
                    (Map.keysSet dms)
                    NeutralSeed
                    ppsEx1

initStEx2A :: ChainState
initStEx2A =
  ( NewEpochState
      (Epoch 0)
      (Nonce 0)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      esEx2A
      Nothing
      (PoolDistr Map.empty)
      overlayEx2A
  , Nonce 0
  , Nonce 0
  , Nothing
  , Slot 0
  )

blockEx2A :: Block
blockEx2A = mkBlock
             Nothing
             (coreNodeKeys 4)
             [txEx2A]
             (Slot 10)
             (Nonce 0)
             (Nonce 1)
             zero
             0

dsEx2A :: DState
dsEx2A = dsEx1
          { _ptrs = Map.fromList [ (Ptr (Slot 10) 0 0, aliceSHK)
                                 , (Ptr (Slot 10) 0 1, bobSHK) ]
          , _stKeys = StakeKeys $ Map.fromList [ (aliceSHK, Slot 10)
                                               , (bobSHK, Slot 10) ]
          , _rewards = Map.fromList [ (RewardAcnt aliceSHK, Coin 0)
                                    , (RewardAcnt bobSHK, Coin 0) ]
          }

psEx2A :: PState
psEx2A = psEx1
          { _stPools = StakePools $ Map.singleton (hk alicePool) (Slot 10)
          , _pParams = Map.singleton (hk alicePool) alicePoolParams
          , _cCounters = Map.insert (hk alicePool) 0 (_cCounters psEx1)
          }

updateStEx2A :: ( PPUpdate
               , AVUpdate
               , Map Slot Applications
               , Applications)
updateStEx2A =
  ( ppupEx2A
  , AVUpdate Map.empty
  , Map.empty
  , Applications Map.empty)

expectedLSEx2A :: LedgerState
expectedLSEx2A = LedgerState
               (UTxOState
                 (UTxO . Map.fromList $
                   [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
                   , (TxIn (txid txbodyEx2A) 0, TxOut aliceAddr (Coin 9733))
                   ])
                 (Coin 264)
                 (Coin 3)
                 updateStEx2A)
               (DPState dsEx2A psEx2A)
               0

blockEx2AHash :: Maybe HashHeader
blockEx2AHash = Just (bhHash (bheader blockEx2A))

expectedStEx2A :: ChainState
expectedStEx2A =
  ( NewEpochState
      (Epoch 0)
      (Nonce 0)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx2A emptySnapShots expectedLSEx2A ppsEx1)
      Nothing
      (PoolDistr Map.empty)
      overlayEx2A
  , Nonce 0 ⭒ Nonce 1
  , Nonce 0 ⭒ Nonce 1
  , blockEx2AHash
  , Slot 10
  )

ex2A :: CHAINExample
ex2A = CHAINExample (Slot 10) initStEx2A blockEx2A expectedStEx2A


-- | Example 2B - continuing on after example 2, process a block late enough
-- in the epoch in order to create a reward update.
-- The block delegates Alice's and Bob's stake to Alice's pool.

txbodyEx2B :: TxBody
txbodyEx2B = TxBody
           (Set.fromList [TxIn (txid txbodyEx2A) 0])
           [TxOut aliceAddr (Coin 9729)]
           (fromList [ Delegate $ Delegation aliceSHK (hk alicePool)
           , Delegate $ Delegation bobSHK (hk alicePool)
           ])
           Map.empty
           (Coin 4)
           (Slot 99)
           emptyUpdate

txEx2B :: Tx
txEx2B = Tx
          txbodyEx2B
          (makeWitnessesVKey txbodyEx2B [alicePay, aliceStake, bobStake, cold $ coreNodeKeys 0])
          Map.empty

blockEx2B :: Block
blockEx2B = mkBlock
             blockEx2AHash
             (coreNodeKeys 3)
             [txEx2B]
             (Slot 90)
             (Nonce 0)
             (Nonce 2)
             zero
             1

blockEx2BHash :: Maybe HashHeader
blockEx2BHash = Just (bhHash (bheader blockEx2B))

utxoEx2B :: UTxO
utxoEx2B = UTxO . Map.fromList $
                   [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
                   , (TxIn (txid txbodyEx2B) 0, TxOut aliceAddr (Coin 9729))
                   ]

delegsEx2B :: Map Credential KeyHash
delegsEx2B = Map.fromList
              [ (aliceSHK, hk alicePool)
              , (bobSHK, hk alicePool)
              ]

dsEx2B :: DState
dsEx2B = dsEx2A { _delegations = delegsEx2B }

expectedLSEx2B :: LedgerState
expectedLSEx2B = LedgerState
               (UTxOState
                 utxoEx2B
                 (Coin 264)
                 (Coin 7)
                 updateStEx2A)
               (DPState dsEx2B psEx2A)
               0

expectedStEx2Bgeneric :: PParams -> ChainState
expectedStEx2Bgeneric pp =
  ( NewEpochState
      (Epoch 0)
      (Nonce 0)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx2A emptySnapShots expectedLSEx2B pp)
      (Just RewardUpdate { deltaT = Coin 0
                         , deltaR = Coin 0
                         , rs     = Map.empty
                         , deltaF = Coin 0
                         })
      (PoolDistr Map.empty)
      overlayEx2A
  , Nonce 0 ⭒ Nonce 1 ⭒ Nonce 2
  , Nonce 0 ⭒ Nonce 1
  , blockEx2BHash
  , Slot 90
  )


expectedStEx2B :: ChainState
expectedStEx2B = expectedStEx2Bgeneric ppsEx1

expectedStEx2Bbis :: ChainState
expectedStEx2Bbis = expectedStEx2Bgeneric ppsExNoDecay

expectedStEx2Bter :: ChainState
expectedStEx2Bter = expectedStEx2Bgeneric ppsExFullRefund

expectedStEx2Bquater :: ChainState
expectedStEx2Bquater = expectedStEx2Bgeneric ppsExInstantDecay

ex2B :: CHAINExample
ex2B = CHAINExample (Slot 90) expectedStEx2A blockEx2B expectedStEx2B

-- | Example 2C - continuing on after example 3, process an empty block in the next epoch
-- so that the (empty) reward update is applied and a stake snapshot is made.


blockEx2C :: Block
blockEx2C = mkBlock
             blockEx2BHash
             (coreNodeKeys 4)
             []
             (Slot 110)
             (Nonce 0)
             (Nonce 3)
             zero
             1

epoch1OSchedEx2C :: Map Slot (Maybe VKeyGenesis)
epoch1OSchedEx2C = overlaySchedule
                    (Epoch 1)
                    (Map.keysSet dms)
                    (Nonce 0 ⭒ Nonce 1)
                    ppsEx1

snapEx2C :: (Stake, Map Credential KeyHash)
snapEx2C = ( Stake ( Map.fromList [(aliceSHK, Coin 9729), (bobSHK, bobInitCoin)])
          , delegsEx2B )

snapsEx2Cgeneric :: Coin -> SnapShots
snapsEx2Cgeneric feeSnapShot = emptySnapShots { _pstakeMark = snapEx2C
                          , _poolsSS = Map.singleton (hk alicePool) alicePoolParams
                          , _feeSS = feeSnapShot
                          }

snapsEx2C :: SnapShots
snapsEx2C = snapsEx2Cgeneric 20

snapsEx2Cbis :: SnapShots
snapsEx2Cbis = snapsEx2Cgeneric 7

snapsEx2Cter :: SnapShots
snapsEx2Cter = snapsEx2Cgeneric 7

snapsEx2Cquater :: SnapShots
snapsEx2Cquater = snapsEx2Cgeneric 140

expectedLSEx2Cgeneric :: Coin -> Coin -> LedgerState
expectedLSEx2Cgeneric lsDeposits lsFees =
  LedgerState
  (UTxOState
    utxoEx2B
    lsDeposits
    lsFees
    emptyUpdateState) -- Note that the ppup is gone now
  (DPState dsEx2B psEx2A)
               0

expectedLSEx2C :: LedgerState
expectedLSEx2C = expectedLSEx2Cgeneric 251 20

expectedLSEx2Cbis :: LedgerState
expectedLSEx2Cbis = expectedLSEx2Cgeneric 264 7

expectedLSEx2Cter :: LedgerState
expectedLSEx2Cter = expectedLSEx2Cgeneric 264 7

expectedLSEx2Cquater :: LedgerState
expectedLSEx2Cquater = expectedLSEx2Cgeneric 131 140

blockEx2CHash :: Maybe HashHeader
blockEx2CHash = Just (bhHash (bheader blockEx2C))

expectedStEx2Cgeneric :: SnapShots -> LedgerState -> PParams -> ChainState
expectedStEx2Cgeneric ss ls pp =
  ( NewEpochState
      (Epoch 1)
      (Nonce 0 ⭒ Nonce 1)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx2A ss ls pp)
      Nothing
      (PoolDistr Map.empty)
      epoch1OSchedEx2C
  , mkSeqNonce 3
  , mkSeqNonce 3
  , blockEx2CHash
  , Slot 110
  )

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

ex2C :: CHAINExample
ex2C = CHAINExample (Slot 110) expectedStEx2B blockEx2C expectedStEx2C

ex2Cbis :: CHAINExample
ex2Cbis = CHAINExample (Slot 110) expectedStEx2Bbis blockEx2C expectedStEx2Cbis

ex2Cter :: CHAINExample
ex2Cter = CHAINExample (Slot 110) expectedStEx2Bter blockEx2C expectedStEx2Cter

ex2Cquater :: CHAINExample
ex2Cquater =
  CHAINExample (Slot 110) expectedStEx2Bquater blockEx2C expectedStEx2Cquater


-- | Example 2D - continuing on after example 4, process an empty block late enough
-- in the epoch in order to create a second reward update, preparing the way for
-- the first non-empty pool distribution in this running example.


blockEx2D :: Block
blockEx2D = mkBlock
             blockEx2CHash
             (coreNodeKeys 3)
             []
             (Slot 190)
             (Nonce 0 ⭒ Nonce 1)
             (Nonce 4)
             zero
             2

blockEx2DHash :: Maybe HashHeader
blockEx2DHash = Just (bhHash (bheader blockEx2D))

expectedStEx2D :: ChainState
expectedStEx2D =
  ( NewEpochState
      (Epoch 1)
      (Nonce 0 ⭒ Nonce 1)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx2A snapsEx2C expectedLSEx2C ppsEx1)
      (Just RewardUpdate { deltaT = Coin 168
                         , deltaR = Coin 0
                         , rs     = Map.empty
                         , deltaF = Coin (-168)
                         })
      (PoolDistr Map.empty)
      epoch1OSchedEx2C
  , mkSeqNonce 4
  , mkSeqNonce 3
  , blockEx2DHash
  , Slot 190
  )

ex2D :: CHAINExample
ex2D = CHAINExample (Slot 190) expectedStEx2C blockEx2D expectedStEx2D


-- | Example 2E - continuing on after example 5, create the first non-empty pool distribution
-- by creating a block in the third epoch of this running example.


blockEx2E :: Block
blockEx2E = mkBlock
             blockEx2DHash
             (coreNodeKeys 3)
             []
             (Slot 220)
             (mkSeqNonce 3)
             (Nonce 5)
             zero
             2

epoch1OSchedEx2E :: Map Slot (Maybe VKeyGenesis)
epoch1OSchedEx2E = overlaySchedule
                    (Epoch 2)
                    (Map.keysSet dms)
                    (mkSeqNonce 3)
                    ppsEx1

snapsEx2E :: SnapShots
snapsEx2E = emptySnapShots { _pstakeMark = snapEx2C
                          , _pstakeSet = snapEx2C
                          , _poolsSS = Map.singleton (hk alicePool) alicePoolParams
                          , _feeSS = Coin 0
                          }

expectedLSEx2E :: LedgerState
expectedLSEx2E = LedgerState
               (UTxOState
                 utxoEx2B
                 (Coin 0)
                 (Coin 0)
                 emptyUpdateState)
               (DPState dsEx2B psEx2A)
               0

blockEx2EHash :: Maybe HashHeader
blockEx2EHash = Just (bhHash (bheader blockEx2E))

acntEx2E :: AccountState
acntEx2E = AccountState
            { _treasury = Coin 271
            , _reserves = Coin 45*1000*1000*1000*1000*1000
            }

expectedStEx2E :: ChainState
expectedStEx2E =
  ( NewEpochState
      (Epoch 2)
      (mkSeqNonce 3)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx2E snapsEx2E expectedLSEx2E ppsEx1)
      Nothing
      (PoolDistr
        (Map.singleton
           (hk alicePool)
           (1, hashKey (vKey $ vrf alicePool))))
      epoch1OSchedEx2E
  , mkSeqNonce 5
  , mkSeqNonce 5
  , blockEx2EHash
  , Slot 220
  )

ex2E :: CHAINExample
ex2E = CHAINExample (Slot 220) expectedStEx2D blockEx2E expectedStEx2E


-- | Example 2F - continuing on after example 6, create a decentralized Praos block
-- (ie one not in the overlay schedule)


blockEx2F :: Block
blockEx2F = mkBlock
             blockEx2EHash
             alicePool
             []
             (Slot 295) -- odd slots open for decentralization in epoch1OSchedEx2E
             (mkSeqNonce 3)
             (Nonce 6)
             zero
             3

blockEx2FHash :: Maybe HashHeader
blockEx2FHash = Just (bhHash (bheader blockEx2F))

pdEx2F :: PoolDistr
pdEx2F = PoolDistr $ Map.singleton (hk alicePool) (1, hashKey $ vKey $ vrf alicePool)

expectedStEx2F :: ChainState
expectedStEx2F =
  ( NewEpochState
      (Epoch 2)
      (mkSeqNonce 3)
      (BlocksMade Map.empty)
      (BlocksMade $ Map.singleton (hk alicePool) 1)
      (EpochState acntEx2E snapsEx2E expectedLSEx2E ppsEx1)
      (Just RewardUpdate { deltaT = Coin 0
                         , deltaR = Coin 0
                         , rs     = Map.empty
                         , deltaF = Coin 0
                         })
      pdEx2F
      epoch1OSchedEx2E
  , mkSeqNonce 6
  , mkSeqNonce 5
  , blockEx2FHash
  , Slot 295
  )

ex2F :: CHAINExample
ex2F = CHAINExample (Slot 295) expectedStEx2E blockEx2F expectedStEx2F


-- | Example 2G - continuing on after example 7, create an empty block in the next epoch
-- to prepare the way for the first non-trivial reward update


blockEx2G :: Block
blockEx2G = mkBlock
             blockEx2FHash
             (coreNodeKeys 4)
             []
             (Slot 310)
             (mkSeqNonce 5)
             (Nonce 7)
             zero
             3

blockEx2GHash :: Maybe HashHeader
blockEx2GHash = Just (bhHash (bheader blockEx2G))

epoch1OSchedEx2G :: Map Slot (Maybe VKeyGenesis)
epoch1OSchedEx2G = overlaySchedule
                    (Epoch 3)
                    (Map.keysSet dms)
                    (mkSeqNonce 5)
                    ppsEx1

snapsEx2G :: SnapShots
snapsEx2G = snapsEx2E { _pstakeGo = snapEx2C }

expectedStEx2G :: ChainState
expectedStEx2G =
  ( NewEpochState
      (Epoch 3)
      (mkSeqNonce 5)
      (BlocksMade $ Map.singleton (hk alicePool) 1)
      (BlocksMade Map.empty)
      (EpochState acntEx2E snapsEx2G expectedLSEx2E ppsEx1)
      Nothing
      pdEx2F
      epoch1OSchedEx2G
  , mkSeqNonce 7
  , mkSeqNonce 7
  , blockEx2GHash
  , Slot 310
  )

ex2G :: CHAINExample
ex2G = CHAINExample (Slot 310) expectedStEx2F blockEx2G expectedStEx2G


-- | Example 2H - continuing on after example 8, create the first non-trivial reward update


blockEx2H :: Block
blockEx2H = mkBlock
             blockEx2GHash
             (coreNodeKeys 3)
             []
             (Slot 390)
             (mkSeqNonce 5)
             (Nonce 8)
             zero
             4

blockEx2HHash :: Maybe HashHeader
blockEx2HHash = Just (bhHash (bheader blockEx2H))

rewardsEx2H :: Map RewardAcnt Coin
rewardsEx2H = Map.fromList [ (RewardAcnt aliceSHK, Coin 82593524514)
                          , (RewardAcnt bobSHK, Coin 730001159951) ]

expectedStEx2H :: ChainState
expectedStEx2H =
  ( NewEpochState
      (Epoch 3)
      (mkSeqNonce 5)
      (BlocksMade $ Map.singleton (hk alicePool) 1)
      (BlocksMade Map.empty)
      (EpochState acntEx2E snapsEx2G expectedLSEx2E ppsEx1)
      (Just RewardUpdate { deltaT = Coin 8637405315535
                         , deltaR = Coin (-9450000000000)
                         , rs = rewardsEx2H
                         , deltaF = Coin 0
                         })
      pdEx2F
      epoch1OSchedEx2G
  , mkSeqNonce 8
  , mkSeqNonce 7
  , blockEx2HHash
  , Slot 390
  )

ex2H :: CHAINExample
ex2H = CHAINExample (Slot 390) expectedStEx2G blockEx2H expectedStEx2H


-- | Example 2I - continuing on after example 9, apply the first non-trivial reward update


blockEx2I :: Block
blockEx2I = mkBlock
              blockEx2HHash
              (coreNodeKeys 4)
              []
              (Slot 410)
              (mkSeqNonce 7)
              (Nonce 9)
              zero
              4

blockEx2IHash :: Maybe HashHeader
blockEx2IHash = Just (bhHash (bheader blockEx2I))

epoch1OSchedEx2I :: Map Slot (Maybe VKeyGenesis)
epoch1OSchedEx2I = overlaySchedule
                     (Epoch 4)
                     (Map.keysSet dms)
                     (mkSeqNonce 7)
                     ppsEx1

acntEx2I :: AccountState
acntEx2I = AccountState
            { _treasury = Coin 8637405315806
            , _reserves = Coin 44990550000000000
            }

dsEx2I :: DState
dsEx2I = dsEx2B { _rewards = rewardsEx2H }

expectedLSEx2I :: LedgerState
expectedLSEx2I = LedgerState
               (UTxOState
                 utxoEx2B
                 (Coin 0)
                 (Coin 0)
                 emptyUpdateState)
               (DPState dsEx2I psEx2A)
               0

expectedStEx2I :: ChainState
expectedStEx2I =
  ( NewEpochState
      (Epoch 4)
      (mkSeqNonce 7)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx2I snapsEx2G expectedLSEx2I ppsEx1)
      Nothing
      pdEx2F
      epoch1OSchedEx2I
  , mkSeqNonce 9
  , mkSeqNonce 9
  , blockEx2IHash
  , Slot 410
  )

ex2I :: CHAINExample
ex2I = CHAINExample (Slot 410) expectedStEx2H blockEx2I expectedStEx2I


-- | Example 3A - Setting up for a successful protocol parameter update,
-- have three genesis keys vote on the same new parameters


ppupEx3A :: PPUpdate
ppupEx3A = PPUpdate $ Map.fromList [ (coreNodeVKG 0, Set.singleton (PoolDeposit 200))
                                   , (coreNodeVKG 3, Set.singleton (PoolDeposit 200))
                                   , (coreNodeVKG 4, Set.singleton (PoolDeposit 200))
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
             Nothing
             (coreNodeKeys 4)
             [txEx3A]
             (Slot 10)
             (Nonce 0)
             (Nonce 1)
             zero
             0

updateStEx3A :: ( PPUpdate
               , AVUpdate
               , Map Slot Applications
               , Applications)
updateStEx3A =
  ( ppupEx3A
  , AVUpdate Map.empty
  , Map.empty
  , Applications Map.empty)

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

blockEx3AHash :: Maybe HashHeader
blockEx3AHash = Just (bhHash (bheader blockEx3A))

expectedStEx3A :: ChainState
expectedStEx3A =
  ( NewEpochState
      (Epoch 0)
      (Nonce 0)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx2A emptySnapShots expectedLSEx3A ppsEx1)
      Nothing
      (PoolDistr Map.empty)
      overlayEx2A
  , Nonce 0 ⭒ Nonce 1
  , Nonce 0 ⭒ Nonce 1
  , blockEx3AHash
  , Slot 10
  )

ex3A :: CHAINExample
ex3A = CHAINExample (Slot 10) initStEx2A blockEx3A expectedStEx3A


-- | Example 3B - Finish getting enough votes for the protocol parameter update.


ppupEx3B :: PPUpdate
ppupEx3B = PPUpdate $ Map.fromList [ (coreNodeVKG 1, Set.singleton (PoolDeposit 200))
                                   , (coreNodeVKG 5, Set.singleton (PoolDeposit 200))
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
             (Nonce 0)
             (Nonce 2)
             zero
             0

updateStEx3B :: ( PPUpdate
               , AVUpdate
               , Map Slot Applications
               , Applications)
updateStEx3B =
  ( ppupEx3A `updatePPup` ppupEx3B
  , AVUpdate Map.empty
  , Map.empty
  , Applications Map.empty)

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

blockEx3BHash :: Maybe HashHeader
blockEx3BHash = Just (bhHash (bheader blockEx3B))

expectedStEx3B :: ChainState
expectedStEx3B =
  ( NewEpochState
      (Epoch 0)
      (Nonce 0)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx2A emptySnapShots expectedLSEx3B ppsEx1)
      Nothing
      (PoolDistr Map.empty)
      overlayEx2A
  , mkSeqNonce 2
  , mkSeqNonce 2
  , blockEx3BHash
  , Slot 20
  )

ex3B :: CHAINExample
ex3B = CHAINExample (Slot 20) expectedStEx3A blockEx3B expectedStEx3B


-- | Example 3C - Adopt protocol parameter update


blockEx3C :: Block
blockEx3C = mkBlock
             blockEx3BHash
             (coreNodeKeys 4)
             []
             (Slot 110)
             (mkSeqNonce 2)
             (Nonce 3)
             zero
             1

blockEx3CHash :: Maybe HashHeader
blockEx3CHash = Just (bhHash (bheader blockEx3C))

overlayEx3C :: Map Slot (Maybe VKeyGenesis)
overlayEx3C = overlaySchedule
                    (Epoch 1)
                    (Map.keysSet dms)
                    (mkSeqNonce 2)
                    ppsEx1

snapsEx3C :: SnapShots
snapsEx3C = emptySnapShots { _feeSS = Coin 2 }

expectedLSEx3C :: LedgerState
expectedLSEx3C = LedgerState
               (UTxOState
                 utxoEx3B
                 (Coin 0)
                 (Coin 2)
                 emptyUpdateState)
               (DPState dsEx1 psEx1)
               0

ppsEx3C :: PParams
ppsEx3C = ppsEx1 { _poolDeposit = Coin 200 }

expectedStEx3C :: ChainState
expectedStEx3C =
  ( NewEpochState
      (Epoch 1)
      (mkSeqNonce 2)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx2A snapsEx3C expectedLSEx3C ppsEx3C)
      Nothing
      (PoolDistr Map.empty)
      overlayEx3C
  , mkSeqNonce 3
  , mkSeqNonce 3
  , blockEx3CHash
  , Slot 110
  )

ex3C :: CHAINExample
ex3C = CHAINExample (Slot 110) expectedStEx3B blockEx3C expectedStEx3C


-- | Example 4A - Setting up for a successful application version update,
-- have three genesis keys vote on the same new version


appsEx4A :: Applications
appsEx4A = Applications $ Map.singleton
                            (ApName $ pack "Daedalus")
                            (ApVer 2, Metadata)

avupEx4A :: AVUpdate
avupEx4A = AVUpdate $ Map.fromList [ (coreNodeVKG 0, appsEx4A)
                                   , (coreNodeVKG 3, appsEx4A)
                                   , (coreNodeVKG 4, appsEx4A)
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
             Nothing
             (coreNodeKeys 4)
             [txEx4A]
             (Slot 10)
             (Nonce 0)
             (Nonce 1)
             zero
             0

updateStEx4A :: ( PPUpdate
               , AVUpdate
               , Map Slot Applications
               , Applications)
updateStEx4A =
  ( PPUpdate Map.empty
  , avupEx4A
  , Map.empty
  , Applications Map.empty)

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

blockEx4AHash :: Maybe HashHeader
blockEx4AHash = Just (bhHash (bheader blockEx4A))

expectedStEx4A :: ChainState
expectedStEx4A =
  ( NewEpochState
      (Epoch 0)
      (Nonce 0)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx2A emptySnapShots expectedLSEx4A ppsEx1)
      Nothing
      (PoolDistr Map.empty)
      overlayEx2A
  , Nonce 0 ⭒ Nonce 1
  , Nonce 0 ⭒ Nonce 1
  , blockEx4AHash
  , Slot 10
  )

ex4A :: CHAINExample
ex4A = CHAINExample (Slot 10) initStEx2A blockEx4A expectedStEx4A


-- | Example 4B - Finish getting enough votes for the application version update.

avupEx4B :: AVUpdate
avupEx4B = AVUpdate $ Map.fromList [ (coreNodeVKG 1, appsEx4A)
                                   , (coreNodeVKG 5, appsEx4A)
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
             (Nonce 0)
             (Nonce 2)
             zero
             0

updateStEx4B :: ( PPUpdate
               , AVUpdate
               , Map Slot Applications
               , Applications)
updateStEx4B =
  ( PPUpdate Map.empty
  , AVUpdate Map.empty
  , Map.singleton (Slot 53) appsEx4A
  , Applications Map.empty)

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

blockEx4BHash :: Maybe HashHeader
blockEx4BHash = Just (bhHash (bheader blockEx4B))

expectedStEx4B :: ChainState
expectedStEx4B =
  ( NewEpochState
      (Epoch 0)
      (Nonce 0)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx2A emptySnapShots expectedLSEx4B ppsEx1)
      Nothing
      (PoolDistr Map.empty)
      overlayEx2A
  , mkSeqNonce 2
  , mkSeqNonce 2
  , blockEx4BHash
  , Slot 20
  )

ex4B :: CHAINExample
ex4B = CHAINExample (Slot 20) expectedStEx4A blockEx4B expectedStEx4B


-- | Example 4C - Adopt application version update


blockEx4C :: Block
blockEx4C = mkBlock
             blockEx4BHash
             (coreNodeKeys 5)
             []
             (Slot 60)
             (Nonce 0)
             (Nonce 3)
             zero
             0

updateStEx4C :: ( PPUpdate
               , AVUpdate
               , Map Slot Applications
               , Applications)
updateStEx4C =
  ( PPUpdate Map.empty
  , AVUpdate Map.empty
  , Map.empty
  , appsEx4A)

expectedLSEx4C :: LedgerState
expectedLSEx4C = LedgerState
               (UTxOState
                 utxoEx4B
                 (Coin 0)
                 (Coin 2)
                 updateStEx4C)
               (DPState dsEx1 psEx1)
               0
blockEx4CHash :: Maybe HashHeader
blockEx4CHash = Just (bhHash (bheader blockEx4C))

expectedStEx4C :: ChainState
expectedStEx4C =
  ( NewEpochState
      (Epoch 0)
      (Nonce 0)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      (EpochState acntEx2A emptySnapShots expectedLSEx4C ppsEx1)
      (Just RewardUpdate { deltaT = Coin 0
                         , deltaR = Coin 0
                         , rs     = Map.empty
                         , deltaF = Coin 0
                         })
      (PoolDistr Map.empty)
      overlayEx2A
  , mkSeqNonce 3
  , mkSeqNonce 3
  , blockEx4CHash
  , Slot 60
  )


ex4C :: CHAINExample
ex4C = CHAINExample (Slot 60) expectedStEx4B blockEx4C expectedStEx4C
