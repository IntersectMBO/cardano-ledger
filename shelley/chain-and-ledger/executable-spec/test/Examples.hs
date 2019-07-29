{-# LANGUAGE PatternSynonyms #-}

module Examples
  ( CHAINExample(..)
  , ex1
  , ex2
  , ex3
  , ex4
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
                     KeyPair, LedgerState, NewEpochState, PState, PoolParams, SKey, SKeyES,
                     SnapShots, Tx, TxBody, UTxO, UTxOState, VKey, VKeyES, VKeyGenesis)
import           Numeric.Natural (Natural)

import           BaseTypes (Seed (..), UnitInterval, mkUnitInterval)
import           BlockChain (pattern BHBody, pattern BHeader, pattern Block, pattern Proof,
                     ProtVer (..), bBodySize, bhHash, bhbHash, bheader)
import           Coin (Coin (..))
import           Delegation.Certificates (PoolDistr (..), pattern RegKey, pattern RegPool)
import           EpochBoundary (BlocksMade (..), emptySnapShots, _feeSS, _poolsSS)
import           Keys (pattern Dms, pattern KeyPair, pattern SKey, pattern SKeyES, pattern VKey,
                     pattern VKeyES, pattern VKeyGenesis, hashKey, sKey, sign, signKES, vKey)
import           LedgerState (pattern DPState, pattern EpochState, pattern LedgerState,
                     pattern NewEpochState, pattern RewardUpdate, pattern UTxOState, deltaF,
                     deltaR, deltaT, emptyAccount, emptyDState, emptyPState, genesisCoins,
                     genesisId, overlaySchedule, rs, _cCounters, _dms, _pParams, _ptrs, _rewards,
                     _stKeys, _stPools)
import           OCert (KESPeriod (..), pattern OCert)
import           PParams (PParams (..), emptyPParams)
import           Slot (Epoch (..), Slot (..))
import           TxData (pattern AddrBase, pattern KeyHashObj, pattern PoolParams, Ptr (..),
                     pattern RewardAcnt, pattern StakeKeys, pattern StakePools, pattern Tx,
                     pattern TxBody, pattern TxIn, pattern TxOut, _poolCost, _poolMargin,
                     _poolOwners, _poolPledge, _poolPubKey, _poolRAcnt)
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

aliceOperatorHK :: KeyHash
aliceOperatorHK = hashKey $ vKey aliceOperator

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
    , _poolPledge = Coin 1
    , _poolCost = Coin 5
    , _poolMargin = unsafeMkUnitInterval 0.1
    , _poolRAcnt = RewardAcnt aliceSHK
    , _poolOwners = Set.singleton $ (hashKey . vKey) aliceStake
    }


-- | Helper Functions


mkBlock :: Maybe HashHeader -> KeyPair -> (SKeyES, VKeyES) -> [Tx] -> Slot -> Seed -> Seed
  -> UnitInterval -> Natural -> Block
mkBlock prev cold (shot, vhot) txns s enonce bnonce l kesPeriod =
  let
    bhb = BHBody
            prev
            (vKey cold)
            s
            bnonce
            (Proof (vKey cold) enonce)
            l
            (Proof (vKey cold) l)
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
blockEx1 = mkBlock Nothing gerolamoCold gerolamoHot [] slot1 (Nonce 0) (Nonce 1) zero 0

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
           [ RegKey $ (KeyHashObj . hashKey) $ vKey aliceStake
           ,RegPool alicePoolParams
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
blockEx2 = mkBlock Nothing gerolamoCold gerolamoHot [txEx2] slot1 (Nonce 0) (Nonce 1) zero 0

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
               (DPState
                 (dsEx1
                     { _ptrs = Map.singleton (Ptr (Slot 1) 0 1) aliceSHK
                     , _stKeys = StakeKeys $ Map.singleton aliceSHK (Slot 1)
                     , _rewards = Map.singleton (RewardAcnt aliceSHK) (Coin 0)
                 })
                 psEx1
                     { _stPools = StakePools $ Map.singleton aliceOperatorHK (Slot 1)
                     , _pParams = Map.singleton aliceOperatorHK alicePoolParams
                 })
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
      -- Note that blocks in the overlay schedule do not add to this count.
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


-- | Example 3 - continuing on after example 2, process an empty block late enough
-- in the epoch in order to create a reward update


blockEx3 :: Block
blockEx3 = mkBlock blockEx2Hash gerolamoCold gerolamoHot [] (Slot 89) (Nonce 0) (Nonce 2) zero 0

blockEx3Hash :: Maybe HashHeader
blockEx3Hash = Just (bhHash (bheader blockEx3))

expectedStEx3 :: ChainState
expectedStEx3 =
  ( NewEpochState
      (Epoch 0)
      (Nonce 0)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      -- Note that blocks in the overlay schedule do not add to this count.
      (EpochState emptyAccount emptySnapShots expectedLSEx2 ppsEx1)
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
             gerolamoHot
             []
             (Slot 110)
             (Nonce 77)
             (Nonce 88)
             zero
             1

epoch1OSchedEx4 :: Map Slot (Maybe VKeyGenesis)
epoch1OSchedEx4 = overlaySchedule
                    (Epoch 1)
                    (Set.singleton gerolamoVKG)
                    (SeedOp (Nonce 0) (Nonce 1))
                    ppsEx1

snapsEx4 :: SnapShots
snapsEx4 = emptySnapShots { _poolsSS = Map.singleton aliceOperatorHK alicePoolParams
                          , _feeSS = Coin 260
                          }

expectedLSEx4 :: LedgerState
expectedLSEx4 = LedgerState
               (UTxOState
                 (UTxO . Map.fromList $
                   [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
                   , (TxIn (txid txbodyEx2) 0, TxOut aliceAddr (Coin 9740))
                   ])
                 (Coin 0)   -- TODO check that both deposits really decayed completely
                 (Coin 260) -- TODO shouldn't this pot have moved to the treasury?
                 emptyUpdateState)
               (DPState
                 (dsEx1
                     { _ptrs = Map.singleton (Ptr (Slot 1) 0 1) aliceSHK
                     , _stKeys = StakeKeys $ Map.singleton aliceSHK (Slot 1)
                     , _rewards = Map.singleton (RewardAcnt aliceSHK) (Coin 0)
                 })
                 psEx1
                     { _stPools = StakePools $ Map.singleton aliceOperatorHK (Slot 1)
                     , _pParams = Map.singleton aliceOperatorHK alicePoolParams
                 })
               0

expectedStEx4 :: ChainState
expectedStEx4 =
  ( NewEpochState
      (Epoch 1)
      (SeedOp (Nonce 0) (Nonce 1))
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      -- Note that blocks in the overlay schedule do not add to this count.
      (EpochState emptyAccount snapsEx4 expectedLSEx4 ppsEx1)
      Nothing
      (PoolDistr Map.empty)
      epoch1OSchedEx4
  , SeedOp (SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 2)) (Nonce 88)
  , SeedOp (SeedOp (Nonce 0) (Nonce 1)) (Nonce 88)
  , Just (bhHash (bheader blockEx4))
  , Slot 110
  )

ex4 :: CHAINExample
ex4 = CHAINExample (Slot 110) expectedStEx3 blockEx4 expectedStEx4
