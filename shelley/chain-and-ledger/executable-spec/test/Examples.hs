{-# LANGUAGE PatternSynonyms #-}

module Examples
  ( CHAINExample(..)
  , ex1
  , ex2
  )
where

import qualified Data.Map.Strict as Map (empty, fromList, singleton)
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Word (Word64)

import           Cardano.Crypto.DSIGN (deriveVerKeyDSIGN, genKeyDSIGN)
import           Cardano.Crypto.KES (deriveVerKeyKES, genKeyKES)
import           Crypto.Random (drgNewTest, withDRG)
import           MockTypes (Addr, BHBody, BHeader, Block, Credential, DState, EpochState,
                     HashHeader, KeyPair, LedgerState, NewEpochState, PState, SKey, SKeyES, Tx,
                     TxBody, UTxO, UTxOState, VKey, VKeyES, VKeyGenesis)

import           BaseTypes (Seed (..), UnitInterval, mkUnitInterval)
import           BlockChain (pattern BHBody, pattern BHeader, pattern Block, pattern Proof,
                     bBodySize, bhHash, bhbHash)
import           Coin (Coin (..))
import           Delegation.Certificates (PoolDistr (..), pattern RegKey)
import           EpochBoundary (BlocksMade (..), emptySnapShots)
import           Keys (pattern Dms, pattern KeyPair, pattern SKey, pattern SKeyES, pattern VKey,
                     pattern VKeyES, pattern VKeyGenesis, hashKey, sKey, sign, signKES, vKey)
import           LedgerState (pattern DPState, pattern EpochState, pattern LedgerState,
                     pattern NewEpochState, pattern UTxOState, emptyAccount, emptyDState,
                     emptyPState, genesisCoins, genesisId, _cCounters, _dms, _ptrs, _rewards,
                     _stKeys)
import           OCert (KESPeriod (..), pattern OCert)
import           PParams (PParams (..), emptyPParams)
import           Slot (Epoch (..), Slot (..))
import           TxData (pattern AddrBase, pattern KeyHashObj, Ptr (..), pattern RewardAcnt,
                     pattern StakeKeys, pattern Tx, pattern TxBody, pattern TxIn, pattern TxOut)
import           Updates (emptyUpdate, emptyUpdateState)
import           UTxO (pattern UTxO, makeWitnessesVKey, txid)


data CHAINExample =
  CHAINExample
    Slot
    (NewEpochState, Seed, Seed, Maybe HashHeader, Slot)
    Block
    (NewEpochState, Seed, Seed, Maybe HashHeader, Slot)

type ChainState = (NewEpochState, Seed, Seed, Maybe HashHeader, Slot)


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
ppsEx1 = emptyPParams { _maxBBSize = 1000
                   , _maxBHSize = 1000
                   , _keyDeposit = Coin 7 }

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
zero = fromMaybe (error "could not construct unit interval") $ mkUnitInterval 0

bhbEx1 :: BHBody
bhbEx1 = BHBody
        Nothing
        (vKey gerolamoCold)
        (Slot 1)
        (Nonce 1)
        (Proof (vKey gerolamoCold) (Nonce 0))
        zero
        (Proof (vKey gerolamoCold) zero)
        (sign (sKey gerolamoCold) [])
        0
        (bhbHash [])
        (OCert
          (snd gerolamoHot)
          (vKey gerolamoCold)
          0
          (KESPeriod 0)
          (sign (sKey gerolamoCold) (snd gerolamoHot, 0, KESPeriod 0))
        )

bhEx1 :: BHeader
bhEx1 = BHeader bhbEx1 (Keys.signKES (fst gerolamoHot) bhbEx1 0)

blockEx1 :: Block
blockEx1 = Block bhEx1 []

expectedStEx1 :: (NewEpochState, Seed, Seed, Maybe HashHeader, Slot)
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
  , Just (bhHash bhEx1)
  , Slot 1
  )

slot1 :: Slot
slot1 = Slot 1

ex1 :: CHAINExample
ex1 = CHAINExample slot1 initStEx1 blockEx1 expectedStEx1


-- | Example 2 - apply CHAIN transition to register a stake key


utxoEx2 :: UTxO
utxoEx2 = genesisCoins
       [ TxOut aliceAddr aliceInitCoin
       , TxOut bobAddr bobInitCoin]

txbodyEx2 :: TxBody
txbodyEx2 = TxBody
           (Set.fromList [TxIn genesisId 0])
           [TxOut aliceAddr (Coin 9990)]
           [ RegKey $ (KeyHashObj . hashKey) $ vKey aliceStake ]
           Map.empty
           (Coin 3)
           (Slot 10)
           emptyUpdate

txEx2 :: Tx
txEx2 = Tx txbodyEx2 (makeWitnessesVKey txbodyEx2 [alicePay, aliceStake]) Map.empty

utxostEx2 :: UTxOState
utxostEx2 = UTxOState utxoEx2 (Coin 0) (Coin 0) emptyUpdateState

lsEx2 :: LedgerState
lsEx2 = LedgerState utxostEx2 (DPState dsEx1 psEx1) 0

esEx2 :: EpochState
esEx2 = EpochState emptyAccount emptySnapShots lsEx2 ppsEx1

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
      (Map.singleton (Slot 1) (Just gerolamoVKG))
      -- The overlay schedule has one entry, setting Gerolamo to slot 1.
  , Nonce 0
  , Nonce 0
  , Nothing
  , Slot 0
  )

bhbEx2 :: BHBody
bhbEx2 = BHBody
        Nothing
        (vKey gerolamoCold)
        (Slot 1)
        (Nonce 1)
        (Proof (vKey gerolamoCold) (Nonce 0))
        zero
        (Proof (vKey gerolamoCold) zero)
        (sign (sKey gerolamoCold) [])
        (fromIntegral $ bBodySize [txEx2])
        (bhbHash [txEx2])
        (OCert
          (snd gerolamoHot)
          (vKey gerolamoCold)
          0
          (KESPeriod 0)
          (sign (sKey gerolamoCold) (snd gerolamoHot, 0, KESPeriod 0))
        )

bhEx2 :: BHeader
bhEx2 = BHeader bhbEx2 (Keys.signKES (fst gerolamoHot) bhbEx2 0)

blockEx2 :: Block
blockEx2 = Block bhEx2 [txEx2]

expUtxo :: UTxO
expUtxo = UTxO $ Map.fromList [ (TxIn genesisId 1, TxOut bobAddr bobInitCoin)
                              , (TxIn (txid txbodyEx2) 0, TxOut aliceAddr (Coin 9990))
                              ]
expectedLS :: LedgerState
expectedLS = LedgerState
               (UTxOState expUtxo (Coin 7) (Coin 3) emptyUpdateState)
               (DPState
                 (dsEx1 { _ptrs = Map.singleton (Ptr (Slot 1) 0 0) aliceSHK
                     , _stKeys = StakeKeys $ Map.singleton aliceSHK (Slot 1)
                     , _rewards = Map.singleton (RewardAcnt aliceSHK) (Coin 0)
                 })
                 psEx1)
               0

expectedStEx2 :: (NewEpochState, Seed, Seed, Maybe HashHeader, Slot)
expectedStEx2 =
  ( NewEpochState
      (Epoch 0)
      (Nonce 0)
      (BlocksMade Map.empty)
      (BlocksMade Map.empty)
      -- Note that blocks in the overlay schedule do not add to this count.
      (EpochState emptyAccount emptySnapShots expectedLS ppsEx1)
      Nothing
      (PoolDistr Map.empty)
      (Map.singleton (Slot 1) (Just gerolamoVKG))
  , SeedOp (Nonce 0) (Nonce 1)
  , SeedOp (Nonce 0) (Nonce 1)
  , Just (bhHash bhEx2)
  , Slot 1
  )

ex2 :: CHAINExample
ex2 = CHAINExample slot1 initStEx2 blockEx2 expectedStEx2
