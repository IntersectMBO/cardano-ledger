{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.Examples
  ( CHAINExample (..),
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
    exampleShelleyGenesis,
    -- key pairs and example addresses
    coreNodeSKG, -- TODO remove
    -- blocks
    blockEx4A,
    blockEx4B,
    blockEx5A,
    blockEx5B,
    blockEx5D,
    -- transactions
    txEx4A,
    txEx5A,
    txEx5B,
    txEx5D,
    txEx5D',
    txEx5D'',
    testCHAINExample,
  )
where

import Cardano.Crypto.Hash (HashAlgorithm)
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Slotting.Slot (EpochSize (..), WithOrigin (..))
import Control.Iterate.SetAlgebra (biMapFromList)
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace (checkTrace, (.-), (.->))
import qualified Data.ByteString.Char8 as BS (pack)
import Data.List (foldl')
import qualified Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust, maybe)
import Data.Proxy
import Data.Scientific
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Shelley.Spec.Ledger.Address (Addr (..))
import Shelley.Spec.Ledger.BaseTypes
  ( Globals (..),
    Network (..),
    Nonce (..),
    Port (..),
    StrictMaybe (..),
    mkNonceFromOutputVRF,
    randomnessStabilisationWindow,
    textToDns,
    textToUrl,
    truncateUnitInterval,
    (⭒),
  )
import Shelley.Spec.Ledger.BlockChain
  ( Block,
    HashHeader (..),
    LastAppliedBlock (..),
    bhHash,
    bhbody,
    bheader,
    bheaderEta,
    hashHeaderToNonce,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential
  ( Credential (..),
    Ptr (..),
    StakeReference (..),
  )
import Shelley.Spec.Ledger.Crypto (Crypto (..))
import Shelley.Spec.Ledger.Delegation.Certificates
  ( PoolDistr (..),
    pattern Delegate,
    pattern GenesisDelegCert,
    pattern MIRCert,
    pattern RegKey,
    pattern RegPool,
  )
import Shelley.Spec.Ledger.EpochBoundary
  ( BlocksMade (..),
    emptySnapShots,
  )
import Shelley.Spec.Ledger.Genesis (ShelleyGenesis (..), ShelleyGenesisStaking (..), sgsPools)
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
import Shelley.Spec.Ledger.Keys
  ( GenDelegPair (..),
    GenDelegs (..),
    KeyHash (..),
    KeyPair (..),
    KeyRole (..),
    SignKeyDSIGN,
    VKey (..),
    VerKeyVRF,
    asWitness,
    coerceKeyRole,
    hashKey,
    hashVerKeyVRF,
    vKey,
  )
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    DState,
    EpochState (..),
    FutureGenDeleg (..),
    InstantaneousRewards (..),
    LedgerState (..),
    NewEpochState (..),
    OBftSlot,
    PState (..),
    RewardUpdate (..),
    UTxOState (..),
    emptyDState,
    emptyPPUPState,
    emptyPState,
    emptyRewardUpdate,
    esAccountState,
    esLState,
    nesEs,
    overlaySchedule,
    _delegationState,
    _dstate,
    _fGenDelegs,
    _fPParams,
    _genDelegs,
    _irwd,
    _pParams,
    _ptrs,
    _reserves,
    _rewards,
    _treasury,
    pattern ActiveSlot,
    pattern DPState,
    pattern NonActiveSlot,
    pattern PPUPState,
  )
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Shelley.Spec.Ledger.PParams
  ( PParams,
    PParams' (..),
    ProposedPPUpdates (..),
    Update (..),
    emptyPPPUpdates,
    emptyPParams,
  )
import Shelley.Spec.Ledger.Rewards (emptyNonMyopic)
import Shelley.Spec.Ledger.STS.Bbody (pattern LedgersFailure)
import Shelley.Spec.Ledger.STS.Chain
  ( CHAIN,
    ChainState (..),
    chainCandidateNonce,
    chainNes,
    chainPrevEpochNonce,
    initialShelleyState,
    totalAda,
    pattern BbodyFailure,
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
  ( BlockNo (..),
    Duration (..),
    EpochNo (..),
    SlotNo (..),
    (+*),
  )
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    WitnessSetHKD (..),
  )
import Shelley.Spec.Ledger.TxData
  ( MIRPot (..),
    PoolMetaData (..),
    PoolParams (..),
    StakePoolRelay (..),
    TxBody (..),
    TxIn (..),
    TxOut (..),
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
    pattern DCertDeleg,
    pattern DCertGenesis,
    pattern DCertMir,
    pattern DCertPool,
    pattern Delegation,
    pattern RewardAcnt,
  )
import qualified Shelley.Spec.Ledger.TxData as TxData (TxBody (..))
import Shelley.Spec.Ledger.UTxO
  ( UTxO (..),
    balance,
    makeWitnessesVKey,
    txid,
  )
import Shelley.Spec.Ledger.Value(CV,vinject,vcoin)

import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C, Mock)
import qualified Test.Shelley.Spec.Ledger.Examples.Cast as Cast
import Test.Shelley.Spec.Ledger.Generator.Core
  ( AllIssuerKeys (..),
    NatNonce (..),
    genesisCoins,
    genesisId,
    mkBlock,
    mkOCert,
    zero,
  )
import Test.Shelley.Spec.Ledger.Utils
import Test.Tasty.HUnit ((@?=), Assertion, assertBool, assertFailure)

--type CVSG c v = (VRF.Signable v, CV c v)

data CHAINExample h v = CHAINExample
  { -- | State to start testing with
    startState :: ChainState h v,
    -- | Block to run chain state transition system on
    newBlock :: Block h v,
    -- | type of fatal error, if failure expected and final chain state if success expected
    intendedResult :: Either [[PredicateFailure (CHAIN h v)]] (ChainState h v)
  }


-- | Runs example, applies chain state transition system rule (STS),
--   and checks that trace ends with expected state or expected error.
testCHAINExample :: forall c v. (Mock c,CV c v) => CHAINExample c v -> Assertion
testCHAINExample (CHAINExample initSt block (Right expectedSt)) = do
  (checkTrace @(CHAIN c v) runShelleyBase () $ pure initSt .- block .-> expectedSt)
    >> (totalAda expectedSt @?= maxLLSupply)
testCHAINExample (CHAINExample initSt block predicateFailure@(Left _)) = do
  let st = runShelleyBase $ applySTSTest @(CHAIN c v) (TRC ((), initSt, block))
  st @?= predicateFailure

data MIRExample h v = MIRExample
  { mirStkCred :: Credential 'Staking h,
    mirRewards :: Coin,
    target :: Either [[PredicateFailure (CHAIN h v)]] (ChainState h v)
  }
  deriving (Show, Eq)

mkAllIssuerKeys ::
  (Crypto c) =>
  Word64 ->
  AllIssuerKeys c r
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

coreNodes :: Crypto c => proxy c -> [((SignKeyDSIGN c, VKey 'Genesis c), AllIssuerKeys c 'GenesisDelegate)]
coreNodes _ = [(mkGenKey (x, 0, 0, 0, 0), mkAllIssuerKeys x) | x <- [101 .. 100 + numCoreNodes]]

coreNodeSKG :: Crypto c => proxy c -> Int -> SignKeyDSIGN c
coreNodeSKG p = fst . fst . (coreNodes p !!)

coreNodeVKG :: forall c. Crypto c => Int -> VKey 'Genesis c
coreNodeVKG = snd . fst . (coreNodes p !!)
  where
    p :: Proxy c
    p = Proxy

coreNodeKeys :: forall proxy c. Crypto c => proxy c -> Int -> AllIssuerKeys c 'GenesisDelegate
coreNodeKeys p = snd . (coreNodes p !!)

-- | Given the slot and an overlay schedule appropriate for this epoch, find the
-- correct core keys for the node with rights to issue a block in this slot.
coreNodeKeysForSlot ::
  forall c.
  (HasCallStack, Crypto c) =>
  Map SlotNo (OBftSlot c) ->
  Word64 ->
  AllIssuerKeys c 'GenesisDelegate
coreNodeKeysForSlot overlay slot = case Map.lookup (SlotNo slot) overlay of
  Nothing -> error $ "coreNodesForSlot: Cannot find keys for slot " <> show slot
  Just NonActiveSlot -> error $ "coreNodesForSlot: Non-active slot " <> show slot
  Just (ActiveSlot gkh) ->
    case Data.List.find (\((_, gk), _) -> hashKey gk == gkh) (coreNodes p) of
      Nothing -> error $ "coreNodesForSlot: Cannot find key hash in coreNodes: " <> show gkh
      Just ((_, _), ak) -> ak
  where
    p :: Proxy c
    p = Proxy

-- | Calculate the overlay schedule for a given epoch
overlayScheduleFor :: Crypto c => EpochNo -> Map SlotNo (OBftSlot c)
overlayScheduleFor e =
  runShelleyBase $
    overlaySchedule
      e
      (Map.keysSet genDelegs)
      ppsEx1

-- | Look up the correct core node to issue a block in the given slot, over any epoch
slotKeys :: (HasCallStack, Crypto c) => Word64 -> AllIssuerKeys c 'GenesisDelegate
slotKeys = coreNodeKeysForSlot fullOSched
  where
    fullOSched = Map.unions $ [overlayScheduleFor e | e <- [0 .. 10]]

genDelegs :: forall c. Crypto c => Map (KeyHash 'Genesis c) (GenDelegPair c)
genDelegs =
  Map.fromList
    [ ( hashKey $ snd gkey,
        ( GenDelegPair
            (coerceKeyRole . hashKey . vKey $ cold pkeys)
            (hashVerKeyVRF . snd . vrf $ pkeys)
        )
      )
      | (gkey, pkeys) <- coreNodes p
    ]
  where
    p :: Proxy c
    p = Proxy

aliceInitCoin :: Coin
aliceInitCoin = 10 * 1000 * 1000 * 1000 * 1000 * 1000

bobInitCoin :: Coin
bobInitCoin = 1 * 1000 * 1000 * 1000 * 1000 * 1000

-- | Helper Functions

-- | The first block of the Shelley era will point back to the last block of the Byron era.
--  For our purposes in this test we can bootstrap the chain by just coercing the value.
--  When this transition actually occurs, the consensus layer will do the work of making
--  sure that the hash gets translated across the fork
lastByronHeaderHash :: forall c v . CV c v => HashHeader c v
lastByronHeaderHash = HashHeader @c @v (mkHash 0)

nonce0 :: forall c v . CV c v => Proxy (c,v) -> Nonce
nonce0 _ = hashHeaderToNonce @c @v (lastByronHeaderHash @c @v)

-- * Example 1 - apply CHAIN transition to an empty block

dsEx1 :: Crypto c => DState c
dsEx1 = emptyDState {_genDelegs = GenDelegs genDelegs}

oCertIssueNosEx1 :: Crypto c => Map (KeyHash 'BlockIssuer c) Word64
oCertIssueNosEx1 = Map.fromList (fmap f (Map.elems genDelegs))
  where
    f (GenDelegPair vk _) = (coerceKeyRole vk, 0)

psEx1 :: PState h
psEx1 = emptyPState

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

getBlockNonce' :: forall c v. CV c v => Proxy c -> Block c v -> Nonce
getBlockNonce' _ =
  mkNonceFromOutputVRF . VRF.certifiedOutput . bheaderEta . bhbody . bheader

makeEvolvedNonce :: forall c v. CV c v => Proxy c -> Nonce -> [Block c v] -> Nonce
makeEvolvedNonce p n bs = foldl' (\n' b -> n' ⭒ getBlockNonce' p b) n bs

-- * Example 2A - apply CHAIN transition to register stake keys and a pool

-- | Unspent transaction output for example 2A,
--   so that users actually have coins to spend.
utxoEx2A :: CV c v => proxy c -> UTxO c v
utxoEx2A _ =
  genesisCoins
    [ TxOut Cast.aliceAddr (vinject $ aliceInitCoin),
      TxOut Cast.bobAddr (vinject $ bobInitCoin)
    ]

-- | Register a single pool with 255 coins of deposit
ppupEx2A :: Crypto c => ProposedPPUpdates c
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
updateEx2A :: Crypto c => Update c
updateEx2A = Update ppupEx2A (EpochNo 0)

aliceCoinEx2A :: Coin
aliceCoinEx2A = aliceInitCoin - (_poolDeposit ppsEx1) - 3 * (_keyDeposit ppsEx1) - 3

-- | Transaction body to be processed.
txbodyEx2A :: CV c v => TxBody c v
txbodyEx2A =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.fromList [TxOut Cast.aliceAddr (vinject $ aliceCoinEx2A)])
    ( StrictSeq.fromList
        ( [ DCertDeleg (RegKey Cast.aliceSHK),
            DCertDeleg (RegKey Cast.bobSHK),
            DCertDeleg (RegKey Cast.carlSHK),
            DCertPool (RegPool Cast.alicePoolParams)
          ]
            ++ [ DCertMir
                   ( MIRCert
                       ReservesMIR
                       ( Map.fromList
                           [ (Cast.carlSHK, 110),
                             (Cast.dariaSHK, 99)
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

txEx2A :: forall c v. (Mock c, CV c v) => Tx c v
txEx2A =
  Tx
    txbodyEx2A
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated txbodyEx2A)
            ( (asWitness <$> [Cast.alicePay, Cast.carlPay])
                <> (asWitness <$> [Cast.aliceStake])
                <> [asWitness $ cold Cast.alicePoolKeys]
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
    p :: Proxy c
    p = Proxy

-- | Pointer address to address of Alice address.
alicePtrAddr :: Crypto c => Addr c
alicePtrAddr =
  Addr
    Testnet
    (KeyHashObj . hashKey $ vKey Cast.alicePay)
    (StakeRefPtr $ Ptr (SlotNo 10) 0 0)

acntEx2A :: Crypto c => Proxy c -> AccountState
acntEx2A p =
  AccountState
    { _treasury = Coin 0,
      _reserves = maxLLSupply - balance (utxoEx2A p)
    }

initStEx2A :: forall c v. CV c v => ChainState c v
initStEx2A =
  initialShelleyState
    (At $ LastAppliedBlock (BlockNo 0) (SlotNo 0) (lastByronHeaderHash @c @v))
    (EpochNo 0)
    (utxoEx2A p)
    (maxLLSupply - balance (utxoEx2A p))
    genDelegs
    (overlayScheduleFor (EpochNo 0))
    ppsEx1
    (hashHeaderToNonce @c @v (lastByronHeaderHash @c @v))
  where
    p :: Proxy c
    p = Proxy

blockEx2A :: forall c v. (Mock c, CV c v) => Block c v
blockEx2A =
  mkBlock
    (lastByronHeaderHash @c @v)
    (slotKeys 10)
    [txEx2A]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 (Proxy::Proxy(c,v)))
    (NatNonce 1)
    zero
    0
    0
    (mkOCert (slotKeys 10) 0 (KESPeriod 0))


dsEx2A :: Crypto c => DState c
dsEx2A =
  dsEx1
    { _ptrs =
        biMapFromList
          (\l _r -> l)
          [ (Ptr (SlotNo 10) 0 0, Cast.aliceSHK),
            (Ptr (SlotNo 10) 0 1, Cast.bobSHK),
            (Ptr (SlotNo 10) 0 2, Cast.carlSHK)
          ],
      _rewards =
        Map.fromList
          [ (Cast.aliceSHK, Coin 0),
            (Cast.bobSHK, Coin 0),
            (Cast.carlSHK, Coin 0)
          ],
      _irwd =
        InstantaneousRewards
          { iRReserves =
              Map.fromList
                [ (Cast.carlSHK, 110),
                  (Cast.dariaSHK, 99)
                ],
            iRTreasury = Map.empty
          }
    }

psEx2A :: forall c. Crypto c => PState c
psEx2A =
  psEx1
    { _pParams = Map.singleton (hk Cast.alicePoolKeys) Cast.alicePoolParams
    }

expectedLSEx2A :: CV c v => LedgerState c v
expectedLSEx2A =
  LedgerState
    ( UTxOState
        ( UTxO . Map.fromList $
            [ (TxIn genesisId 1, TxOut Cast.bobAddr (vinject $ bobInitCoin)),
              (TxIn (txid txbodyEx2A) 0, TxOut Cast.aliceAddr (vinject $ aliceCoinEx2A))
            ]
        )
        (Coin 271)
        (Coin 3)
        (PPUPState ppupEx2A emptyPPPUpdates)
    )
    (DPState dsEx2A psEx2A)

blockEx2AHash :: (Mock c, CV c v) => HashHeader c v
blockEx2AHash = bhHash (bheader blockEx2A)

-- | Expected state after update is processed and STS applied.
expectedStEx2A :: forall c v. (Mock c,CV c v) => ChainState c v
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
    (nonce0 (Proxy::Proxy(c,v)))
    (makeEvolvedNonce @c @v p (nonce0 (Proxy::Proxy(c,v))) [blockEx2A])
    (makeEvolvedNonce @c @v p (nonce0 (Proxy::Proxy(c,v))) [blockEx2A])
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 1)
          (SlotNo 10)
          blockEx2AHash
    )
  where
    p :: Proxy c
    p = Proxy

-- * Example 2B - process a block late enough in the epoch in order to create a reward update.

aliceCoinEx2BBase :: Coin
aliceCoinEx2BBase = 5 * 1000 * 1000 * 1000 * 1000 * 1000

aliceCoinEx2BPtr :: Coin
aliceCoinEx2BPtr = aliceCoinEx2A - (aliceCoinEx2BBase + 4)

-- | The transaction delegates Alice's and Bob's stake to Alice's pool.
--   Additionally, we split Alice's ADA between a base address and a pointer address.
txbodyEx2B :: forall c v. CV c v => TxBody c v
txbodyEx2B =
  TxBody
    { TxData._inputs = Set.fromList [TxIn (txid txbodyEx2A) 0],
      TxData._outputs =
        StrictSeq.fromList
          [ TxOut Cast.aliceAddr (vinject $ aliceCoinEx2BBase),
            TxOut alicePtrAddr (vinject $ aliceCoinEx2BPtr)
          ],
      --  Delegation certificates
      TxData._certs =
        StrictSeq.fromList
          [ DCertDeleg (Delegate $ Delegation Cast.aliceSHK (hk Cast.alicePoolKeys)),
            DCertDeleg (Delegate $ Delegation Cast.bobSHK (hk Cast.alicePoolKeys))
          ],
      TxData._wdrls = Wdrl Map.empty,
      TxData._txfee = Coin 4,
      TxData._ttl = SlotNo 90,
      TxData._txUpdate = SNothing,
      TxData._mdHash = SNothing
    }

txEx2B :: (Mock c, CV c v) => Tx c v
txEx2B =
  Tx
    txbodyEx2B -- Body of the transaction
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated txbodyEx2B)
            [asWitness Cast.alicePay, asWitness Cast.aliceStake, asWitness Cast.bobStake]
      }
    SNothing

blockEx2B :: forall c v. (Mock c, CV c v) => Block c v
blockEx2B =
  mkBlock
    blockEx2AHash -- Hash of previous block
    (slotKeys 90)
    [txEx2B] -- Single transaction to record
    (SlotNo 90) -- Current slot
    (BlockNo 2)
    (nonce0 (Proxy::Proxy(c,v))) -- Epoch nonce
    (NatNonce 2) -- Block nonce
    zero -- Praos leader value
    4 -- Period of KES (key evolving signature scheme)
    0
    (mkOCert (slotKeys 90) 0 (KESPeriod 0))

-- | Example 4A - Genesis key delegation
newGenDelegate :: Crypto c => KeyPair 'GenesisDelegate c
newGenDelegate = KeyPair vkCold skCold
  where
    (skCold, vkCold) = mkKeyPair (108, 0, 0, 0, 1)

newGenesisVrfKH ::
  forall h v.
  (HashAlgorithm h, VRF.VRFAlgorithm v) =>
  Hash.Hash h (VRF.VerKeyVRF v)
newGenesisVrfKH = hashVerKeyVRF . snd $ mkVRFKeyPair (9, 8, 7, 6, 5)

aliceCoinEx4A :: Coin
aliceCoinEx4A = aliceInitCoin - 1

txbodyEx4A :: CV c v => TxBody c v
txbodyEx4A =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr (vinject $ aliceCoinEx4A))
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

txEx4A :: forall c v. (Mock c, CV c v) => Tx c v
txEx4A =
  Tx
    txbodyEx4A
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated txbodyEx4A)
            ( [asWitness Cast.alicePay]
                <> [asWitness $ KeyPair (coreNodeVKG 0) (coreNodeSKG p 0)]
            )
      }
    SNothing
  where
    p :: Proxy c
    p = Proxy

blockEx4A :: forall c v. (Mock c, CV c v) => Block c v
blockEx4A =
  mkBlock
    (lastByronHeaderHash @c @v)
    (slotKeys 10)
    [txEx4A]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 (Proxy::Proxy(c,v)))
    (NatNonce 1)
    zero
    0
    0
    (mkOCert (slotKeys 10) 0 (KESPeriod 0))


blockEx4AHash :: (Mock c, CV c v) => HashHeader c v
blockEx4AHash = bhHash (bheader blockEx4A)

dsEx4A :: Crypto c => DState c
dsEx4A =
  dsEx1
    { _fGenDelegs =
        Map.singleton
          (FutureGenDeleg (SlotNo 43) (hashKey $ coreNodeVKG 0))
          (GenDelegPair (hashKey . vKey $ newGenDelegate) newGenesisVrfKH)
    }

utxoEx4A :: CV c v => UTxO c v
utxoEx4A =
  UTxO . Map.fromList $
    [ (TxIn genesisId 1, TxOut Cast.bobAddr (vinject $ bobInitCoin)),
      (TxIn (txid txbodyEx4A) 0, TxOut Cast.aliceAddr (vinject $ aliceCoinEx4A))
    ]

expectedLSEx4A :: CV c v => LedgerState c v
expectedLSEx4A =
  LedgerState
    ( UTxOState
        utxoEx4A
        (Coin 0)
        (Coin 1)
        emptyPPUPState
    )
    (DPState dsEx4A psEx1)

expectedStEx4A :: forall c v. (Mock c, CV c v) => ChainState c v
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
    (nonce0 (Proxy::Proxy(c,v)))
    (makeEvolvedNonce @c @v p (nonce0 (Proxy::Proxy(c,v))) [blockEx4A])
    (makeEvolvedNonce @c @v p (nonce0 (Proxy::Proxy(c,v))) [blockEx4A])
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 1)
          (SlotNo 10)
          blockEx4AHash
    )
  where
    p :: Proxy c
    p = Proxy

ex4A :: (Mock c, CV c v) => proxy c -> CHAINExample c v
ex4A _ = CHAINExample initStEx2A blockEx4A (Right expectedStEx4A)

-- | Example 4B - New genesis key delegation updated from future delegations
blockEx4B :: forall c v. (Mock c, CV c v) => Block c v
blockEx4B =
  mkBlock
    blockEx4AHash
    (slotKeys 50)
    []
    (SlotNo 50)
    (BlockNo 2)
    (nonce0 (Proxy::Proxy(c,v)))
    (NatNonce 2)
    zero
    2
    0
    (mkOCert (slotKeys 50) 0 (KESPeriod 0))


blockEx4BHash :: (Mock c, CV c v) => HashHeader c v
blockEx4BHash = bhHash (bheader blockEx4B)

dsEx4B :: Crypto c => DState c
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

expectedLSEx4B :: CV c v => LedgerState c v
expectedLSEx4B =
  LedgerState
    ( UTxOState
        utxoEx4A
        (Coin 0)
        (Coin 1)
        emptyPPUPState
    )
    (DPState dsEx4B psEx1)

expectedStEx4B :: forall c v. (Mock c, CV c v) => ChainState c v
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
    (nonce0 (Proxy::Proxy(c,v)))
    (makeEvolvedNonce @c @v p (nonce0 (Proxy::Proxy(c,v))) [blockEx4A, blockEx4B])
    (makeEvolvedNonce @c @v p (nonce0 (Proxy::Proxy(c,v))) [blockEx4A, blockEx4B])
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 2)
          (SlotNo 50)
          blockEx4BHash
    )
  where
    p :: Proxy c
    p = Proxy

ex4B :: (Mock c, CV c v) => proxy c -> CHAINExample c v
ex4B _ = CHAINExample expectedStEx4A blockEx4B (Right expectedStEx4B)

-- | Example 5A - Genesis key delegation
ir :: Crypto c => Map (Credential 'Staking c) Coin
ir = Map.fromList [(Cast.aliceSHK, Coin 100)]

aliceCoinEx5A :: Coin
aliceCoinEx5A = aliceInitCoin - 1

txbodyEx5A :: CV c v => MIRPot -> TxBody c v
txbodyEx5A pot =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr (vinject $ aliceCoinEx5A))
    (StrictSeq.fromList [DCertMir (MIRCert pot ir)])
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 10)
    SNothing
    SNothing

txEx5A :: (Mock c, CV c v) => MIRPot -> Tx c v
txEx5A pot =
  Tx
    (txbodyEx5A pot)
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ txbodyEx5A pot)
            ( [asWitness Cast.alicePay]
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
     where p :: Proxy c
           p = Proxy

blockEx5A :: forall c v. (Mock c, CV c v) => MIRPot -> Block c v
blockEx5A pot =
  mkBlock
    (lastByronHeaderHash @c @v)
    (slotKeys 10)
    [txEx5A pot]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 (Proxy::Proxy(c,v)))
    (NatNonce 1)
    zero
    0
    0
    (mkOCert (slotKeys 10) 0 (KESPeriod 0))

blockEx5AHash :: (Mock c, CV c v) => MIRPot -> HashHeader c v
blockEx5AHash pot = bhHash (bheader $ blockEx5A pot)

utxoEx5A :: CV c v => MIRPot -> UTxO c v
utxoEx5A pot =
  UTxO . Map.fromList $
    [ (TxIn genesisId 1, TxOut Cast.bobAddr (vinject $ bobInitCoin)),
      (TxIn (txid $ txbodyEx5A pot) 0, TxOut Cast.aliceAddr (vinject $ aliceCoinEx5A))
    ]

dsEx5A :: Crypto c => MIRPot -> DState c
dsEx5A pot = dsEx1 {_irwd = InstantaneousRewards {iRReserves = r, iRTreasury = t}}
  where
    (r, t) = case pot of
      ReservesMIR -> (Map.fromList [(Cast.aliceSHK, Coin 100)], Map.empty)
      TreasuryMIR -> (Map.empty, Map.fromList [(Cast.aliceSHK, Coin 100)])

expectedLSEx5A :: CV c v => MIRPot -> LedgerState c v
expectedLSEx5A pot =
  LedgerState
    ( UTxOState
        (utxoEx5A pot)
        (Coin 0)
        (Coin 1)
        emptyPPUPState
    )
    (DPState (dsEx5A pot) psEx1)

treasuryEx5A :: Coin
treasuryEx5A = Coin 1000

setChainStateAccountState :: AccountState -> ChainState h v -> ChainState h v
setChainStateAccountState as cs = cs {chainNes = (chainNes cs) {nesEs = es'}}
  where
    es' = (nesEs $ chainNes cs) {esAccountState = as}

initStEx5A :: forall c v. CV c v => ChainState c v
initStEx5A =
  setChainStateAccountState
    ( AccountState
        { _treasury = 1000,
          _reserves = maxLLSupply - (1000 + balance (utxoEx2A p))
        }
    )
    initStEx2A
  where
    p :: Proxy c
    p = Proxy

acntEx5A :: Crypto c => Proxy c -> AccountState
acntEx5A p =
  AccountState
    { _treasury = treasuryEx5A,
      _reserves = maxLLSupply - (balance (utxoEx2A p) + treasuryEx5A)
    }

expectedStEx5A :: forall c v. (Mock c, CV c v) => MIRPot -> ChainState c v
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
    (nonce0 (Proxy::Proxy(c,v)))
    (makeEvolvedNonce @c @v p (nonce0 (Proxy::Proxy(c,v))) [blockEx5A pot])
    (makeEvolvedNonce @c @v p (nonce0 (Proxy::Proxy(c,v))) [blockEx5A pot])
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 1)
          (SlotNo 10)
          (blockEx5AHash pot)
    )
  where
    p :: Proxy c
    p = Proxy

ex5A :: (Mock c, CV c v) => proxy c -> MIRPot -> CHAINExample c v
ex5A _ pot = CHAINExample initStEx5A (blockEx5A pot) (Right $ expectedStEx5A pot)

ex5AReserves :: (Mock c, CV c v) => proxy c -> CHAINExample c v
ex5AReserves p = ex5A p ReservesMIR

ex5ATreasury :: (Mock c, CV c v) => proxy c -> CHAINExample c v
ex5ATreasury p = ex5A p TreasuryMIR

-- | Example 5B - Instantaneous rewards with insufficient core node signatures
txEx5B :: (Mock c, CV c v) => MIRPot -> Tx c v
txEx5B pot =
  Tx
    (txbodyEx5A pot)
    ( mempty
        { addrWits =
            makeWitnessesVKey
              (hashAnnotated $ txbodyEx5A pot)
              ( [asWitness Cast.alicePay]
                  <> ( asWitness
                         <$> [ cold (coreNodeKeys p 0),
                               cold (coreNodeKeys p 1),
                               cold (coreNodeKeys p 2),
                               cold (coreNodeKeys p 3)
                             ]
                     )
              )
        }
    )
    SNothing
  where
    p :: Proxy c
    p = Proxy

blockEx5B :: forall c v. (Mock c, CV c v) => MIRPot -> Block c v
blockEx5B pot =
  mkBlock
    (lastByronHeaderHash @c @v)
    (slotKeys 10)
    [txEx5B pot]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 (Proxy::Proxy(c,v)))
    (NatNonce 1)
    zero
    0
    0
    (mkOCert (slotKeys 10) 0 (KESPeriod 0))


mirWitsEx5B :: Crypto c => Set (KeyHash 'Witness c)
mirWitsEx5B = Set.fromList [asWitness . hk . coreNodeKeys p $ i | i <- [0 .. 3]]
  where
    p :: Proxy c
    p = Proxy

expectedStEx5B :: CV c v => PredicateFailure (CHAIN c v)
expectedStEx5B = BbodyFailure (LedgersFailure (LedgerFailure (UtxowFailure $ MIRInsufficientGenesisSigsUTXOW mirWitsEx5B)))

ex5B :: (Mock c, CV c v) => proxy c -> MIRPot -> CHAINExample c v
ex5B _ pot = CHAINExample initStEx5A (blockEx5B pot) (Left [[expectedStEx5B]])

ex5BReserves :: (Mock c, CV c v) => proxy c -> CHAINExample c v
ex5BReserves p = ex5B p ReservesMIR

ex5BTreasury :: (Mock c, CV c v) => proxy c -> CHAINExample c v
ex5BTreasury p = ex5B p TreasuryMIR

-- | Example 5C - Instantaneous rewards that overrun the available reserves
initStEx5C :: CV c v => ChainState c v
initStEx5C =
  setChainStateAccountState
    (AccountState {_treasury = 99, _reserves = 99})
    initStEx2A

ex5C :: (Mock c, CV c v) => proxy c -> MIRPot -> CHAINExample c v
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

ex5CReserves :: (Mock c, CV c v) => proxy c -> CHAINExample c v
ex5CReserves p = ex5C p ReservesMIR

ex5CTreasury :: (Mock c, CV c v) => proxy c -> CHAINExample c v
ex5CTreasury p = ex5C p TreasuryMIR

-- | Example 5D - Apply instantaneous rewards at epoch boundary

-- | The first transaction adds the MIR certificate that transfers a value of
-- 100 to Alice.
aliceCoinEx5D :: Coin
aliceCoinEx5D = aliceInitCoin - (_keyDeposit ppsEx1) - 1

txbodyEx5D :: CV c v => MIRPot -> TxBody c v
txbodyEx5D pot =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr (vinject $ aliceCoinEx5D))
    (StrictSeq.fromList [DCertDeleg (RegKey Cast.aliceSHK), DCertMir (MIRCert pot ir)])
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 99)
    SNothing
    SNothing

txEx5D :: (Mock c, CV c v) => MIRPot -> Tx c v
txEx5D pot =
  Tx
    (txbodyEx5D pot)
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated $ txbodyEx5D pot)
            ( [asWitness Cast.alicePay, asWitness Cast.aliceStake]
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
    p :: Proxy c
    p = Proxy

blockEx5D :: forall c v. (Mock c, CV c v) => MIRPot -> Block c v
blockEx5D pot =
  mkBlock
    (lastByronHeaderHash @c @v)
    (slotKeys 10)
    [txEx5D pot]
    (SlotNo 10)
    (BlockNo 1)
    (nonce0 (Proxy::Proxy(c,v)))
    (NatNonce 1)
    zero
    0
    0
    (mkOCert (slotKeys 10) 0 (KESPeriod 0))


-- | The second transaction in the next epoch and at least `randomnessStabilisationWindow` slots
-- after the transaction carrying the MIR certificate, then creates the rewards
-- update that contains the transfer of `100` to Alice.
aliceCoinEx5D' :: Coin
aliceCoinEx5D' = aliceCoinEx5D - 1

txbodyEx5D' :: CV c v => MIRPot -> TxBody c v
txbodyEx5D' pot =
  TxBody
    (Set.fromList [TxIn (txid $ txbodyEx5D pot) 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr (vinject $ aliceCoinEx5D'))
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    ( (slotFromEpoch $ EpochNo 1)
        +* Duration (randomnessStabilisationWindow testGlobals) + SlotNo 7
    )
    SNothing
    SNothing

txEx5D' :: (Mock c, CV c v) => MIRPot -> Tx c v
txEx5D' pot =
  Tx
    (txbodyEx5D' pot)
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated $ txbodyEx5D' pot) [Cast.alicePay]
      }
    SNothing

blockEx5D' :: forall c v. (Mock c,CV c v) => MIRPot -> Block c v
blockEx5D' pot =
  mkBlock
    (bhHash (bheader $ blockEx5D pot))
    (slotKeys s)
    [txEx5D' pot]
    (slot)
    (BlockNo 2)
    (makeEvolvedNonce @c @v p (nonce0 (Proxy::Proxy(c,v))) [blockEx5D pot])
    (NatNonce 2)
    zero
    7
    0
    (mkOCert (slotKeys s) 0 (KESPeriod 0))
  where
    slot@(SlotNo s) =
      (slotFromEpoch $ EpochNo 1)
        +* Duration (randomnessStabilisationWindow testGlobals) + SlotNo 7
    p :: Proxy c
    p = Proxy

-- | The third transaction in the next epoch applies the reward update to 1)
-- register a staking credential for Alice, 2) deducing the key deposit from the
-- 100 and to 3) create the reward account with an initial amount of 93.
aliceCoinEx5D'' :: Coin
aliceCoinEx5D'' = aliceCoinEx5D' - 1

txbodyEx5D'' :: CV c v => MIRPot -> TxBody c v
txbodyEx5D'' pot =
  TxBody
    (Set.fromList [TxIn (txid $ txbodyEx5D' pot) 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr (vinject $ aliceCoinEx5D''))
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    ((slotFromEpoch $ EpochNo 2) + SlotNo 10)
    SNothing
    SNothing

txEx5D'' :: (Mock c, CV c v) => MIRPot -> Tx c v
txEx5D'' pot =
  Tx
    (txbodyEx5D'' pot)
    mempty {addrWits = makeWitnessesVKey (hashAnnotated $ txbodyEx5D'' pot) [Cast.alicePay]}
    SNothing

blockEx5D'' :: (Mock c, CV c v) => MIRPot -> Nonce -> Block c v
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

ex5D' :: forall c v. (Mock c, CV c v) => Proxy c -> MIRPot -> Either [[PredicateFailure (CHAIN c v)]] (ChainState c v)
ex5D' _p pot = do
  nextState <- runShelleyBase $ applySTSTest @(CHAIN c v) (TRC ((), initStEx5A, blockEx5D pot))
  midState <-
    runShelleyBase $
      applySTSTest @(CHAIN c v) (TRC ((), nextState, blockEx5D' pot))
  let finalEpochNonce = (chainCandidateNonce midState) ⭒ (chainPrevEpochNonce midState)
  finalState <-
    runShelleyBase $ applySTSTest @(CHAIN c v) (TRC ((), midState, blockEx5D'' pot finalEpochNonce))

  pure finalState

ex5DReserves' :: (Mock c, CV c v) => Proxy c -> Either [[PredicateFailure (CHAIN c v)]] (ChainState c v)
ex5DReserves' p = ex5D' p ReservesMIR

ex5DTreasury' :: (Mock c, CV c v) => Proxy c -> Either [[PredicateFailure (CHAIN c v)]] (ChainState c v)
ex5DTreasury' p = ex5D' p TreasuryMIR

-- | Tests that after getting instantaneous rewards, creating the update and
-- then applying the update, Alice's key is actually registered, the key deposit
-- value deducted and the remaining value credited as reward.
test5D :: forall c v. (Mock c,CV c v) => Proxy (c,v) -> MIRPot -> Assertion
test5D _ pot = do
  case ex5D' @c @v (Proxy :: Proxy c) pot of
    Left e -> assertFailure (show e)
    Right ex5DState -> do
      let rews = _rewards . _dstate . _delegationState . esLState . nesEs . chainNes $ ex5DState
          rewEntry = rews Map.!? Cast.aliceSHK
      assertBool "Alice's reward account does not exist" $ isJust rewEntry
      assertBool "Alice's rewards are wrong" $ maybe False (== Coin 100) rewEntry
      assertBool "Total amount of ADA is not preserved" $ maxLLSupply == (vcoin $ totalAda ex5DState)

test5DReserves :: forall c v. (Mock c,CV c v) => Proxy (c,v) -> Assertion
test5DReserves p = test5D p ReservesMIR

test5DTreasury :: forall c v. (Mock c,CV c v) => Proxy (c,v) -> Assertion
test5DTreasury p = test5D p TreasuryMIR

-- * Example 6A - apply CHAIN transition to re-register a stake pool late in the epoch

-- This example continues on from example 2A.

feeEx6A :: Coin
feeEx6A = Coin 3

aliceCoinEx6A :: Coin
aliceCoinEx6A = aliceCoinEx2A - feeEx6A

alicePoolParams6A :: Crypto c => PoolParams c
alicePoolParams6A = Cast.alicePoolParams {_poolCost = Coin 500}

txbodyEx6A :: CV c v => TxBody c v
txbodyEx6A =
  TxBody
    (Set.fromList [TxIn (txid txbodyEx2A) 0])
    (StrictSeq.fromList [TxOut Cast.aliceAddr (vinject $ aliceCoinEx6A)])
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

txEx6A :: (Mock c, CV c v) => Tx c v
txEx6A =
  Tx
    txbodyEx6A
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated txbodyEx6A)
            ( (asWitness <$> [Cast.alicePay])
                <> (asWitness <$> [Cast.aliceStake])
                <> [asWitness $ cold Cast.alicePoolKeys]
            )
      }
    SNothing

earlySlotEx6 :: Word64
earlySlotEx6 = 20

lateSlotEx6 :: Word64
lateSlotEx6 = 90

word64SlotToKesPeriodWord :: Word64 -> Word
word64SlotToKesPeriodWord slot =
  (fromIntegral $ toInteger slot) `div` (fromIntegral $ toInteger $ slotsPerKESPeriod testGlobals)

blockEx6A :: forall c v. (Mock c, CV c v) => Word64 -> Block c v
blockEx6A slot =
  mkBlock
    blockEx2AHash
    (slotKeys slot)
    [txEx6A]
    (SlotNo slot)
    (BlockNo 2)
    (nonce0 (Proxy::Proxy(c,v)))
    (NatNonce 2)
    zero
    (word64SlotToKesPeriodWord slot)
    0
    (mkOCert (slotKeys slot) 0 (KESPeriod 0))

blockEx6AHash :: (Mock c, CV c v) => Word64 -> HashHeader c v
blockEx6AHash slot = bhHash (bheader $ blockEx6A slot)

psEx6A :: Crypto c => PState c
psEx6A = psEx2A {_fPParams = Map.singleton (hk Cast.alicePoolKeys) alicePoolParams6A}

expectedLSEx6A :: CV c v => LedgerState c v
expectedLSEx6A =
  LedgerState
    ( UTxOState
        ( UTxO . Map.fromList $
            [ (TxIn genesisId 1, TxOut Cast.bobAddr (vinject $ bobInitCoin)),
              (TxIn (txid txbodyEx6A) 0, TxOut Cast.aliceAddr (vinject $ aliceCoinEx6A))
            ]
        )
        (Coin 271)
        (Coin 3 + feeEx6A)
        (PPUPState ppupEx2A emptyPPPUpdates)
    )
    (DPState dsEx2A psEx6A)

rewardUpdateEx6A :: StrictMaybe (RewardUpdate h)
rewardUpdateEx6A = SNothing

rewardUpdateEx6A' :: StrictMaybe (RewardUpdate h)
rewardUpdateEx6A' = SJust emptyRewardUpdate

candidateNonceEx6A :: forall c v. (Mock c,CV c v) => Proxy (c,v) -> Nonce
candidateNonceEx6A p = makeEvolvedNonce @c @v (Proxy::Proxy c) (nonce0 p) [blockEx2A, blockEx2B]

candidateNonceEx6A' :: forall c v. (Mock c,CV c v) => Proxy (c,v) -> Nonce
candidateNonceEx6A' p = makeEvolvedNonce @c @v (Proxy :: Proxy c) (nonce0 p) [blockEx2A]

expectedStEx6A :: forall c v. (Mock c, CV c v) => Word64 -> StrictMaybe (RewardUpdate c) -> Nonce -> ChainState c v
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
    (nonce0 (Proxy::Proxy(c,v)))
    (makeEvolvedNonce @c @v p (nonce0 (Proxy::Proxy(c,v))) [blockEx2A, blockEx2B])
    cn
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 2)
          (SlotNo slot)
          (blockEx6AHash slot)
    )
  where
    p :: Proxy c
    p = Proxy

ex6A :: forall c v. (Mock c, CV c v) => Proxy c -> CHAINExample c v
ex6A _ =
  CHAINExample
    expectedStEx2A
    (blockEx6A earlySlotEx6)
    (Right $ expectedStEx6A earlySlotEx6 rewardUpdateEx6A (candidateNonceEx6A (Proxy::Proxy(c,v))))

ex6A' :: forall c v. (Mock c, CV c v) => Proxy c -> CHAINExample c v
ex6A' _ =
  CHAINExample
    expectedStEx2A
    (blockEx6A lateSlotEx6)
    (Right $ expectedStEx6A lateSlotEx6 rewardUpdateEx6A' (candidateNonceEx6A' (Proxy::Proxy(c,v))))

-- * Example 6B - If The TICK rule is applied to the NewEpochState

-- in expectedStEx6A, then the future pool parameters should be adopted

ex6BExpectedNES :: forall c v. (Mock c, CV c v) => NewEpochState c v
ex6BExpectedNES = chainNes (expectedStEx6A earlySlotEx6 rewardUpdateEx6A (candidateNonceEx6A p))
  where
    p :: Proxy (c,v)
    p = Proxy

ex6BExpectedNES' :: forall c v. (Mock c, CV c v) => NewEpochState c v
ex6BExpectedNES' = chainNes (expectedStEx6A lateSlotEx6 rewardUpdateEx6A' (candidateNonceEx6A' p))
  where
    p :: Proxy (c,v)
    p = Proxy

ex6BPoolParams :: Crypto c => Map (KeyHash 'StakePool c) (PoolParams c)
ex6BPoolParams = Map.singleton (hk Cast.alicePoolKeys) alicePoolParams6A

exampleShelleyGenesis :: forall c v. (Mock c, CV c v) => ShelleyGenesis c v
exampleShelleyGenesis =
  ShelleyGenesis
    { sgSystemStart = posixSecondsToUTCTime $ realToFrac (1234566789 :: Integer),
      sgNetworkMagic = 4036000900,
      sgNetworkId = Testnet,
      sgActiveSlotsCoeff = 6.259,
      sgSecurityParam = 120842,
      sgEpochLength = EpochSize 1215,
      sgSlotsPerKESPeriod = 8541,
      sgMaxKESEvolutions = 28899,
      sgSlotLength = 8,
      sgUpdateQuorum = 16991,
      sgMaxLovelaceSupply = 71,
      sgProtocolParams =
        emptyPParams
          { _d = truncateUnitInterval . realToFrac $ (1.9e-2 :: Scientific),
            _maxBBSize = 239857,
            _maxBHSize = 217569
          },
      sgGenDelegs = Map.fromList [(genesisVerKeyHash, genDelegPair)],
      sgInitialFunds = Map.fromList [(initialFundedAddress, vinject initialFunds)],
      sgStaking = staking
    }
  where
    -- hash of the genesis verification key
    genesisVerKeyHash :: KeyHash 'Genesis c
    genesisVerKeyHash = KeyHash "23d51e9123d51e91"
    -- hash of the delegators verififation key
    genDelegPair = GenDelegPair delegVerKeyHash delegVrfKeyHash
    delegVerKeyHash :: KeyHash 'GenesisDelegate c
    delegVerKeyHash = KeyHash "839b047f839b047f"
    delegVrfKeyHash :: Hash.Hash (HASH c) (VerKeyVRF c)
    delegVrfKeyHash = "231391e7231391e7"
    initialFundedAddress :: Addr c
    initialFundedAddress = Addr Testnet paymentCredential (StakeRefBase stakingCredential)
      where
        paymentCredential =
          KeyHashObj $
            KeyHash
              "1c14ee8e1c14ee8e"
        stakingCredential =
          KeyHashObj $
            KeyHash
              "e37a65eae37a65ea"
    initialFunds :: Coin
    initialFunds = Coin 12157196
    relays =
      StrictSeq.fromList
        [ SingleHostAddr (SJust $ Port 1234) (SJust $ read "0.0.0.0") (SJust $ read "2001:db8:a::123"),
          SingleHostName SNothing (fromJust $ textToDns "cool.domain.com"),
          MultiHostName (fromJust $ textToDns "cool.domain.com")
        ]
    poolParams :: PoolParams c
    poolParams =
      PoolParams
        { _poolPubKey = (hashKey . vKey . cold) (mkAllIssuerKeys @c 1),
          _poolVrf = hashVerKeyVRF . snd $ vrf (mkAllIssuerKeys @c 1),
          _poolPledge = Coin 1,
          _poolCost = Coin 5,
          _poolMargin = unsafeMkUnitInterval 0.25,
          _poolRAcnt = RewardAcnt Testnet Cast.aliceSHK,
          _poolOwners = Set.singleton $ (hashKey . vKey) Cast.aliceStake,
          _poolRelays = relays,
          _poolMD =
            SJust $
              PoolMetaData
                { _poolMDUrl = fromJust $ textToUrl "best.pool.com",
                  _poolMDHash = BS.pack "100ab{}100ab{}"
                }
        }
    staking =
      ShelleyGenesisStaking
        { sgsPools = Map.fromList [(KeyHash "3dbe00a13dbe00a1", poolParams)],
          sgsStake = Map.fromList [(KeyHash "1c14ee8e1c14ee8e", KeyHash "1c14ee8e1c14ee8e")]
        }
