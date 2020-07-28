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
    ex3D,
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
    blockEx3D,
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
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Prelude (asks)
import Cardano.Slotting.Slot (EpochSize (..), WithOrigin (..))
import Control.Iterate.SetAlgebra (biMapFromList)
import Control.State.Transition.Extended hiding (Assertion)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.List (foldl')
import qualified Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust, maybe)
import Data.Proxy
import Data.Ratio ((%))
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
    mkNonceFromNumber,
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
    pattern DeRegKey,
    pattern Delegate,
    pattern GenesisDelegCert,
    pattern MIRCert,
    pattern RegKey,
    pattern RegPool,
    pattern RetirePool,
  )
import Shelley.Spec.Ledger.EpochBoundary
  ( BlocksMade (..),
    SnapShot (SnapShot),
    SnapShots (SnapShots),
    emptySnapShots,
    unStake,
    _feeSS,
    _pstakeGo,
    _pstakeMark,
    _pstakeSet,
    _stake,
    pattern Stake,
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
    RewardAccounts,
    RewardUpdate (..),
    UTxOState (..),
    deltaF,
    deltaR,
    deltaT,
    emptyDState,
    emptyInstantaneousRewards,
    emptyPPUPState,
    emptyPState,
    emptyRewardUpdate,
    esAccountState,
    esLState,
    nesEs,
    nonMyopic,
    overlaySchedule,
    rs,
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
    PParamsUpdate,
    ProposedPPUpdates (..),
    Update (..),
    emptyPPPUpdates,
    emptyPParams,
  )
import Shelley.Spec.Ledger.Rewards
  ( Likelihood (..),
    NonMyopic (..),
    emptyNonMyopic,
    leaderProbability,
    likelihood,
    rewardPotNM,
  )
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
    epochInfoSize,
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
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( Mock,
  )
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
  { mirStkCred :: Credential 'Staking h,
    mirRewards :: Coin,
    target :: Either [[PredicateFailure (CHAIN h)]] (ChainState h)
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
lastByronHeaderHash :: forall proxy c. Crypto c => proxy c -> HashHeader c
lastByronHeaderHash _ = HashHeader $ mkHash 0

nonce0 :: Crypto c => proxy c -> Nonce
nonce0 p = hashHeaderToNonce (lastByronHeaderHash p)

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

getBlockNonce' :: forall c. Crypto c => Proxy c -> Block c -> Nonce
getBlockNonce' _ =
  mkNonceFromOutputVRF . VRF.certifiedOutput . bheaderEta . bhbody . bheader

makeEvolvedNonce :: forall c. Crypto c => Proxy c -> Nonce -> [Block c] -> Nonce
makeEvolvedNonce p n bs = foldl' (\n' b -> n' ⭒ getBlockNonce' p b) n bs

-- * Example 2A - apply CHAIN transition to register stake keys and a pool

-- | Unspent transaction output for example 2A,
--   so that users actually have coins to spend.
utxoEx2A :: Crypto c => proxy c -> UTxO c
utxoEx2A _ =
  genesisCoins
    [ TxOut Cast.aliceAddr aliceInitCoin,
      TxOut Cast.bobAddr bobInitCoin
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
txbodyEx2A :: Crypto c => TxBody c
txbodyEx2A =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.fromList [TxOut Cast.aliceAddr aliceCoinEx2A])
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

txEx2A :: forall c. Mock c => Tx c
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

initStEx2A :: forall c. Crypto c => ChainState c
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
    p :: Proxy c
    p = Proxy

blockEx2A :: forall c. Mock c => Block c
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
    p :: Proxy c
    p = Proxy

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

expectedLSEx2A :: Crypto c => LedgerState c
expectedLSEx2A =
  LedgerState
    ( UTxOState
        ( UTxO . Map.fromList $
            [ (TxIn genesisId 1, TxOut Cast.bobAddr bobInitCoin),
              (TxIn (txid txbodyEx2A) 0, TxOut Cast.aliceAddr aliceCoinEx2A)
            ]
        )
        (Coin 271)
        (Coin 3)
        (PPUPState ppupEx2A emptyPPPUpdates)
    )
    (DPState dsEx2A psEx2A)

blockEx2AHash :: Mock c => HashHeader c
blockEx2AHash = bhHash (bheader blockEx2A)

-- | Expected state after update is processed and STS applied.
expectedStEx2A :: forall c. Mock c => ChainState c
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
    (makeEvolvedNonce p (nonce0 p) [blockEx2A])
    (makeEvolvedNonce p (nonce0 p) [blockEx2A])
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

ex2A :: Mock c => proxy c -> CHAINExample c
ex2A _ = CHAINExample initStEx2A blockEx2A (Right expectedStEx2A)

-- * Example 2B - process a block late enough in the epoch in order to create a reward update.

aliceCoinEx2BBase :: Coin
aliceCoinEx2BBase = 5 * 1000 * 1000 * 1000 * 1000 * 1000

aliceCoinEx2BPtr :: Coin
aliceCoinEx2BPtr = aliceCoinEx2A - (aliceCoinEx2BBase + 4)

-- | The transaction delegates Alice's and Bob's stake to Alice's pool.
--   Additionally, we split Alice's ADA between a base address and a pointer address.
txbodyEx2B :: forall c. Crypto c => TxBody c
txbodyEx2B =
  TxBody
    { TxData._inputs = Set.fromList [TxIn (txid txbodyEx2A) 0],
      TxData._outputs =
        StrictSeq.fromList
          [ TxOut Cast.aliceAddr aliceCoinEx2BBase,
            TxOut alicePtrAddr aliceCoinEx2BPtr
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

txEx2B :: Mock c => Tx c
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

blockEx2B :: forall c. Mock c => Block c
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
    p :: Proxy c
    p = Proxy

blockEx2BHash :: Mock c => proxy c -> HashHeader c
blockEx2BHash _ = bhHash (bheader blockEx2B)

utxoEx2B :: Crypto c => UTxO c
utxoEx2B =
  UTxO . Map.fromList $
    [ (TxIn genesisId 1, TxOut Cast.bobAddr bobInitCoin),
      (TxIn (txid txbodyEx2B) 0, TxOut Cast.aliceAddr aliceCoinEx2BBase),
      (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr aliceCoinEx2BPtr)
    ]

-- | Both Alice and Bob delegate to the Alice pool
delegsEx2B :: forall c. Crypto c => Map (Credential 'Staking c) (KeyHash 'StakePool c)
delegsEx2B =
  Map.fromList
    [ (Cast.aliceSHK, hk Cast.alicePoolKeys),
      (Cast.bobSHK, hk Cast.alicePoolKeys)
    ]

carlMIR :: Coin
carlMIR = Coin 110

dariaMIR :: Coin
dariaMIR = Coin 99

dsEx2B :: Crypto c => DState c
dsEx2B =
  dsEx2A
    { _delegations = delegsEx2B,
      _irwd =
        InstantaneousRewards
          { iRReserves =
              Map.fromList
                [ (Cast.carlSHK, carlMIR),
                  (Cast.dariaSHK, dariaMIR)
                ],
            iRTreasury = Map.empty
          }
    }

expectedLSEx2B :: Crypto c => LedgerState c
expectedLSEx2B =
  LedgerState
    ( UTxOState
        utxoEx2B
        (Coin 271)
        (Coin 7)
        (PPUPState ppupEx2A emptyPPPUpdates)
    )
    (DPState dsEx2B psEx2A)

-- | Expected state after transition
expectedStEx2B :: forall c. Mock c => ChainState c
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
    (makeEvolvedNonce p (nonce0 p) [blockEx2A, blockEx2B])
    (makeEvolvedNonce p (nonce0 p) [blockEx2A])
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 2) -- Current block no
          (SlotNo 90) -- Current slot
          (blockEx2BHash p) -- Hash header of the chain
    )
  where
    p :: Proxy c
    p = Proxy

ex2B :: Mock c => proxy c -> CHAINExample c
ex2B _ = CHAINExample expectedStEx2A blockEx2B (Right expectedStEx2B)

-- | Example 2C - process an empty block in the next epoch
-- so that the (empty) reward update is applied and a stake snapshot is made.
blockEx2C :: forall c. Mock c => Block c
blockEx2C =
  mkBlock
    (blockEx2BHash p) -- Hash of previous block
    (slotKeys 110)
    [] -- No transactions at all (empty block)
    (SlotNo 110) -- Current slot
    (BlockNo 3) -- Second block within the epoch
    (makeEvolvedNonce p (nonce0 p) [blockEx2A])
    (NatNonce 3) -- Block nonce
    zero -- Praos leader value
    5 -- Period of KES (key evolving signature scheme)
    0
    (mkOCert (slotKeys 110) 0 (KESPeriod 0))
  where
    p :: Proxy c
    p = Proxy

-- | Snapshot of stakes for Alice and Bob
snapEx2C :: Crypto c => SnapShot c
snapEx2C =
  SnapShot
    ( Stake
        ( Map.fromList
            [ (Cast.aliceSHK, aliceCoinEx2BBase + aliceCoinEx2BPtr),
              (Cast.bobSHK, bobInitCoin)
            ]
        )
    )
    delegsEx2B
    (Map.singleton (hk Cast.alicePoolKeys) Cast.alicePoolParams)

-- | Snapshots with given fees.
snapsEx2C :: Crypto c => SnapShots c
snapsEx2C =
  emptySnapShots
    { _pstakeMark = snapEx2C, -- snapshot of stake pools and parameters
      _feeSS = 7
    }

expectedLSEx2C :: Crypto c => LedgerState c
expectedLSEx2C =
  LedgerState
    ( UTxOState
        utxoEx2B
        (Coin 271)
        (Coin 7)
        emptyPPUPState -- Note that the ppup is gone now
    )
    ( DPState
        dsEx2B
          { _irwd = emptyInstantaneousRewards,
            _rewards = Map.insert (Cast.carlSHK) 110 $ _rewards dsEx2B
          }
        psEx2A
    )

blockEx2CHash :: Mock c => HashHeader c
blockEx2CHash = bhHash (bheader blockEx2C)

expectedStEx2Cgeneric :: forall c. Mock c => SnapShots c -> LedgerState c -> PParams -> ChainState c
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
    (makeEvolvedNonce p (nonce0 p) [blockEx2A])
    (makeEvolvedNonce p (nonce0 p) [blockEx2A, blockEx2B, blockEx2C])
    (makeEvolvedNonce p (nonce0 p) [blockEx2A, blockEx2B, blockEx2C])
    (hashHeaderToNonce (blockEx2BHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 3)
          (SlotNo 110)
          blockEx2CHash
    )
  where
    p :: Proxy c
    p = Proxy

-- ** Expected chain state after STS

expectedStEx2C :: Mock c => ChainState c
expectedStEx2C = expectedStEx2Cgeneric snapsEx2C expectedLSEx2C ppsEx1

ex2C :: Mock c => proxy c -> CHAINExample c
ex2C _ = CHAINExample expectedStEx2B blockEx2C (Right expectedStEx2C)

-- | Example 2D - process a block late enough
-- in the epoch in order to create a second reward update, preparing the way for
-- the first non-empty pool distribution in this running example.
-- Additionally, in order to have the stake distribution change,
-- Carl delegates his stake.

-- | The transaction delegates Carl's stake to Alice's pool.
aliceCoinEx2DBase :: Coin
aliceCoinEx2DBase = aliceCoinEx2BBase - 5

txbodyEx2D :: forall c. Crypto c => TxBody c
txbodyEx2D =
  TxBody
    { TxData._inputs = Set.fromList [TxIn (txid txbodyEx2B) 0],
      TxData._outputs = StrictSeq.fromList [TxOut Cast.aliceAddr aliceCoinEx2DBase],
      TxData._certs =
        StrictSeq.fromList [DCertDeleg (Delegate $ Delegation Cast.carlSHK (hk Cast.alicePoolKeys))],
      TxData._wdrls = Wdrl Map.empty,
      TxData._txfee = Coin 5,
      TxData._ttl = SlotNo 500,
      TxData._txUpdate = SNothing,
      TxData._mdHash = SNothing
    }

txEx2D :: Mock c => Tx c
txEx2D =
  Tx
    txbodyEx2D
    mempty
      { addrWits =
          makeWitnessesVKey (hashAnnotated txbodyEx2D) [asWitness Cast.alicePay, asWitness Cast.carlStake]
      }
    SNothing

blockEx2D :: forall c. Mock c => Block c
blockEx2D =
  mkBlock
    blockEx2CHash
    (slotKeys 190)
    [txEx2D]
    (SlotNo 190)
    (BlockNo 4)
    (makeEvolvedNonce p (nonce0 p) [blockEx2A])
    (NatNonce 4)
    zero
    9
    0
    (mkOCert (slotKeys 190) 0 (KESPeriod 0))
  where
    p :: Proxy c
    p = Proxy

blockEx2DHash :: Mock c => proxy c -> HashHeader c
blockEx2DHash _ = bhHash (bheader blockEx2D)

utxoEx2D :: Crypto c => UTxO c
utxoEx2D =
  UTxO . Map.fromList $
    [ (TxIn genesisId 1, TxOut Cast.bobAddr bobInitCoin),
      (TxIn (txid txbodyEx2D) 0, TxOut Cast.aliceAddr aliceCoinEx2DBase),
      (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr aliceCoinEx2BPtr)
    ]

delegsEx2D :: forall c. Crypto c => Map (Credential 'Staking c) (KeyHash 'StakePool c)
delegsEx2D =
  Map.fromList
    [ (Cast.aliceSHK, hk Cast.alicePoolKeys),
      (Cast.bobSHK, hk Cast.alicePoolKeys),
      (Cast.carlSHK, hk Cast.alicePoolKeys)
    ]

dsEx2D :: Crypto c => DState c
dsEx2D = (dsEx2C) {_delegations = delegsEx2D}
  where
    dsEx2C = (_dstate . _delegationState) expectedLSEx2C

expectedLSEx2D :: Crypto c => LedgerState c
expectedLSEx2D =
  LedgerState
    ( UTxOState
        utxoEx2D
        (Coin 271)
        (Coin 12)
        emptyPPUPState
    )
    (DPState dsEx2D psEx2A)

expectedStEx2D :: forall c. Mock c => ChainState c
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
              { deltaT = Coin 1,
                deltaR = Coin 6,
                rs = Map.empty,
                deltaF = Coin (-7),
                nonMyopic = emptyNonMyopic {rewardPotNM = Coin 6}
              }
        )
        (PoolDistr Map.empty)
        (overlayScheduleFor (EpochNo 1))
    )
    oCertIssueNosEx1
    (makeEvolvedNonce p (nonce0 p) [blockEx2A])
    (makeEvolvedNonce p (nonce0 p) [blockEx2A, blockEx2B, blockEx2C, blockEx2D])
    (makeEvolvedNonce p (nonce0 p) [blockEx2A, blockEx2B, blockEx2C])
    (hashHeaderToNonce (blockEx2BHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 4)
          (SlotNo 190)
          (blockEx2DHash p)
    )
  where
    p :: Proxy c
    p = Proxy

ex2D :: Mock c => proxy c -> CHAINExample c
ex2D _ = CHAINExample expectedStEx2C blockEx2D (Right expectedStEx2D)

-- | Example 2E - create the first non-empty pool distribution
-- by creating a block in the third epoch of this running example.
blockEx2E :: forall c. Mock c => Block c
blockEx2E =
  mkBlock
    (blockEx2DHash p)
    (slotKeys 220)
    []
    (SlotNo 220)
    (BlockNo 5)
    ( makeEvolvedNonce p (nonce0 p) [blockEx2A, blockEx2B, blockEx2C]
        ⭒ (hashHeaderToNonce (blockEx2BHash p))
    )
    (NatNonce 5)
    zero
    11
    10
    (mkOCert (slotKeys 220) 1 (KESPeriod 10))
  where
    p :: Proxy c
    p = Proxy

snapEx2E :: forall c. Crypto c => SnapShot c
snapEx2E =
  SnapShot
    ( Stake
        ( Map.fromList
            [ (Cast.aliceSHK, aliceCoinEx2DBase + aliceCoinEx2BPtr),
              (Cast.carlSHK, carlMIR),
              (Cast.bobSHK, bobInitCoin)
            ]
        )
    )
    delegsEx2D
    (Map.singleton (hk Cast.alicePoolKeys) Cast.alicePoolParams)

snapsEx2E :: Crypto c => proxy c -> SnapShots c
snapsEx2E _ =
  emptySnapShots
    { _pstakeMark = snapEx2E,
      _pstakeSet = snapEx2C,
      _feeSS = Coin 5
    }

expectedLSEx2E :: Crypto c => LedgerState c
expectedLSEx2E =
  LedgerState
    ( UTxOState
        utxoEx2D
        (Coin 271)
        (Coin 5)
        emptyPPUPState
    )
    ( DPState
        dsEx2D
          { _irwd = emptyInstantaneousRewards,
            _rewards = Map.insert Cast.carlSHK 110 $ _rewards dsEx2B
          }
        psEx2A
    )

blockEx2EHash :: Mock c => HashHeader c
blockEx2EHash = bhHash (bheader blockEx2E)

acntEx2E :: Crypto c => proxy c -> AccountState
acntEx2E p =
  AccountState
    { _treasury = Coin 1,
      _reserves = maxLLSupply - balance (utxoEx2A p) - carlMIR + Coin 6
    }

oCertIssueNosEx2 :: Crypto c => Map (KeyHash 'BlockIssuer c) Word64
oCertIssueNosEx2 =
  Map.insert
    (coerceKeyRole . hashKey $ vKey $ cold $ slotKeys 220)
    1
    oCertIssueNosEx1

nonMyopicEx2E :: NonMyopic h
nonMyopicEx2E = emptyNonMyopic {rewardPotNM = Coin 6}

expectedStEx2E :: forall c. Mock c => ChainState c
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
                (hk Cast.alicePoolKeys)
                (IndividualPoolStake 1 (hashVerKeyVRF (snd $ vrf (Cast.alicePoolKeys @c))))
            )
        )
        (overlayScheduleFor (EpochNo 2))
    )
    oCertIssueNosEx2
    (makeEvolvedNonce p (nonce0 p) [blockEx2A, blockEx2B, blockEx2C] ⭒ hashHeaderToNonce (blockEx2BHash p))
    (makeEvolvedNonce p (nonce0 p) [blockEx2A, blockEx2B, blockEx2C, blockEx2D, blockEx2E])
    (makeEvolvedNonce p (nonce0 p) [blockEx2A, blockEx2B, blockEx2C, blockEx2D, blockEx2E])
    (hashHeaderToNonce (blockEx2DHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 5)
          (SlotNo 220)
          blockEx2EHash
    )
  where
    p :: Proxy c
    p = Proxy

ex2E :: Mock c => proxy c -> CHAINExample c
ex2E _ = CHAINExample expectedStEx2D blockEx2E (Right expectedStEx2E)

-- | Example 2F - create a decentralized Praos block (ie one not in the overlay schedule)
oCertIssueNosEx2F :: forall c. Crypto c => Map (KeyHash 'BlockIssuer c) Word64
oCertIssueNosEx2F = Map.insert (coerceKeyRole $ hk Cast.alicePoolKeys) 0 oCertIssueNosEx2

blockEx2F :: forall c. Mock c => Block c
blockEx2F =
  mkBlock
    blockEx2EHash
    Cast.alicePoolKeys
    []
    (SlotNo 295) -- odd slots open for decentralization in epoch1OSchedEx2E
    (BlockNo 6)
    (makeEvolvedNonce p (nonce0 p) [blockEx2A, blockEx2B, blockEx2C] ⭒ hashHeaderToNonce (blockEx2BHash p))
    (NatNonce 6)
    zero
    14
    14
    (mkOCert Cast.alicePoolKeys 0 (KESPeriod 14))
  where
    p :: Proxy c
    p = Proxy

blockEx2FHash :: Mock c => proxy c -> HashHeader c
blockEx2FHash _ = bhHash (bheader blockEx2F)

pdEx2F :: forall c. Crypto c => PoolDistr c
pdEx2F =
  PoolDistr $
    Map.singleton
      (hk Cast.alicePoolKeys)
      ( IndividualPoolStake
          1
          ( hashVerKeyVRF $
              snd $ vrf (Cast.alicePoolKeys @c)
          )
      )

nonMyopicEx2F :: NonMyopic h
nonMyopicEx2F = emptyNonMyopic {rewardPotNM = Coin 4}

expectedStEx2F :: forall c. Mock c => ChainState c
expectedStEx2F =
  ChainState
    ( NewEpochState
        (EpochNo 2)
        (BlocksMade Map.empty)
        (BlocksMade $ Map.singleton (hk Cast.alicePoolKeys) 1)
        (EpochState (acntEx2E p) (snapsEx2E p) expectedLSEx2E ppsEx1 ppsEx1 nonMyopicEx2E)
        ( SJust
            RewardUpdate
              { deltaT = Coin 1,
                deltaR = Coin 4,
                rs = Map.empty,
                deltaF = Coin (-5),
                nonMyopic = nonMyopicEx2F
              }
        )
        pdEx2F
        (overlayScheduleFor (EpochNo 2))
    )
    oCertIssueNosEx2F
    (makeEvolvedNonce p (nonce0 p) [blockEx2A, blockEx2B, blockEx2C] ⭒ hashHeaderToNonce (blockEx2BHash p))
    (makeEvolvedNonce p (nonce0 p) [blockEx2A, blockEx2B, blockEx2C, blockEx2D, blockEx2E, blockEx2F])
    (makeEvolvedNonce p (nonce0 p) [blockEx2A, blockEx2B, blockEx2C, blockEx2D, blockEx2E])
    (hashHeaderToNonce (blockEx2DHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 6)
          (SlotNo 295)
          (blockEx2FHash p)
    )
  where
    p :: Proxy c
    p = Proxy

ex2F :: Mock c => Proxy c -> CHAINExample c
ex2F _ = CHAINExample expectedStEx2E blockEx2F (Right expectedStEx2F)

epochNonceEx2G :: Mock c => Proxy c -> Nonce
epochNonceEx2G p =
  makeEvolvedNonce
    p
    (nonce0 p)
    [blockEx2A, blockEx2B, blockEx2C, blockEx2D, blockEx2E]
    ⭒ hashHeaderToNonce (blockEx2DHash p)

evolNonceEx2G :: Mock c => Proxy c -> Nonce
evolNonceEx2G p =
  makeEvolvedNonce
    p
    (nonce0 p)
    [blockEx2A, blockEx2B, blockEx2C, blockEx2D, blockEx2E, blockEx2F, blockEx2G]

-- | Example 2G - create an empty block in the next epoch
-- to prepare the way for the first non-trivial reward update
blockEx2G :: forall c. Mock c => Block c
blockEx2G =
  mkBlock
    (blockEx2FHash p)
    (slotKeys 310)
    []
    (SlotNo 310)
    (BlockNo 7)
    (epochNonceEx2G p)
    (NatNonce 7)
    zero
    15
    15
    (mkOCert (slotKeys 310) 1 (KESPeriod 15))
  where
    p :: Proxy c
    p = Proxy

blockEx2GHash :: Mock c => HashHeader c
blockEx2GHash = bhHash (bheader blockEx2G)

snapsEx2G :: Crypto c => proxy c -> SnapShots c
snapsEx2G p =
  (snapsEx2E p)
    { _pstakeMark = snapEx2E,
      _pstakeSet = snapEx2E,
      _pstakeGo = snapEx2C,
      _feeSS = 0
    }

expectedLSEx2G :: Crypto c => LedgerState c
expectedLSEx2G =
  LedgerState
    ( UTxOState
        utxoEx2D
        (Coin 271)
        (Coin 0)
        emptyPPUPState
    )
    ( DPState
        dsEx2D
        psEx2A
    )

oCertIssueNosEx2G :: Crypto c => Map (KeyHash 'BlockIssuer c) Word64
oCertIssueNosEx2G =
  Map.insert
    (coerceKeyRole . hashKey $ vKey $ cold $ slotKeys 310)
    1
    oCertIssueNosEx2F

acntEx2G :: Crypto c => proxy c -> AccountState
acntEx2G p =
  (acntEx2E p)
    { _treasury = Coin 2,
      _reserves = maxLLSupply - balance (utxoEx2A p) - carlMIR + Coin 10
    }

expectedStEx2G :: forall c. Mock c => ChainState c
expectedStEx2G =
  ChainState
    ( NewEpochState
        (EpochNo 3)
        (BlocksMade $ Map.singleton (hk Cast.alicePoolKeys) 1)
        (BlocksMade Map.empty)
        (EpochState (acntEx2G p) (snapsEx2G p) expectedLSEx2G ppsEx1 ppsEx1 nonMyopicEx2F)
        SNothing
        pdEx2F
        (overlayScheduleFor (EpochNo 3))
    )
    oCertIssueNosEx2G
    (epochNonceEx2G p)
    (evolNonceEx2G p)
    (evolNonceEx2G p)
    (hashHeaderToNonce (blockEx2FHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 7)
          (SlotNo 310)
          blockEx2GHash
    )
  where
    p :: Proxy c
    p = Proxy

ex2G :: Mock c => proxy c -> CHAINExample c
ex2G _ = CHAINExample expectedStEx2F blockEx2G (Right expectedStEx2G)

-- | Example 2H - create the first non-trivial reward update
blockEx2H :: forall c. Mock c => Block c
blockEx2H =
  mkBlock
    blockEx2GHash
    (slotKeys 390)
    []
    (SlotNo 390)
    (BlockNo 8)
    (epochNonceEx2G p)
    (NatNonce 8)
    zero
    19
    19
    (mkOCert (slotKeys 390) 2 (KESPeriod 19))
  where
    p :: Proxy c
    p = Proxy

blockEx2HHash :: Mock c => proxy c -> HashHeader c
blockEx2HHash _ = bhHash (bheader blockEx2H)

aliceRAcnt2H :: Coin
aliceRAcnt2H = Coin 11654787878

bobRAcnt2H :: Coin
bobRAcnt2H = Coin 1038545454

rewardsEx2H :: Crypto c => RewardAccounts c
rewardsEx2H =
  Map.fromList
    [ (Cast.aliceSHK, aliceRAcnt2H),
      (Cast.bobSHK, bobRAcnt2H)
    ]

oCertIssueNosEx2H :: Crypto c => Map (KeyHash 'BlockIssuer c) Word64
oCertIssueNosEx2H =
  Map.insert
    (coerceKeyRole . hashKey $ vKey $ cold $ slotKeys 390)
    2
    oCertIssueNosEx2G

alicePerfEx2H :: forall c. Crypto c => Proxy c -> Likelihood
alicePerfEx2H p = likelihood blocks t slotsPerEpoch
  where
    slotsPerEpoch = runShelleyBase $ do
      ei <- asks epochInfo
      epochInfoSize ei 0
    blocks = 1
    t = leaderProbability f relativeStake (_d ppsEx1)
    (Coin stake) = aliceCoinEx2BBase + aliceCoinEx2BPtr + bobInitCoin
    reserves = _reserves (acntEx2G p)
    (Coin tot) = maxLLSupply - reserves
    relativeStake = fromRational (stake % tot)
    f = runShelleyBase (asks activeSlotCoeff)

deltaT2H :: Coin
deltaT2H = Coin 317333333333

deltaR2H :: Coin
deltaR2H = Coin (-330026666665)

nonMyopicEx2H :: forall c. Crypto c => NonMyopic c
nonMyopicEx2H =
  NonMyopic
    (Map.singleton (hk Cast.alicePoolKeys) (alicePerfEx2H p))
    (Coin 1269333333333)
    snapEx2C
  where
    p :: Proxy c
    p = Proxy

evolNonceEx2H :: Mock c => Proxy c -> Nonce
evolNonceEx2H p =
  makeEvolvedNonce
    p
    (nonce0 p)
    [blockEx2A, blockEx2B, blockEx2C, blockEx2D, blockEx2E, blockEx2F, blockEx2G, blockEx2H]

expectedStEx2H :: forall c. Mock c => ChainState c
expectedStEx2H =
  ChainState
    ( NewEpochState
        (EpochNo 3)
        (BlocksMade $ Map.singleton (hk Cast.alicePoolKeys) 1)
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
    (epochNonceEx2G p)
    (evolNonceEx2H p)
    (evolNonceEx2G p)
    (hashHeaderToNonce (blockEx2FHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 8)
          (SlotNo 390)
          (blockEx2HHash p)
    )
  where
    p :: Proxy c
    p = Proxy

ex2H :: Mock c => proxy c -> CHAINExample c
ex2H _ = CHAINExample expectedStEx2G blockEx2H (Right expectedStEx2H)

epochNonceEx2I :: Mock c => Proxy c -> Nonce
epochNonceEx2I p =
  makeEvolvedNonce
    p
    (nonce0 p)
    [blockEx2A, blockEx2B, blockEx2C, blockEx2D, blockEx2E, blockEx2F, blockEx2G]
    ⭒ hashHeaderToNonce (blockEx2FHash p)

evolNonceEx2I :: Mock c => Proxy c -> Nonce
evolNonceEx2I p = makeEvolvedNonce p (evolNonceEx2G p) [blockEx2H, blockEx2I]

-- | Example 2I - apply the first non-trivial reward update
blockEx2I :: forall c. Mock c => Block c
blockEx2I =
  mkBlock
    (blockEx2HHash p)
    (slotKeys 410)
    []
    (SlotNo 410)
    (BlockNo 9)
    (epochNonceEx2I p)
    (NatNonce 9)
    zero
    20
    20
    (mkOCert (slotKeys 410) 2 (KESPeriod 20))
  where
    p :: Proxy c
    p = Proxy

blockEx2IHash :: Mock c => HashHeader c
blockEx2IHash = bhHash (bheader blockEx2I)

acntEx2I :: Crypto c => proxy c -> AccountState
acntEx2I p =
  AccountState
    { _treasury = (_treasury (acntEx2G p)) + deltaT2H,
      _reserves = (_reserves (acntEx2G p)) + deltaR2H
    }

dsEx2I :: Crypto c => DState c
dsEx2I = dsEx2D {_rewards = Map.insert Cast.carlSHK 110 rewardsEx2H}

expectedLSEx2I :: Crypto c => LedgerState c
expectedLSEx2I =
  LedgerState
    ( UTxOState
        utxoEx2D
        (Coin 271)
        (Coin 0)
        emptyPPUPState
    )
    (DPState dsEx2I psEx2A)

snapsEx2I :: forall c. Crypto c => Proxy c -> SnapShots c
snapsEx2I p =
  (snapsEx2G p)
    { _pstakeMark =
        SnapShot
          ( Stake
              ( Map.fromList
                  [ (Cast.bobSHK, bobInitCoin + bobRAcnt2H),
                    (Cast.aliceSHK, aliceCoinEx2DBase + aliceCoinEx2BPtr + aliceRAcnt2H),
                    (Cast.carlSHK, carlMIR)
                  ]
              )
          )
          delegsEx2D
          (Map.singleton (hk Cast.alicePoolKeys) Cast.alicePoolParams),
      -- The stake snapshots have bigger values now, due to the new rewards
      _pstakeSet = snapEx2E,
      _pstakeGo = snapEx2E,
      _feeSS = Coin 0
    }

oCertIssueNosEx2I :: Crypto c => Map (KeyHash 'BlockIssuer c) Word64
oCertIssueNosEx2I =
  Map.insert
    (coerceKeyRole . hashKey $ vKey $ cold $ slotKeys 410)
    2
    oCertIssueNosEx2H

expectedStEx2I :: forall c. Mock c => ChainState c
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
    (epochNonceEx2I p)
    (evolNonceEx2I p)
    (evolNonceEx2I p)
    (hashHeaderToNonce (blockEx2HHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 9)
          (SlotNo 410)
          blockEx2IHash
    )
  where
    p :: Proxy c
    p = Proxy

ex2I :: Mock c => proxy c -> CHAINExample c
ex2I _ = CHAINExample expectedStEx2H blockEx2I (Right expectedStEx2I)

-- | Example 2J - drain reward account and de-register stake key
bobAda2J :: Coin
bobAda2J =
  bobRAcnt2H -- reward account
    + bobInitCoin -- txin we will consume (must spend at least one)
    + Coin 7 -- stake registration refund
    - Coin 9 -- tx fee

txbodyEx2J :: Crypto c => TxBody c
txbodyEx2J =
  TxBody
    (Set.fromList [TxIn genesisId 1])
    (StrictSeq.singleton $ TxOut Cast.bobAddr bobAda2J)
    (StrictSeq.fromList [DCertDeleg (DeRegKey Cast.bobSHK)])
    (Wdrl $ Map.singleton (RewardAcnt Testnet Cast.bobSHK) bobRAcnt2H)
    (Coin 9)
    (SlotNo 500)
    SNothing
    SNothing

txEx2J :: Mock c => Tx c
txEx2J =
  Tx
    txbodyEx2J
    mempty
      { addrWits =
          makeWitnessesVKey (hashAnnotated txbodyEx2J) [asWitness Cast.bobPay, asWitness Cast.bobStake]
      }
    SNothing

blockEx2J :: forall c. Mock c => Block c
blockEx2J =
  mkBlock
    blockEx2IHash
    (slotKeys 420)
    [txEx2J]
    (SlotNo 420)
    (BlockNo 10)
    (epochNonceEx2I p)
    (NatNonce 10)
    zero
    21
    19
    (mkOCert (slotKeys 420) 2 (KESPeriod 19))
  where
    p :: Proxy c
    p = Proxy

blockEx2JHash :: Mock c => HashHeader c
blockEx2JHash = bhHash (bheader blockEx2J)

utxoEx2J :: Crypto c => UTxO c
utxoEx2J =
  UTxO . Map.fromList $
    [ (TxIn (txid txbodyEx2J) 0, TxOut Cast.bobAddr bobAda2J),
      (TxIn (txid txbodyEx2D) 0, TxOut Cast.aliceAddr aliceCoinEx2DBase),
      (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr aliceCoinEx2BPtr)
    ]

dsEx2J :: Crypto c => DState c
dsEx2J =
  dsEx1
    { _ptrs =
        biMapFromList
          (\l _r -> l)
          [ (Ptr (SlotNo 10) 0 0, Cast.aliceSHK),
            (Ptr (SlotNo 10) 0 2, Cast.carlSHK)
          ],
      _delegations = Map.fromList [(Cast.aliceSHK, hk Cast.alicePoolKeys), (Cast.carlSHK, hk Cast.alicePoolKeys)],
      _rewards = Map.fromList [(Cast.aliceSHK, aliceRAcnt2H), (Cast.carlSHK, carlMIR)]
    }

expectedLSEx2J :: Crypto c => LedgerState c
expectedLSEx2J =
  LedgerState
    ( UTxOState
        utxoEx2J
        (Coin 264)
        (Coin 9)
        emptyPPUPState
    )
    (DPState dsEx2J psEx2A)

oCertIssueNosEx2J :: Crypto c => Map (KeyHash 'BlockIssuer c) Word64
oCertIssueNosEx2J =
  Map.insert
    (coerceKeyRole . hashKey $ vKey $ cold $ slotKeys 420)
    2
    oCertIssueNosEx2I

evolNonceEx2J :: Mock c => Proxy c -> Nonce
evolNonceEx2J p = makeEvolvedNonce p (evolNonceEx2I p) [blockEx2J]

expectedStEx2J :: forall c. Mock c => ChainState c
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
    (epochNonceEx2I p)
    (evolNonceEx2J p)
    (evolNonceEx2J p)
    (hashHeaderToNonce (blockEx2HHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 10)
          (SlotNo 420)
          blockEx2JHash
    )
  where
    p :: Proxy c
    p = Proxy

ex2J :: Mock c => proxy c -> CHAINExample c
ex2J _ = CHAINExample expectedStEx2I blockEx2J (Right expectedStEx2J)

-- | Example 2K - start stake pool retirement
aliceCoinEx2KPtr :: Coin
aliceCoinEx2KPtr = aliceCoinEx2DBase - 2

txbodyEx2K :: Crypto c => TxBody c
txbodyEx2K =
  TxBody
    (Set.fromList [TxIn (txid txbodyEx2D) 0])
    (StrictSeq.singleton $ TxOut alicePtrAddr aliceCoinEx2KPtr)
    (StrictSeq.fromList [DCertPool (RetirePool (hk Cast.alicePoolKeys) (EpochNo 5))])
    (Wdrl Map.empty)
    (Coin 2)
    (SlotNo 500)
    SNothing
    SNothing

txEx2K :: Mock c => Tx c
txEx2K =
  Tx
    txbodyEx2K
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated txbodyEx2K)
            ( [asWitness Cast.alicePay]
                <> [asWitness $ cold Cast.alicePoolKeys]
            )
      }
    SNothing

blockEx2K :: forall c. Mock c => Block c
blockEx2K =
  mkBlock
    blockEx2JHash
    (slotKeys 490)
    [txEx2K]
    (SlotNo 490)
    (BlockNo 11)
    (epochNonceEx2I p)
    (NatNonce 11)
    zero
    24
    19
    (mkOCert (slotKeys 490) 2 (KESPeriod 19))
  where
    p :: Proxy c
    p = Proxy

blockEx2KHash :: Mock c => proxy c -> HashHeader c
blockEx2KHash _ = bhHash (bheader blockEx2K)

utxoEx2K :: Crypto c => UTxO c
utxoEx2K =
  UTxO . Map.fromList $
    [ (TxIn (txid txbodyEx2J) 0, TxOut Cast.bobAddr bobAda2J),
      (TxIn (txid txbodyEx2K) 0, TxOut alicePtrAddr aliceCoinEx2KPtr),
      (TxIn (txid txbodyEx2B) 1, TxOut alicePtrAddr aliceCoinEx2BPtr)
    ]

psEx2K :: Crypto c => PState c
psEx2K = psEx2A {_retiring = Map.singleton (hk Cast.alicePoolKeys) (EpochNo 5)}

expectedLSEx2K :: Crypto c => LedgerState c
expectedLSEx2K =
  LedgerState
    ( UTxOState
        utxoEx2K
        (Coin 264)
        (Coin 11)
        emptyPPUPState
    )
    (DPState dsEx2J psEx2K)

alicePerfEx2K :: forall c. Crypto c => Proxy c -> Likelihood
alicePerfEx2K p = (alicePerfEx2H p) <> epoch4Likelihood
  where
    epoch4Likelihood = likelihood blocks t slotsPerEpoch
    slotsPerEpoch = runShelleyBase $ do
      ei <- asks epochInfo
      epochInfoSize ei 0
    blocks = 0
    t = leaderProbability f relativeStake (_d ppsEx1)
    (Coin stake) = sum . unStake . _stake . _pstakeSet $ (snapsEx2I p) -- everyone has delegated to Alice's Pool
    relativeStake = fromRational (stake % supply)
    (Coin supply) = maxLLSupply - _reserves (acntEx2I p)
    f = runShelleyBase (asks activeSlotCoeff)

nonMyopicEx2K :: forall c. Crypto c => NonMyopic c
nonMyopicEx2K =
  NonMyopic
    (Map.singleton (hk Cast.alicePoolKeys) (alicePerfEx2K p))
    (Coin 0)
    snapEx2E
  where
    p :: Proxy c
    p = Proxy

evolNonceEx2K :: Mock c => Proxy c -> Nonce
evolNonceEx2K p = makeEvolvedNonce p (evolNonceEx2J p) [blockEx2K]

expectedStEx2K :: forall c. Mock c => ChainState c
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
    (epochNonceEx2I p)
    (evolNonceEx2K p)
    (evolNonceEx2J p)
    (hashHeaderToNonce (blockEx2HHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 11)
          (SlotNo 490)
          (blockEx2KHash p)
    )
  where
    p :: Proxy c
    p = Proxy

ex2K :: Mock c => proxy c -> CHAINExample c
ex2K _ = CHAINExample expectedStEx2J blockEx2K (Right expectedStEx2K)

-- | Example 2L - reap a stake pool
epochNonceEx2L :: Mock c => Proxy c -> Nonce
epochNonceEx2L p = (evolNonceEx2J p) ⭒ hashHeaderToNonce (blockEx2HHash p)

blockEx2L :: forall c. Mock c => Block c
blockEx2L =
  mkBlock
    (blockEx2KHash p)
    (slotKeys 510)
    []
    (SlotNo 510)
    (BlockNo 12)
    (epochNonceEx2L p)
    (NatNonce 12)
    zero
    25
    25
    (mkOCert (slotKeys 510) 3 (KESPeriod 25))
  where
    p :: Proxy c
    p = Proxy

blockEx2LHash :: Mock c => HashHeader c
blockEx2LHash = bhHash (bheader blockEx2L)

acntEx2L :: Crypto c => proxy c -> AccountState
acntEx2L p =
  (acntEx2I p)
    { _treasury =
        _treasury (acntEx2I p) --previous amount
    }

snapsEx2L :: Crypto c => SnapShots c
snapsEx2L =
  SnapShots
    { _pstakeMark =
        SnapShot
          ( Stake
              ( Map.fromList
                  [ (Cast.aliceSHK, aliceRAcnt2H + aliceCoinEx2BPtr + aliceCoinEx2KPtr),
                    (Cast.carlSHK, carlMIR)
                  ]
              )
          )
          (Map.fromList [(Cast.aliceSHK, hk Cast.alicePoolKeys), (Cast.carlSHK, hk Cast.alicePoolKeys)])
          (Map.singleton (hk Cast.alicePoolKeys) Cast.alicePoolParams),
      _pstakeSet = _pstakeMark (snapsEx2I p),
      _pstakeGo = _pstakeSet (snapsEx2I p),
      _feeSS = Coin 11
    }
  where
    p :: Proxy c
    p = Proxy

dsEx2L :: Crypto c => DState c
dsEx2L =
  dsEx1
    { _ptrs =
        biMapFromList
          (\l _r -> l)
          [ (Ptr (SlotNo 10) 0 0, Cast.aliceSHK),
            (Ptr (SlotNo 10) 0 2, Cast.carlSHK)
          ],
      _rewards =
        Map.fromList
          [ (Cast.aliceSHK, aliceRAcnt2H + Coin 250),
            (Cast.carlSHK, carlMIR)
          ]
          -- Note the pool cert refund of 201
    }

expectedLSEx2L :: Crypto c => LedgerState c
expectedLSEx2L =
  LedgerState
    ( UTxOState
        utxoEx2K
        (Coin 14)
        (Coin 11)
        emptyPPUPState
    )
    (DPState dsEx2L psEx1) -- Note the stake pool is reaped

oCertIssueNosEx2L :: Crypto c => Map (KeyHash 'BlockIssuer c) Word64
oCertIssueNosEx2L =
  Map.insert (coerceKeyRole . hashKey $ vKey $ cold $ slotKeys 510) 3 oCertIssueNosEx2J

evolNonceEx2L :: Mock c => Proxy c -> Nonce
evolNonceEx2L p = makeEvolvedNonce p (evolNonceEx2K p) [blockEx2L]

expectedStEx2L :: forall c. Mock c => ChainState c
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
    (epochNonceEx2L p)
    (evolNonceEx2L p)
    (evolNonceEx2L p)
    (hashHeaderToNonce (blockEx2KHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 12)
          (SlotNo 510)
          blockEx2LHash
    )
  where
    p :: Proxy c
    p = Proxy

ex2L :: Mock c => proxy c -> CHAINExample c
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
      _extraEntropy = SJust (mkNonceFromNumber 123),
      _protocolVersion = SNothing,
      _minUTxOValue = SNothing,
      _minPoolCost = SNothing
    }

ppupEx3A :: Crypto c => ProposedPPUpdates c
ppupEx3A =
  ProposedPPUpdates $
    Map.fromList
      [ (hashKey $ coreNodeVKG 0, ppVote3A),
        (hashKey $ coreNodeVKG 3, ppVote3A),
        (hashKey $ coreNodeVKG 4, ppVote3A)
      ]

updateEx3A :: Crypto c => Update c
updateEx3A = Update ppupEx3A (EpochNo 0)

aliceCoinEx3A :: Coin
aliceCoinEx3A = aliceInitCoin - 1

txbodyEx3A :: Crypto c => TxBody c
txbodyEx3A =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr aliceCoinEx3A)
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 10)
    (SJust updateEx3A)
    SNothing

txEx3A :: Mock c => Tx c
txEx3A =
  Tx
    txbodyEx3A
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated txbodyEx3A)
            ( [asWitness Cast.alicePay]
                <> [ asWitness . cold $ coreNodeKeys p 0,
                     asWitness . cold $ coreNodeKeys p 3,
                     asWitness . cold $ coreNodeKeys p 4
                   ]
            )
      }
    SNothing
  where
    p :: Proxy c
    p = Proxy

blockEx3A :: forall c. Mock c => Block c
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
    p :: Proxy c
    p = Proxy

expectedLSEx3A :: Crypto c => LedgerState c
expectedLSEx3A =
  LedgerState
    ( UTxOState
        ( UTxO . Map.fromList $
            [ (TxIn genesisId 1, TxOut Cast.bobAddr bobInitCoin),
              (TxIn (txid txbodyEx3A) 0, TxOut Cast.aliceAddr aliceCoinEx3A)
            ]
        )
        (Coin 0)
        (Coin 1)
        (PPUPState ppupEx3A emptyPPPUpdates)
    )
    (DPState dsEx1 psEx1)

blockEx3AHash :: Mock c => HashHeader c
blockEx3AHash = bhHash (bheader blockEx3A)

expectedStEx3A :: forall c. Mock c => ChainState c
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
    (makeEvolvedNonce p (nonce0 p) [blockEx3A])
    (makeEvolvedNonce p (nonce0 p) [blockEx3A])
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 1)
          (SlotNo 10)
          blockEx3AHash
    )
  where
    p :: Proxy c
    p = Proxy

ex3A :: Mock c => proxy c -> CHAINExample c
ex3A _ = CHAINExample initStEx2A blockEx3A (Right expectedStEx3A)

-- | Example 3B - Finish getting enough votes for the protocol parameter update.
ppupEx3B :: Crypto c => ProposedPPUpdates c
ppupEx3B =
  ProposedPPUpdates $
    Map.fromList
      [ (hashKey $ coreNodeVKG 1, ppVote3A),
        (hashKey $ coreNodeVKG 5, ppVote3A)
      ]

updateEx3B :: Crypto c => Update c
updateEx3B = Update ppupEx3B (EpochNo 0)

aliceCoinEx3B :: Coin
aliceCoinEx3B = aliceCoinEx3A - 1

txbodyEx3B :: Crypto c => TxBody c
txbodyEx3B =
  TxBody
    (Set.fromList [TxIn (txid txbodyEx3A) 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr aliceCoinEx3B)
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 31)
    (SJust updateEx3B)
    SNothing

txEx3B :: Mock c => Tx c
txEx3B =
  Tx
    txbodyEx3B
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated txbodyEx3B)
            ( [asWitness Cast.alicePay]
                <> [ asWitness . cold $ coreNodeKeys p 1,
                     asWitness . cold $ coreNodeKeys p 5
                   ]
            )
      }
    SNothing
  where
    p :: Proxy c
    p = Proxy

blockEx3B :: forall c. Mock c => Block c
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
    p :: Proxy c
    p = Proxy

utxoEx3B :: Crypto c => UTxO c
utxoEx3B =
  UTxO . Map.fromList $
    [ (TxIn genesisId 1, TxOut Cast.bobAddr bobInitCoin),
      (TxIn (txid txbodyEx3B) 0, TxOut Cast.aliceAddr aliceCoinEx3B)
    ]

ppupEx3B' :: Crypto c => ProposedPPUpdates c
ppupEx3B' =
  ProposedPPUpdates $
    Map.fromList $
      fmap (\n -> (hashKey $ coreNodeVKG n, ppVote3A)) [0, 1, 3, 4, 5]

expectedLSEx3B :: Crypto c => LedgerState c
expectedLSEx3B =
  LedgerState
    ( UTxOState
        utxoEx3B
        (Coin 0)
        (Coin 2)
        (PPUPState ppupEx3B' emptyPPPUpdates)
    )
    (DPState dsEx1 psEx1)

blockEx3BHash :: Mock c => proxy c -> HashHeader c
blockEx3BHash _ = bhHash (bheader blockEx3B)

expectedStEx3B :: forall c. Mock c => ChainState c
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
    (makeEvolvedNonce p (nonce0 p) [blockEx3A, blockEx3B])
    (makeEvolvedNonce p (nonce0 p) [blockEx3A, blockEx3B])
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 2)
          (SlotNo 20)
          (blockEx3BHash p)
    )
  where
    p :: Proxy c
    p = Proxy

ex3B :: Mock c => proxy c -> CHAINExample c
ex3B _ = CHAINExample expectedStEx3A blockEx3B (Right expectedStEx3B)

-- | Example 3C - Vote Late in the epoch
ppVote3C :: PParamsUpdate
ppVote3C =
  PParams
    { _minfeeA = SNothing,
      _minfeeB = SNothing,
      _maxBBSize = SNothing,
      _maxTxSize = SNothing,
      _maxBHSize = SNothing,
      _keyDeposit = SNothing,
      _poolDeposit = SNothing,
      _eMax = SNothing,
      _nOpt = SNothing,
      _a0 = SNothing,
      _rho = SNothing,
      _tau = SNothing,
      _d = SNothing,
      _extraEntropy = SNothing,
      _protocolVersion = SNothing,
      _minUTxOValue = SJust 99,
      _minPoolCost = SNothing
    }

ppupEx3C :: Crypto c => ProposedPPUpdates c
ppupEx3C =
  ProposedPPUpdates $
    Map.fromList
      [ (hashKey $ coreNodeVKG 1, ppVote3C)
      ]

updateEx3C :: Crypto c => Update c
updateEx3C = Update ppupEx3C (EpochNo 1)

aliceCoinEx3C :: Coin
aliceCoinEx3C = aliceCoinEx3B - 1

txbodyEx3C :: Crypto c => TxBody c
txbodyEx3C =
  TxBody
    (Set.fromList [TxIn (txid txbodyEx3B) 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr aliceCoinEx3C)
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 81)
    (SJust updateEx3C)
    SNothing

txEx3C :: Mock c => Tx c
txEx3C =
  Tx
    txbodyEx3C
    mempty
      { addrWits =
          makeWitnessesVKey
            (hashAnnotated txbodyEx3C)
            [asWitness Cast.alicePay, asWitness . cold $ coreNodeKeys p 1]
      }
    SNothing
  where
    p :: Proxy c
    p = Proxy

blockEx3C :: forall c. Mock c => Block c
blockEx3C =
  mkBlock
    (blockEx3BHash p)
    (slotKeys 80)
    [txEx3C]
    (SlotNo 80)
    (BlockNo 3)
    (nonce0 p)
    (NatNonce 3)
    zero
    4
    0
    (mkOCert (slotKeys 80) 0 (KESPeriod 0))
  where
    p :: Proxy c
    p = Proxy

utxoEx3C :: Crypto c => UTxO c
utxoEx3C =
  UTxO . Map.fromList $
    [ (TxIn genesisId 1, TxOut Cast.bobAddr bobInitCoin),
      (TxIn (txid txbodyEx3C) 0, TxOut Cast.aliceAddr aliceCoinEx3C)
    ]

expectedLSEx3C :: Crypto c => LedgerState c
expectedLSEx3C =
  LedgerState
    ( UTxOState
        utxoEx3C
        (Coin 0)
        (Coin 3)
        (PPUPState ppupEx3B' ppupEx3C)
    )
    (DPState dsEx1 psEx1)

blockEx3CHash :: Mock c => proxy c -> HashHeader c
blockEx3CHash _ = bhHash (bheader blockEx3C)

expectedStEx3C :: forall c. Mock c => ChainState c
expectedStEx3C =
  ChainState
    ( NewEpochState
        (EpochNo 0)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        (EpochState (acntEx2A p) emptySnapShots expectedLSEx3C ppsEx1 ppsEx1 emptyNonMyopic)
        (SJust emptyRewardUpdate)
        (PoolDistr Map.empty)
        (overlayScheduleFor (EpochNo 0))
    )
    oCertIssueNosEx1
    (nonce0 p)
    (makeEvolvedNonce p (nonce0 p) [blockEx3A, blockEx3B, blockEx3C])
    (makeEvolvedNonce p (nonce0 p) [blockEx3A, blockEx3B])
    NeutralNonce
    ( At $
        LastAppliedBlock
          (BlockNo 3)
          (SlotNo 80)
          (blockEx3CHash p)
    )
  where
    p :: Proxy c
    p = Proxy

ex3C :: Mock c => proxy c -> CHAINExample c
ex3C _ = CHAINExample expectedStEx3B blockEx3C (Right expectedStEx3C)

-- | Example 3D - Adopt protocol parameter update
-- | And make future updates become the new proposals
blockEx3D :: forall c. Mock c => Block c
blockEx3D =
  mkBlock
    (blockEx3CHash p)
    (slotKeys 110)
    []
    (SlotNo 110)
    (BlockNo 4)
    (makeEvolvedNonce p (nonce0 p) [blockEx3A, blockEx3B] ⭒ mkNonceFromNumber 123)
    (NatNonce 4)
    zero
    5
    0
    (mkOCert (slotKeys 110) 0 (KESPeriod 0))
  where
    p :: Proxy c
    p = Proxy

blockEx3DHash :: Mock c => HashHeader c
blockEx3DHash = bhHash (bheader blockEx3D)

snapsEx3D :: SnapShots h
snapsEx3D = emptySnapShots {_feeSS = Coin 3}

expectedLSEx3D :: Crypto c => LedgerState c
expectedLSEx3D =
  LedgerState
    ( UTxOState
        utxoEx3C
        (Coin 0)
        (Coin 3)
        (PPUPState ppupEx3C emptyPPPUpdates)
    )
    (DPState dsEx1 psEx1)

ppsEx3D :: PParams
ppsEx3D = ppsEx1 {_poolDeposit = Coin 200, _extraEntropy = mkNonceFromNumber 123}

expectedStEx3D :: forall c. Mock c => ChainState c
expectedStEx3D =
  ChainState
    ( NewEpochState
        (EpochNo 1)
        (BlocksMade Map.empty)
        (BlocksMade Map.empty)
        (EpochState (acntEx2A p) snapsEx3D expectedLSEx3D ppsEx1 ppsEx3D emptyNonMyopic)
        SNothing
        (PoolDistr Map.empty)
        (overlayScheduleFor (EpochNo 1))
    )
    oCertIssueNosEx1
    (makeEvolvedNonce p (nonce0 p) [blockEx3A, blockEx3B] ⭒ mkNonceFromNumber 123)
    (makeEvolvedNonce p (nonce0 p) [blockEx3A, blockEx3B, blockEx3C, blockEx3D])
    (makeEvolvedNonce p (nonce0 p) [blockEx3A, blockEx3B, blockEx3C, blockEx3D])
    (hashHeaderToNonce (blockEx3CHash p))
    ( At $
        LastAppliedBlock
          (BlockNo 4)
          (SlotNo 110)
          blockEx3DHash
    )
  where
    p :: Proxy c
    p = Proxy

ex3D :: Mock c => proxy c -> CHAINExample c
ex3D _ = CHAINExample expectedStEx3C blockEx3D (Right expectedStEx3D)

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

txbodyEx4A :: Crypto c => TxBody c
txbodyEx4A =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr aliceCoinEx4A)
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

txEx4A :: forall c. Mock c => Tx c
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

blockEx4A :: forall c. Mock c => Block c
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
    p :: Proxy c
    p = Proxy

blockEx4AHash :: Mock c => HashHeader c
blockEx4AHash = bhHash (bheader blockEx4A)

dsEx4A :: Crypto c => DState c
dsEx4A =
  dsEx1
    { _fGenDelegs =
        Map.singleton
          (FutureGenDeleg (SlotNo 43) (hashKey $ coreNodeVKG 0))
          (GenDelegPair (hashKey . vKey $ newGenDelegate) newGenesisVrfKH)
    }

utxoEx4A :: Crypto c => UTxO c
utxoEx4A =
  UTxO . Map.fromList $
    [ (TxIn genesisId 1, TxOut Cast.bobAddr bobInitCoin),
      (TxIn (txid txbodyEx4A) 0, TxOut Cast.aliceAddr aliceCoinEx4A)
    ]

expectedLSEx4A :: Crypto c => LedgerState c
expectedLSEx4A =
  LedgerState
    ( UTxOState
        utxoEx4A
        (Coin 0)
        (Coin 1)
        emptyPPUPState
    )
    (DPState dsEx4A psEx1)

expectedStEx4A :: forall c. Mock c => ChainState c
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
    (makeEvolvedNonce p (nonce0 p) [blockEx4A])
    (makeEvolvedNonce p (nonce0 p) [blockEx4A])
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

ex4A :: Mock c => proxy c -> CHAINExample c
ex4A _ = CHAINExample initStEx2A blockEx4A (Right expectedStEx4A)

-- | Example 4B - New genesis key delegation updated from future delegations
blockEx4B :: forall c. Mock c => Block c
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
    p :: Proxy c
    p = Proxy

blockEx4BHash :: Mock c => HashHeader c
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

expectedLSEx4B :: Crypto c => LedgerState c
expectedLSEx4B =
  LedgerState
    ( UTxOState
        utxoEx4A
        (Coin 0)
        (Coin 1)
        emptyPPUPState
    )
    (DPState dsEx4B psEx1)

expectedStEx4B :: forall c. Mock c => ChainState c
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
    (makeEvolvedNonce p (nonce0 p) [blockEx4A, blockEx4B])
    (makeEvolvedNonce p (nonce0 p) [blockEx4A, blockEx4B])
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

ex4B :: Mock c => proxy c -> CHAINExample c
ex4B _ = CHAINExample expectedStEx4A blockEx4B (Right expectedStEx4B)

-- | Example 5A - Genesis key delegation
ir :: Crypto c => Map (Credential 'Staking c) Coin
ir = Map.fromList [(Cast.aliceSHK, Coin 100)]

aliceCoinEx5A :: Coin
aliceCoinEx5A = aliceInitCoin - 1

txbodyEx5A :: Crypto c => MIRPot -> TxBody c
txbodyEx5A pot =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr aliceCoinEx5A)
    (StrictSeq.fromList [DCertMir (MIRCert pot ir)])
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 10)
    SNothing
    SNothing

txEx5A :: Mock c => MIRPot -> Tx c
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
  where
    p :: Proxy c
    p = Proxy

blockEx5A :: forall c. Mock c => MIRPot -> Block c
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
    p :: Proxy c
    p = Proxy

blockEx5AHash :: Mock c => MIRPot -> HashHeader c
blockEx5AHash pot = bhHash (bheader $ blockEx5A pot)

utxoEx5A :: Crypto c => MIRPot -> UTxO c
utxoEx5A pot =
  UTxO . Map.fromList $
    [ (TxIn genesisId 1, TxOut Cast.bobAddr bobInitCoin),
      (TxIn (txid $ txbodyEx5A pot) 0, TxOut Cast.aliceAddr aliceCoinEx5A)
    ]

dsEx5A :: Crypto c => MIRPot -> DState c
dsEx5A pot = dsEx1 {_irwd = InstantaneousRewards {iRReserves = r, iRTreasury = t}}
  where
    (r, t) = case pot of
      ReservesMIR -> (Map.fromList [(Cast.aliceSHK, Coin 100)], Map.empty)
      TreasuryMIR -> (Map.empty, Map.fromList [(Cast.aliceSHK, Coin 100)])

expectedLSEx5A :: Crypto c => MIRPot -> LedgerState c
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

setChainStateAccountState :: AccountState -> ChainState h -> ChainState h
setChainStateAccountState as cs = cs {chainNes = (chainNes cs) {nesEs = es'}}
  where
    es' = (nesEs $ chainNes cs) {esAccountState = as}

initStEx5A :: forall c. Crypto c => ChainState c
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

expectedStEx5A :: forall c. Mock c => MIRPot -> ChainState c
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
    (makeEvolvedNonce p (nonce0 p) [blockEx5A pot])
    (makeEvolvedNonce p (nonce0 p) [blockEx5A pot])
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

ex5A :: Mock c => proxy c -> MIRPot -> CHAINExample c
ex5A _ pot = CHAINExample initStEx5A (blockEx5A pot) (Right $ expectedStEx5A pot)

ex5AReserves :: Mock c => proxy c -> CHAINExample c
ex5AReserves p = ex5A p ReservesMIR

ex5ATreasury :: Mock c => proxy c -> CHAINExample c
ex5ATreasury p = ex5A p TreasuryMIR

-- | Example 5B - Instantaneous rewards with insufficient core node signatures
txEx5B :: Mock c => MIRPot -> Tx c
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

blockEx5B :: forall c. Mock c => MIRPot -> Block c
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
    p :: Proxy c
    p = Proxy

mirWitsEx5B :: Crypto c => Set (KeyHash 'Witness c)
mirWitsEx5B = Set.fromList [asWitness . hk . coreNodeKeys p $ i | i <- [0 .. 3]]
  where
    p :: Proxy c
    p = Proxy

expectedStEx5B :: Crypto c => PredicateFailure (CHAIN c)
expectedStEx5B = BbodyFailure (LedgersFailure (LedgerFailure (UtxowFailure $ MIRInsufficientGenesisSigsUTXOW mirWitsEx5B)))

ex5B :: Mock c => proxy c -> MIRPot -> CHAINExample c
ex5B _ pot = CHAINExample initStEx5A (blockEx5B pot) (Left [[expectedStEx5B]])

ex5BReserves :: Mock c => proxy c -> CHAINExample c
ex5BReserves p = ex5B p ReservesMIR

ex5BTreasury :: Mock c => proxy c -> CHAINExample c
ex5BTreasury p = ex5B p TreasuryMIR

-- | Example 5C - Instantaneous rewards that overrun the available reserves
initStEx5C :: Crypto c => ChainState c
initStEx5C =
  setChainStateAccountState
    (AccountState {_treasury = 99, _reserves = 99})
    initStEx2A

ex5C :: Mock c => proxy c -> MIRPot -> CHAINExample c
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

ex5CReserves :: Mock c => proxy c -> CHAINExample c
ex5CReserves p = ex5C p ReservesMIR

ex5CTreasury :: Mock c => proxy c -> CHAINExample c
ex5CTreasury p = ex5C p TreasuryMIR

-- | Example 5D - Apply instantaneous rewards at epoch boundary

-- | The first transaction adds the MIR certificate that transfers a value of
-- 100 to Alice.
aliceCoinEx5D :: Coin
aliceCoinEx5D = aliceInitCoin - (_keyDeposit ppsEx1) - 1

txbodyEx5D :: Crypto c => MIRPot -> TxBody c
txbodyEx5D pot =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr aliceCoinEx5D)
    (StrictSeq.fromList [DCertDeleg (RegKey Cast.aliceSHK), DCertMir (MIRCert pot ir)])
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 99)
    SNothing
    SNothing

txEx5D :: Mock c => MIRPot -> Tx c
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

blockEx5D :: forall c. Mock c => MIRPot -> Block c
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
    p :: Proxy c
    p = Proxy

-- | The second transaction in the next epoch and at least `randomnessStabilisationWindow` slots
-- after the transaction carrying the MIR certificate, then creates the rewards
-- update that contains the transfer of `100` to Alice.
aliceCoinEx5D' :: Coin
aliceCoinEx5D' = aliceCoinEx5D - 1

txbodyEx5D' :: Crypto c => MIRPot -> TxBody c
txbodyEx5D' pot =
  TxBody
    (Set.fromList [TxIn (txid $ txbodyEx5D pot) 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr aliceCoinEx5D')
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    ( (slotFromEpoch $ EpochNo 1)
        +* Duration (randomnessStabilisationWindow testGlobals) + SlotNo 7
    )
    SNothing
    SNothing

txEx5D' :: Mock c => MIRPot -> Tx c
txEx5D' pot =
  Tx
    (txbodyEx5D' pot)
    mempty
      { addrWits = makeWitnessesVKey (hashAnnotated $ txbodyEx5D' pot) [Cast.alicePay]
      }
    SNothing

blockEx5D' :: forall c. Mock c => MIRPot -> Block c
blockEx5D' pot =
  mkBlock
    (bhHash (bheader $ blockEx5D pot))
    (slotKeys s)
    [txEx5D' pot]
    (slot)
    (BlockNo 2)
    (makeEvolvedNonce p (nonce0 p) [blockEx5D pot])
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

txbodyEx5D'' :: Crypto c => MIRPot -> TxBody c
txbodyEx5D'' pot =
  TxBody
    (Set.fromList [TxIn (txid $ txbodyEx5D' pot) 0])
    (StrictSeq.singleton $ TxOut Cast.aliceAddr aliceCoinEx5D'')
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    ((slotFromEpoch $ EpochNo 2) + SlotNo 10)
    SNothing
    SNothing

txEx5D'' :: Mock c => MIRPot -> Tx c
txEx5D'' pot =
  Tx
    (txbodyEx5D'' pot)
    mempty {addrWits = makeWitnessesVKey (hashAnnotated $ txbodyEx5D'' pot) [Cast.alicePay]}
    SNothing

blockEx5D'' :: Mock c => MIRPot -> Nonce -> Block c
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

ex5D' :: forall proxy c. Mock c => proxy c -> MIRPot -> Either [[PredicateFailure (CHAIN c)]] (ChainState c)
ex5D' _p pot = do
  nextState <- runShelleyBase $ applySTSTest @(CHAIN c) (TRC ((), initStEx5A, blockEx5D pot))
  midState <-
    runShelleyBase $
      applySTSTest @(CHAIN c) (TRC ((), nextState, blockEx5D' pot))
  let finalEpochNonce = (chainCandidateNonce midState) ⭒ (chainPrevEpochNonce midState)
  finalState <-
    runShelleyBase $ applySTSTest @(CHAIN c) (TRC ((), midState, blockEx5D'' pot finalEpochNonce))

  pure finalState

ex5DReserves' :: Mock c => proxy c -> Either [[PredicateFailure (CHAIN c)]] (ChainState c)
ex5DReserves' p = ex5D' p ReservesMIR

ex5DTreasury' :: Mock c => proxy c -> Either [[PredicateFailure (CHAIN c)]] (ChainState c)
ex5DTreasury' p = ex5D' p TreasuryMIR

-- | Tests that after getting instantaneous rewards, creating the update and
-- then applying the update, Alice's key is actually registered, the key deposit
-- value deducted and the remaining value credited as reward.
test5D :: Mock c => proxy c -> MIRPot -> Assertion
test5D p pot = do
  case ex5D' p pot of
    Left e -> assertFailure (show e)
    Right ex5DState -> do
      let rews = _rewards . _dstate . _delegationState . esLState . nesEs . chainNes $ ex5DState
          rewEntry = rews Map.!? Cast.aliceSHK
      assertBool "Alice's reward account does not exist" $ isJust rewEntry
      assertBool "Alice's rewards are wrong" $ maybe False (== Coin 100) rewEntry
      assertBool "Total amount of ADA is not preserved" $ maxLLSupply == totalAda ex5DState

test5DReserves :: Mock c => proxy c -> Assertion
test5DReserves p = test5D p ReservesMIR

test5DTreasury :: Mock c => proxy c -> Assertion
test5DTreasury p = test5D p TreasuryMIR

-- * Example 6A - apply CHAIN transition to re-register a stake pool late in the epoch

-- This example continues on from example 2A.

feeEx6A :: Coin
feeEx6A = Coin 3

aliceCoinEx6A :: Coin
aliceCoinEx6A = aliceCoinEx2A - feeEx6A

alicePoolParams6A :: Crypto c => PoolParams c
alicePoolParams6A = Cast.alicePoolParams {_poolCost = Coin 500}

txbodyEx6A :: Crypto c => TxBody c
txbodyEx6A =
  TxBody
    (Set.fromList [TxIn (txid txbodyEx2A) 0])
    (StrictSeq.fromList [TxOut Cast.aliceAddr aliceCoinEx6A])
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

txEx6A :: Mock c => Tx c
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

blockEx6A :: forall c. Mock c => Word64 -> Block c
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
    p :: Proxy c
    p = Proxy

blockEx6AHash :: Mock c => Word64 -> HashHeader c
blockEx6AHash slot = bhHash (bheader $ blockEx6A slot)

psEx6A :: Crypto c => PState c
psEx6A = psEx2A {_fPParams = Map.singleton (hk Cast.alicePoolKeys) alicePoolParams6A}

expectedLSEx6A :: Crypto c => LedgerState c
expectedLSEx6A =
  LedgerState
    ( UTxOState
        ( UTxO . Map.fromList $
            [ (TxIn genesisId 1, TxOut Cast.bobAddr bobInitCoin),
              (TxIn (txid txbodyEx6A) 0, TxOut Cast.aliceAddr aliceCoinEx6A)
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

candidateNonceEx6A :: Mock c => Proxy c -> Nonce
candidateNonceEx6A p = makeEvolvedNonce p (nonce0 p) [blockEx2A, blockEx2B]

candidateNonceEx6A' :: Mock c => Proxy c -> Nonce
candidateNonceEx6A' p = makeEvolvedNonce p (nonce0 p) [blockEx2A]

expectedStEx6A :: forall c. Mock c => Word64 -> StrictMaybe (RewardUpdate c) -> Nonce -> ChainState c
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
    (makeEvolvedNonce p (nonce0 p) [blockEx2A, blockEx2B])
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

ex6A :: Mock c => Proxy c -> CHAINExample c
ex6A p =
  CHAINExample
    expectedStEx2A
    (blockEx6A earlySlotEx6)
    (Right $ expectedStEx6A earlySlotEx6 rewardUpdateEx6A (candidateNonceEx6A p))

ex6A' :: Mock c => Proxy c -> CHAINExample c
ex6A' p =
  CHAINExample
    expectedStEx2A
    (blockEx6A lateSlotEx6)
    (Right $ expectedStEx6A lateSlotEx6 rewardUpdateEx6A' (candidateNonceEx6A' p))

-- * Example 6B - If The TICK rule is applied to the NewEpochState

-- in expectedStEx6A, then the future pool parameters should be adopted

ex6BExpectedNES :: forall c. Mock c => NewEpochState c
ex6BExpectedNES = chainNes (expectedStEx6A earlySlotEx6 rewardUpdateEx6A (candidateNonceEx6A p))
  where
    p :: Proxy c
    p = Proxy

ex6BExpectedNES' :: forall c. Mock c => NewEpochState c
ex6BExpectedNES' = chainNes (expectedStEx6A lateSlotEx6 rewardUpdateEx6A' (candidateNonceEx6A' p))
  where
    p :: Proxy c
    p = Proxy

ex6BPoolParams :: Crypto c => Map (KeyHash 'StakePool c) (PoolParams c)
ex6BPoolParams = Map.singleton (hk Cast.alicePoolKeys) alicePoolParams6A

exampleShelleyGenesis :: forall c. Mock c => ShelleyGenesis c
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
      sgInitialFunds = Map.fromList [(initialFundedAddress, initialFunds)],
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
