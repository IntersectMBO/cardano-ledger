{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : LedgerState
-- Description : Operational Rules
--
-- This module implements the operation rules for treating UTxO transactions ('Tx')
-- as state transformations on a ledger state ('LedgerState'),
-- as specified in /A Simplified Formal Specification of a UTxO Ledger/.
module Shelley.Spec.Ledger.LedgerState
  ( LedgerState (..),
    Ix,
    DPState (..),
    DState (..),
    AccountState (..),
    RewardUpdate (..),
    RewardAccounts,
    InstantaneousRewards (..),
    emptyInstantaneousRewards,
    totalInstantaneousReservesRewards,
    totalInstantaneousTreasuryRewards,
    emptyRewardUpdate,
    FutureGenDeleg (..),
    EpochState (..),
    emptyEpochState,
    emptyLedgerState,
    emptyUTxOState,
    clearPpup,
    PState (..),
    KeyPairs,
    UTxOState (..),
    OBftSlot (..),
    emptyAccount,
    emptyPState,
    emptyDState,

    -- * state transitions
    emptyDelegation,
    applyTxBody,

    -- * Genesis State
    genesisId,
    genesisCoins,
    genesisState,

    -- * Validation
    minfee,
    txsize,
    produced,
    consumed,
    verifiedWits,
    witsVKeyNeeded,
    -- DelegationState
    -- refunds
    keyRefunds,
    keyRefund,
    decayedTx,
    -- epoch boundary
    stakeDistr,
    applyRUpd,
    createRUpd,
    --
    NewEpochState (..),
    NewEpochEnv (..),
    overlaySchedule,
    getGKeys,
    updateNES,
  )
where

import Byron.Spec.Ledger.Core (dom, (∪), (∪+), (⋪), (▷), (◁))
import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    TokenType (TypeNull),
    decodeNull,
    encodeListLen,
    encodeNull,
    enforceSize,
    peekTokenType,
  )
import Cardano.Crypto.Hash (hashWithSerialiser)
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.Monad.Trans.Reader (ReaderT (..), asks)
import qualified Data.ByteString.Lazy as BSL (length)
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.Address (Addr (..))
import Shelley.Spec.Ledger.BaseTypes
  ( Globals (..),
    ShelleyBase,
    StrictMaybe (..),
    UnitInterval,
    activeSlotVal,
    intervalValue,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Delegation.Certificates
  ( DCert (..),
    PoolDistr (..),
    StakeCreds (..),
    StakePools (..),
    decayKey,
    delegCWitness,
    genesisCWitness,
    poolCWitness,
    refund,
    requiresVKeyWitness,
  )
import Shelley.Spec.Ledger.EpochBoundary
  ( BlocksMade (..),
    SnapShot (..),
    SnapShots (..),
    Stake (..),
    aggregateOuts,
    baseStake,
    emptySnapShots,
    ptrStake,
    rewardStake,
  )
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    GenDelegs (..),
    Hash,
    KeyHash,
    KeyHash (..),
    KeyPair,
    KeyRole (..),
    VKey,
    VerKeyVRF,
    asWitness,
    hash,
  )
import Shelley.Spec.Ledger.PParams
  ( PParams,
    ProposedPPUpdates (..),
    Update (..),
    _d,
    _keyDecayRate,
    _keyDeposit,
    _keyMinRefund,
    _minfeeA,
    _minfeeB,
    _rho,
    _tau,
    emptyPPPUpdates,
    emptyPParams,
  )
import Shelley.Spec.Ledger.Rewards
  ( ApparentPerformance (..),
    NonMyopic (..),
    emptyNonMyopic,
    reward,
  )
import Shelley.Spec.Ledger.Serialization (mapFromCBOR, mapToCBOR)
import Shelley.Spec.Ledger.Slot
  ( (+*),
    (-*),
    Duration (..),
    EpochNo (..),
    SlotNo (..),
    epochInfoEpoch,
    epochInfoFirst,
    epochInfoSize,
  )
import Shelley.Spec.Ledger.Tx (Tx (..), extractKeyHash)
import Shelley.Spec.Ledger.TxData
  ( DelegCert (..),
    Ix,
    PoolCert (..),
    PoolParams (..),
    Ptr (..),
    RewardAcnt (..),
    TxBody (..),
    TxId (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
    WitVKey (..),
    getRwdCred,
  )
import Shelley.Spec.Ledger.UTxO
  ( UTxO (..),
    balance,
    totalDeposits,
    txinLookup,
    txins,
    txouts,
    txup,
    verifyWitVKey,
  )

-- | Representation of a list of pairs of key pairs, e.g., pay and stake keys
type KeyPairs crypto = [(KeyPair 'Payment crypto, KeyPair 'Staking crypto)]

type RewardAccounts crypto =
  Map (RewardAcnt crypto) Coin

data FutureGenDeleg crypto = FutureGenDeleg
  { fGenDelegSlot :: !SlotNo,
    fGenDelegGenKeyHash :: !(KeyHash 'Genesis crypto)
  }
  deriving (Show, Eq, Ord, Generic)

instance NoUnexpectedThunks (FutureGenDeleg crypto)

instance Crypto crypto => ToCBOR (FutureGenDeleg crypto) where
  toCBOR (FutureGenDeleg a b) =
    encodeListLen 2 <> toCBOR a <> toCBOR b

instance Crypto crypto => FromCBOR (FutureGenDeleg crypto) where
  fromCBOR = do
    enforceSize "FutureGenDeleg" 2
    a <- fromCBOR
    b <- fromCBOR
    pure $ FutureGenDeleg a b

data InstantaneousRewards crypto = InstantaneousRewards
  { iRReserves :: !(Map (Credential 'Staking crypto) Coin),
    iRTreasury :: !(Map (Credential 'Staking crypto) Coin)
  }
  deriving (Show, Eq, Generic)

totalInstantaneousReservesRewards :: InstantaneousRewards crypto -> Coin
totalInstantaneousReservesRewards (InstantaneousRewards irR _) = sum irR

totalInstantaneousTreasuryRewards :: InstantaneousRewards crypto -> Coin
totalInstantaneousTreasuryRewards (InstantaneousRewards _ irT) = sum irT

instance NoUnexpectedThunks (InstantaneousRewards crypto)

instance Crypto crypto => ToCBOR (InstantaneousRewards crypto) where
  toCBOR (InstantaneousRewards irR irT) =
    encodeListLen 2 <> mapToCBOR irR <> mapToCBOR irT

instance Crypto crypto => FromCBOR (InstantaneousRewards crypto) where
  fromCBOR = do
    enforceSize "InstantaneousRewards" 2
    irR <- mapFromCBOR
    irT <- mapFromCBOR
    pure $ InstantaneousRewards irR irT

-- | State of staking pool delegations and rewards
data DState crypto = DState
  { -- | The active stake keys.
    _stkCreds :: !(StakeCreds crypto),
    -- | The active reward accounts.
    _rewards :: !(RewardAccounts crypto),
    -- | The current delegations.
    _delegations :: !(Map (Credential 'Staking crypto) (KeyHash 'StakePool crypto)),
    -- | The pointed to hash keys.
    _ptrs :: !(Map Ptr (Credential 'Staking crypto)),
    -- | future genesis key delegations
    _fGenDelegs ::
      !( Map
           (FutureGenDeleg crypto)
           (KeyHash 'GenesisDelegate crypto, Hash crypto (VerKeyVRF crypto))
       ),
    -- | Genesis key delegations
    _genDelegs :: !(GenDelegs crypto),
    -- | Instantaneous Rewards
    _irwd :: !(InstantaneousRewards crypto)
  }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (DState crypto)

instance Crypto crypto => ToCBOR (DState crypto) where
  toCBOR (DState sc rw dlg p fgs gs ir) =
    encodeListLen 7 <> toCBOR sc <> toCBOR rw <> toCBOR dlg <> toCBOR p
      <> toCBOR fgs
      <> toCBOR gs
      <> toCBOR ir

instance Crypto crypto => FromCBOR (DState crypto) where
  fromCBOR = do
    enforceSize "DState" 7
    sc <- fromCBOR
    rw <- fromCBOR
    dlg <- fromCBOR
    p <- fromCBOR
    fgs <- fromCBOR
    gs <- fromCBOR
    ir <- fromCBOR
    pure $ DState sc rw dlg p fgs gs ir

-- | Current state of staking pools and their certificate counters.
data PState crypto = PState
  { -- | The active stake pools.
    _stPools :: !(StakePools crypto),
    -- | The pool parameters.
    _pParams :: !(Map (KeyHash 'StakePool crypto) (PoolParams crypto)),
    -- | The future pool parameters.
    _fPParams :: !(Map (KeyHash 'StakePool crypto) (PoolParams crypto)),
    -- | A map of retiring stake pools to the epoch when they retire.
    _retiring :: !(Map (KeyHash 'StakePool crypto) EpochNo)
  }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (PState crypto)

instance Crypto crypto => ToCBOR (PState crypto) where
  toCBOR (PState a b c d) =
    encodeListLen 4 <> toCBOR a <> toCBOR b <> toCBOR c <> toCBOR d

instance Crypto crypto => FromCBOR (PState crypto) where
  fromCBOR = do
    enforceSize "PState" 4
    a <- fromCBOR
    b <- fromCBOR
    c <- fromCBOR
    d <- fromCBOR
    pure $ PState a b c d

-- | The state associated with the current stake delegation.
data DPState crypto = DPState
  { _dstate :: !(DState crypto),
    _pstate :: !(PState crypto)
  }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (DPState crypto)

instance Crypto crypto => ToCBOR (DPState crypto) where
  toCBOR (DPState ds ps) =
    encodeListLen 2 <> toCBOR ds <> toCBOR ps

instance Crypto crypto => FromCBOR (DPState crypto) where
  fromCBOR = do
    enforceSize "DPState" 2
    ds <- fromCBOR
    ps <- fromCBOR
    pure $ DPState ds ps

data RewardUpdate crypto = RewardUpdate
  { deltaT :: !Coin,
    deltaR :: !Coin,
    rs :: !(Map (RewardAcnt crypto) Coin),
    deltaF :: !Coin,
    nonMyopic :: !(NonMyopic crypto)
  }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (RewardUpdate crypto)

instance Crypto crypto => ToCBOR (RewardUpdate crypto) where
  toCBOR (RewardUpdate dt dr rw df nm) =
    encodeListLen 5
      <> toCBOR dt
      <> toCBOR (- dr) -- TODO change Coin serialization to use integers?
      <> toCBOR rw
      <> toCBOR (- df) -- TODO change Coin serialization to use integers?
      <> toCBOR nm

instance Crypto crypto => FromCBOR (RewardUpdate crypto) where
  fromCBOR = do
    enforceSize "RewardUpdate" 5
    dt <- fromCBOR
    dr <- fromCBOR -- TODO change Coin serialization to use integers?
    rw <- fromCBOR
    df <- fromCBOR -- TODO change Coin serialization to use integers?
    nm <- fromCBOR
    pure $ RewardUpdate dt (- dr) rw (- df) nm

emptyRewardUpdate :: RewardUpdate crypto
emptyRewardUpdate = RewardUpdate (Coin 0) (Coin 0) Map.empty (Coin 0) emptyNonMyopic

data AccountState = AccountState
  { _treasury :: !Coin,
    _reserves :: !Coin
  }
  deriving (Show, Eq, Generic)

instance ToCBOR AccountState where
  toCBOR (AccountState t r) =
    encodeListLen 2 <> toCBOR t <> toCBOR r

instance FromCBOR AccountState where
  fromCBOR = do
    enforceSize "AccountState" 2
    t <- fromCBOR
    r <- fromCBOR
    pure $ AccountState t r

instance NoUnexpectedThunks AccountState

data EpochState crypto = EpochState
  { esAccountState :: !AccountState,
    esSnapshots :: !(SnapShots crypto),
    esLState :: !(LedgerState crypto),
    esPrevPp :: !PParams,
    esPp :: !PParams,
    esNonMyopic :: !(NonMyopic crypto)
  }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (EpochState crypto)

instance Crypto crypto => ToCBOR (EpochState crypto) where
  toCBOR (EpochState a s l r p n) =
    encodeListLen 6 <> toCBOR a <> toCBOR s <> toCBOR l <> toCBOR r <> toCBOR p <> toCBOR n

instance Crypto crypto => FromCBOR (EpochState crypto) where
  fromCBOR = do
    enforceSize "EpochState" 6
    a <- fromCBOR
    s <- fromCBOR
    l <- fromCBOR
    r <- fromCBOR
    p <- fromCBOR
    n <- fromCBOR
    pure $ EpochState a s l r p n

emptyUTxOState :: UTxOState crypto
emptyUTxOState = UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyPPPUpdates

emptyEpochState :: EpochState crypto
emptyEpochState =
  EpochState emptyAccount emptySnapShots emptyLedgerState emptyPParams emptyPParams emptyNonMyopic

emptyLedgerState :: LedgerState crypto
emptyLedgerState =
  LedgerState
    emptyUTxOState
    emptyDelegation

emptyAccount :: AccountState
emptyAccount = AccountState (Coin 0) (Coin 0)

emptyDelegation :: DPState crypto
emptyDelegation =
  DPState emptyDState emptyPState

emptyInstantaneousRewards :: InstantaneousRewards crypto
emptyInstantaneousRewards = InstantaneousRewards Map.empty Map.empty

emptyDState :: DState crypto
emptyDState =
  DState
    (StakeCreds Map.empty)
    Map.empty
    Map.empty
    Map.empty
    Map.empty
    (GenDelegs Map.empty)
    emptyInstantaneousRewards

emptyPState :: PState crypto
emptyPState =
  PState (StakePools Map.empty) Map.empty Map.empty Map.empty

-- | Clear the protocol parameter updates
clearPpup ::
  UTxOState crypto ->
  UTxOState crypto
clearPpup utxoSt = utxoSt {_ppups = emptyPPPUpdates}

data UTxOState crypto = UTxOState
  { _utxo :: !(UTxO crypto),
    _deposited :: !Coin,
    _fees :: !Coin,
    _ppups :: !(ProposedPPUpdates crypto)
  }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (UTxOState crypto)

instance Crypto crypto => ToCBOR (UTxOState crypto) where
  toCBOR (UTxOState ut dp fs us) =
    encodeListLen 4 <> toCBOR ut <> toCBOR dp <> toCBOR fs <> toCBOR us

instance Crypto crypto => FromCBOR (UTxOState crypto) where
  fromCBOR = do
    enforceSize "UTxOState" 4
    ut <- fromCBOR
    dp <- fromCBOR
    fs <- fromCBOR
    us <- fromCBOR
    pure $ UTxOState ut dp fs us

data OBftSlot crypto
  = NonActiveSlot
  | ActiveSlot !(KeyHash 'Genesis crypto)
  deriving (Show, Eq, Ord, Generic)

instance
  Crypto crypto =>
  ToCBOR (OBftSlot crypto)
  where
  toCBOR NonActiveSlot = encodeNull
  toCBOR (ActiveSlot k) = toCBOR k

instance
  Crypto crypto =>
  FromCBOR (OBftSlot crypto)
  where
  fromCBOR = do
    peekTokenType >>= \case
      TypeNull -> do
        decodeNull
        pure NonActiveSlot
      _ -> ActiveSlot <$> fromCBOR

instance NoUnexpectedThunks (OBftSlot crypto)

-- | New Epoch state and environment
data NewEpochState crypto = NewEpochState
  { -- | Last epoch
    nesEL :: !EpochNo,
    -- | Blocks made before current epoch
    nesBprev :: !(BlocksMade crypto),
    -- | Blocks made in current epoch
    nesBcur :: !(BlocksMade crypto),
    -- | Epoch state before current
    nesEs :: !(EpochState crypto),
    -- | Possible reward update
    nesRu :: !(StrictMaybe (RewardUpdate crypto)),
    -- | Stake distribution within the stake pool
    nesPd :: !(PoolDistr crypto),
    -- | Overlay schedule for PBFT vs Praos
    nesOsched :: !(Map SlotNo (OBftSlot crypto))
  }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (NewEpochState crypto)

instance Crypto crypto => ToCBOR (NewEpochState crypto) where
  toCBOR (NewEpochState e bp bc es ru pd os) =
    encodeListLen 7 <> toCBOR e <> toCBOR bp <> toCBOR bc <> toCBOR es
      <> toCBOR ru
      <> toCBOR pd
      <> toCBOR (compactOverlaySchedule os)

instance Crypto crypto => FromCBOR (NewEpochState crypto) where
  fromCBOR = do
    enforceSize "NewEpochState" 7
    e <- fromCBOR
    bp <- fromCBOR
    bc <- fromCBOR
    es <- fromCBOR
    ru <- fromCBOR
    pd <- fromCBOR
    os <- decompactOverlaySchedule <$> fromCBOR
    pure $ NewEpochState e bp bc es ru pd os

-- | Convert the overlay schedule to a representation that is more compact
-- when serialised to a bytestring, but less efficient for lookups.
--
-- Each genesis key hash will only be stored once, instead of each time it is
-- assigned to a slot.
compactOverlaySchedule ::
  Map SlotNo (OBftSlot crypto) ->
  Map (OBftSlot crypto) (NonEmpty SlotNo)
compactOverlaySchedule =
  Map.foldrWithKey'
    ( \slot obftSlot ->
        Map.insertWith (<>) obftSlot (pure slot)
    )
    Map.empty

-- | Inverse of 'compactOverlaySchedule'
decompactOverlaySchedule ::
  Map (OBftSlot crypto) (NonEmpty SlotNo) ->
  Map SlotNo (OBftSlot crypto)
decompactOverlaySchedule compact =
  Map.fromList
    [ (slot, obftSlot)
      | (obftSlot, slots) <- Map.toList compact,
        slot <- NonEmpty.toList slots
    ]

getGKeys ::
  NewEpochState crypto ->
  Set (KeyHash 'Genesis crypto)
getGKeys nes = Map.keysSet genDelegs
  where
    NewEpochState _ _ _ es _ _ _ = nes
    EpochState _ _ ls _ _ _ = es
    LedgerState _ (DPState (DState _ _ _ _ _ (GenDelegs genDelegs) _) _) = ls

data NewEpochEnv crypto = NewEpochEnv
  { neeS :: SlotNo,
    neeGkeys :: Set (KeyHash 'Genesis crypto)
  }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (NewEpochEnv crypto)

-- | The state associated with a 'Ledger'.
data LedgerState crypto = LedgerState
  { -- | The current unspent transaction outputs.
    _utxoState :: !(UTxOState crypto),
    -- | The current delegation state
    _delegationState :: !(DPState crypto)
  }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (LedgerState crypto)

instance Crypto crypto => ToCBOR (LedgerState crypto) where
  toCBOR (LedgerState u dp) =
    encodeListLen 2 <> toCBOR u <> toCBOR dp

instance Crypto crypto => FromCBOR (LedgerState crypto) where
  fromCBOR = do
    enforceSize "LedgerState" 2
    u <- fromCBOR
    dp <- fromCBOR
    pure $ LedgerState u dp

-- | The transaction Id for 'UTxO' included at the beginning of a new ledger.
genesisId ::
  (Crypto crypto) =>
  TxId crypto
genesisId =
  TxId $
    hash
      ( TxBody
          Set.empty
          StrictSeq.Empty
          StrictSeq.Empty
          (Wdrl Map.empty)
          (Coin 0)
          (SlotNo 0)
          SNothing
          SNothing
      )

-- | Creates the UTxO for a new ledger with the specified transaction outputs.
genesisCoins ::
  (Crypto crypto) =>
  [TxOut crypto] ->
  UTxO crypto
genesisCoins outs =
  UTxO $
    Map.fromList [(TxIn genesisId idx, out) | (idx, out) <- zip [0 ..] outs]

-- | Creates the ledger state for an empty ledger which
--  contains the specified transaction outputs.
genesisState ::
  Map
    (KeyHash 'Genesis crypto)
    (KeyHash 'GenesisDelegate crypto, Hash crypto (VerKeyVRF crypto)) ->
  UTxO crypto ->
  LedgerState crypto
genesisState genDelegs0 utxo0 =
  LedgerState
    ( UTxOState
        utxo0
        (Coin 0)
        (Coin 0)
        emptyPPPUpdates
    )
    (DPState dState emptyPState)
  where
    dState = emptyDState {_genDelegs = GenDelegs genDelegs0}

-- | Implementation of abstract transaction size
txsize :: forall crypto. (Crypto crypto) => Tx crypto -> Integer
txsize tx = numInputs * inputSize + numOutputs * outputSize + rest
  where
    uint = 5
    smallArray = 1
    hashLen = 32
    hashObj = 2 + hashLen
    addrHashLen = 28
    addrHeader = 1
    address = 2 + addrHeader + 2 * addrHashLen
    txbody = _body tx
    numInputs = toInteger . length . _inputs $ txbody
    inputSize = smallArray + uint + hashObj
    numOutputs = toInteger . length . _outputs $ txbody
    outputSize = smallArray + uint + address
    rest = fromIntegral $ BSL.length (txFullBytes tx) - extraSize txbody

-- | Minimum fee calculation
minfee :: forall crypto. (Crypto crypto) => PParams -> Tx crypto -> Coin
minfee pp tx = Coin $ fromIntegral (_minfeeA pp) * txsize tx + fromIntegral (_minfeeB pp)

-- | Compute the lovelace which are created by the transaction
produced ::
  (Crypto crypto) =>
  PParams ->
  StakePools crypto ->
  TxBody crypto ->
  Coin
produced pp stakePools tx =
  balance (txouts tx) + _txfee tx + totalDeposits pp stakePools (toList $ _certs tx)

-- | Compute the key deregistration refunds in a transaction
keyRefunds ::
  Crypto crypto =>
  PParams ->
  StakeCreds crypto ->
  TxBody crypto ->
  Coin
keyRefunds pp stk tx =
  sum [keyRefund dval dmin lambda stk (_ttl tx) c | c@(DCertDeleg (DeRegKey _)) <- toList $ _certs tx]
  where
    (dval, dmin, lambda) = decayKey pp

-- | Key refund for a deregistration certificate.
keyRefund ::
  Coin ->
  UnitInterval ->
  Rational ->
  StakeCreds crypto ->
  SlotNo ->
  DCert crypto ->
  Coin
keyRefund dval dmin lambda (StakeCreds stkcreds) slot c =
  case c of
    DCertDeleg (DeRegKey key) -> case Map.lookup key stkcreds of
      Nothing -> Coin 0
      Just s -> refund dval dmin lambda $ slot -* s
    _ -> Coin 0

-- | Functions to calculate decayed deposits
decayedKey ::
  PParams ->
  StakeCreds crypto ->
  SlotNo ->
  DCert crypto ->
  ShelleyBase Coin
decayedKey pp stk@(StakeCreds stkcreds) cslot cert =
  case cert of
    DCertDeleg (DeRegKey key) ->
      case Map.lookup key stkcreds of
        Nothing -> pure 0
        Just created' -> do
          start <- do
            ei <- asks epochInfo
            fs <- epochInfoFirst ei =<< epochInfoEpoch ei cslot
            pure $ max fs created'
          let dval = _keyDeposit pp
              dmin = _keyMinRefund pp
              lambda = _keyDecayRate pp
              epochRefund = keyRefund dval dmin lambda stk start cert
              currentRefund = keyRefund dval dmin lambda stk cslot cert
          pure $ epochRefund - currentRefund
    _ -> pure 0

-- | Decayed deposit portions
decayedTx ::
  Crypto crypto =>
  PParams ->
  StakeCreds crypto ->
  TxBody crypto ->
  ShelleyBase Coin
decayedTx pp stk tx =
  lsum [decayedKey pp stk (_ttl tx) c | c@(DCertDeleg (DeRegKey _)) <- toList $ _certs tx]
  where
    lsum xs = ReaderT $ \s -> sum $ fmap (flip runReaderT s) xs

-- | Compute the lovelace which are destroyed by the transaction
consumed ::
  Crypto crypto =>
  PParams ->
  UTxO crypto ->
  StakeCreds crypto ->
  TxBody crypto ->
  Coin
consumed pp u stakeKeys tx =
  balance (txins tx ◁ u) + refunds + withdrawals
  where
    refunds = keyRefunds pp stakeKeys tx
    withdrawals = sum . unWdrl $ _wdrls tx

-- | Collect the set of hashes of keys that needs to sign a
--  given transaction. This set consists of the txin owners,
--  certificate authors, and withdrawal reward accounts.
witsVKeyNeeded ::
  forall crypto.
  Crypto crypto =>
  UTxO crypto ->
  Tx crypto ->
  GenDelegs crypto ->
  Set (KeyHash 'Witness crypto)
witsVKeyNeeded utxo' tx@(Tx txbody _ _ _) _genDelegs =
  inputAuthors
    `Set.union` wdrlAuthors
    `Set.union` certAuthors
    `Set.union` updateKeys
    `Set.union` owners
  where
    inputAuthors = asWitness `Set.map` Set.foldr insertHK Set.empty (_inputs txbody)
    unspendableKeyHash = KeyHash (coerce (hash 0 :: Hash crypto Int))
    insertHK txin hkeys =
      case txinLookup txin utxo' of
        Just (TxOut (Addr _ (KeyHashObj pay) _) _) -> Set.insert pay hkeys
        Just (TxOut (AddrBootstrap _) _) -> Set.insert unspendableKeyHash hkeys
        -- NOTE: Until Byron addresses are supported, we insert an unspendible keyhash
        _ -> hkeys
    wdrlAuthors =
      Set.map asWitness
        . Set.fromList
        . extractKeyHash
        $ map getRwdCred (Map.keys (unWdrl $ _wdrls txbody))
    owners =
      foldl'
        Set.union
        Set.empty
        [ ((Set.map asWitness) . _poolOwners) pool
          | DCertPool (RegPool pool) <- toList $ _certs txbody
        ]
    certAuthors = Set.map asWitness . Set.fromList $ foldl (++) [] $ fmap getCertHK certificates
    getCertHK = cwitness
    -- key reg requires no witness but this is already filtered out before the
    -- call to `cwitness`
    cwitness (DCertDeleg dc) = asWitness <$> extractKeyHash [delegCWitness dc]
    cwitness (DCertPool pc) = asWitness <$> extractKeyHash [poolCWitness pc]
    cwitness (DCertGenesis gc) = [asWitness $ genesisCWitness gc]
    cwitness c = error $ show c ++ " does not have a witness"
    certificates = filter requiresVKeyWitness (toList $ _certs txbody)
    updateKeys = asWitness `Set.map` propWits (txup tx) _genDelegs

-- | Given a ledger state, determine if the UTxO witnesses in a given
--  transaction are correct.
verifiedWits ::
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Tx crypto ->
  Either [VKey 'Witness crypto] ()
verifiedWits (Tx txbody wits _ _) =
  case failed == mempty of
    True -> Right ()
    False -> Left $ fmap (\(WitVKey vk _) -> vk) failed
  where
    failed = filter (not . verifyWitVKey (hashWithSerialiser toCBOR txbody)) (Set.toList wits)

-- | Calculate the set of hash keys of the required witnesses for update
-- proposals.
propWits ::
  Maybe (Update crypto) ->
  GenDelegs crypto ->
  Set (KeyHash 'Witness crypto)
propWits Nothing _ = Set.empty
propWits (Just (Update (ProposedPPUpdates pup) _)) (GenDelegs genDelegs) =
  Set.map asWitness . Set.fromList $ Map.elems updateKeys
  where
    updateKeys' = Map.keysSet pup ◁ genDelegs
    updateKeys = Map.map fst updateKeys'

-- Functions for stake delegation model

-- | Calculate the change to the deposit pool for a given transaction.
depositPoolChange ::
  Crypto crypto =>
  LedgerState crypto ->
  PParams ->
  TxBody crypto ->
  Coin
depositPoolChange ls pp tx = (currentPool + txDeposits) - txRefunds
  where
    -- Note that while (currentPool + txDeposits) >= txRefunds,
    -- it could be that txDeposits < txRefunds. We keep the parenthesis above
    -- to emphasize this point.

    currentPool = (_deposited . _utxoState) ls
    txDeposits =
      totalDeposits pp ((_stPools . _pstate . _delegationState) ls) (toList $ _certs tx)
    txRefunds = keyRefunds pp ((_stkCreds . _dstate . _delegationState) ls) tx

-- | Apply a transaction body as a state transition function on the ledger state.
applyTxBody ::
  (Crypto crypto) =>
  LedgerState crypto ->
  PParams ->
  TxBody crypto ->
  LedgerState crypto
applyTxBody ls pp tx =
  ls
    { _utxoState =
        us
          { _utxo = txins tx ⋪ (_utxo us) ∪ txouts tx,
            _deposited = depositPoolChange ls pp tx,
            _fees = (_txfee tx) + (_fees . _utxoState $ ls)
          },
      _delegationState =
        dels
          { _dstate = dst {_rewards = newAccounts}
          }
    }
  where
    dels = _delegationState ls
    dst = _dstate dels
    us = _utxoState ls
    newAccounts =
      reapRewards
        ((_rewards . _dstate . _delegationState) ls)
        (unWdrl $ _wdrls tx)

reapRewards ::
  RewardAccounts crypto ->
  RewardAccounts crypto ->
  RewardAccounts crypto
reapRewards dStateRewards withdrawals =
  Map.mapWithKey removeRewards dStateRewards
  where
    removeRewards k v = if k `Map.member` withdrawals then Coin 0 else v

---------------------------------
-- epoch boundary calculations --
---------------------------------

-- | Stake distribution
stakeDistr ::
  forall crypto.
  UTxO crypto ->
  DState crypto ->
  PState crypto ->
  SnapShot crypto
stakeDistr u ds ps =
  SnapShot
    (Stake $ dom activeDelegs ◁ aggregatePlus stakeRelation)
    delegs
    poolParams
  where
    DState (StakeCreds stkcreds) rewards' delegs ptrs' _ _ _ = ds
    PState (StakePools stpools) poolParams _ _ = ps
    outs = aggregateOuts u
    stakeRelation :: [(Credential 'Staking crypto, Coin)]
    stakeRelation = baseStake outs ∪ ptrStake outs ptrs' ∪ rewardStake rewards'
    activeDelegs = dom stkcreds ◁ delegs ▷ dom stpools
    aggregatePlus = Map.fromListWith (+)

-- | Apply a reward update
applyRUpd ::
  RewardUpdate crypto ->
  EpochState crypto ->
  EpochState crypto
applyRUpd ru (EpochState as ss ls pr pp nm) = EpochState as' ss ls' pr pp nm
  where
    utxoState_ = _utxoState ls
    delegState = _delegationState ls
    dState = _dstate delegState
    as' =
      as
        { _treasury = _treasury as + deltaT ru,
          _reserves = _reserves as + deltaR ru
        }
    ls' =
      ls
        { _utxoState =
            utxoState_ {_fees = _fees utxoState_ + deltaF ru},
          _delegationState =
            delegState
              { _dstate =
                  dState
                    { _rewards = _rewards dState ∪+ rs ru
                    }
              }
        }

updateNonMypopic ::
  NonMyopic crypto ->
  Coin ->
  Map (KeyHash 'StakePool crypto) Rational ->
  SnapShot crypto ->
  NonMyopic crypto
updateNonMypopic nm rPot aps ss =
  nm
    { apparentPerformances = aps',
      rewardPot = rPot,
      snap = ss
    }
  where
    SnapShot _ _ poolParams = ss
    absentPools =
      Set.toList $
        (Map.keysSet poolParams) `Set.difference` (Map.keysSet aps)
    performanceZero = Map.fromList $ fmap (\p -> (p, 0)) absentPools
    -- TODO how to handle pools with near zero stake?

    expMovAvgWeight = 0.5 -- TODO move to globals or protocol parameters?
    prev = apparentPerformances nm
    performance kh ap = case Map.lookup kh prev of
      Nothing -> ApparentPerformance $ fromRational ap -- TODO give new pools the average performance?
      Just (ApparentPerformance p) ->
        ApparentPerformance $
          expMovAvgWeight * p + (1 - expMovAvgWeight) * (fromRational ap)
    aps' = Map.mapWithKey performance (aps `Map.union` performanceZero)

-- | Create a reward update
createRUpd ::
  EpochNo ->
  BlocksMade crypto ->
  EpochState crypto ->
  Coin ->
  ShelleyBase (RewardUpdate crypto)
createRUpd e b@(BlocksMade b') (EpochState acnt ss ls pr _ nm) total = do
  ei <- asks epochInfo
  slotsPerEpoch <- epochInfoSize ei e
  asc <- asks activeSlotCoeff
  network <- asks networkId
  let SnapShot stake' delegs' poolParams = _pstakeGo ss
      Coin reserves = _reserves acnt
      ds = _dstate $ _delegationState ls
      -- reserves and rewards change
      deltaR_ = (floor $ min 1 eta * intervalValue (_rho pr) * fromIntegral reserves)
      expectedBlocks =
        intervalValue (activeSlotVal asc) * fromIntegral slotsPerEpoch
      eta = fromIntegral blocksMade / expectedBlocks
      Coin rPot = _feeSS ss + deltaR_
      deltaT1 = floor $ intervalValue (_tau pr) * fromIntegral rPot
      _R = Coin $ rPot - deltaT1
      circulation = total - (_reserves acnt)
      (rs_, aps) =
        reward network pr b _R (Map.keysSet $ _rewards ds) poolParams stake' delegs' circulation
      deltaT2 = _R - (Map.foldr (+) (Coin 0) rs_)
      blocksMade = fromIntegral $ Map.foldr (+) 0 b' :: Integer
  pure $
    RewardUpdate
      (Coin deltaT1 + deltaT2)
      (- deltaR_)
      rs_
      (- (_feeSS ss))
      (updateNonMypopic nm _R aps (_pstakeGo ss))

-- | Overlay schedule
-- This is just a very simple round-robin, evenly spaced schedule.
overlaySchedule ::
  EpochNo ->
  Set (KeyHash 'Genesis crypto) ->
  PParams ->
  ShelleyBase (Map SlotNo (OBftSlot crypto))
overlaySchedule e gkeys pp = do
  let dval = intervalValue $ _d pp
  if dval == 0
    then pure Map.empty
    else do
      ei <- asks epochInfo
      slotsPerEpoch <- epochInfoSize ei e
      firstSlotNo <- epochInfoFirst ei e
      asc <- asks activeSlotCoeff
      let numActive = dval * fromIntegral slotsPerEpoch
          dInv = 1 / dval
          ascValue = (intervalValue . activeSlotVal) asc
          toRelativeSlotNo x = (Duration . floor) (dInv * fromInteger x)
          toSlotNo x = firstSlotNo +* toRelativeSlotNo x
          genesisSlots = [toSlotNo x | x <- [0 .. (floor numActive - 1)]]
          numInactivePerActive = floor (1 / ascValue) - 1
          activitySchedule = cycle (True : replicate numInactivePerActive False)
          unassignedSched = zip activitySchedule genesisSlots
          genesisCycle = if Set.null gkeys then [] else cycle (Set.toList gkeys)
          active =
            Map.fromList $
              fmap
                (\(gk, (_, s)) -> (s, ActiveSlot gk))
                (zip genesisCycle (filter fst unassignedSched))
          inactive =
            Map.fromList $
              fmap
                (\x -> (snd x, NonActiveSlot))
                (filter (not . fst) unassignedSched)
      pure $ Map.union active inactive

-- | Update new epoch state
updateNES ::
  NewEpochState crypto ->
  BlocksMade crypto ->
  LedgerState crypto ->
  NewEpochState crypto
updateNES
  ( NewEpochState
      eL
      bprev
      _
      (EpochState acnt ss _ pr pp nm)
      ru
      pd
      osched
    )
  bcur
  ls =
    NewEpochState eL bprev bcur (EpochState acnt ss ls pr pp nm) ru pd osched
