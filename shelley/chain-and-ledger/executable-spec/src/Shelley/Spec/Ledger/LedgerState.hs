{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
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
  ( AccountState (..),
    DPState (..),
    DState (..),
    EpochState (..),
    FutureGenDeleg (..),
    InstantaneousRewards (..),
    Ix,
    KeyPairs,
    LedgerState (..),
    OBftSlot (..),
    PPUPState (..),
    PState (..),
    RewardAccounts,
    RewardUpdate (..),
    UTxOState (..),
    depositPoolChange,
    emptyAccount,
    emptyDPState,
    emptyDState,
    emptyEpochState,
    emptyInstantaneousRewards,
    emptyLedgerState,
    emptyPPUPState,
    emptyPState,
    emptyRewardUpdate,
    emptyUTxOState,
    pvCanFollow,
    reapRewards,
    totalInstantaneousReservesRewards,
    totalInstantaneousTreasuryRewards,
    updatePpup,

    -- * state transitions
    emptyDelegation,

    -- * Genesis State
    genesisState,

    -- * Validation
    WitHashes (..),
    nullWitHashes,
    diffWitHashes,
    minfee,
    minfeeBound,
    txsize,
    txsizeBound,
    produced,
    consumed,
    verifiedWits,
    witsVKeyNeeded,
    witsFromWitnessSet,

    -- * DelegationState
    keyRefunds,

    -- * Epoch boundary
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

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    TokenType (TypeNull),
    decodeNull,
    encodeListLen,
    encodeNull,
    peekTokenType,
  )
import Cardano.Prelude (NFData, NoUnexpectedThunks (..))
import Control.Iterate.SetAlgebra (Bimap, biMapEmpty, dom, eval, forwards, range, (∈), (∪+), (▷), (◁))
import Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Lazy as BSL (length)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Quiet
import Shelley.Spec.Ledger.Address (Addr (..), bootstrapKeyHash)
import Shelley.Spec.Ledger.Address.Bootstrap
  ( BootstrapWitness (..),
    bootstrapWitKeyHash,
    verifyBootstrapWit,
  )
import Shelley.Spec.Ledger.BaseTypes
  ( Globals (..),
    ShelleyBase,
    StrictMaybe (..),
    activeSlotVal,
    intervalValue,
    unitIntervalToRational,
  )
import Shelley.Spec.Ledger.Coin (Coin (..), rationalToCoinViaFloor)
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Delegation.Certificates
  ( DCert (..),
    PoolDistr (..),
    delegCWitness,
    genesisCWitness,
    isDeRegKey,
    poolCWitness,
    requiresVKeyWitness,
  )
import Shelley.Spec.Ledger.EpochBoundary
  ( BlocksMade (..),
    SnapShot (..),
    SnapShots (..),
    Stake (..),
    aggregateUtxoCoinByCredential,
    emptySnapShots,
  )
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    GenDelegPair (..),
    GenDelegs (..),
    Hash,
    KeyHash (..),
    KeyPair,
    KeyRole (..),
    VKey,
    asWitness,
  )
import Shelley.Spec.Ledger.PParams
  ( PParams,
    PParams' (..),
    ProposedPPUpdates (..),
    ProtVer (..),
    Update (..),
    emptyPPPUpdates,
    emptyPParams,
  )
import Shelley.Spec.Ledger.Rewards
  ( Likelihood,
    NonMyopic (..),
    emptyNonMyopic,
    reward,
  )
import Shelley.Spec.Ledger.Serialization (decodeRecordNamed, mapFromCBOR, mapToCBOR)
import Shelley.Spec.Ledger.Slot
  ( Duration (..),
    EpochNo (..),
    SlotNo (..),
    epochInfoFirst,
    epochInfoSize,
    (+*),
  )
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    WitnessSet,
    WitnessSetHKD (..),
    addrWits,
    extractKeyHashWitnessSet,
  )
import Shelley.Spec.Ledger.TxData
  ( Ix,
    PoolCert (..),
    PoolParams (..),
    Ptr (..),
    RewardAcnt (..),
    TxBody (..),
    TxOut (..),
    Wdrl (..),
    WitVKey (..),
    getRwdCred,
    witKeyHash,
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
  Map (Credential 'Staking crypto) Coin

data FutureGenDeleg crypto = FutureGenDeleg
  { fGenDelegSlot :: !SlotNo,
    fGenDelegGenKeyHash :: !(KeyHash 'Genesis crypto)
  }
  deriving (Show, Eq, Ord, Generic)

instance NoUnexpectedThunks (FutureGenDeleg crypto)

instance NFData (FutureGenDeleg crypto)

instance Crypto crypto => ToCBOR (FutureGenDeleg crypto) where
  toCBOR (FutureGenDeleg a b) =
    encodeListLen 2 <> toCBOR a <> toCBOR b

instance Crypto crypto => FromCBOR (FutureGenDeleg crypto) where
  fromCBOR = do
    decodeRecordNamed "FutureGenDeleg" (const 2) $ do
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

instance NFData (InstantaneousRewards crypto)

instance Crypto crypto => ToCBOR (InstantaneousRewards crypto) where
  toCBOR (InstantaneousRewards irR irT) =
    encodeListLen 2 <> mapToCBOR irR <> mapToCBOR irT

instance Crypto crypto => FromCBOR (InstantaneousRewards crypto) where
  fromCBOR = do
    decodeRecordNamed "InstantaneousRewards" (const 2) $ do
      irR <- mapFromCBOR
      irT <- mapFromCBOR
      pure $ InstantaneousRewards irR irT

-- | State of staking pool delegations and rewards
data DState crypto = DState
  { -- | The active reward accounts.
    _rewards :: !(RewardAccounts crypto),
    -- | The current delegations.
    _delegations :: !(Map (Credential 'Staking crypto) (KeyHash 'StakePool crypto)),
    -- | The pointed to hash keys.
    _ptrs :: !(Bimap Ptr (Credential 'Staking crypto)),
    -- | future genesis key delegations
    _fGenDelegs :: !(Map (FutureGenDeleg crypto) (GenDelegPair crypto)),
    -- | Genesis key delegations
    _genDelegs :: !(GenDelegs crypto),
    -- | Instantaneous Rewards
    _irwd :: !(InstantaneousRewards crypto)
  }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (DState crypto)

instance NFData (DState crypto)

instance Crypto crypto => ToCBOR (DState crypto) where
  toCBOR (DState rw dlg p fgs gs ir) =
    encodeListLen 6
      <> toCBOR rw
      <> toCBOR dlg
      <> toCBOR p
      <> toCBOR fgs
      <> toCBOR gs
      <> toCBOR ir

instance Crypto crypto => FromCBOR (DState crypto) where
  fromCBOR = do
    decodeRecordNamed "DState" (const 6) $ do
      rw <- fromCBOR
      dlg <- fromCBOR
      p <- fromCBOR
      fgs <- fromCBOR
      gs <- fromCBOR
      ir <- fromCBOR
      pure $ DState rw dlg p fgs gs ir

-- | Current state of staking pools and their certificate counters.
data PState crypto = PState
  { -- | The pool parameters.
    _pParams :: !(Map (KeyHash 'StakePool crypto) (PoolParams crypto)),
    -- | The future pool parameters.
    _fPParams :: !(Map (KeyHash 'StakePool crypto) (PoolParams crypto)),
    -- | A map of retiring stake pools to the epoch when they retire.
    _retiring :: !(Map (KeyHash 'StakePool crypto) EpochNo)
  }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (PState crypto)

instance NFData (PState crypto)

instance Crypto crypto => ToCBOR (PState crypto) where
  toCBOR (PState a b c) =
    encodeListLen 3 <> toCBOR a <> toCBOR b <> toCBOR c

instance Crypto crypto => FromCBOR (PState crypto) where
  fromCBOR = do
    decodeRecordNamed "PState" (const 3) $ do
      a <- fromCBOR
      b <- fromCBOR
      c <- fromCBOR
      pure $ PState a b c

-- | The state associated with the current stake delegation.
data DPState crypto = DPState
  { _dstate :: !(DState crypto),
    _pstate :: !(PState crypto)
  }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (DPState crypto)

instance NFData (DPState crypto)

instance Crypto crypto => ToCBOR (DPState crypto) where
  toCBOR (DPState ds ps) =
    encodeListLen 2 <> toCBOR ds <> toCBOR ps

instance Crypto crypto => FromCBOR (DPState crypto) where
  fromCBOR = do
    decodeRecordNamed "DPState" (const 2) $ do
      ds <- fromCBOR
      ps <- fromCBOR
      pure $ DPState ds ps

data RewardUpdate crypto = RewardUpdate
  { deltaT :: !Coin,
    deltaR :: !Coin,
    rs :: !(Map (Credential 'Staking crypto) Coin),
    deltaF :: !Coin,
    nonMyopic :: !(NonMyopic crypto)
  }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (RewardUpdate crypto)

instance NFData (RewardUpdate crypto)

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
    decodeRecordNamed "RewardUpdate" (const 5) $ do
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
    decodeRecordNamed "AccountState" (const 2) $ do
      t <- fromCBOR
      r <- fromCBOR
      pure $ AccountState t r

instance NoUnexpectedThunks AccountState

instance NFData AccountState

data EpochState crypto = EpochState
  { esAccountState :: !AccountState,
    esSnapshots :: !(SnapShots crypto),
    esLState :: !(LedgerState crypto),
    esPrevPp :: !PParams,
    esPp :: !PParams,
    esNonMyopic :: !(NonMyopic crypto) -- TODO document this in the formal spec, see github #1319
  }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (EpochState crypto)

instance NFData (EpochState crypto)

instance Crypto crypto => ToCBOR (EpochState crypto) where
  toCBOR (EpochState a s l r p n) =
    encodeListLen 6 <> toCBOR a <> toCBOR s <> toCBOR l <> toCBOR r <> toCBOR p <> toCBOR n

instance Crypto crypto => FromCBOR (EpochState crypto) where
  fromCBOR = do
    decodeRecordNamed "EpochState" (const 6) $ do
      a <- fromCBOR
      s <- fromCBOR
      l <- fromCBOR
      r <- fromCBOR
      p <- fromCBOR
      n <- fromCBOR
      pure $ EpochState a s l r p n

emptyPPUPState :: PPUPState crypto
emptyPPUPState = PPUPState emptyPPPUpdates emptyPPPUpdates

emptyUTxOState :: UTxOState crypto
emptyUTxOState = UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyPPUPState

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
    Map.empty
    Map.empty
    biMapEmpty
    Map.empty
    (GenDelegs Map.empty)
    emptyInstantaneousRewards

emptyPState :: PState crypto
emptyPState =
  PState Map.empty Map.empty Map.empty

emptyDPState :: DPState crypto
emptyDPState = DPState emptyDState emptyPState

data PPUPState crypto = PPUPState
  { proposals :: ProposedPPUpdates crypto,
    futureProposals :: ProposedPPUpdates crypto
  }
  deriving (Show, Eq, Generic, NFData, NoUnexpectedThunks)

instance Crypto crypto => ToCBOR (PPUPState crypto) where
  toCBOR (PPUPState ppup fppup) =
    encodeListLen 2 <> toCBOR ppup <> toCBOR fppup

instance Crypto crypto => FromCBOR (PPUPState crypto) where
  fromCBOR = do
    decodeRecordNamed "PPUPState" (const 2) $ do
      ppup <- fromCBOR
      fppup <- fromCBOR
      pure $ PPUPState ppup fppup

pvCanFollow :: ProtVer -> StrictMaybe ProtVer -> Bool
pvCanFollow _ SNothing = True
pvCanFollow (ProtVer m n) (SJust (ProtVer m' n')) =
  (m + 1, 0) == (m', n') || (m, n + 1) == (m', n')

-- | Update the protocol parameter updates by clearing out the proposals
-- and making the future proposals become the new proposals,
-- provided the new proposals can follow (otherwise reset them).
updatePpup :: UTxOState crypto -> PParams -> UTxOState crypto
updatePpup utxoSt pp = utxoSt {_ppups = PPUPState ps emptyPPPUpdates}
  where
    (ProposedPPUpdates newProposals) = futureProposals . _ppups $ utxoSt
    goodPV = pvCanFollow (_protocolVersion pp) . _protocolVersion
    ps = if all goodPV newProposals then ProposedPPUpdates newProposals else emptyPPPUpdates

data UTxOState crypto = UTxOState
  { _utxo :: !(UTxO crypto),
    _deposited :: !Coin,
    _fees :: !Coin,
    _ppups :: !(PPUPState crypto)
  }
  deriving (Show, Eq, Generic, NFData)

instance NoUnexpectedThunks (UTxOState crypto)

instance Crypto crypto => ToCBOR (UTxOState crypto) where
  toCBOR (UTxOState ut dp fs us) =
    encodeListLen 4 <> toCBOR ut <> toCBOR dp <> toCBOR fs <> toCBOR us

instance Crypto crypto => FromCBOR (UTxOState crypto) where
  fromCBOR = do
    decodeRecordNamed "UTxOState" (const 4) $ do
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

instance NFData (OBftSlot crypto)

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

instance NFData (NewEpochState crypto)

instance NoUnexpectedThunks (NewEpochState crypto)

instance Crypto crypto => ToCBOR (NewEpochState crypto) where
  toCBOR (NewEpochState e bp bc es ru pd os) =
    encodeListLen 7 <> toCBOR e <> toCBOR bp <> toCBOR bc <> toCBOR es
      <> toCBOR ru
      <> toCBOR pd
      <> toCBOR (compactOverlaySchedule os)

instance Crypto crypto => FromCBOR (NewEpochState crypto) where
  fromCBOR = do
    decodeRecordNamed "NewEpochState" (const 7) $ do
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
    LedgerState _ (DPState (DState _ _ _ _ (GenDelegs genDelegs) _) _) = ls

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

instance NFData (LedgerState crypto)

instance Crypto crypto => ToCBOR (LedgerState crypto) where
  toCBOR (LedgerState u dp) =
    encodeListLen 2 <> toCBOR u <> toCBOR dp

instance Crypto crypto => FromCBOR (LedgerState crypto) where
  fromCBOR = do
    decodeRecordNamed "LedgerState" (const 2) $ do
      u <- fromCBOR
      dp <- fromCBOR
      pure $ LedgerState u dp

-- | Creates the ledger state for an empty ledger which
--  contains the specified transaction outputs.
genesisState ::
  Map (KeyHash 'Genesis crypto) (GenDelegPair crypto) ->
  UTxO crypto ->
  LedgerState crypto
genesisState genDelegs0 utxo0 =
  LedgerState
    ( UTxOState
        utxo0
        (Coin 0)
        (Coin 0)
        emptyPPUPState
    )
    (DPState dState emptyPState)
  where
    dState = emptyDState {_genDelegs = GenDelegs genDelegs0}

-- | Implementation of abstract transaction size
txsize :: Tx crypto -> Integer
txsize = fromIntegral . BSL.length . txFullBytes

-- | Convenience Function to bound the txsize function.
-- | It can be helpful for coin selection.
txsizeBound :: forall crypto. (Crypto crypto) => Tx crypto -> Integer
txsizeBound tx = numInputs * inputSize + numOutputs * outputSize + rest
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
minfee :: PParams -> Tx crypto -> Coin
minfee pp tx = Coin $ fromIntegral (_minfeeA pp) * txsize tx + fromIntegral (_minfeeB pp)

-- | Minimum fee bound using txsizeBound
minfeeBound :: forall crypto. (Crypto crypto) => PParams -> Tx crypto -> Coin
minfeeBound pp tx = Coin $ fromIntegral (_minfeeA pp) * txsizeBound tx + fromIntegral (_minfeeB pp)

-- | Compute the lovelace which are created by the transaction
produced ::
  (Crypto crypto) =>
  PParams ->
  Map (KeyHash 'StakePool crypto) (PoolParams crypto) ->
  TxBody crypto ->
  Coin
produced pp stakePools tx =
  balance (txouts tx) + _txfee tx + totalDeposits pp stakePools (toList $ _certs tx)

-- | Compute the key deregistration refunds in a transaction
keyRefunds ::
  Crypto crypto =>
  PParams ->
  TxBody crypto ->
  Coin
keyRefunds pp tx = (_keyDeposit pp) * (fromIntegral $ length deregistrations)
  where
    deregistrations = filter isDeRegKey (toList $ _certs tx)

-- | Compute the lovelace which are destroyed by the transaction
consumed ::
  Crypto crypto =>
  PParams ->
  UTxO crypto ->
  TxBody crypto ->
  Coin
consumed pp u tx =
  balance (eval (txins tx ◁ u)) + refunds + withdrawals
  where
    -- balance (UTxO (Map.restrictKeys v (txins tx))) + refunds + withdrawals
    refunds = keyRefunds pp tx
    withdrawals = sum . unWdrl $ _wdrls tx

newtype WitHashes crypto = WitHashes
  {unWitHashes :: Set (KeyHash 'Witness crypto)}
  deriving (Eq, Generic)
  deriving (Show) via Quiet (WitHashes crypto)

instance Crypto crypto => NoUnexpectedThunks (WitHashes crypto)

-- | Check if a set of witness hashes is empty.
nullWitHashes :: WitHashes crypto -> Bool
nullWitHashes (WitHashes a) = Set.null a

-- | Extract the difference between two sets of witness hashes.
diffWitHashes :: WitHashes crypto -> WitHashes crypto -> WitHashes crypto
diffWitHashes (WitHashes x) (WitHashes x') =
  WitHashes (x `Set.difference` x')

-- | Extract the witness hashes from the Witness set.
witsFromWitnessSet ::
  Crypto crypto => WitnessSet crypto -> WitHashes crypto
witsFromWitnessSet (WitnessSet aWits _ bsWits) =
  WitHashes $
    Set.map witKeyHash aWits
      `Set.union` Set.map bootstrapWitKeyHash bsWits

-- | Collect the set of hashes of keys that needs to sign a
--  given transaction. This set consists of the txin owners,
--  certificate authors, and withdrawal reward accounts.
witsVKeyNeeded ::
  forall crypto.
  Crypto crypto =>
  UTxO crypto ->
  Tx crypto ->
  GenDelegs crypto ->
  WitHashes crypto
witsVKeyNeeded utxo' tx@(Tx txbody _ _) genDelegs =
  WitHashes $
    certAuthors
      `Set.union` inputAuthors
      `Set.union` owners
      `Set.union` wdrlAuthors
      `Set.union` updateKeys
  where
    inputAuthors :: Set (KeyHash 'Witness crypto)
    inputAuthors = foldr accum Set.empty (_inputs txbody)
      where
        accum txin ans =
          case txinLookup txin utxo' of
            Just (TxOut (Addr _ (KeyHashObj pay) _) _) -> Set.insert (asWitness pay) ans
            Just (TxOut (AddrBootstrap bootAddr) _) -> Set.insert (asWitness (bootstrapKeyHash bootAddr)) ans
            _other -> ans
    wdrlAuthors :: Set (KeyHash 'Witness crypto)
    wdrlAuthors = Map.foldrWithKey accum Set.empty (unWdrl (_wdrls txbody))
      where
        accum key _ ans = Set.union (extractKeyHashWitnessSet [getRwdCred key]) ans
    owners :: Set (KeyHash 'Witness crypto)
    owners = foldr accum Set.empty (_certs txbody)
      where
        accum (DCertPool (RegPool pool)) ans = Set.union (Set.map asWitness (_poolOwners pool)) ans
        accum _cert ans = ans
    cwitness (DCertDeleg dc) = extractKeyHashWitnessSet [delegCWitness dc]
    cwitness (DCertPool pc) = extractKeyHashWitnessSet [poolCWitness pc]
    cwitness (DCertGenesis gc) = Set.singleton (asWitness $ genesisCWitness gc)
    cwitness c = error $ show c ++ " does not have a witness"
    -- key reg requires no witness but this is already filtered outby requiresVKeyWitness
    -- before the call to `cwitness`, so this error should never be reached.

    certAuthors :: Set (KeyHash 'Witness crypto)
    certAuthors = foldr accum Set.empty (_certs txbody)
      where
        accum cert ans | requiresVKeyWitness cert = Set.union (cwitness cert) ans
        accum _cert ans = ans
    updateKeys :: Set (KeyHash 'Witness crypto)
    updateKeys = asWitness `Set.map` propWits (txup tx) genDelegs

-- | Given a ledger state, determine if the UTxO witnesses in a given
--  transaction are correct.
verifiedWits ::
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Tx crypto ->
  Either [VKey 'Witness crypto] ()
verifiedWits (Tx txbody wits _) =
  case (failed <> failedBootstrap) of
    [] -> Right ()
    nonEmpty -> Left nonEmpty
  where
    wvkKey (WitVKey k _) = k
    failed =
      wvkKey
        <$> filter
          (not . verifyWitVKey (hashAnnotated txbody))
          (Set.toList $ addrWits wits)
    failedBootstrap =
      bwKey
        <$> filter
          (not . verifyBootstrapWit (hashAnnotated txbody))
          (Set.toList $ bootWits wits)

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
    updateKeys' = eval (Map.keysSet pup ◁ genDelegs)
    updateKeys = Map.map genDelegKeyHash updateKeys'

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
      totalDeposits pp ((_pParams . _pstate . _delegationState) ls) (toList $ _certs tx)
    txRefunds = keyRefunds pp tx

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

stakeDistr ::
  forall crypto.
  Crypto crypto =>
  UTxO crypto ->
  DState crypto ->
  PState crypto ->
  SnapShot crypto
stakeDistr u ds ps =
  SnapShot
    (Stake $ eval (dom activeDelegs ◁ stakeRelation))
    delegs
    poolParams
  where
    DState rewards' delegs ptrs' _ _ _ = ds
    PState poolParams _ _ = ps
    stakeRelation :: Map (Credential 'Staking crypto) Coin
    stakeRelation = aggregateUtxoCoinByCredential (forwards ptrs') u rewards'
    activeDelegs :: Map (Credential 'Staking crypto) (KeyHash 'StakePool crypto)
    activeDelegs = eval ((dom rewards' ◁ delegs) ▷ dom poolParams)

-- | Apply a reward update
applyRUpd ::
  RewardUpdate crypto ->
  EpochState crypto ->
  EpochState crypto
applyRUpd ru (EpochState as ss ls pr pp _nm) = EpochState as' ss ls' pr pp nm'
  where
    utxoState_ = _utxoState ls
    delegState = _delegationState ls
    dState = _dstate delegState
    (regRU, unregRU) =
      Map.partitionWithKey
        (\k _ -> eval (k ∈ dom (_rewards dState)))
        (rs ru)
    as' =
      as
        { _treasury = _treasury as + deltaT ru + sum (range unregRU),
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
                    { _rewards = eval (_rewards dState ∪+ regRU)
                    }
              }
        }
    nm' = nonMyopic ru

updateNonMypopic ::
  NonMyopic crypto ->
  Coin ->
  Map (KeyHash 'StakePool crypto) Likelihood ->
  SnapShot crypto ->
  NonMyopic crypto
updateNonMypopic nm rPot newLikelihoods ss =
  nm
    { likelihoodsNM = updatedLikelihoods,
      rewardPotNM = rPot,
      snapNM = ss
    }
  where
    history = likelihoodsNM nm
    performance kh newPerf = fromMaybe mempty (Map.lookup kh history) <> newPerf
    updatedLikelihoods = Map.mapWithKey performance newLikelihoods

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
  let SnapShot stake' delegs' poolParams = _pstakeGo ss
      Coin reserves = _reserves acnt
      ds = _dstate $ _delegationState ls
      -- reserves and rewards change
      deltaR1 = (rationalToCoinViaFloor $ min 1 eta * unitIntervalToRational (_rho pr) * fromIntegral reserves)
      d = unitIntervalToRational (_d pr)
      expectedBlocks =
        floor $
          (1 - d) * unitIntervalToRational (activeSlotVal asc) * fromIntegral slotsPerEpoch
      -- TODO asc is a global constant, and slotsPerEpoch should not change often at all,
      -- it would be nice to not have to compute expectedBlocks every epoch
      eta
        | intervalValue (_d pr) >= 0.8 = 1
        | otherwise = blocksMade % expectedBlocks
      Coin rPot = _feeSS ss + deltaR1
      deltaT1 = floor $ intervalValue (_tau pr) * fromIntegral rPot
      _R = Coin $ rPot - deltaT1
      circulation = total - (_reserves acnt)
      (rs_, newLikelihoods) =
        reward pr b _R (Map.keysSet $ _rewards ds) poolParams stake' delegs' circulation asc slotsPerEpoch
      deltaR2 = _R - (Map.foldr (+) (Coin 0) rs_)
      blocksMade = fromIntegral $ Map.foldr (+) 0 b' :: Integer
  pure $
    RewardUpdate
      { deltaT = (Coin deltaT1),
        deltaR = (- deltaR1 + deltaR2),
        rs = rs_,
        deltaF = (- (_feeSS ss)),
        nonMyopic = (updateNonMypopic nm _R newLikelihoods (_pstakeGo ss))
      }

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
