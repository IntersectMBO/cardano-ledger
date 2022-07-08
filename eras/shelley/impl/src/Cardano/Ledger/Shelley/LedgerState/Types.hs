{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.LedgerState.Types where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Ledger.BaseTypes
  ( BlocksMade (..),
    StrictMaybe (..),
  )
import Cardano.Ledger.Coin
  ( Coin (..),
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Keys
  ( KeyHash (..),
    KeyPair,
    KeyRole (..),
  )
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.SafeHash (HashAnnotated)
import Cardano.Ledger.Serialization (decodeRecordNamedT, mapFromCBOR, mapToCBOR)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Constraints (TransValue)
import Cardano.Ledger.Shelley.EpochBoundary
  ( SnapShots (..),
  )
import Cardano.Ledger.Shelley.LedgerState.DPState (DPState)
import Cardano.Ledger.Shelley.PoolRank
  ( NonMyopic (..),
  )
import Cardano.Ledger.Shelley.RewardUpdate
  ( PulsingRewUpdate (..),
  )
import Cardano.Ledger.Shelley.TxBody
  ( EraIndependentTxBody,
    Ptr (..),
  )
import Cardano.Ledger.Shelley.UTxO
  ( UTxO (..),
  )
import Cardano.Ledger.Slot
  ( EpochNo (..),
  )
import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Trans
import Control.State.Transition (STS (State))
import Data.Coders
  ( Decode (..),
    decode,
    decodeRecordNamed,
    (<!),
  )
import Data.Default.Class (Default, def)
import Data.Group (Group, invert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sharing
import GHC.Generics (Generic)
import Lens.Micro (_1, _2)
import NoThunks.Class (NoThunks (..))

-- | Representation of a list of pairs of key pairs, e.g., pay and stake keys
type KeyPairs crypto = [(KeyPair 'Payment crypto, KeyPair 'Staking crypto)]

type RewardAccounts crypto =
  Map (Credential 'Staking crypto) Coin

data AccountState = AccountState
  { _treasury :: !Coin,
    _reserves :: !Coin
  }
  deriving (Show, Eq, Generic)

instance ToCBOR AccountState where
  toCBOR (AccountState t r) =
    encodeListLen 2 <> toCBOR t <> toCBOR r

instance FromCBOR AccountState where
  fromCBOR =
    decodeRecordNamed "AccountState" (const 2) $ AccountState <$> fromCBOR <*> fromCBOR

instance NoThunks AccountState

instance NFData AccountState

data EpochState era = EpochState
  { esAccountState :: !AccountState,
    esSnapshots :: !(SnapShots (Crypto era)),
    esLState :: !(LedgerState era),
    esPrevPp :: !(Core.PParams era),
    esPp :: !(Core.PParams era),
    -- | This field, esNonMyopic, does not appear in the formal spec
    -- and is not a part of the protocol. It is only used for providing
    -- data to the stake pool ranking calculation @getNonMyopicMemberRewards@.
    -- See https://hydra.iohk.io/job/Cardano/cardano-ledger/specs.pool-ranking/latest/download-by-type/doc-pdf/pool-ranking
    esNonMyopic :: !(NonMyopic (Crypto era))
  }
  deriving (Generic)

deriving stock instance
  ( CC.Crypto (Crypto era),
    Show (Core.TxOut era),
    Show (Core.PParams era),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  Show (EpochState era)

deriving stock instance
  ( CC.Crypto (Crypto era),
    Eq (Core.TxOut era),
    Eq (Core.PParams era),
    Eq (State (Core.EraRule "PPUP" era))
  ) =>
  Eq (EpochState era)

instance
  ( Era era,
    NoThunks (Core.TxOut era),
    NoThunks (State (Core.EraRule "PPUP" era)),
    NoThunks (Core.Value era),
    NoThunks (Core.PParams era),
    ToCBOR (Core.TxBody era),
    ToCBOR (Core.TxOut era),
    ToCBOR (Core.Value era)
  ) =>
  NoThunks (EpochState era)

instance
  ( Era era,
    NFData (Core.TxOut era),
    NFData (Core.PParams era),
    NFData (State (Core.EraRule "PPUP" era))
  ) =>
  NFData (EpochState era)

instance
  ( Era era,
    ToCBOR (Core.TxOut era),
    ToCBOR (Core.PParams era),
    ToCBOR (State (Core.EraRule "PPUP" era))
  ) =>
  ToCBOR (EpochState era)
  where
  toCBOR EpochState {esAccountState, esLState, esSnapshots, esPrevPp, esPp, esNonMyopic} =
    encodeListLen 6
      <> toCBOR esAccountState
      <> toCBOR esLState -- We get better sharing when encoding ledger state before snaphots
      <> toCBOR esSnapshots
      <> toCBOR esPrevPp
      <> toCBOR esPp
      <> toCBOR esNonMyopic

instance
  ( FromCBOR (Core.PParams era),
    TransValue FromCBOR era,
    HashAnnotated (Core.TxBody era) EraIndependentTxBody (Crypto era),
    FromSharedCBOR (Core.TxOut era),
    Share (Core.TxOut era) ~ Interns (Credential 'Staking (Crypto era)),
    FromCBOR (State (Core.EraRule "PPUP" era)),
    Era era
  ) =>
  FromCBOR (EpochState era)
  where
  fromCBOR =
    decodeRecordNamed "EpochState" (const 6) $
      flip evalStateT mempty $ do
        esAccountState <- lift fromCBOR
        esLState <- fromSharedPlusCBOR
        esSnapshots <- fromSharedPlusCBOR
        esPrevPp <- lift fromCBOR
        esPp <- lift fromCBOR
        esNonMyopic <- fromSharedLensCBOR _2
        pure EpochState {esAccountState, esSnapshots, esLState, esPrevPp, esPp, esNonMyopic}

data UpecState era = UpecState
  { -- | Current protocol parameters.
    currentPp :: !(Core.PParams era),
    -- | State of the protocol update transition system.
    ppupState :: !(State (Core.EraRule "PPUP" era))
  }

deriving stock instance
  ( Show (State (Core.EraRule "PPUP" era)),
    Show (Core.PParams era)
  ) =>
  Show (UpecState era)

-- =============================

-- | Incremental Stake, Stake along with possible missed coins from danging Ptrs.
--   Transactions can use Ptrs to refer to a stake credential in a TxOut. The Ptr
--   does not have to point to anything until the epoch boundary, when we compute
--   rewards and aggregate staking information for ranking. This is unusual but legal.
--   In a non incremental system, we use whatever 'legal' Ptrs exist at the epoch
--   boundary. Here we are computing things incrementally, so we need to remember Ptrs
--   that might point to something by the time the epoch boundary is reached. When
--   the epoch boundary is reached we 'resolve' these pointers, to see if any have
--   become non-dangling since the time they were first used in the incremental computation.
data IncrementalStake crypto = IStake
  { credMap :: !(Map (Credential 'Staking crypto) Coin),
    ptrMap :: !(Map Ptr Coin)
  }
  deriving (Generic, Show, Eq, Ord, NoThunks, NFData)

instance CC.Crypto crypto => ToCBOR (IncrementalStake crypto) where
  toCBOR (IStake st dangle) =
    encodeListLen 2 <> mapToCBOR st <> mapToCBOR dangle

instance CC.Crypto crypto => FromSharedCBOR (IncrementalStake crypto) where
  type Share (IncrementalStake crypto) = Interns (Credential 'Staking crypto)
  fromSharedCBOR credInterns =
    decodeRecordNamed "Stake" (const 2) $ do
      stake <- fromSharedCBOR (credInterns, mempty)
      IStake stake <$> mapFromCBOR

instance Semigroup (IncrementalStake c) where
  (IStake a b) <> (IStake c d) = IStake (Map.unionWith (<>) a c) (Map.unionWith (<>) b d)

instance Monoid (IncrementalStake c) where
  mempty = IStake Map.empty Map.empty

instance Data.Group.Group (IncrementalStake c) where
  invert (IStake m1 m2) = IStake (Map.map invert m1) (Map.map invert m2)

instance Default (IncrementalStake c) where
  def = IStake Map.empty Map.empty

-- =============================

-- | There is a serious invariant that we must maintain in the UTxOState.
--   Given (UTxOState utxo _ _ _ istake) it must be the case that
--   istake == (updateStakeDistribution (UTxO Map.empty) (UTxO Map.empty) utxo)
--   Of course computing the RHS of the above equality can be very expensive, so we only
--   use this route in the testing function smartUTxO. But we are very carefull, wherever
--   we update the UTxO, we carefully make INCREMENTAL changes to istake to maintain
--   this invariant. This happens in the UTxO rule.
data UTxOState era = UTxOState
  { _utxo :: !(UTxO era),
    _deposited :: !Coin,
    _fees :: !Coin,
    _ppups :: !(State (Core.EraRule "PPUP" era)),
    _stakeDistro :: !(IncrementalStake (Crypto era))
  }
  deriving (Generic)

instance
  ( Era era,
    NFData (Core.TxOut era),
    NFData (State (Core.EraRule "PPUP" era))
  ) =>
  NFData (UTxOState era)

deriving stock instance
  ( CC.Crypto (Crypto era),
    Show (Core.TxOut era),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  Show (UTxOState era)

deriving stock instance
  ( CC.Crypto (Crypto era),
    Eq (Core.TxOut era),
    Eq (State (Core.EraRule "PPUP" era))
  ) =>
  Eq (UTxOState era)

instance
  ( Era era,
    NoThunks (Core.TxOut era),
    NoThunks (State (Core.EraRule "PPUP" era)),
    NoThunks (Core.Value era),
    ToCBOR (Core.TxBody era),
    ToCBOR (Core.TxOut era),
    ToCBOR (Core.Value era)
  ) =>
  NoThunks (UTxOState era)

instance
  ( Era era,
    ToCBOR (Core.TxOut era),
    ToCBOR (State (Core.EraRule "PPUP" era))
  ) =>
  ToCBOR (UTxOState era)
  where
  toCBOR (UTxOState ut dp fs us sd) =
    encodeListLen 5 <> toCBOR ut <> toCBOR dp <> toCBOR fs <> toCBOR us <> toCBOR sd

instance
  ( TransValue FromCBOR era,
    FromCBOR (State (Core.EraRule "PPUP" era)),
    FromSharedCBOR (Core.TxOut era),
    Share (Core.TxOut era) ~ Interns (Credential 'Staking (Crypto era)),
    HashAnnotated (Core.TxBody era) EraIndependentTxBody (Crypto era)
  ) =>
  FromSharedCBOR (UTxOState era)
  where
  type
    Share (UTxOState era) =
      Interns (Credential 'Staking (Crypto era))
  fromSharedCBOR credInterns =
    decodeRecordNamed "UTxOState" (const 5) $ do
      _utxo <- fromSharedCBOR credInterns
      _deposited <- fromCBOR
      _fees <- fromCBOR
      _ppups <- fromCBOR
      _stakeDistro <- fromSharedCBOR credInterns
      pure UTxOState {_utxo, _deposited, _fees, _ppups, _stakeDistro}

-- | New Epoch state and environment
data NewEpochState era = NewEpochState
  { -- | Last epoch
    nesEL :: !EpochNo,
    -- | Blocks made before current epoch
    nesBprev :: !(BlocksMade (Crypto era)),
    -- | Blocks made in current epoch
    nesBcur :: !(BlocksMade (Crypto era)),
    -- | Epoch state before current
    nesEs :: !(EpochState era),
    -- | Possible reward update
    nesRu :: !(StrictMaybe (PulsingRewUpdate (Crypto era))),
    -- | Stake distribution within the stake pool
    nesPd :: !(PoolDistr (Crypto era)),
    -- | AVVM addresses to be removed at the end of the Shelley era. Note that
    -- the existence of this field is a hack, related to the transition of UTxO
    -- to disk. We remove AVVM addresses from the UTxO on the Shelley/Allegra
    -- boundary. However, by this point the UTxO will be moved to disk, and
    -- hence doing a scan of the UTxO for AVVM addresses will be expensive. Our
    -- solution to this is to do a scan of the UTxO on the Byron/Shelley
    -- boundary (since Byron UTxO are still on disk), stash the results here,
    -- and then remove them at the Shelley/Allegra boundary.
    --
    -- This is very much an awkward implementation hack, and hence we hide it
    -- from as many places as possible.
    stashedAVVMAddresses :: !(StashedAVVMAddresses era)
  }
  deriving (Generic)

newtype Ignore t = Ignore ()

instance Semigroup (Ignore t) where
  Ignore () <> Ignore () = Ignore ()

instance Monoid (Ignore t) where
  mempty = Ignore ()

data LwNewEpochState era = LwNewEpochState
  { -- | Last epoch
    lwNesEL :: !(Ignore EpochNo),
    -- | Blocks made before current epoch
    lwNesBprev :: !(Ignore (BlocksMade (Crypto era))),
    -- | Blocks made in current epoch
    lwNesBcur :: !(Ignore (BlocksMade (Crypto era))),
    -- | Epoch state before current
    lwNesEs :: !(EpochState era),
    -- | Possible reward update
    lwNesRu :: !(Ignore (StrictMaybe (PulsingRewUpdate (Crypto era)))),
    -- | Stake distribution within the stake pool
    lwNesPd :: !(Ignore (PoolDistr (Crypto era))),
    -- | AVVM addresses to be removed at the end of the Shelley era. Note that
    -- the existence of this field is a hack, related to the transition of UTxO
    -- to disk. We remove AVVM addresses from the UTxO on the Shelley/Allegra
    -- boundary. However, by this point the UTxO will be moved to disk, and
    -- hence doing a scan of the UTxO for AVVM addresses will be expensive. Our
    -- solution to this is to do a scan of the UTxO on the Byron/Shelley
    -- boundary (since Byron UTxO are still on disk), stash the results here,
    -- and then remove them at the Shelley/Allegra boundary.
    --
    -- This is very much an awkward implementation hack, and hence we hide it
    -- from as many places as possible.
    lwStashedAVVMAddresses :: !(Ignore (StashedAVVMAddresses era))
  }
  deriving (Generic)

type family StashedAVVMAddresses era where
  StashedAVVMAddresses (ShelleyEra c) = UTxO (ShelleyEra c)
  StashedAVVMAddresses _ = ()

deriving stock instance
  ( CC.Crypto (Crypto era),
    Show (Core.TxOut era),
    Show (Core.PParams era),
    Show (State (Core.EraRule "PPUP" era)),
    Show (StashedAVVMAddresses era)
  ) =>
  Show (NewEpochState era)

deriving stock instance
  ( CC.Crypto (Crypto era),
    Eq (Core.TxOut era),
    Eq (Core.PParams era),
    Eq (State (Core.EraRule "PPUP" era)),
    Eq (StashedAVVMAddresses era)
  ) =>
  Eq (NewEpochState era)

instance
  ( Era era,
    NFData (Core.TxOut era),
    NFData (Core.PParams era),
    NFData (State (Core.EraRule "PPUP" era)),
    NFData (StashedAVVMAddresses era)
  ) =>
  NFData (NewEpochState era)

instance
  ( Era era,
    NoThunks (Core.TxOut era),
    NoThunks (Core.PParams era),
    NoThunks (State (Core.EraRule "PPUP" era)),
    NoThunks (Core.Value era),
    NoThunks (StashedAVVMAddresses era),
    ToCBOR (Core.TxBody era),
    ToCBOR (Core.TxOut era),
    ToCBOR (Core.Value era)
  ) =>
  NoThunks (NewEpochState era)

instance
  ( Era era,
    ToCBOR (Core.TxOut era),
    ToCBOR (Core.PParams era),
    ToCBOR (State (Core.EraRule "PPUP" era)),
    ToCBOR (StashedAVVMAddresses era)
  ) =>
  ToCBOR (NewEpochState era)
  where
  toCBOR (NewEpochState e bp bc es ru pd av) =
    encodeListLen 7
      <> toCBOR e
      <> toCBOR bp
      <> toCBOR bc
      <> toCBOR es
      <> toCBOR ru
      <> toCBOR pd
      <> toCBOR av

instance
  ( Era era,
    FromCBOR (Core.PParams era),
    FromSharedCBOR (Core.TxOut era),
    Share (Core.TxOut era) ~ Interns (Credential 'Staking (Crypto era)),
    FromCBOR (Core.Value era),
    FromCBOR (State (Core.EraRule "PPUP" era)),
    FromCBOR (StashedAVVMAddresses era)
  ) =>
  FromCBOR (NewEpochState era)
  where
  fromCBOR =
    decode $
      RecD NewEpochState
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance
  ( Era era,
    FromCBOR (Core.PParams era),
    FromSharedCBOR (Core.TxOut era),
    Share (Core.TxOut era) ~ Interns (Credential 'Staking (Crypto era)),
    FromCBOR (Core.Value era),
    FromCBOR (State (Core.EraRule "PPUP" era)),
    FromCBOR (StashedAVVMAddresses era)
  ) =>
  FromCBOR (LwNewEpochState era)
  where
  fromCBOR =
    decode $
      RecD LwNewEpochState
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance forall t.
  ( FromCBOR t
  ) =>
  FromCBOR (Ignore t)
  where
  fromCBOR =
    decode $
      Map (const @_ @t (Ignore ())) From

-- | The state associated with a 'Ledger'.
data LedgerState era = LedgerState
  { -- | The current unspent transaction outputs.
    lsUTxOState :: !(UTxOState era),
    -- | The current delegation state
    lsDPState :: !(DPState (Crypto era))
  }
  deriving (Generic)

deriving stock instance
  ( CC.Crypto (Crypto era),
    Show (Core.TxOut era),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  Show (LedgerState era)

deriving stock instance
  ( CC.Crypto (Crypto era),
    Eq (Core.TxOut era),
    Eq (State (Core.EraRule "PPUP" era))
  ) =>
  Eq (LedgerState era)

instance
  ( Era era,
    NoThunks (Core.TxOut era),
    NoThunks (State (Core.EraRule "PPUP" era)),
    NoThunks (Core.Value era),
    ToCBOR (Core.TxBody era),
    ToCBOR (Core.TxOut era),
    ToCBOR (Core.Value era)
  ) =>
  NoThunks (LedgerState era)

instance
  ( Era era,
    NFData (Core.TxOut era),
    NFData (State (Core.EraRule "PPUP" era))
  ) =>
  NFData (LedgerState era)

instance
  ( Era era,
    ToCBOR (Core.TxOut era),
    ToCBOR (State (Core.EraRule "PPUP" era))
  ) =>
  ToCBOR (LedgerState era)
  where
  toCBOR LedgerState {lsUTxOState, lsDPState} =
    encodeListLen 2
      <> toCBOR lsDPState -- encode delegation state first to improve sharing
      <> toCBOR lsUTxOState

instance
  ( Era era,
    HashAnnotated (Core.TxBody era) EraIndependentTxBody (Crypto era),
    FromCBOR (Core.Value era),
    FromSharedCBOR (Core.TxOut era),
    Share (Core.TxOut era) ~ Interns (Credential 'Staking (Crypto era)),
    FromCBOR (State (Core.EraRule "PPUP" era))
  ) =>
  FromSharedCBOR (LedgerState era)
  where
  type
    Share (LedgerState era) =
      (Interns (Credential 'Staking (Crypto era)), Interns (KeyHash 'StakePool (Crypto era)))
  fromSharedPlusCBOR =
    decodeRecordNamedT "LedgerState" (const 2) $ do
      lsDPState <- fromSharedPlusCBOR
      lsUTxOState <- fromSharedLensCBOR _1
      pure LedgerState {lsUTxOState, lsDPState}

-- ====================================================

--------------------------------------------------------------------------------
-- Default instances
--------------------------------------------------------------------------------

instance
  (Default (State (Core.EraRule "PPUP" era)), CC.Crypto (Crypto era)) =>
  Default (UTxOState era)
  where
  def = UTxOState mempty mempty mempty def mempty

instance
  (Default (LedgerState era), Default (Core.PParams era)) =>
  Default (EpochState era)
  where
  def = EpochState def def def def def def

instance Default (UTxOState era) => Default (LedgerState era) where
  def = LedgerState def def

instance Default AccountState where
  def = AccountState (Coin 0) (Coin 0)
