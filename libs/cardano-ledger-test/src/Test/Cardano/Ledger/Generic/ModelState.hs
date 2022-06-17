{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | The data types in this file constitute a Model of the NewEpochState
--   sufficient for generating transactions that can run in the
--   MockChain STS instance. This is a model so we drop some details of the
--   real NewEpochState, and add some additional details specific to Tx generation.
--   Dropped details include
--   1) Efficiency concerns
--      a) Pulsing Reward updates
--      b) Inverse of the Rewards Map
--      c) esNonMyopic field
--      d) _stakeDistro, Incremental Stake distribution
--   2) Transaction features that make changes to the Protocol Parameters
--   3) Using Hashes of the TxBody as the Ix, instead we maintain an index of
--      an arbitrary Hash to the sequence (Count) in which the TxBody was generated.
--   Additional details
--   1) Utxo entries to pay fees. It is incredibly hard to generate Txs with the
--      correct fees. So we keep a small set of UtxoEntrys, where it is allowed to
--      mutate the value field of the TxOut. We have to be sure these entries are carefully
--      managed, as they do not follow the rules of the real world.
--   Additional comments
--   1) We include data in the Model for Epoch boundary computations, but we do not
--      do anything with them at this time.
module Test.Cardano.Ledger.Generic.ModelState where

import Cardano.Ledger.BaseTypes (BlocksMade (..))
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Keys
  ( GenDelegs (..),
    KeyHash (..),
    KeyRole (..),
  )
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.Pretty
  ( PDoc,
    ppAccountState,
    ppCoin,
    ppEpochNo,
    ppInt,
    ppKeyHash,
    ppMap,
    ppNatural,
    ppRecord,
    ppString,
  )
import Cardano.Ledger.Pretty.Alonzo ()
import Cardano.Ledger.Pretty.Babbage ()
import Cardano.Ledger.Shelley.EpochBoundary
  ( SnapShot (..),
    SnapShots (..),
    Stake (..),
  )
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    DPState (..),
    DState (..),
    EpochState (..),
    IncrementalStake (..),
    InstantaneousRewards (..),
    LedgerState (..),
    NewEpochState (..),
    PPUPState (..),
    PState (..),
    StashedAVVMAddresses,
    UTxOState (..),
    completeRupd,
    smartUTxOState,
  )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..))
import Cardano.Ledger.Shelley.PoolRank (NonMyopic (..))
import Cardano.Ledger.Shelley.RewardUpdate (PulsingRewUpdate (..), RewardUpdate (..))
import Cardano.Ledger.Shelley.TxBody (PoolParams (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Control.Monad.Trans ()
import Control.Provenance (runProvM)
import Control.State.Transition (STS (State))
import Data.Default.Class (Default (def))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Text (Text)
import qualified Data.UMap as UMap
import qualified Data.VMap as VMap
import GHC.Natural (Natural)
import Test.Cardano.Ledger.Generic.PrettyCore
  ( PrettyC (..),
    credSummary,
    keyHashSummary,
    pcCredential,
    pcIndividualPoolStake,
    pcKeyHash,
    pcPoolParams,
    pcTxId,
    pcTxIn,
    pcTxOut,
  )
import Test.Cardano.Ledger.Generic.Proof
  ( BabbageEra,
    Evidence (..),
    Mock,
    Proof (..),
    Reflect (..),
  )
import Test.Cardano.Ledger.Shelley.Utils (runShelleyBase)

-- =============================================

-- | MUtxo = Model UTxO. In the Model we represent the
--   UTxO as a Map (not a newtype around a Map)
type MUtxo era = Map (TxIn (Crypto era)) (Core.TxOut era)

pcMUtxo :: Reflect era => Proof era -> MUtxo era -> PDoc
pcMUtxo proof m = ppMap pcTxIn (pcTxOut proof) m

-- ===========================================================

data ModelNewEpochState era = ModelNewEpochState
  { -- PState fields
    mPoolParams :: !(Map (KeyHash 'StakePool (Crypto era)) (PoolParams (Crypto era))),
    -- DState state fields
    mRewards :: !(Map (Credential 'Staking (Crypto era)) Coin),
    mDelegations :: !(Map (Credential 'Staking (Crypto era)) (KeyHash 'StakePool (Crypto era))),
    --  _fGenDelegs,  _genDelegs, and _irwd, are for
    --  changing the PParams and are abstracted away

    -- UTxO state fields (and extra stuff)
    mUTxO :: !(Map (TxIn (Crypto era)) (Core.TxOut era)),
    -- | The current UTxO
    mMutFee :: !(Map (TxIn (Crypto era)) (Core.TxOut era)),
    -- _ppups is for changing PParams, and _stakeDistro is for efficiency
    -- and are abstracted away.

    -- EpochState fields
    mAccountState :: !AccountState,
    -- esPrevPp and esPp are for changing PParams
    -- esNonMyopic is for efficiency, and all are abstracted away

    -- Model NewEpochState fields
    mPoolDistr :: !(Map (KeyHash 'StakePool (Crypto era)) (IndividualPoolStake (Crypto era))),
    mPParams :: !(Core.PParams era),
    mDeposited :: !Coin,
    mFees :: !Coin,
    mCount :: !Int,
    mIndex :: !(Map Int (TxId (Crypto era))),
    -- below here NO EFFECT until we model EpochBoundary
    mFPoolParams :: !(Map (KeyHash 'StakePool (Crypto era)) (PoolParams (Crypto era))),
    mRetiring :: !(Map (KeyHash 'StakePool (Crypto era)) EpochNo),
    mSnapshots :: !(SnapShots (Crypto era)),
    mEL :: !EpochNo, -- The current epoch,
    mBprev :: !(Map (KeyHash 'StakePool (Crypto era)) Natural), --  Blocks made before current epoch, NO EFFECT until we model EpochBoundar
    mBcur :: !(Map (KeyHash 'StakePool (Crypto era)) Natural),
    -- | Blocks made in current epoch
    mRu :: !(StrictMaybe (RewardUpdate (Crypto era))) -- Possible reward update
  }

type UtxoEntry era = (TxIn (Crypto era), Core.TxOut era)

type Model era = ModelNewEpochState era

-- ======================================================================
-- Empty or default values, these are usefull for many things, not the
-- least of, for making Model instances.

blocksMadeZero :: BlocksMade crypto
blocksMadeZero = BlocksMade Map.empty

poolDistrZero :: PoolDistr crypto
poolDistrZero = PoolDistr Map.empty

stakeZero :: Stake crypto
stakeZero = Stake VMap.empty

snapShotZero :: SnapShot crypto
snapShotZero = SnapShot stakeZero VMap.empty VMap.empty

snapShotsZero :: SnapShots crypto
snapShotsZero = SnapShots snapShotZero snapShotZero snapShotZero (Coin 0)

accountStateZero :: AccountState
accountStateZero = AccountState (Coin 0) (Coin 0)

utxoZero :: UTxO era
utxoZero = UTxO Map.empty

genDelegsZero :: GenDelegs crypto
genDelegsZero = GenDelegs Map.empty

instantaneousRewardsZero :: InstantaneousRewards crypto
instantaneousRewardsZero = InstantaneousRewards Map.empty Map.empty mempty mempty

dStateZero :: DState crypto
dStateZero = DState UMap.empty Map.empty genDelegsZero instantaneousRewardsZero

pStateZero :: PState crypto
pStateZero = PState Map.empty Map.empty Map.empty

dPStateZero :: DPState crypto
dPStateZero = DPState dStateZero pStateZero

incrementalStakeZero :: IncrementalStake crypto
incrementalStakeZero = IStake Map.empty Map.empty

proposedPPUpdatesZero :: ProposedPPUpdates era
proposedPPUpdatesZero = ProposedPPUpdates Map.empty

nonMyopicZero :: NonMyopic crypto
nonMyopicZero = NonMyopic Map.empty mempty

pPUPStateZeroByProof :: Proof era -> State (Core.EraRule "PPUP" era)
pPUPStateZeroByProof (Babbage _) = PPUPState proposedPPUpdatesZero proposedPPUpdatesZero
pPUPStateZeroByProof (Alonzo _) = PPUPState proposedPPUpdatesZero proposedPPUpdatesZero
pPUPStateZeroByProof (Mary _) = PPUPState proposedPPUpdatesZero proposedPPUpdatesZero
pPUPStateZeroByProof (Allegra _) = PPUPState proposedPPUpdatesZero proposedPPUpdatesZero
pPUPStateZeroByProof (Shelley _) = PPUPState proposedPPUpdatesZero proposedPPUpdatesZero

pParamsZeroByProof :: Proof era -> Core.PParams era
pParamsZeroByProof (Babbage _) = def
pParamsZeroByProof (Alonzo _) = def
pParamsZeroByProof (Mary _) = def
pParamsZeroByProof (Allegra _) = def
pParamsZeroByProof (Shelley _) = def

pPUPStateZero :: forall era. Reflect era => State (Core.EraRule "PPUP" era)
pPUPStateZero = pPUPStateZeroByProof @era (reify :: Proof era)

uTxOStateZero :: forall era. Reflect era => UTxOState era
uTxOStateZero = smartUTxOState utxoZero mempty mempty (pPUPStateZero @era)

pParamsZero :: Reflect era => Core.PParams era
pParamsZero = lift pParamsZeroByProof

ledgerStateZero :: Reflect era => LedgerState era
ledgerStateZero = LedgerState uTxOStateZero dPStateZero

epochStateZero :: Reflect era => EpochState era
epochStateZero = EpochState accountStateZero snapShotsZero ledgerStateZero pParamsZero pParamsZero nonMyopicZero

newEpochStateZero :: forall era. Reflect era => NewEpochState era
newEpochStateZero =
  NewEpochState
    (EpochNo 0)
    blocksMadeZero
    blocksMadeZero
    epochStateZero
    SNothing
    poolDistrZero
    (stashedAVVMAddressesZero (reify :: Proof era))

stashedAVVMAddressesZero :: Proof era -> StashedAVVMAddresses era
stashedAVVMAddressesZero (Shelley _) = utxoZero
stashedAVVMAddressesZero (Babbage _) = ()
stashedAVVMAddressesZero (Alonzo _) = ()
stashedAVVMAddressesZero (Mary _) = ()
stashedAVVMAddressesZero (Allegra _) = ()

mNewEpochStateZero :: Reflect era => ModelNewEpochState era
mNewEpochStateZero =
  ModelNewEpochState
    { mPoolParams = Map.empty,
      mRewards = Map.empty,
      mDelegations = Map.empty,
      mUTxO = Map.empty,
      mMutFee = Map.empty,
      mAccountState = accountStateZero,
      mPoolDistr = Map.empty,
      mPParams = pParamsZero,
      mDeposited = Coin 0,
      mFees = Coin 0,
      mCount = 0,
      mIndex = Map.empty,
      -- below here NO EFFECT until we model EpochBoundary
      mFPoolParams = Map.empty,
      mRetiring = Map.empty,
      mSnapshots = snapShotsZero,
      mEL = EpochNo 0,
      mBprev = Map.empty,
      mBcur = Map.empty,
      mRu = SNothing
    }

testNES :: NewEpochState (BabbageEra Mock)
testNES = newEpochStateZero

testMNES :: ModelNewEpochState (BabbageEra Mock)
testMNES = mNewEpochStateZero

-- ======================================================================

class Extract t era where
  extract :: ModelNewEpochState era -> t

instance Crypto era ~ c => Extract (DState c) era where
  extract x =
    DState
      (UMap.unify (mRewards x) (mDelegations x) Map.empty)
      Map.empty
      genDelegsZero
      instantaneousRewardsZero

instance Crypto era ~ c => Extract (PState c) era where
  extract x = PState (mPoolParams x) (mFPoolParams x) (mRetiring x)

instance Crypto era ~ c => Extract (DPState c) era where
  extract x = DPState (extract x) (extract x)

instance Reflect era => Extract (UTxOState era) era where
  extract x =
    smartUTxOState
      (UTxO (mUTxO x))
      (mDeposited x)
      (mFees x)
      (pPUPStateZero @era)

instance Reflect era => Extract (LedgerState era) era where
  extract x = LedgerState (extract x) (extract x)

instance Reflect era => Extract (EpochState era) era where
  extract x =
    EpochState
      (mAccountState x)
      (mSnapshots x)
      (extract x)
      (pParamsZero @era)
      (pParamsZero @era)
      nonMyopicZero

instance forall era. Reflect era => Extract (NewEpochState era) era where
  extract x =
    NewEpochState
      (mEL x)
      (BlocksMade (mBprev x))
      (BlocksMade (mBcur x))
      (extract x)
      (Complete <$> (mRu x))
      (PoolDistr (mPoolDistr x))
      (stashedAVVMAddressesZero (reify :: Proof era))

abstract :: NewEpochState era -> ModelNewEpochState era
abstract x =
  ModelNewEpochState
    { mPoolParams = (_pParams . dpsPState . lsDPState . esLState . nesEs) x,
      mRewards = (UMap.rewView . _unified . dpsDState . lsDPState . esLState . nesEs) x,
      mDelegations = (UMap.delView . _unified . dpsDState . lsDPState . esLState . nesEs) x,
      mUTxO = (unUTxO . _utxo . lsUTxOState . esLState . nesEs) x,
      mMutFee = Map.empty,
      mAccountState = (esAccountState . nesEs) x,
      mPoolDistr = (unPoolDistr . nesPd) x,
      mPParams = (esPp . nesEs) x,
      mDeposited = (_deposited . lsUTxOState . esLState . nesEs) x,
      mFees = (_fees . lsUTxOState . esLState . nesEs) x,
      mCount = 0,
      mIndex = Map.empty,
      -- below here NO EFFECT until we model EpochBoundary
      mFPoolParams = (_fPParams . dpsPState . lsDPState . esLState . nesEs) x,
      mRetiring = (_retiring . dpsPState . lsDPState . esLState . nesEs) x,
      mSnapshots = (esSnapshots . nesEs) x,
      mEL = nesEL x,
      mBprev = unBlocksMade (nesBprev x),
      mBcur = unBlocksMade (nesBcur x),
      mRu = case nesRu x of
        SNothing -> SNothing
        SJust pru -> SJust (complete pru)
        -- SNothing -- There is no way to complete (nesRu x) to get a RewardUpdate
    }

complete :: PulsingRewUpdate crypto -> RewardUpdate crypto
complete (Complete r) = r
complete (Pulsing rewsnap pulser) = fst $ runShelleyBase $ runProvM (completeRupd (Pulsing rewsnap pulser))

-- =====================================================================

pcModelNewEpochState :: Reflect era => Proof era -> ModelNewEpochState era -> PDoc
pcModelNewEpochState proof x =
  ppRecord "ModelNewEpochState" $
    [ ("poolparams", ppMap keyHashSummary pcPoolParams (mPoolParams x)),
      ("rewards", ppMap credSummary ppCoin (mRewards x)),
      ("delegations", ppMap pcCredential pcKeyHash (mDelegations x)),
      ("utxo", ppMap pcTxIn (pcTxOut proof) (mUTxO x)),
      ("mutFees", ppMap pcTxIn (pcTxOut proof) (mMutFee x)),
      ("account", ppAccountState (mAccountState x)),
      ("pool distr", ppMap pcKeyHash pcIndividualPoolStake (mPoolDistr x)),
      ("protocol params", ppString "PParams ..."),
      ("deposited", ppCoin (mDeposited x)),
      ("fees", ppCoin (mFees x)),
      ("count", ppInt (mCount x)),
      ("index", ppMap ppInt pcTxId (mIndex x))
      -- Add additional EpochBoundary fields here
    ]
      ++ Maybe.catMaybes (epochBoundaryPDoc proof x)

epochBoundaryPDoc :: Proof era -> ModelNewEpochState era -> [Maybe (Text, PDoc)]
epochBoundaryPDoc _proof x =
  [ if Map.null futurepp then Nothing else Just ("future pparams", ppMap ppKeyHash pcPoolParams futurepp),
    if Map.null retiring then Nothing else Just ("retiring", ppMap ppKeyHash ppEpochNo retiring),
    if lastepoch == EpochNo 0 then Nothing else Just ("last epoch", ppEpochNo lastepoch),
    if Map.null prevBlocks then Nothing else Just ("prev blocks", ppMap ppKeyHash ppNatural prevBlocks),
    if Map.null curBlocks then Nothing else Just ("current blocks", ppMap ppKeyHash ppNatural curBlocks)
  ]
  where
    futurepp = mFPoolParams x
    retiring = mRetiring x
    lastepoch = mEL x
    prevBlocks = (mBprev x)
    curBlocks = (mBcur x)

-- SnapShots and PulsingRewUdate delberately ommitted from pretty printer

instance Reflect era => PrettyC (ModelNewEpochState era) era where prettyC = pcModelNewEpochState

instance era ~ BabbageEra Mock => Show (ModelNewEpochState era) where
  show x = show (prettyC (Babbage Mock) x)
