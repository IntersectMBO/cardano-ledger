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
{-# LANGUAGE TypeOperators #-}
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
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.EpochBoundary (SnapShots, emptySnapShots)
import Cardano.Ledger.Keys (
  GenDelegs (..),
  KeyHash (..),
  KeyRole (..),
 )
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.Pretty (
  PDoc,
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
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  CertState (..),
  DState (..),
  EpochState (..),
  IncrementalStake (..),
  InstantaneousRewards (..),
  LedgerState (..),
  NewEpochState (..),
  PState (..),
  StashedAVVMAddresses,
  UTxOState (..),
  VState (..),
  completeRupd,
  curPParamsEpochStateL,
  prevPParamsEpochStateL,
  smartUTxOState,
 )
import Cardano.Ledger.Shelley.PoolRank (NonMyopic (..))
import Cardano.Ledger.Shelley.RewardUpdate (PulsingRewUpdate (..), RewardUpdate (..))
import Cardano.Ledger.Shelley.TxBody (PoolParams (..))
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.UTxO (UTxO (..))
import Control.Monad.Trans ()
import Data.Default.Class (Default (def))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Text (Text)
import GHC.Natural (Natural)
import Lens.Micro ((&), (.~))
import Lens.Micro.Extras (view)
import Test.Cardano.Ledger.Generic.PrettyCore (
  PrettyC (..),
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
import Test.Cardano.Ledger.Generic.Proof (
  BabbageEra,
  Evidence (..),
  Mock,
  Proof (..),
  Reflect (..),
 )
import Test.Cardano.Ledger.Shelley.Utils (runShelleyBase)

-- =============================================

-- | MUtxo = Model UTxO. In the Model we represent the
--   UTxO as a Map (not a newtype around a Map)
type MUtxo era = Map (TxIn (EraCrypto era)) (TxOut era)

pcMUtxo :: Reflect era => Proof era -> MUtxo era -> PDoc
pcMUtxo proof m = ppMap pcTxIn (pcTxOut proof) m

-- ===========================================================

data ModelNewEpochState era = ModelNewEpochState
  { -- PState fields
    mPoolParams :: !(Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
  , mPoolDeposits :: !(Map (KeyHash 'StakePool (EraCrypto era)) Coin)
  , -- DState state fields
    mRewards :: !(Map (Credential 'Staking (EraCrypto era)) Coin)
  , mDelegations :: !(Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
  , mKeyDeposits :: !(Map (Credential 'Staking (EraCrypto era)) Coin)
  , --  _fGenDelegs,  _genDelegs, and _irwd, are for
    --  changing the PParams and are abstracted away

    -- UTxO state fields (and extra stuff)
    mUTxO :: !(Map (TxIn (EraCrypto era)) (TxOut era))
  , mMutFee :: !(Map (TxIn (EraCrypto era)) (TxOut era))
  -- ^ The current UTxO
  , -- _ppups is for changing PParams, and _stakeDistro is for efficiency
    -- and are abstracted away.

    -- EpochState fields
    mAccountState :: !AccountState
  , -- esPrevPp and esPp are for changing PParams
    -- esNonMyopic is for efficiency, and all are abstracted away

    -- Model NewEpochState fields
    mPoolDistr :: !(Map (KeyHash 'StakePool (EraCrypto era)) (IndividualPoolStake (EraCrypto era)))
  , mPParams :: !(PParams era)
  , mDeposited :: !Coin
  , mFees :: !Coin
  , mCount :: !Int
  , mIndex :: !(Map Int (TxId (EraCrypto era)))
  , -- below here NO EFFECT until we model EpochBoundary
    mFPoolParams :: !(Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
  , mRetiring :: !(Map (KeyHash 'StakePool (EraCrypto era)) EpochNo)
  , mSnapshots :: !(SnapShots (EraCrypto era))
  , mEL :: !EpochNo -- The current epoch,
  , mBprev :: !(Map (KeyHash 'StakePool (EraCrypto era)) Natural) --  Blocks made before current epoch, NO EFFECT until we model EpochBoundar
  , mBcur :: !(Map (KeyHash 'StakePool (EraCrypto era)) Natural)
  , mRu :: !(StrictMaybe (RewardUpdate (EraCrypto era))) -- Possible reward update
  }

type UtxoEntry era = (TxIn (EraCrypto era), TxOut era)

type Model era = ModelNewEpochState era

-- ======================================================================
-- Empty or default values, these are usefull for many things, not the
-- least of, for making Model instances.

blocksMadeZero :: BlocksMade c
blocksMadeZero = BlocksMade Map.empty

poolDistrZero :: PoolDistr c
poolDistrZero = PoolDistr Map.empty

accountStateZero :: AccountState
accountStateZero = AccountState (Coin 0) (Coin 0)

utxoZero :: UTxO era
utxoZero = UTxO Map.empty

genDelegsZero :: GenDelegs c
genDelegsZero = GenDelegs Map.empty

instantaneousRewardsZero :: InstantaneousRewards c
instantaneousRewardsZero = InstantaneousRewards Map.empty Map.empty mempty mempty

dStateZero :: DState c
dStateZero =
  DState
    { dsUnified = UM.empty
    , dsFutureGenDelegs = Map.empty
    , dsGenDelegs = genDelegsZero
    , dsIRewards = instantaneousRewardsZero
    }

pStateZero :: PState c
pStateZero =
  PState
    { psStakePoolParams = Map.empty
    , psFutureStakePoolParams = Map.empty
    , psRetiring = Map.empty
    , psDeposits = Map.empty
    }

dPStateZero :: CertState era
dPStateZero = CertState def pStateZero dStateZero

incrementalStakeZero :: IncrementalStake c
incrementalStakeZero = IStake Map.empty Map.empty

nonMyopicZero :: NonMyopic c
nonMyopicZero = NonMyopic Map.empty mempty

pParamsZeroByProof :: Proof era -> PParams era
pParamsZeroByProof (Conway _) = def
pParamsZeroByProof (Babbage _) = def
pParamsZeroByProof (Alonzo _) = def
pParamsZeroByProof (Mary _) = def
pParamsZeroByProof (Allegra _) = def
pParamsZeroByProof (Shelley _) = def

uTxOStateZero :: forall era. Reflect era => UTxOState era
uTxOStateZero =
  smartUTxOState
    pParamsZero
    utxoZero
    mempty
    mempty
    emptyGovState
    mempty

pParamsZero :: Reflect era => PParams era
pParamsZero = lift pParamsZeroByProof

ledgerStateZero :: forall era. Reflect era => LedgerState era
ledgerStateZero = LedgerState uTxOStateZero dPStateZero

epochStateZero :: Reflect era => EpochState era
epochStateZero =
  EpochState
    accountStateZero
    ledgerStateZero
    emptySnapShots
    nonMyopicZero
    & curPParamsEpochStateL .~ pParamsZero
    & prevPParamsEpochStateL .~ pParamsZero

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
stashedAVVMAddressesZero (Conway _) = ()
stashedAVVMAddressesZero (Babbage _) = ()
stashedAVVMAddressesZero (Alonzo _) = ()
stashedAVVMAddressesZero (Mary _) = ()
stashedAVVMAddressesZero (Allegra _) = ()

mNewEpochStateZero :: Reflect era => ModelNewEpochState era
mNewEpochStateZero =
  ModelNewEpochState
    { mPoolParams = Map.empty
    , mPoolDeposits = Map.empty
    , mRewards = Map.empty
    , mDelegations = Map.empty
    , mKeyDeposits = Map.empty
    , mUTxO = Map.empty
    , mMutFee = Map.empty
    , mAccountState = accountStateZero
    , mPoolDistr = Map.empty
    , mPParams = pParamsZero
    , mDeposited = Coin 0
    , mFees = Coin 0
    , mCount = 0
    , mIndex = Map.empty
    , -- below here NO EFFECT until we model EpochBoundary
      mFPoolParams = Map.empty
    , mRetiring = Map.empty
    , mSnapshots = emptySnapShots
    , mEL = EpochNo 0
    , mBprev = Map.empty
    , mBcur = Map.empty
    , mRu = SNothing
    }

testNES :: NewEpochState (BabbageEra Mock)
testNES = newEpochStateZero

testMNES :: ModelNewEpochState (BabbageEra Mock)
testMNES = mNewEpochStateZero

-- ======================================================================

class Extract t era where
  extract :: ModelNewEpochState era -> t

instance Extract (DState era) era where
  extract x =
    DState
      (UM.unify (makeRewards x) Map.empty (mDelegations x) Map.empty)
      Map.empty
      genDelegsZero
      instantaneousRewardsZero

makeRewards :: ModelNewEpochState era -> Map.Map (Credential 'Staking (EraCrypto era)) UM.RDPair
makeRewards mnes = Map.mapWithKey f credRewMap
  where
    credRewMap = mRewards mnes
    credDepMap = mKeyDeposits mnes
    f cred rew = case Map.lookup cred credDepMap of
      Just dep -> UM.RDPair (UM.compactCoinOrError rew) (UM.compactCoinOrError dep)
      Nothing -> error ("In makeRewards the reward and deposit maps are not in synch " ++ show cred)

instance Extract (PState era) era where
  extract x = PState (mPoolParams x) (mFPoolParams x) (mRetiring x) Map.empty

instance Extract (VState era) era where
  extract _ = VState def def (EpochNo 0)

instance Extract (CertState era) era where
  extract x = CertState (extract x) (extract x) (extract x)

instance Reflect era => Extract (UTxOState era) era where
  extract x =
    smartUTxOState
      (mPParams x)
      (UTxO (mUTxO x))
      (mDeposited x)
      (mFees x)
      emptyGovState
      mempty

instance Reflect era => Extract (LedgerState era) era where
  extract x = LedgerState (extract x) (extract x)

instance Reflect era => Extract (EpochState era) era where
  extract x =
    EpochState
      (mAccountState x)
      (extract x)
      (mSnapshots x)
      nonMyopicZero
      & curPParamsEpochStateL .~ mPParams x
      & prevPParamsEpochStateL .~ mPParams x

instance forall era. Reflect era => Extract (NewEpochState era) era where
  extract x =
    NewEpochState
      (mEL x)
      (BlocksMade (mBprev x))
      (BlocksMade (mBcur x))
      (extract x)
      (Complete <$> mRu x)
      (PoolDistr (mPoolDistr x))
      (stashedAVVMAddressesZero (reify :: Proof era))

abstract :: EraGov era => NewEpochState era -> ModelNewEpochState era
abstract x =
  ModelNewEpochState
    { mPoolParams = (psStakePoolParams . certPState . lsCertState . esLState . nesEs) x
    , mPoolDeposits = (psDeposits . certPState . lsCertState . esLState . nesEs) x
    , mRewards = (UM.rewardMap . dsUnified . certDState . lsCertState . esLState . nesEs) x
    , mDelegations = (UM.sPoolMap . dsUnified . certDState . lsCertState . esLState . nesEs) x
    , mKeyDeposits = (UM.depositMap . dsUnified . certDState . lsCertState . esLState . nesEs) x
    , mUTxO = (unUTxO . utxosUtxo . lsUTxOState . esLState . nesEs) x
    , mMutFee = Map.empty
    , mAccountState = (esAccountState . nesEs) x
    , mPoolDistr = (unPoolDistr . nesPd) x
    , mPParams = (view curPParamsEpochStateL . nesEs) x
    , mDeposited = (utxosDeposited . lsUTxOState . esLState . nesEs) x
    , mFees = (utxosFees . lsUTxOState . esLState . nesEs) x
    , mCount = 0
    , mIndex = Map.empty
    , -- below here NO EFFECT until we model EpochBoundary
      mFPoolParams = (psFutureStakePoolParams . certPState . lsCertState . esLState . nesEs) x
    , mRetiring = (psRetiring . certPState . lsCertState . esLState . nesEs) x
    , mSnapshots = (esSnapshots . nesEs) x
    , mEL = nesEL x
    , mBprev = unBlocksMade (nesBprev x)
    , mBcur = unBlocksMade (nesBcur x)
    , mRu = case nesRu x of
        SNothing -> SNothing -- <- There is no way to complete (nesRu x) to get a RewardUpdate
        SJust pru -> SJust (complete pru)
    }

complete :: PulsingRewUpdate c -> RewardUpdate c
complete (Complete r) = r
complete (Pulsing rewsnap pulser) = fst $ runShelleyBase (completeRupd (Pulsing rewsnap pulser))

-- =====================================================================

pcModelNewEpochState :: Reflect era => Proof era -> ModelNewEpochState era -> PDoc
pcModelNewEpochState proof x =
  ppRecord "ModelNewEpochState" $
    [ ("poolparams", ppMap keyHashSummary pcPoolParams (mPoolParams x))
    , ("pool deposits", ppMap keyHashSummary ppCoin (mPoolDeposits x))
    , ("rewards", ppMap credSummary ppCoin (mRewards x))
    , ("delegations", ppMap pcCredential pcKeyHash (mDelegations x))
    , ("key deposits", ppMap credSummary ppCoin (mKeyDeposits x))
    , ("utxo", ppMap pcTxIn (pcTxOut proof) (mUTxO x))
    , ("mutFees", ppMap pcTxIn (pcTxOut proof) (mMutFee x))
    , ("account", ppAccountState (mAccountState x))
    , ("pool distr", ppMap pcKeyHash pcIndividualPoolStake (mPoolDistr x))
    , ("protocol params", ppString "PParams ...")
    , ("deposited", ppCoin (mDeposited x))
    , ("fees", ppCoin (mFees x))
    , ("count", ppInt (mCount x))
    , ("index", ppMap ppInt pcTxId (mIndex x))
    -- Add additional EpochBoundary fields here
    ]
      ++ Maybe.catMaybes (epochBoundaryPDoc proof x)

epochBoundaryPDoc :: Proof era -> ModelNewEpochState era -> [Maybe (Text, PDoc)]
epochBoundaryPDoc _proof x =
  [ if Map.null futurepp then Nothing else Just ("future pparams", ppMap ppKeyHash pcPoolParams futurepp)
  , if Map.null retiring then Nothing else Just ("retiring", ppMap ppKeyHash ppEpochNo retiring)
  , if lastepoch == EpochNo 0 then Nothing else Just ("last epoch", ppEpochNo lastepoch)
  , if Map.null prevBlocks then Nothing else Just ("prev blocks", ppMap ppKeyHash ppNatural prevBlocks)
  , if Map.null curBlocks then Nothing else Just ("current blocks", ppMap ppKeyHash ppNatural curBlocks)
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
