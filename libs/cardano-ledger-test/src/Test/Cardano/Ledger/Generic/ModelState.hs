{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin))
import Cardano.Ledger.Conway.State (ConwayEraCertState (..), VState (..), Accounts)
import Cardano.Ledger.Hashes (GenDelegs (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  ChainAccountState (..),
  DState (..),
  EpochState (..),
  InstantaneousRewards (..),
  LedgerState (..),
  NewEpochState (..),
  PState (..),
  StashedAVVMAddresses,
  UTxOState (..),
  completeRupd,
  curPParamsEpochStateL,
  prevPParamsEpochStateL,
  smartUTxOState,
 )
import Cardano.Ledger.Shelley.PoolRank (NonMyopic (..))
import Cardano.Ledger.Shelley.RewardUpdate (PulsingRewUpdate (..), RewardUpdate (..))
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Ledger.State (
  ChainAccountState (..),
  EraCertState (..),
  IndividualPoolStake (..),
  PoolDistr (..),
  SnapShots,
  UTxO (..),
  emptySnapShots,
 )
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Control.Monad.Trans ()
import Data.Default (Default (def))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.TreeDiff (Expr, ToExpr (toExpr))
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Extras (view)
import Test.Cardano.Ledger.Generic.Proof (
  BabbageEra,
  CertStateWit (..),
  Proof (..),
  Reflect (..),
  whichCertState,
 )
import Test.Cardano.Ledger.Shelley.Era
import Test.Cardano.Ledger.Shelley.Utils (runShelleyBase)

-- | MUtxo = Model UTxO. In the Model we represent the
--   UTxO as a Map (not a newtype around a Map)
type MUtxo era = Map TxIn (TxOut era)

-- ===========================================================

data ModelNewEpochState era = ModelNewEpochState
  { -- PState fields
    mPoolParams :: !(Map (KeyHash 'StakePool) PoolParams)
  , mPoolDeposits :: !(Map (KeyHash 'StakePool) Coin)
  , -- DState state fields
    mAccounts :: !(Accounts era)
  , --  _fGenDelegs,  _genDelegs, and _irwd, are for
    --  changing the PParams and are abstracted away

    -- UTxO state fields (and extra stuff)
    mUTxO :: !(Map TxIn (TxOut era))
  , mMutFee :: !(Map TxIn (TxOut era))
  -- ^ The current UTxO
  , -- _ppups is for changing PParams, and _stakeDistro is for efficiency
    -- and are abstracted away.

    -- EpochState fields
    mChainAccountState :: !ChainAccountState
  , -- esPrevPp and esPp are for changing PParams
    -- esNonMyopic is for efficiency, and all are abstracted away

    -- Model NewEpochState fields
    mPoolDistr :: !(Map (KeyHash 'StakePool) IndividualPoolStake)
  , mPParams :: !(PParams era)
  , mDeposited :: !Coin
  , mFees :: !Coin
  , mCount :: !Int
  , mIndex :: !(Map Int TxId)
  , -- below here NO EFFECT until we model EpochBoundary
    mFPoolParams :: !(Map (KeyHash 'StakePool) PoolParams)
  , mRetiring :: !(Map (KeyHash 'StakePool) EpochNo)
  , mSnapshots :: !SnapShots
  , mEL :: !EpochNo -- The current epoch,
  , mBprev :: !(Map (KeyHash 'StakePool) Natural) --  Blocks made before current epoch, NO EFFECT until we model EpochBoundar
  , mBcur :: !(Map (KeyHash 'StakePool) Natural)
  , mRu :: !(StrictMaybe RewardUpdate) -- Possible reward update
  }
  deriving (Generic)

instance ShelleyEraTest era => ToExpr (ModelNewEpochState era)

type UtxoEntry era = (TxIn, TxOut era)

type Model era = ModelNewEpochState era

-- ======================================================================
-- Empty or default values, these are usefull for many things, not the
-- least of, for making Model instances.

blocksMadeZero :: BlocksMade
blocksMadeZero = BlocksMade Map.empty

poolDistrZero :: PoolDistr
poolDistrZero = PoolDistr Map.empty $ CompactCoin 1

accountStateZero :: ChainAccountState
accountStateZero = ChainAccountState (Coin 0) (Coin 0)

utxoZero :: UTxO era
utxoZero = UTxO Map.empty

genDelegsZero :: GenDelegs
genDelegsZero = GenDelegs Map.empty

instantaneousRewardsZero :: InstantaneousRewards
instantaneousRewardsZero = InstantaneousRewards Map.empty Map.empty mempty mempty

dStateZero :: EraCertState era => DState era
dStateZero =
  DState
    { dsAccounts = def
    , dsFutureGenDelegs = Map.empty
    , dsGenDelegs = genDelegsZero
    , dsIRewards = instantaneousRewardsZero
    }

pStateZero :: PState era
pStateZero =
  PState
    { psStakePoolParams = Map.empty
    , psFutureStakePoolParams = Map.empty
    , psRetiring = Map.empty
    , psDeposits = Map.empty
    }

dPStateZero :: EraCertState era => CertState era
dPStateZero =
  def
    & certPStateL .~ pStateZero
    & certDStateL .~ dStateZero

nonMyopicZero :: NonMyopic
nonMyopicZero = NonMyopic Map.empty mempty

pParamsZeroByProof :: Proof era -> PParams era
pParamsZeroByProof Conway = def
pParamsZeroByProof Babbage = def
pParamsZeroByProof Alonzo = def
pParamsZeroByProof Mary = def
pParamsZeroByProof Allegra = def
pParamsZeroByProof Shelley = def

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
stashedAVVMAddressesZero Shelley = utxoZero
stashedAVVMAddressesZero Conway = ()
stashedAVVMAddressesZero Babbage = ()
stashedAVVMAddressesZero Alonzo = ()
stashedAVVMAddressesZero Mary = ()
stashedAVVMAddressesZero Allegra = ()

mNewEpochStateZero :: Reflect era => ModelNewEpochState era
mNewEpochStateZero =
  ModelNewEpochState
    { mPoolParams = Map.empty
    , mPoolDeposits = Map.empty
    , mAccounts = def
    , mUTxO = Map.empty
    , mMutFee = Map.empty
    , mChainAccountState = accountStateZero
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

testNES :: NewEpochState BabbageEra
testNES = newEpochStateZero

testMNES :: ModelNewEpochState BabbageEra
testMNES = mNewEpochStateZero

-- ======================================================================

class Extract t era where
  extract :: ModelNewEpochState era -> t

instance Extract (DState era) era where
  extract x =
    DState
      (mAccounts x)
      Map.empty
      genDelegsZero
      instantaneousRewardsZero

instance Extract (PState era) era where
  extract x = PState (mPoolParams x) (mFPoolParams x) (mRetiring x) Map.empty

instance Extract (VState era) era where
  extract _ = VState def def (EpochNo 0)

instance Reflect era => Extract (UTxOState era) era where
  extract x =
    smartUTxOState
      (mPParams x)
      (UTxO (mUTxO x))
      (mDeposited x)
      (mFees x)
      emptyGovState
      mempty

extractCertState ::
  forall era. Reflect era => ModelNewEpochState era -> CertState era
extractCertState x = case whichCertState (reify @era) of
  CertStateShelleyToBabbage ->
    def
      & (certPStateL .~ extract x)
      & (certDStateL .~ extract x)
  CertStateConwayToConway ->
    def
      & (certPStateL .~ extract x)
      & (certDStateL .~ extract x)
      & (certVStateL .~ extract x)

instance Reflect era => Extract (LedgerState era) era where
  extract x = LedgerState (extract x) (extractCertState x)

instance Reflect era => Extract (EpochState era) era where
  extract x =
    EpochState
      (mChainAccountState x)
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
      (PoolDistr (mPoolDistr x) (CompactCoin 1))
      (stashedAVVMAddressesZero (reify :: Proof era))

abstract :: (EraGov era, EraCertState era) => NewEpochState era -> ModelNewEpochState era
abstract x =
  ModelNewEpochState
    { mPoolParams = (psStakePoolParams . certPState . lsCertState . esLState . nesEs) x
    , mPoolDeposits = (psDeposits . certPState . lsCertState . esLState . nesEs) x
    , mAccounts = (dsAccounts . certDState . lsCertState . esLState . nesEs) x
    , mUTxO = (unUTxO . utxosUtxo . lsUTxOState . esLState . nesEs) x
    , mMutFee = Map.empty
    , mChainAccountState = (esChainAccountState . nesEs) x
    , mPoolDistr = (unPoolDistr . nesPd) x
    , mPParams = (view curPParamsEpochStateL . nesEs) x
    , mDeposited = (utxosDeposited . lsUTxOState . esLState . nesEs) x
    , mFees = (utxosFees . lsUTxOState . esLState . nesEs) x
    , mCount = 0
    , mIndex = Map.empty
    , -- below here NO EFFECT until we model EpochBoundary
      mFPoolParams = (psFutureStakePoolParams . certPState . lsCertState . esLState . nesEs) x
    , mRetiring = (psRetiring . certPState . lsCertState . esLState . nesEs) x
    , mSnapshots = esSnapshots (nesEs x)
    , mEL = nesEL x
    , mBprev = unBlocksMade (nesBprev x)
    , mBcur = unBlocksMade (nesBcur x)
    , mRu = case nesRu x of
        SNothing -> SNothing -- <- There is no way to complete (nesRu x) to get a RewardUpdate
        SJust pru -> SJust (complete pru)
    }
  where
    certPState certState = certState ^. certPStateL
    certDState certState = certState ^. certDStateL

complete :: PulsingRewUpdate -> RewardUpdate
complete (Complete r) = r
complete (Pulsing rewsnap pulser) = fst $ runShelleyBase (completeRupd (Pulsing rewsnap pulser))

-- =====================================================================

pcModelNewEpochState :: ShelleyEraTest era => ModelNewEpochState era -> Expr
pcModelNewEpochState = toExpr

-- SnapShots and PulsingRewUdate delberately ommitted from pretty printer

instance (Reflect era, ShelleyEraTest era) => Show (ModelNewEpochState era) where
  show = show . toExpr
