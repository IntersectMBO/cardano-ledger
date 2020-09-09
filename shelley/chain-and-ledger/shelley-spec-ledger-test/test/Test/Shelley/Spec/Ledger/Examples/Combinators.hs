{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Test.Shelley.Spec.Ledger.Examples.Combinators
-- Description : Chain State Combinators
--
-- A collection of combinators for manipulating Chain State.
-- The idea is to provide a clear way of describing the
-- changes to the chain state when a block is processed.
module Test.Shelley.Spec.Ledger.Examples.Combinators
  ( evolveNonceFrozen,
    evolveNonceUnfrozen,
    newLab,
    feesAndDeposits,
    newUTxO,
    newStakeCred,
    deregStakeCred,
    delegation,
    newPool,
    reregPool,
    updatePoolParams,
    stageRetirement,
    reapPool,
    mir,
    applyMIR,
    rewardUpdate,
    applyRewardUpdate,
    setPoolDistr,
    setOCertCounter,
    newSnapshot,
    incrBlockCount,
    newEpoch,
    setCurrentProposals,
    setFutureProposals,
    setPParams,
    setFutureGenDeleg,
    adoptFutureGenDeleg,
  )
where

import Cardano.Ledger.Era (Era)
import Cardano.Slotting.Slot (EpochNo, WithOrigin (..))
import Control.Iterate.SetAlgebra (eval, setSingleton, singleton, (∪), (⋪), (⋫))
import Data.Foldable (fold)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word64)
import Shelley.Spec.Ledger.BaseTypes (Nonce (..), StrictMaybe (..), (⭒))
import Shelley.Spec.Ledger.BlockChain
  ( BHBody (..),
    Block (..),
    LastAppliedBlock (..),
    bhHash,
    bhbody,
    bheader,
    lastAppliedHash,
    prevHashToNonce,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential
  ( Credential (..),
    Ptr,
  )
import Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr (..))
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..), SnapShot, SnapShots (..))
import Shelley.Spec.Ledger.Keys
  ( GenDelegPair,
    GenDelegs (..),
    KeyHash,
    KeyRole (..),
  )
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    DPState (..),
    DState (..),
    EpochState (..),
    FutureGenDeleg (..),
    InstantaneousRewards (..),
    LedgerState (..),
    NewEpochState (..),
    PPUPState (..),
    PState (..),
    RewardUpdate (..),
    UTxOState (..),
    applyRUpd,
    emptyInstantaneousRewards,
  )
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..), ProposedPPUpdates)
import Shelley.Spec.Ledger.STS.Chain (ChainState (..))
import Shelley.Spec.Ledger.TxBody (MIRPot (..), PoolParams (..), RewardAcnt (..), TxBody (..))
import Shelley.Spec.Ledger.UTxO (txins, txouts)
import qualified Cardano.Ledger.Val as Val
import Test.Shelley.Spec.Ledger.Examples.Federation (overlayScheduleFor)
import Test.Shelley.Spec.Ledger.Utils (epochFromSlotNo, getBlockNonce)

-- | = Evolve Nonces - Frozen
--
-- Evolve the appropriate nonces under the assumption
-- that the candidate nonce is now frozen.
evolveNonceFrozen :: forall era. Nonce -> ChainState era -> ChainState era
evolveNonceFrozen n cs = cs {chainEvolvingNonce = chainEvolvingNonce cs ⭒ n}

-- | = Evolve Nonces - Unfrozen
--
-- Evolve the appropriate nonces under the assumption
-- that the candidate nonce is not frozen.
-- Note: do not use this function when crossing the epoch boundary,
-- instead use 'newEpoch'.
evolveNonceUnfrozen :: forall era. Nonce -> ChainState era -> ChainState era
evolveNonceUnfrozen n cs =
  cs
    { chainCandidateNonce = chainCandidateNonce cs ⭒ n,
      chainEvolvingNonce = chainEvolvingNonce cs ⭒ n
    }

-- | = New 'LastAppliedBlock' (*NOT* on epoch boundaries)
--
-- Update the chain state with the details of 'LastAppliedBlock'
-- that occur when a new block is processed.
-- Note: do not use this function when crossing the epoch boundary,
-- instead use 'newEpoch'.
newLab ::
  forall era.
  Era era =>
  Block era ->
  ChainState era ->
  ChainState era
newLab b cs =
  cs {chainLastAppliedBlock = At $ LastAppliedBlock bn sn (bhHash bh)}
  where
    bh = bheader b
    bn = bheaderBlockNo . bhbody $ bh
    sn = bheaderSlotNo . bhbody $ bh

-- | = Update Fees and Deposits
--
-- Update the fee pot and deposit pot with the new fees
-- and the change to the deposit pot.
-- Note: do not use this function when crossing the epoch boundary,
-- instead use 'newEpoch'.
feesAndDeposits ::
  forall era.
  Coin ->
  Coin ->
  ChainState era ->
  ChainState era
feesAndDeposits newFees depositChange cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    utxoSt = _utxoState ls
    utxoSt' =
      utxoSt
        { _deposited = (_deposited utxoSt) <> depositChange,
          _fees = (_fees utxoSt) <> newFees
        }
    ls' = ls {_utxoState = utxoSt'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Update the UTxO
--
-- Update the UTxO for given transaction body.
newUTxO ::
  forall era.
  Era era =>
  TxBody era ->
  ChainState era ->
  ChainState era
newUTxO txb cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    utxoSt = _utxoState ls
    utxo = _utxo utxoSt
    utxo' = eval ((txins txb ⋪ utxo) ∪ txouts txb)
    utxoSt' = utxoSt {_utxo = utxo'}
    ls' = ls {_utxoState = utxoSt'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = New Stake Credential
--
-- Add a newly registered stake credential
newStakeCred ::
  forall era.
  Credential 'Staking era ->
  Ptr ->
  ChainState era ->
  ChainState era
newStakeCred cred ptr cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = _delegationState ls
    ds = _dstate dps
    ds' =
      ds
        { _rewards = Map.insert cred (Coin 0) (_rewards ds),
          _ptrs = eval (_ptrs ds ∪ (singleton ptr cred))
        }
    dps' = dps {_dstate = ds'}
    ls' = ls {_delegationState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = De-Register Stake Credential
--
-- De-register a stake credential and all associated data.
deregStakeCred ::
  forall era.
  Credential 'Staking era ->
  ChainState era ->
  ChainState era
deregStakeCred cred cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = _delegationState ls
    ds = _dstate dps
    ds' =
      ds
        { _rewards = Map.delete cred (_rewards ds),
          _ptrs = eval (_ptrs ds ⋫ setSingleton cred),
          _delegations = Map.delete cred (_delegations ds)
        }
    dps' = dps {_dstate = ds'}
    ls' = ls {_delegationState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = New Delegation
--
-- Create a delegation from the given stake credential to the given
-- stake pool.
delegation ::
  forall era.
  Credential 'Staking era ->
  KeyHash 'StakePool era ->
  ChainState era ->
  ChainState era
delegation cred pool cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = _delegationState ls
    ds = _dstate dps
    ds' =
      ds
        { _delegations = Map.insert cred pool (_delegations ds)
        }
    dps' = dps {_dstate = ds'}
    ls' = ls {_delegationState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = New Stake Pool
--
-- Add a newly registered stake pool
newPool ::
  forall era.
  PoolParams era ->
  ChainState era ->
  ChainState era
newPool pool cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = _delegationState ls
    ps = _pstate dps
    ps' =
      ps
        { _pParams = Map.insert (_poolPubKey pool) pool (_pParams ps)
        }
    dps' = dps {_pstate = ps'}
    ls' = ls {_delegationState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Re-Register Stake Pool
reregPool ::
  forall era.
  PoolParams era ->
  ChainState era ->
  ChainState era
reregPool pool cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = _delegationState ls
    ps = _pstate dps
    ps' =
      ps
        { _fPParams = Map.insert (_poolPubKey pool) pool (_pParams ps)
        }
    dps' = dps {_pstate = ps'}
    ls' = ls {_delegationState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Re-Register Stake Pool
updatePoolParams ::
  forall era.
  PoolParams era ->
  ChainState era ->
  ChainState era
updatePoolParams pool cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = _delegationState ls
    ps = _pstate dps
    ps' =
      ps
        { _pParams = Map.insert (_poolPubKey pool) pool (_pParams ps),
          _fPParams = Map.delete (_poolPubKey pool) (_pParams ps)
        }
    dps' = dps {_pstate = ps'}
    ls' = ls {_delegationState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Pool Retirement
--
-- Stage a stake pool for retirement.
stageRetirement ::
  forall era.
  KeyHash 'StakePool era ->
  EpochNo ->
  ChainState era ->
  ChainState era
stageRetirement kh e cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = _delegationState ls
    ps = _pstate dps
    ps' = ps {_retiring = Map.insert kh e (_retiring ps)}
    dps' = dps {_pstate = ps'}
    ls' = ls {_delegationState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Reap Pool
--
-- Remove a stake pool.
reapPool ::
  forall era.
  PoolParams era ->
  ChainState era ->
  ChainState era
reapPool pool cs = cs {chainNes = nes'}
  where
    kh = _poolPubKey pool
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = _delegationState ls
    ps = _pstate dps
    ps' =
      ps
        { _retiring = Map.delete kh (_retiring ps),
          _pParams = Map.delete kh (_pParams ps)
        }
    pp = esPp es
    ds = _dstate dps
    RewardAcnt _ rewardAddr = _poolRAcnt pool
    (rewards, unclaimed) =
      case Map.lookup rewardAddr (_rewards ds) of
        Nothing -> (_rewards ds, _poolDeposit pp)
        Just era -> (Map.insert rewardAddr (era <> _poolDeposit pp) (_rewards ds), Coin 0)
    ds' =
      ds
        { _delegations = eval (_delegations ds ⋫ Set.singleton kh),
          _rewards = rewards
        }
    as = esAccountState es
    as' = as {_treasury = (_treasury as) <> unclaimed}
    utxoSt = _utxoState ls
    utxoSt' = utxoSt {_deposited = (_deposited utxoSt) Val.~~ (_poolDeposit pp)}
    dps' = dps {_pstate = ps', _dstate = ds'}
    ls' = ls {_delegationState = dps', _utxoState = utxoSt'}
    es' = es {esLState = ls', esAccountState = as'}
    nes' = nes {nesEs = es'}

-- | = MIR
--
-- Add a credential to the MIR mapping for the given pot (reserves or treasury)
mir ::
  forall era.
  Credential 'Staking era ->
  MIRPot ->
  Coin ->
  ChainState era ->
  ChainState era
mir cred pot amnt cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = _delegationState ls
    ds = _dstate dps
    InstantaneousRewards
      { iRReserves = ir,
        iRTreasury = it
      } = _irwd ds
    irwd' = case pot of
      ReservesMIR -> InstantaneousRewards (Map.insert cred amnt ir) it
      TreasuryMIR -> InstantaneousRewards ir (Map.insert cred amnt it)
    ds' = ds {_irwd = irwd'}
    dps' = dps {_dstate = ds'}
    ls' = ls {_delegationState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Apply MIR
--
-- On the epoch boundary, reset the MIR mappings and augment the rewards.
applyMIR ::
  forall era.
  MIRPot ->
  Map (Credential 'Staking era) Coin ->
  ChainState era ->
  ChainState era
applyMIR pot rewards cs = cs {chainNes = nes'}
  where
    tot = fold rewards
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = _delegationState ls
    ds = _dstate dps
    ds' =
      ds
        { _rewards = Map.unionWith (<>) rewards (_rewards ds),
          _irwd = emptyInstantaneousRewards
        }
    dps' = dps {_dstate = ds'}
    ls' = ls {_delegationState = dps'}
    as = esAccountState es
    as' =
      if pot == ReservesMIR
        then as {_reserves = (_reserves as) Val.~~ tot}
        else as {_treasury = (_treasury as) Val.~~ tot}
    es' = es {esAccountState = as', esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Reward Update
--
-- Update the chain state with the given reward update
rewardUpdate ::
  forall era.
  RewardUpdate era ->
  ChainState era ->
  ChainState era
rewardUpdate ru cs = cs {chainNes = nes'}
  where
    nes' = (chainNes cs) {nesRu = SJust ru}

-- | = Apply a Reward Update
--
-- Apply the given reward update to the chain state
applyRewardUpdate ::
  forall era.
  RewardUpdate era ->
  ChainState era ->
  ChainState era
applyRewardUpdate ru cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es' = applyRUpd ru (nesEs nes)
    nes' = (chainNes cs) {nesEs = es', nesRu = SNothing}

-- | = New Snapshot
--
-- Add a new snapshot and rotate the others
newSnapshot ::
  forall era.
  SnapShot era ->
  Coin ->
  ChainState era ->
  ChainState era
newSnapshot snap fee cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    SnapShots
      { _pstakeMark = ssMark,
        _pstakeSet = ssSet
      } = esSnapshots es
    snaps =
      SnapShots
        { _pstakeMark = snap,
          _pstakeSet = ssMark,
          _pstakeGo = ssSet,
          _feeSS = fee
        }
    es' = es {esSnapshots = snaps}
    nes' = nes {nesEs = es'}

-- | = Set Pool Distribution
--
-- Set the stake pool distribution to the given one.
setPoolDistr ::
  forall era.
  PoolDistr era ->
  ChainState era ->
  ChainState era
setPoolDistr pd cs = cs {chainNes = nes'}
  where
    nes' = (chainNes cs) {nesPd = pd}

-- | = Set Operation Certificate Counter
--
-- Set the operational certificates counter for a given stake pool.
setOCertCounter ::
  forall era.
  KeyHash 'BlockIssuer era ->
  Word64 ->
  ChainState era ->
  ChainState era
setOCertCounter kh n cs = cs {chainOCertIssue = counters}
  where
    counters = Map.insert kh n (chainOCertIssue cs)

-- | = Increase Block Count
--
-- Record that the given stake pool (non-core node) produced a block.
incrBlockCount ::
  forall era.
  KeyHash 'StakePool era ->
  ChainState era ->
  ChainState era
incrBlockCount kh cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    BlocksMade bs = nesBcur nes
    n = 1 + Map.findWithDefault 0 kh bs
    bs' = BlocksMade $ Map.insert kh n bs
    nes' = nes {nesBcur = bs'}

-- | = New Epoch
--
-- Update the new epoch number, set the nonces, set the last applied block,
-- and reset blocks made.
-- Note: This function subsumes the manipulations done by
-- 'newLab', 'evolveNonceUnfrozen', and 'evolveNonceFrozen'.
newEpoch ::
  forall era.
  Era era =>
  Block era ->
  ChainState era ->
  ChainState era
newEpoch b cs = cs'
  where
    ChainState
      { chainNes = nes,
        chainEvolvingNonce = evNonce,
        chainCandidateNonce = cNonce,
        chainPrevEpochNonce = pNonce,
        chainLastAppliedBlock = lab
      } = cs
    bh = bheader b
    bn = bheaderBlockNo . bhbody $ bh
    sn = bheaderSlotNo . bhbody $ bh
    pp = esPp . nesEs $ nes
    e = epochFromSlotNo . bheaderSlotNo . bhbody . bheader $ b
    nes' =
      nes
        { nesEL = e,
          nesOsched = overlayScheduleFor e pp,
          nesBprev = (nesBcur nes),
          nesBcur = BlocksMade Map.empty
        }
    n = getBlockNonce b
    cs' =
      cs
        { chainNes = nes',
          chainEpochNonce = cNonce ⭒ pNonce ⭒ _extraEntropy pp,
          chainEvolvingNonce = evNonce ⭒ n,
          chainCandidateNonce = evNonce ⭒ n,
          chainPrevEpochNonce = prevHashToNonce . lastAppliedHash $ lab,
          chainLastAppliedBlock = At $ LastAppliedBlock bn sn (bhHash bh)
        }

-- | = Set Current Proposals
--
-- Set the current protocol parameter proposals.
setCurrentProposals ::
  forall era.
  ProposedPPUpdates era ->
  ChainState era ->
  ChainState era
setCurrentProposals ps cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    utxoSt = _utxoState ls
    ppupSt = _ppups utxoSt
    ppupSt' = ppupSt {proposals = ps}
    utxoSt' = utxoSt {_ppups = ppupSt'}
    ls' = ls {_utxoState = utxoSt'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Set Future Proposals
--
-- Set the future protocol parameter proposals.
setFutureProposals ::
  forall era.
  ProposedPPUpdates era ->
  ChainState era ->
  ChainState era
setFutureProposals ps cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    utxoSt = _utxoState ls
    ppupSt = _ppups utxoSt
    ppupSt' = ppupSt {futureProposals = ps}
    utxoSt' = utxoSt {_ppups = ppupSt'}
    ls' = ls {_utxoState = utxoSt'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Set the Protocol Proposals
--
-- Set the protocol parameters.
setPParams ::
  forall era.
  PParams era ->
  ChainState era ->
  ChainState era
setPParams pp cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    es' = es {esPp = pp}
    nes' = nes {nesEs = es'}

-- | = Set a future genesis delegation.
setFutureGenDeleg ::
  forall era.
  (FutureGenDeleg era, GenDelegPair era) ->
  ChainState era ->
  ChainState era
setFutureGenDeleg (fg, gd) cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = _delegationState ls
    ds = _dstate dps
    ds' = ds {_fGenDelegs = Map.insert fg gd (_fGenDelegs ds)}
    dps' = dps {_dstate = ds'}
    ls' = ls {_delegationState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Set a future genesis delegation.
adoptFutureGenDeleg ::
  forall era.
  (FutureGenDeleg era, GenDelegPair era) ->
  ChainState era ->
  ChainState era
adoptFutureGenDeleg (fg, gd) cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = _delegationState ls
    ds = _dstate dps
    gds = GenDelegs $ (Map.insert (fGenDelegGenKeyHash fg) gd (unGenDelegs (_genDelegs ds)))
    ds' =
      ds
        { _fGenDelegs = Map.delete fg (_fGenDelegs ds),
          _genDelegs = gds
        }
    dps' = dps {_dstate = ds'}
    ls' = ls {_delegationState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}
