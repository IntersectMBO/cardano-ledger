{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Test.Cardano.Ledger.Shelley.Examples.Combinators
-- Description : Chain State Combinators
--
-- A collection of combinators for manipulating Chain State.
-- The idea is to provide a clear way of describing the
-- changes to the chain state when a block is processed.
module Test.Cardano.Ledger.Shelley.Examples.Combinators (
  evolveNonceFrozen,
  evolveNonceUnfrozen,
  newLab,
  feesAndKeyRefund,
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
  pulserUpdate,
  applyRewardUpdate,
  setPoolDistr,
  setOCertCounter,
  newSnapshot,
  incrBlockCount,
  newEpoch,
  setCurrentProposals,
  setFutureProposals,
  solidifyProposals,
  setPParams,
  setPrevPParams,
  setFutureGenDeleg,
  adoptFutureGenDeleg,
) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  Nonce (..),
  StrictMaybe (..),
  quorum,
  (⭒),
 )
import Cardano.Ledger.Block (Block (..), bheader)
import Cardano.Ledger.Coin (
  Coin (..),
  CompactForm (CompactCoin),
  addCompactCoin,
  compactCoinOrError,
 )
import Cardano.Ledger.Credential (Credential (..), Ptr)
import Cardano.Ledger.Hashes (GenDelegPair, GenDelegs (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  PulsingRewUpdate (..),
  RewardUpdate (..),
  UTxOState (..),
  applyRUpd,
  curPParamsEpochStateL,
  futurePParamsEpochStateL,
  prevPParamsEpochStateL,
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates)
import Cardano.Ledger.Shelley.Rules (emptyInstantaneousRewards, votedFuturePParams)
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.UMap (
  RDPair (..),
  UView (PtrUView, RewDepUView, SPoolUView),
  fromCompact,
  unUView,
 )
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.Val ((<+>), (<->), (<×>))
import Cardano.Protocol.TPraos.BHeader (
  BHBody (..),
  BHeader,
  LastAppliedBlock (..),
  bhHash,
  bhbody,
  lastAppliedHash,
  prevHashToNonce,
 )
import Cardano.Slotting.Slot (EpochNo, WithOrigin (..))
import Data.Foldable as F (fold, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word64)
import Lens.Micro ((%~), (&), (.~), (^.))
import Lens.Micro.Extras (view)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Rules.Chain (ChainState (..))
import Test.Cardano.Ledger.Shelley.Utils (epochFromSlotNo, getBlockNonce, testGlobals)

-- ======================================================

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
    { chainCandidateNonce = chainCandidateNonce cs ⭒ n
    , chainEvolvingNonce = chainEvolvingNonce cs ⭒ n
    }

-- | = New 'LastAppliedBlock' (*NOT* on epoch boundaries)
--
-- Update the chain state with the details of 'LastAppliedBlock'
-- that occur when a new block is processed.
-- Note: do not use this function when crossing the epoch boundary,
-- instead use 'newEpoch'.
newLab ::
  forall era.
  Block (BHeader MockCrypto) era ->
  ChainState era ->
  ChainState era
newLab b cs =
  cs {chainLastAppliedBlock = At $ LastAppliedBlock bn sn (bhHash bh)}
  where
    bh = bheader b
    bn = bheaderBlockNo $ bhbody bh
    sn = bheaderSlotNo $ bhbody bh

-- | = Update Fees and Deposits
--
-- Update the fee pot and deposit pot with the new fees and deposits
-- adjust the deposit tables in the UTxOState and the CertState.
-- Notes
--   1) do not give this function duplicates in the 'stakes' or 'pools' inputs.
--   2) do not use this function when crossing the epoch boundary,
-- instead use 'newEpoch'.
feesAndDeposits ::
  forall era.
  (EraPParams era, EraCertState era) =>
  PParams era ->
  Coin ->
  [Credential 'Staking] ->
  [PoolParams] ->
  ChainState era ->
  ChainState era
feesAndDeposits ppEx newFees stakes pools cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    certState = lsCertState ls
    pstate = certState ^. certPStateL
    dstate = certState ^. certDStateL
    utxoSt = lsUTxOState ls
    utxoSt' =
      utxoSt
        { utxosDeposited =
            utxosDeposited utxoSt
              <+> (length stakes <×> ppEx ^. ppKeyDepositL)
              <+> (newcount <×> ppEx ^. ppPoolDepositL)
        , utxosFees = utxosFees utxoSt <+> newFees
        }
    ls' = ls {lsUTxOState = utxoSt', lsCertState = dpstate'}
    -- Count the number of new pools, because we don't take a deposit for existing pools
    -- This strategy DOES NOT WORK if there are duplicate PoolParams in one call
    newcount = F.foldl' accum 0 pools
    accum n x = if Map.member (ppId x) (psDeposits pstate) then (n :: Integer) else n + 1
    newDeposits =
      Map.fromList (map (\cred -> (cred, compactCoinOrError (ppEx ^. ppKeyDepositL))) stakes)
    newPools = Map.fromList (map (\p -> (ppId p, ppEx ^. ppPoolDepositL)) pools)
    dpstate' =
      mkShelleyCertState
        (pstate & psDepositsL %~ Map.unionWith (\old _new -> old) newPools)
        (dstate & dsUnifiedL .~ UM.unionKeyDeposits (RewDepUView (dstate ^. dsUnifiedL)) newDeposits)
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

feesAndKeyRefund ::
  forall era.
  EraCertState era =>
  Coin ->
  Credential 'Staking ->
  ChainState era ->
  ChainState era
feesAndKeyRefund newFees key cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    certState = lsCertState ls
    dstate = certState ^. certDStateL
    refund = case UM.lookup key (RewDepUView (dsUnified dstate)) of
      Nothing -> Coin 0
      Just (RDPair _ ccoin) -> fromCompact ccoin
    utxoSt = lsUTxOState ls
    utxoSt' =
      utxoSt
        { utxosDeposited = utxosDeposited utxoSt <-> refund
        , utxosFees = utxosFees utxoSt <+> newFees
        }
    ls' = ls {lsUTxOState = utxoSt', lsCertState = dpstate'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}
    dpstate' = certState & certDStateL . dsUnifiedL %~ (UM.adjust zeroD key . RewDepUView)
    zeroD (RDPair x _) = RDPair x (CompactCoin 0)

-- | = Update the UTxO
--
-- Update the UTxO for given transaction body.
newUTxO ::
  forall era.
  (EraTx era, EraStake era) =>
  TxBody era ->
  ChainState era ->
  ChainState era
newUTxO txb cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    utxoSt = lsUTxOState ls
    utxo = unUTxO $ utxosUtxo utxoSt
    utxoAdd = txouts @era txb
    utxoToDel = Map.restrictKeys utxo (txins @era txb)
    utxoWithout = Map.withoutKeys utxo (txins @era txb)
    utxoDel = UTxO utxoToDel
    utxo' = UTxO (utxoWithout `Map.union` unUTxO utxoAdd)
    is' = deleteInstantStake utxoDel (addInstantStake utxoAdd (utxoSt ^. instantStakeL))
    utxoSt' = utxoSt {utxosUtxo = utxo', utxosInstantStake = is'}
    ls' = ls {lsUTxOState = utxoSt'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = New Stake Credential
--
--   Add a newly registered stake credential, initialize the rdRewards component of the RDPair.
--   The rdDeposit component of the RDPair is set by 'feesAndDeposits'
newStakeCred ::
  forall era.
  EraCertState era =>
  Credential 'Staking ->
  Ptr ->
  ChainState era ->
  ChainState era
newStakeCred cred ptr cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = lsCertState ls
    ds = dps ^. certDStateL
    ds' =
      ds
        { dsUnified =
            let um0 = dsUnified ds
                um1 = UM.insert cred (UM.RDPair (CompactCoin 0) (CompactCoin 0)) (RewDepUView um0)
                um2 = (PtrUView um1 UM.∪ (ptr, cred))
             in um2
        }
    dps' = dps & certDStateL .~ ds'
    ls' = ls {lsCertState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = De-Register Stake Credential
--
-- De-register a stake credential and all associated data.
-- Be sure to run 'feesAndKeyRefund' before you run this
-- because this throws away the stored refund, which then
-- can't be used to balance the utxosDeposited field in 'feesAndKeyRefund'
deregStakeCred ::
  forall era.
  EraCertState era =>
  Credential 'Staking ->
  ChainState era ->
  ChainState era
deregStakeCred cred cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = lsCertState ls
    ds = dps ^. certDStateL
    ds' =
      ds
        { dsUnified =
            let um0 = dsUnified ds
                um1 = UM.delete cred (RewDepUView um0)
                um2 = PtrUView um1 UM.⋫ Set.singleton cred
                um3 = UM.delete cred (SPoolUView um2)
             in um3
        }
    dps' = dps & certDStateL .~ ds'
    ls' = ls {lsCertState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = New Delegation
--
-- Create a delegation from the given stake credential to the given
-- stake pool.
delegation ::
  forall era.
  EraCertState era =>
  Credential 'Staking ->
  KeyHash 'StakePool ->
  ChainState era ->
  ChainState era
delegation cred pool cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = lsCertState ls
    ds = dps ^. certDStateL
    ds' =
      ds
        { dsUnified = UM.insert cred pool (delegations ds)
        }
    dps' = dps & certDStateL .~ ds'
    ls' = ls {lsCertState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = New Stake Pool
--
-- Add a newly registered stake pool
newPool ::
  forall era.
  EraCertState era =>
  PoolParams ->
  ChainState era ->
  ChainState era
newPool pool cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = lsCertState ls
    ps = dps ^. certPStateL
    ps' =
      ps
        { psStakePoolParams = Map.insert (ppId pool) pool (psStakePoolParams ps)
        }
    dps' = dps & certPStateL .~ ps'
    ls' = ls {lsCertState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Re-Register Stake Pool
reregPool ::
  forall era.
  EraCertState era =>
  PoolParams ->
  ChainState era ->
  ChainState era
reregPool pool cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = lsCertState ls
    ps = dps ^. certPStateL
    ps' =
      ps
        { psFutureStakePoolParams = Map.insert (ppId pool) pool (psStakePoolParams ps)
        }
    dps' = dps & certPStateL .~ ps'
    ls' = ls {lsCertState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Re-Register Stake Pool
updatePoolParams ::
  forall era.
  EraCertState era =>
  PoolParams ->
  ChainState era ->
  ChainState era
updatePoolParams pool cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = lsCertState ls
    ps = dps ^. certPStateL
    ps' =
      ps
        { psStakePoolParams = Map.insert (ppId pool) pool (psStakePoolParams ps)
        , psFutureStakePoolParams = Map.delete (ppId pool) (psStakePoolParams ps)
        }
    dps' = dps & certPStateL .~ ps'
    ls' = ls {lsCertState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Pool Retirement
--
-- Stage a stake pool for retirement.
stageRetirement ::
  forall era.
  EraCertState era =>
  KeyHash 'StakePool ->
  EpochNo ->
  ChainState era ->
  ChainState era
stageRetirement kh e cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = lsCertState ls
    ps = dps ^. certPStateL
    ps' = ps {psRetiring = Map.insert kh e (psRetiring ps)}
    dps' = dps & certPStateL .~ ps'
    ls' = ls {lsCertState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Reap Pool
--
-- Remove a stake pool.
reapPool ::
  forall era.
  (EraGov era, EraCertState era) =>
  PoolParams ->
  ChainState era ->
  ChainState era
reapPool pool cs = cs {chainNes = nes'}
  where
    kh = ppId pool
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = lsCertState ls
    ps = dps ^. certPStateL
    ps' =
      ps
        { psRetiring = Map.delete kh (psRetiring ps)
        , psStakePoolParams = Map.delete kh (psStakePoolParams ps)
        , psDeposits = Map.delete kh (psDeposits ps)
        }
    pp = es ^. curPParamsEpochStateL
    ds = dps ^. certDStateL
    RewardAccount _ rewardAddr = ppRewardAccount pool
    (rewards', unclaimed) =
      case UM.lookup rewardAddr (rewards ds) of
        Nothing -> (rewards ds, pp ^. ppPoolDepositL)
        Just (UM.RDPair ccoin dep) ->
          ( UM.insert'
              rewardAddr
              (UM.RDPair (addCompactCoin ccoin (compactCoinOrError (pp ^. ppPoolDepositL))) dep)
              (rewards ds)
          , Coin 0
          )
    -- FIXME shouldn't we look up the pooldeposit here?
    umap1 = unUView rewards'
    umap2 = UM.SPoolUView umap1 UM.⋫ Set.singleton kh
    ds' = ds {dsUnified = umap2}
    chainAccountState = esChainAccountState es
    chainAccountState' = chainAccountState {casTreasury = casTreasury chainAccountState <+> unclaimed}
    utxoSt = lsUTxOState ls
    utxoSt' = utxoSt {utxosDeposited = utxosDeposited utxoSt <-> (pp ^. ppPoolDepositL)}
    dps' =
      dps
        & certPStateL .~ ps'
        & certDStateL .~ ds'
    ls' = ls {lsCertState = dps', lsUTxOState = utxoSt'}
    es' = es {esLState = ls', esChainAccountState = chainAccountState'}
    nes' = nes {nesEs = es'}

-- | = MIR
--
-- Add a credential to the MIR mapping for the given pot (reserves or treasury)
mir ::
  forall era.
  EraCertState era =>
  Credential 'Staking ->
  MIRPot ->
  Coin ->
  ChainState era ->
  ChainState era
mir cred pot amnt cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = lsCertState ls
    ds = dps ^. certDStateL
    InstantaneousRewards
      { iRReserves = ir
      , iRTreasury = it
      , deltaReserves = dr
      , deltaTreasury = dt
      } = dsIRewards ds
    irwd' = case pot of
      ReservesMIR -> InstantaneousRewards (Map.insert cred amnt ir) it dr dt
      TreasuryMIR -> InstantaneousRewards ir (Map.insert cred amnt it) dr dt
    ds' = ds {dsIRewards = irwd'}
    dps' = dps & certDStateL .~ ds'
    ls' = ls {lsCertState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Apply MIR
--
-- On the epoch boundary, reset the MIR mappings and augment the rewards.
applyMIR ::
  forall era.
  EraCertState era =>
  MIRPot ->
  Map (Credential 'Staking) Coin ->
  ChainState era ->
  ChainState era
applyMIR pot newrewards cs = cs {chainNes = nes'}
  where
    tot = fold newrewards
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = lsCertState ls
    ds = dps ^. certDStateL
    ds' =
      ds
        { dsUnified = rewards ds UM.∪+ Map.map compactCoinOrError newrewards
        , dsIRewards = emptyInstantaneousRewards
        }
    dps' = dps & certDStateL .~ ds'
    ls' = ls {lsCertState = dps'}
    chainAccountState = esChainAccountState es
    chainAccountState' =
      if pot == ReservesMIR
        then chainAccountState {casReserves = casReserves chainAccountState <-> tot}
        else chainAccountState {casTreasury = casTreasury chainAccountState <-> tot}
    es' = es {esChainAccountState = chainAccountState', esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Reward Update
--
-- Update the chain state with the given reward update
rewardUpdate ::
  forall era.
  RewardUpdate ->
  ChainState era ->
  ChainState era
rewardUpdate ru cs = cs {chainNes = nes'}
  where
    nes' = (chainNes cs) {nesRu = SJust (Complete ru)}

-- | = Pulser
--
-- Update the chain state with the given reward update pulser
pulserUpdate ::
  forall era.
  PulsingRewUpdate ->
  ChainState era ->
  ChainState era
pulserUpdate p cs = cs {chainNes = nes'}
  where
    nes' = (chainNes cs) {nesRu = SJust p}

-- | = Apply a Reward Update
--
-- Apply the given reward update to the chain state
applyRewardUpdate ::
  forall era.
  (EraGov era, EraCertState era) =>
  RewardUpdate ->
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
  SnapShot ->
  Coin ->
  ChainState era ->
  ChainState era
newSnapshot snap fee cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    SnapShots
      { ssStakeMark = ssMark
      , ssStakeSet = ssSet
      } = esSnapshots es
    snaps =
      SnapShots
        { ssStakeMark = snap
        , ssStakeMarkPoolDistr = calculatePoolDistr snap
        , ssStakeSet = ssMark
        , ssStakeGo = ssSet
        , ssFee = fee
        }
    es' = es {esSnapshots = snaps}
    nes' = nes {nesEs = es'}

-- | = Set Pool Distribution
--
-- Set the stake pool distribution to the given one.
setPoolDistr ::
  forall era.
  PoolDistr ->
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
  KeyHash 'BlockIssuer ->
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
  KeyHash 'StakePool ->
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
  (ProtVerAtMost era 6, EraGov era) =>
  Block (BHeader MockCrypto) era ->
  ChainState era ->
  ChainState era
newEpoch b cs = cs'
  where
    ChainState
      { chainNes = nes
      , chainEvolvingNonce = evNonce
      , chainCandidateNonce = cNonce
      , chainPrevEpochNonce = pNonce
      , chainLastAppliedBlock = lab
      } = cs
    bh = bheader b
    bn = bheaderBlockNo . bhbody $ bh
    sn = bheaderSlotNo . bhbody $ bh
    pp = view curPParamsEpochStateL . nesEs $ nes
    e = epochFromSlotNo . bheaderSlotNo . bhbody . bheader $ b
    nes' =
      nes
        { nesEL = e
        , nesBprev = nesBcur nes
        , nesBcur = BlocksMade Map.empty
        }
    n = getBlockNonce b
    cs' =
      cs
        { chainNes = nes'
        , chainEpochNonce = cNonce ⭒ pNonce ⭒ (pp ^. ppExtraEntropyL)
        , chainEvolvingNonce = evNonce ⭒ n
        , chainCandidateNonce = evNonce ⭒ n
        , chainPrevEpochNonce = prevHashToNonce . lastAppliedHash $ lab
        , chainLastAppliedBlock = At $ LastAppliedBlock bn sn (bhHash bh)
        }

-- | = Set Current Proposals
--
-- Set the current protocol parameter proposals.
setCurrentProposals ::
  forall era.
  (GovState era ~ ShelleyGovState era, EraPParams era) =>
  ProposedPPUpdates era ->
  ChainState era ->
  ChainState era
setCurrentProposals ps cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    utxoSt = lsUTxOState ls
    govState = utxosGovState utxoSt
    pp = sgsCurPParams govState
    govState' =
      govState
        { sgsCurProposals = ps
        , sgsFuturePParams =
            PotentialPParamsUpdate $ votedFuturePParams ps pp (quorum testGlobals)
        }
    utxoSt' = utxoSt {utxosGovState = govState'}
    ls' = ls {lsUTxOState = utxoSt'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Set Future Proposals
--
-- Set the future protocol parameter proposals.
setFutureProposals ::
  forall era.
  GovState era ~ ShelleyGovState era =>
  ProposedPPUpdates era ->
  ChainState era ->
  ChainState era
setFutureProposals ps cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    utxoSt = lsUTxOState ls
    govState = utxosGovState utxoSt
    govState' = govState {sgsFutureProposals = ps}
    utxoSt' = utxoSt {utxosGovState = govState'}
    ls' = ls {lsUTxOState = utxoSt'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

solidifyProposals ::
  forall era.
  EraGov era =>
  ChainState era ->
  ChainState era
solidifyProposals cs = cs {chainNes = nes {nesEs = es}}
  where
    nes = chainNes cs
    es = nesEs nes & futurePParamsEpochStateL %~ solidifyFuturePParams

-- | = Set the Protocol Proposals
--
-- Set the protocol parameters.
setPParams ::
  forall era.
  EraGov era =>
  PParams era ->
  ChainState era ->
  ChainState era
setPParams pp cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    es' = es & curPParamsEpochStateL .~ pp
    nes' = nes {nesEs = es'}

-- | = Set the Previous Protocol Proposals
--
-- Set the previous protocol parameters.
setPrevPParams ::
  forall era.
  EraGov era =>
  PParams era ->
  ChainState era ->
  ChainState era
setPrevPParams pp cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    es' = es & prevPParamsEpochStateL .~ pp
    nes' = nes {nesEs = es'}

-- | = Set a future genesis delegation.
setFutureGenDeleg ::
  forall era.
  EraCertState era =>
  (FutureGenDeleg, GenDelegPair) ->
  ChainState era ->
  ChainState era
setFutureGenDeleg (fg, gd) cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = lsCertState ls
    ds = dps ^. certDStateL
    ds' = ds {dsFutureGenDelegs = Map.insert fg gd (dsFutureGenDelegs ds)}
    dps' = dps & certDStateL .~ ds'
    ls' = ls {lsCertState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

-- | = Set a future genesis delegation.
adoptFutureGenDeleg ::
  forall era.
  EraCertState era =>
  (FutureGenDeleg, GenDelegPair) ->
  ChainState era ->
  ChainState era
adoptFutureGenDeleg (fg, gd) cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = lsCertState ls
    ds = dps ^. certDStateL
    gds = GenDelegs $ Map.insert (fGenDelegGenKeyHash fg) gd (unGenDelegs (dsGenDelegs ds))
    ds' =
      ds
        { dsFutureGenDelegs = Map.delete fg (dsFutureGenDelegs ds)
        , dsGenDelegs = gds
        }
    dps' = dps & certDStateL .~ ds'
    ls' = ls {lsCertState = dps'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}
