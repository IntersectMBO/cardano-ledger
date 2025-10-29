{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | A collection of combinators for manipulating Chain State.
-- The idea is to provide a clear way of describing the
-- changes to the chain state when a block is processed.
module Test.Cardano.Ledger.Shelley.Examples.Combinators (
  evolveNonceFrozen,
  evolveNonceUnfrozen,
  newLab,
  addFees,
  newUTxO,
  newStakeCred,
  deregStakeCred,
  delegation,
  regPool,
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
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Coin (
  Coin (..),
  compactCoinOrError,
 )
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Credential (Credential (..), Ptr)
import Cardano.Ledger.Hashes (GenDelegPair, GenDelegs (..))
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
  esLStateL,
  futurePParamsEpochStateL,
  lsCertStateL,
  lsUTxOStateL,
  nesEsL,
  prevPParamsEpochStateL,
  utxosDepositedL,
  utxosFeesL,
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates)
import Cardano.Ledger.Shelley.Rules (emptyInstantaneousRewards, votedFuturePParams)
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Val ((<+>), (<->))
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
import Data.Foldable (fold)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Lens.Micro
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
    bh = blockHeader b
    bn = bheaderBlockNo $ bhbody bh
    sn = bheaderSlotNo $ bhbody bh

addFees ::
  Coin ->
  ChainState era ->
  ChainState era
addFees newFees cs = cs {chainNes = nes}
  where
    nes =
      chainNes cs & nesEsL . esLStateL . lsUTxOStateL . utxosFeesL <>~ newFees

-- | = Update the UTxO
--
-- Update the UTxO for given transaction body.
newUTxO ::
  forall era.
  (EraTx era, EraStake era) =>
  TxBody TopTx era ->
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
--   Add a newly registered stake credential.
newStakeCred ::
  (EraCertState era, EraGov era, ShelleyEraAccounts era) =>
  Credential 'Staking ->
  Ptr ->
  ChainState era ->
  ChainState era
newStakeCred cred ptr cs = cs {chainNes = nes}
  where
    deposit = chainNes cs ^. nesEsL . curPParamsEpochStateL . ppKeyDepositL
    nes =
      chainNes cs
        & nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
          %~ registerShelleyAccount cred ptr (compactCoinOrError deposit) Nothing
        & nesEsL . esLStateL . lsUTxOStateL . utxosDepositedL <>~ deposit

-- | = De-Register Stake Credential
--
-- De-register a stake credential and all associated data.
deregStakeCred ::
  (HasCallStack, EraCertState era, ShelleyEraAccounts era) =>
  Credential 'Staking ->
  ChainState era ->
  ChainState era
deregStakeCred cred cs = cs {chainNes = nes}
  where
    nes =
      chainNes cs
        & nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL .~ accounts'
        & nesEsL . esLStateL . lsUTxOStateL . utxosDepositedL %~ (<-> refund)
    accounts =
      chainNes cs
        ^. nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
    (mAccountState, accounts') =
      unregisterShelleyAccount cred accounts
    refund = fromCompact (fromJust mAccountState ^. depositAccountStateL)

-- | = New Delegation
--
-- Create a delegation from the given stake credential to the given
-- stake pool.
delegation ::
  EraCertState era =>
  Credential 'Staking ->
  KeyHash 'StakePool ->
  ChainState era ->
  ChainState era
delegation cred poolId cs = cs {chainNes = nes}
  where
    nes =
      chainNes cs
        & nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL . accountsMapL
          %~ Map.adjust (stakePoolDelegationAccountStateL .~ Just poolId) cred

-- | Register a stake pool.
regPool ::
  forall era.
  ( EraCertState era
  , EraGov era
  ) =>
  StakePoolParams ->
  ChainState era ->
  ChainState era
regPool pool cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = lsCertState ls
    ps = dps ^. certPStateL
    poolDeposit = es ^. curPParamsEpochStateL . ppPoolDepositCompactL
    ps' =
      case Map.lookup (sppId pool) $ psStakePools ps of
        Nothing ->
          ps
            { psStakePools =
                Map.insert
                  (sppId pool)
                  (mkStakePoolState poolDeposit pool)
                  (psStakePools ps)
            }
        Just sps ->
          ps
            { psFutureStakePools =
                Map.insert
                  (sppId pool)
                  (mkStakePoolState (spsDeposit sps) pool)
                  (psFutureStakePools ps)
            }
    dps' = dps & certPStateL .~ ps'
    ls' = ls {lsCertState = dps'}
    ls'' =
      ls'
        & lsUTxOStateL . utxosDepositedL
          <>~ maybe (fromCompact poolDeposit) (const $ Coin 0) (Map.lookup (sppId pool) (psStakePools ps))
    es' = es {esLState = ls''}
    nes' = nes {nesEs = es'}

-- | = Re-Register Stake Pool
updatePoolParams ::
  forall era.
  ( EraCertState era
  , EraGov era
  ) =>
  StakePoolParams ->
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
        { psStakePools =
            Map.insert
              (sppId pool)
              (mkStakePoolState (es ^. curPParamsEpochStateL . ppPoolDepositCompactL) pool)
              (psStakePools ps)
        , psFutureStakePools = Map.delete (sppId pool) (psStakePools ps)
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
  StakePoolParams ->
  ChainState era ->
  ChainState era
reapPool pool cs = cs {chainNes = nes'}
  where
    poolId = sppId pool
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = lsCertState ls
    ps = dps ^. certPStateL
    poolDeposit = spsDeposit $ fromJust $ Map.lookup poolId (psStakePools ps)
    ps' =
      ps
        { psRetiring = Map.delete poolId (psRetiring ps)
        , psStakePools = Map.delete poolId (psStakePools ps)
        , psVRFKeyHashes = Map.delete (sppVrf pool) (psVRFKeyHashes ps)
        }
    pp = es ^. curPParamsEpochStateL
    ds = dps ^. certDStateL
    RewardAccount _ poolAccountCred = sppRewardAccount pool
    accounts = ds ^. accountsL
    (accounts', unclaimed) =
      case lookupAccountState poolAccountCred accounts of
        Nothing -> (accounts, poolDeposit)
        Just accountState ->
          let accountState' = accountState & balanceAccountStateL <>~ poolDeposit
           in ( accounts & accountsMapL %~ Map.insert poolAccountCred accountState'
              , mempty
              )
    ds' = ds {dsAccounts = removeStakePoolDelegations (Set.singleton poolId) accounts'}
    chainAccountState = esChainAccountState es
    chainAccountState' = chainAccountState {casTreasury = casTreasury chainAccountState <+> fromCompact unclaimed}
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
applyMIR pot addBalances cs = cs {chainNes = nes'}
  where
    tot = fold addBalances
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    dps = lsCertState ls
    ds = dps ^. certDStateL
    ds' =
      ds
        { dsIRewards = emptyInstantaneousRewards
        }
        & accountsL %~ addToBalanceAccounts (Map.map compactCoinOrError addBalances)
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
  (AtMostEra "Alonzo" era, EraGov era) =>
  Block (BHeader MockCrypto) era ->
  ChainState era ->
  ChainState era
newEpoch block@(Block {blockHeader}) cs = cs'
  where
    ChainState
      { chainNes = nes
      , chainEvolvingNonce = evNonce
      , chainCandidateNonce = cNonce
      , chainPrevEpochNonce = pNonce
      , chainLastAppliedBlock = lab
      } = cs
    bn = bheaderBlockNo $ bhbody blockHeader
    sn = bheaderSlotNo $ bhbody blockHeader
    pp = view curPParamsEpochStateL . nesEs $ nes
    e = epochFromSlotNo . bheaderSlotNo $ bhbody blockHeader
    nes' =
      nes
        { nesEL = e
        , nesBprev = nesBcur nes
        , nesBcur = BlocksMade Map.empty
        }
    n = getBlockNonce block
    cs' =
      cs
        { chainNes = nes'
        , chainEpochNonce = cNonce ⭒ pNonce ⭒ (pp ^. ppExtraEntropyL)
        , chainEvolvingNonce = evNonce ⭒ n
        , chainCandidateNonce = evNonce ⭒ n
        , chainPrevEpochNonce = prevHashToNonce . lastAppliedHash $ lab
        , chainLastAppliedBlock = At $ LastAppliedBlock bn sn (bhHash blockHeader)
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
