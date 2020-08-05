{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Test.Shelley.Spec.Ledger.Examples.Federation
-- Description : Chain State Combinators
--
-- A collection of combinators for manipulating Chain State.
-- The idea is to provide a clear way of describing the
-- changes to the chain state when a block is processed.
module Test.Shelley.Spec.Ledger.Examples.Combinators
  ( -- | = Evolve Nonces - Frozen
    --
    -- Evolve the appropriate nonces under the assumption
    -- that the candidate nonce is now frozen.
    evolveNonceFrozen,
    -- | = Evolve Nonces - Unfrozen
    --
    -- Evolve the appropriate nonces under the assumption
    -- that the candidate nonce is not frozen.
    evolveNonceUnfrozen,
    -- | = New 'LastAppliedBlock'
    --
    -- Update the chain state with the details of 'LastAppliedBlock'
    -- that occur when a new block is processed.
    newLab,
    -- | = Update Fees and Deposits
    --
    -- Update the fee pot and deposit pot with the new fees
    -- and the change to the deposit pot.
    feesAndDeposits,
    -- | = Update the UTxO
    --
    -- Update the UTxO for given transaction body.
    newUTxO,
    -- | = New Stake Credential
    --
    -- Add a newly registered stake credential
    newStakeCred,
    -- | = New Stake Pool
    --
    -- Add a newly registered stake pool
    newPool,
    -- | = MIR
    --
    -- Add a credential to the MIR mapping for the given pot (reserves or treasury)
    mir,
  )
where

import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Iterate.SetAlgebra (eval, singleton, (∪), (⋪))
import qualified Data.Map.Strict as Map
import Shelley.Spec.Ledger.BaseTypes ((⭒))
import Shelley.Spec.Ledger.BaseTypes (Nonce (..))
import Shelley.Spec.Ledger.BlockChain
  ( BHBody (..),
    Block (..),
    LastAppliedBlock (..),
    bhHash,
    bhbody,
    bheader,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential
  ( Credential (..),
    Ptr,
  )
import Shelley.Spec.Ledger.Crypto (Crypto (..))
import Shelley.Spec.Ledger.Keys (KeyRole (..))
import Shelley.Spec.Ledger.LedgerState
  ( DPState (..),
    DState (..),
    EpochState (..),
    InstantaneousRewards (..),
    LedgerState (..),
    NewEpochState (..),
    PState (..),
    UTxOState (..),
  )
import Shelley.Spec.Ledger.STS.Chain (ChainState (..))
import Shelley.Spec.Ledger.TxData (MIRPot (..), PoolParams (..), TxBody (..))
import Shelley.Spec.Ledger.UTxO (txins, txouts)

evolveNonceFrozen :: forall c. Nonce -> ChainState c -> ChainState c
evolveNonceFrozen n cs = cs {chainCandidateNonce = chainCandidateNonce cs ⭒ n}

evolveNonceUnfrozen :: forall c. Nonce -> ChainState c -> ChainState c
evolveNonceUnfrozen n cs =
  cs
    { chainCandidateNonce = chainCandidateNonce cs ⭒ n,
      chainEvolvingNonce = chainEvolvingNonce cs ⭒ n
    }

newLab ::
  forall c.
  Crypto c =>
  Block c ->
  ChainState c ->
  ChainState c
newLab b cs =
  cs {chainLastAppliedBlock = At $ LastAppliedBlock bn sn (bhHash bh)}
  where
    bh = bheader b
    bn = bheaderBlockNo . bhbody $ bh
    sn = bheaderSlotNo . bhbody $ bh

feesAndDeposits ::
  forall c.
  Coin ->
  Coin ->
  ChainState c ->
  ChainState c
feesAndDeposits newFees depositChange cs = cs {chainNes = nes'}
  where
    nes = chainNes cs
    es = nesEs nes
    ls = esLState es
    utxoSt = _utxoState ls
    utxoSt' =
      utxoSt
        { _deposited = (_deposited utxoSt) + depositChange,
          _fees = (_fees utxoSt) + newFees
        }
    ls' = ls {_utxoState = utxoSt'}
    es' = es {esLState = ls'}
    nes' = nes {nesEs = es'}

newUTxO ::
  forall c.
  Crypto c =>
  TxBody c ->
  ChainState c ->
  ChainState c
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

newStakeCred ::
  forall c.
  Credential 'Staking c ->
  Ptr ->
  ChainState c ->
  ChainState c
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

newPool ::
  forall c.
  PoolParams c ->
  ChainState c ->
  ChainState c
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

mir ::
  forall c.
  Credential 'Staking c ->
  MIRPot ->
  Coin ->
  ChainState c ->
  ChainState c
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
