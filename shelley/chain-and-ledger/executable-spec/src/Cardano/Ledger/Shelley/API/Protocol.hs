{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- | Integration between the Shelley ledger and its corresponding (Transitional
-- Praos) protocol.
--
-- In particular, this code supports extracting the components of the ledger
-- state needed for protocol execution, both now and in a 2k-slot window.
module Cardano.Ledger.Shelley.API.Protocol
  ( STS.Prtcl.PrtclEnv,
    mkPrtclEnv,
    LedgerView,
    -- $timetravel
    futureLedgerView,
  )
where

import Cardano.Ledger.Shelley.API.Validation
import Cardano.Ledger.Shelley.Crypto
import Control.Arrow (left, right)
import Control.Monad.Except
import Control.State.Transition (PredicateFailure, TRC (..), applySTS)
import Data.Map.Strict (Map)
import Delegation.Certificates (PoolDistr)
import GHC.Generics (Generic)
import Keys (GenDelegs, GenKeyHash)
import LedgerState
  ( EpochState (..),
    NewEpochEnv (..),
    NewEpochState (..),
    _delegationState,
    _dstate,
    _genDelegs,
    getGKeys,
  )
import PParams (PParams)
import STS.NewEpoch (NEWEPOCH)
import qualified STS.Prtcl
import Slot (Slot, epochFromSlot)

-- | Data required by the Transitional Praos protocol from the Shelley ledger.
data LedgerView crypto
  = LedgerView
      { lvProtParams :: PParams,
        lvOverlaySched :: Map Slot (Maybe (GenKeyHash crypto)),
        lvPoolDistr :: PoolDistr crypto,
        lvGenDelegs :: GenDelegs crypto
      }
  deriving (Eq, Show, Generic)

-- | Construct a protocol environment from the ledger view, along with the
-- current slot and a marker indicating whether this is the first block in a new
-- epoch.
mkPrtclEnv ::
  LedgerView crypto ->
  Slot ->
  -- | New epoch marker. This should be true iff this execution of the PRTCL
  -- rule is being run on the first block in a new epoch.
  Bool ->
  STS.Prtcl.PrtclEnv crypto
mkPrtclEnv
  LedgerView
    { lvProtParams,
      lvOverlaySched,
      lvPoolDistr,
      lvGenDelegs
    } =
    STS.Prtcl.PrtclEnv
      lvProtParams
      lvOverlaySched
      lvPoolDistr
      lvGenDelegs

view :: ShelleyState crypto -> LedgerView crypto
view
  NewEpochState
    { nesPd,
      nesOsched,
      nesEs
    } = LedgerView
    { lvProtParams = esPp nesEs,
      lvOverlaySched =
        nesOsched,
      lvPoolDistr = nesPd,
      lvGenDelegs =
        _genDelegs . _dstate
          . _delegationState
          $ esLState nesEs
    }

-- $timetravel
--
--  Time Travel (or the anachronistic ledger view)
--
--  The ledger needs to expose access to the 'LedgerView' for a window of
--  2k-slots around the current tip of the chain. This functionality allows the
--  protocol layer to validate headers without downloading corresponding blocks.
--
--  The ability to travel backwards in time is obviously always possible by
--  keeping a record of past ledger states (or, more conservatively, ledger
--  views). We do not therefore deal explicitly with it in this module, though
--  see later for discussion on when snapshots should be taken.
--
--  In order to achieve forward time travel, we need a few things:
--  - Transition rules which process the body of a block should not have any
--    effect on the @LedgerView@ for at least 2k slots after they are received.
--    This property should be guaranteed by the design of the ledger.
--  - The effect of transition rules which process the header of a block should
--    be predictable for up to 2k slots.
--
--  In Shelley, there are two transition systems which operate during header
--  processing (the TICK rule itself merely applying checks and delegating
--  these systems):
--  - The RUPD rule deals with creating the reward update at an appropriate
--    point.
--  - The NEWEPOCH rule deals with various updates done at the end of an epoch.
--
--  We make the following claim:
--
--  A future ledger view (within the 2k-slot bound) is either:
--  - Equal to the current ledger view (if we remain in the same epoch), or
--  - Equal to the application of the NEWEPOCH rule on the first slot of the new
--    epoch to the current ledger state, if we are in a new epoch.
--
--  A sketch proof of this claim:
--
--  We will assume, even if not stated in the below, that all
--  blocks/headers/slots are within the 2k slot window of our current slot.
--
--  Lemma 1: Let `ls` be the current state at slot `s_0`, `s` be a future slot
--  within 2k of `s_0`, and `h_s` be some valid header at slot `s`. If `epoch s
--  /= epoch s_0 + 1`, then
--
--  ```
--          h_s
--  ls ------------> ls' => view ls == view ls'
--         TICK
--  ```
--
--  Proof:
--
--  By the rules for NEWEPOCH, since `epoch s /= epoch s_0 + 1` the input state
--  is returned unchanged. The RUPD system modifies only the reward update
--  component of 'NewEpochState', which is thrown away by 'view'.
--
--  Lemma 2: Let `h_0, ..., h_n` be a valid set of headers continuing from
--  ledger state `ls` (that is, there is a valid transition under the TICK
--  system for each header). Let `h_e` be the header which first transitions to
--  a new epoch. Then:
--
--  ```
--           h_e                                       h_n
--  ls_i ------------> ls_e -----------> ls_{n-1} ------------> ls' => view ls' == view ls_e
--          TICK             .....                    TICK
--  ```
--
--  Proof: By induction on the chain, applying lemma 1.
--
--  Lemma 3: Let `h_0, h_e` be a valid set of headers continuing from ledger
--  state `ls` (that is, there is a valid transition under the TICK system for
--  each header). Let `h_e` be the header which first transitions to a new
--  epoch. Then given:
--
--  ```
--         h_0                h_e
--  ls ------------> ls_0 -----------> ls_e
--        TICK               TICK
--  ```
--  ```
--       slot h_e
--  ls ------------> ls'
--       NEWEPOCH
--  ```
--
--  we have that
--
--  ```
--  view ls_e == view ls'
--  ```
--
--  Proof: Since `h_e` marks the start of a new epoch (and we are within the 2k
--  window), the TICK transition under `h_0` must be a RUPD transition and only
--  effect that component of the state.
--
--  The NEWEPOCH rule applies that reward update by calling 'applyRUpd' before
--  the call to the EPOCH rule. By definition of 'applyRUpd', this modifies the
--  treasury and reserves in the accounting state, and the rewards and fees in
--  the ledger state.
--
--  We are then forced to proceed by component selected by 'view', chasing their
--  dependencies through the NEWEPOCH/EPOCH rules and their various subrules:
--
--  - 'lvProtParams' are determined only by the value `pp_new`, which depends
--    only on a subcomponent of `utxoSt'`, goverened by the SNAP rule. But we
--    can see that the SNAP rule does not modify the proposed new protocol
--    parameters.
--  - `lvGenDelegs` is a component of the delegation state (`dstate'`), which is
--    changed only by the POOLREAP rule. But `POOLREAP` does not modify the
--    `genDelegs` component.
--  - 'lvOverlaySched' is not part of the state updated by EPOCH, but is
--    determined according to the function  'overlaySchedule', which takes as
--    its inputs the epoch, the set of genesis keys and the protocol parameters.
--    Per our earlier analysis, the reward update does not modify the set of
--    genesis keys or the set of protocol paramters, and hence it does not
--    modify the overlay schedule.
--  - `lvPoolDistr` is not modified by the EPOCH rule, but is constructed by
--    looking at the pools (from the snapshot state) and the stake distribution,
--    which also derives entirely from the snapshots. So these are also not
--    affected by the reward update.
--
--  Lemma 4: RUPD is idempotent. For slots `s`, `s1` we have that:
--
--  ```
--          s                  s1
--  ru ------------> ru_0 -----------> ru_1 => ru_0 == ru_1
--         RUPD               RUPD
--  ```
--
--  Proof: There is only one case in the RUPD rule which modifies the state. It
--  is predicated upon the reward update being empty, and it sets it to a
--  non-empty value.
--
--  Lemma 5: For successive slots `s`, `s1` such that neither marks the
--  beginning of a new epoch, TICK is idempotent.
--
--  Proof: Since neither slot marks the beginning of a new epoch, in neither
--  slot does NEWEPOCH make any changes. As such both transitions only involve
--  changes from RUPD. But by lemma 4, RUPD is idempotent.
--
--  At this point, we also formalise our "time-travel" assumption in the form of
--  an axiom:
--
--  Axiom 1: Time-travelling headers. Let `b_0,...b_n` be a chain of valid
--  blocks starting at our current slot, with `h_0,...,h_n` their corresponding
--  headers. Let `cs` be our current chain state. Then given:
--
--  ```
--         b_0                                       b_n
--  cs ------------> cs_0 -----------> cs_{n-1} ------------> cs_n
--        CHAIN             .....                   CHAIN
--  ```
--  and
--  ```
--         h_0                                       h_n
--  cs ------------> hs_0 -----------> hs_{n-1} ------------> hs_n
--        TICK             .....                    TICK
--  ```
--
--  we have that
--
--  ```
--  view cs_n == view hs_n
--  ```
--
--  We may finally state the main theorem.
--
--  Theorem: Let `b_0,...b_n` be a chain of valid blocks starting at our current
--  slot, with `h_0,...,h_n` their corresponding headers. Let `cs` be our
--  current chain state. Let `b_e` be the first slot in a new epoch. Let `cs_n`
--  be as in the axiom above. Then given:
--
--  ```
--       slot b_e
--  cs ------------> ls'
--       NEWEPOCH
--  ```
--
--  we have that `view cs_n == view ls'`.
--
--  Proof: Apply axiom 1 to translate the chain of blocks into a chain of
--  headers. Apply lemma 2 on the right of the chain and lemma 5 on the left of
--  the chain. Apply lemma 3 in the middle.

newtype FutureLedgerViewError crypto
  = FutureLedgerViewError [PredicateFailure (NEWEPOCH crypto)]
  deriving (Eq, Show)

-- | Anachronistic ledger view
--
--   Given a slot within a 2k-slot future window of our current slot (the slot
--   corresponding to the passed-in 'ShelleyState'), return a 'LedgerView'
--   appropriate to that slot.
--
--   This 'LedgerView' will in fact be valid for the entire epoch containing
--   that slot. As such (and because we do not store the current slot in the
--   'ShelleyState')
futureLedgerView ::
  forall crypto m.
  ( Crypto crypto,
    MonadError (FutureLedgerViewError crypto) m
  ) =>
  ShelleyState crypto ->
  Slot ->
  m (LedgerView crypto)
futureLedgerView ss slot =
  liftEither
    . right view
    . left (FutureLedgerViewError . join)
    . applySTS @(NEWEPOCH crypto)
    $ TRC (mkNewEpochEnv, ss, epochFromSlot slot)
  where
    mkNewEpochEnv =
      NewEpochEnv
        slot
        (getGKeys ss)
