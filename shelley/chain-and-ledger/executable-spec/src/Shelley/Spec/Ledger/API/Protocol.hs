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
module Shelley.Spec.Ledger.API.Protocol
  ( STS.Prtcl.PrtclEnv,
    mkPrtclEnv,
    LedgerView (..),
    currentLedgerView,
    -- $timetravel
    futureLedgerView,
  )
where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeListLenOf, encodeListLen)
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.Arrow (left, right)
import           Control.Monad.Except
import           Control.Monad.Trans.Reader (runReader)
import           Control.State.Transition.Extended (PredicateFailure, TRC (..), applySTS)
import           Data.Map.Strict (Map)
import           GHC.Generics (Generic)
import           Shelley.Spec.Ledger.API.Validation
import           Shelley.Spec.Ledger.BaseTypes (Globals)
import           Shelley.Spec.Ledger.Crypto
import           Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr)
import           Shelley.Spec.Ledger.Keys (GenDelegs)
import           Shelley.Spec.Ledger.LedgerState (EpochState (..), NewEpochState (..), OBftSlot,
                     getGKeys, _delegationState, _dstate, _genDelegs)
import           Shelley.Spec.Ledger.PParams (PParams)
import           Shelley.Spec.Ledger.Slot (SlotNo)
import qualified Shelley.Spec.Ledger.STS.Prtcl as STS.Prtcl
import           Shelley.Spec.Ledger.STS.Tick (TICK, TickEnv (..))

-- | Data required by the Transitional Praos protocol from the Shelley ledger.
data LedgerView crypto = LedgerView
  { lvProtParams :: PParams,
    lvOverlaySched :: Map SlotNo (OBftSlot crypto),
    lvPoolDistr :: PoolDistr crypto,
    lvGenDelegs :: GenDelegs crypto
  }
  deriving (Eq, Show, Generic)

instance NoUnexpectedThunks (LedgerView crypto)

instance Crypto crypto => FromCBOR (LedgerView crypto) where
  fromCBOR =
    decodeListLenOf 4
      >> LedgerView
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

instance Crypto crypto => ToCBOR (LedgerView crypto) where
  toCBOR
    LedgerView
      { lvProtParams,
        lvOverlaySched,
        lvPoolDistr,
        lvGenDelegs
      } =
      mconcat
        [ encodeListLen 4,
          toCBOR lvProtParams,
          toCBOR lvOverlaySched,
          toCBOR lvPoolDistr,
          toCBOR lvGenDelegs
        ]

-- | Construct a protocol environment from the ledger view, along with the
-- current slot and a marker indicating whether this is the first block in a new
-- epoch.
mkPrtclEnv ::
  LedgerView crypto ->
  SlotNo ->
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
    } =
    LedgerView
      { lvProtParams = esPp nesEs,
        lvOverlaySched =
          nesOsched,
        lvPoolDistr = nesPd,
        lvGenDelegs =
          _genDelegs . _dstate
            . _delegationState
            $ esLState nesEs
      }

-- | Alias of 'view' for export
currentLedgerView :: ShelleyState crypto -> LedgerView crypto
currentLedgerView = view

-- $timetravel
--
--  Time Travel (or the anachronistic ledger view)
--
--  The ledger needs to expose access to the 'LedgerView' for a window of slots
--  around the current tip of the chain. We call this period the stability
--  window, and it corresponds to the number of slots needed to "guarantee" the
--  presence of k blocks (where k is the security parameter). This functionality
--  allows the protocol layer to validate headers without downloading
--  corresponding blocks.
--
--  The ability to travel backwards in time is obviously always possible by
--  keeping a record of past ledger states (or, more conservatively, ledger
--  views). We do not therefore deal explicitly with it in this module, though
--  see later for discussion on when snapshots should be taken.
--
--  In order to achieve forward time travel, we need a few things:
--  - Transition rules which process the body of a block should not have any
--    effect on the @LedgerView@ during the stability window after they are
--    received. This property should be guaranteed by the design of the ledger.
--  - The effect of transition rules which process the header of a block should
--    be predictable for the stability window.
--
--  We make the following claim:
--
--  A future ledger view (within the stability window) is equal to the
--  application of the TICK rule at the target slot to the curernt ledger state.

newtype FutureLedgerViewError crypto
  = FutureLedgerViewError [PredicateFailure (TICK crypto)]
  deriving (Eq, Show)

-- | Anachronistic ledger view
--
--   Given a slot within the future stability window from our current slot (the
--   slot corresponding to the passed-in 'ShelleyState'), return a 'LedgerView'
--   appropriate to that slot.
--
futureLedgerView ::
  forall crypto m.
  ( Crypto crypto,
    MonadError (FutureLedgerViewError crypto) m
  ) =>
  Globals ->
  ShelleyState crypto ->
  SlotNo ->
  m (LedgerView crypto)
futureLedgerView globals ss slot =
  liftEither
    . right view
    . left (FutureLedgerViewError . join)
    $ res
  where
    res =
      flip runReader globals
        . applySTS @(TICK crypto)
        $ TRC (tickEnv, ss, slot)
    tickEnv =
      TickEnv
        (getGKeys ss)
