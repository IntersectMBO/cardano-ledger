{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- | Integration between the Shelley ledger and its corresponding (Transitional
-- Praos) protocol.
--
-- In particular, this code supports extracting the components of the ledger
-- state needed for protocol execution, both now and in a 2k-slot window.
module Shelley.Spec.Ledger.API.Protocol
  ( STS.Prtcl.PrtclEnv,
    LedgerView (..),
    currentLedgerView,
    -- $timetravel
    futureLedgerView,
    -- $chainstate
    ChainDepState (..),
    ChainTransitionError (..),
    tickChainDepState,
    updateChainDepState,
    reupdateChainDepState,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen)
import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.KES.Class
import Cardano.Crypto.VRF.Class
import Cardano.Ledger.Crypto hiding (Crypto)
import Cardano.Ledger.Era (Crypto, Era)
import Control.Arrow (left, right)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended
  ( PredicateFailure,
    TRC (..),
    applySTS,
    reapplySTS,
  )
import Data.Either (fromRight)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.API.Validation
import Shelley.Spec.Ledger.BaseTypes (Globals, Nonce, Seed)
import Shelley.Spec.Ledger.BlockChain
  ( BHBody,
    BHeader,
    bhbody,
    bheaderPrev,
    prevHashToNonce,
  )
import Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr)
import Shelley.Spec.Ledger.Keys (GenDelegs)
import Shelley.Spec.Ledger.LedgerState
  ( EpochState (..),
    NewEpochState (..),
    _delegationState,
    _dstate,
    _genDelegs,
  )
import Shelley.Spec.Ledger.OCert (OCertSignable)
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..))
import qualified Shelley.Spec.Ledger.STS.Prtcl as STS.Prtcl
import Shelley.Spec.Ledger.STS.Tick (TICKF)
import qualified Shelley.Spec.Ledger.STS.Tickn as STS.Tickn
import Shelley.Spec.Ledger.Serialization (decodeRecordNamed)
import Shelley.Spec.Ledger.Slot (SlotNo)

-- | Data required by the Transitional Praos protocol from the Shelley ledger.
data LedgerView era = LedgerView
  { lvProtParams :: PParams era,
    lvPoolDistr :: PoolDistr era,
    lvGenDelegs :: GenDelegs era
  }
  deriving (Eq, Show, Generic)

instance NoThunks (LedgerView era)

instance Era era => FromCBOR (LedgerView era) where
  fromCBOR =
    decodeRecordNamed
      "LedgerView"
      (const 3)
      ( LedgerView
          <$> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
      )

instance Era era => ToCBOR (LedgerView era) where
  toCBOR
    LedgerView
      { lvProtParams,
        lvPoolDistr,
        lvGenDelegs
      } =
      mconcat
        [ encodeListLen 3,
          toCBOR lvProtParams,
          toCBOR lvPoolDistr,
          toCBOR lvGenDelegs
        ]

-- | Construct a protocol environment from the ledger view, along with the
-- current slot and a marker indicating whether this is the first block in a new
-- epoch.
mkPrtclEnv ::
  LedgerView era ->
  -- | Epoch nonce
  Nonce ->
  STS.Prtcl.PrtclEnv era
mkPrtclEnv
  LedgerView
    { lvProtParams,
      lvPoolDistr,
      lvGenDelegs
    } =
    STS.Prtcl.PrtclEnv
      (_d lvProtParams)
      lvPoolDistr
      lvGenDelegs

view :: ShelleyState era -> LedgerView era
view
  NewEpochState
    { nesPd,
      nesEs
    } =
    LedgerView
      { lvProtParams = esPp nesEs,
        lvPoolDistr = nesPd,
        lvGenDelegs =
          _genDelegs . _dstate
            . _delegationState
            $ esLState nesEs
      }

-- | Alias of 'view' for export
currentLedgerView :: ShelleyState era -> LedgerView era
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

newtype FutureLedgerViewError era
  = FutureLedgerViewError [PredicateFailure (TICKF era)]
  deriving (Eq, Show)

-- | Anachronistic ledger view
--
--   Given a slot within the future stability window from our current slot (the
--   slot corresponding to the passed-in 'ShelleyState'), return a 'LedgerView'
--   appropriate to that slot.
futureLedgerView ::
  forall era m.
  ( Era era,
    MonadError (FutureLedgerViewError era) m
  ) =>
  Globals ->
  ShelleyState era ->
  SlotNo ->
  m (LedgerView era)
futureLedgerView globals ss slot =
  liftEither
    . right view
    . left (FutureLedgerViewError . join)
    $ res
  where
    res =
      flip runReader globals
        . applySTS @(TICKF era)
        $ TRC ((), ss, slot)

-- $chainstate
--
-- Chain state operations
--
-- The chain state is an amalgam of the protocol state and the ticked nonce.

data ChainDepState c = ChainDepState
  { csProtocol :: !(STS.Prtcl.PrtclState c),
    csTickn :: !STS.Tickn.TicknState,
    -- | Nonce constructed from the hash of the last applied block header.
    csLabNonce :: !Nonce
  }
  deriving (Eq, Show, Generic)

instance Era era => NoThunks (ChainDepState era)

instance Era era => FromCBOR (ChainDepState era) where
  fromCBOR =
    decodeRecordNamed
      "ChainDepState"
      (const 3)
      ( ChainDepState
          <$> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
      )

instance Era era => ToCBOR (ChainDepState era) where
  toCBOR
    ChainDepState
      { csProtocol,
        csTickn,
        csLabNonce
      } =
      mconcat
        [ encodeListLen 3,
          toCBOR csProtocol,
          toCBOR csTickn,
          toCBOR csLabNonce
        ]

newtype ChainTransitionError era
  = ChainTransitionError [PredicateFailure (STS.Prtcl.PRTCL era)]
  deriving (Generic)

instance (Era era) => NoThunks (ChainTransitionError era)

deriving instance (Era era) => Eq (ChainTransitionError era)

deriving instance (Era era) => Show (ChainTransitionError era)

-- | Tick the chain state to a new epoch.
tickChainDepState ::
  forall era.
  (Era era) =>
  Globals ->
  LedgerView era ->
  -- | Are we in a new epoch?
  Bool ->
  ChainDepState era ->
  ChainDepState era
tickChainDepState
  globals
  LedgerView {lvProtParams}
  isNewEpoch
  cs@ChainDepState {csProtocol, csTickn, csLabNonce} = cs {csTickn = newTickState}
    where
      STS.Prtcl.PrtclState _ _ candidateNonce = csProtocol
      err = error "Panic! tickChainDepState failed."
      newTickState =
        fromRight err . flip runReader globals
          . applySTS @(STS.Tickn.TICKN era)
          $ TRC
            ( STS.Tickn.TicknEnv
                lvProtParams
                candidateNonce
                csLabNonce,
              csTickn,
              isNewEpoch
            )

-- | Update the chain state based upon a new block header.
--
--   This also updates the last applied block hash.
updateChainDepState ::
  forall era m.
  ( Era era,
    MonadError (ChainTransitionError era) m,
    Cardano.Crypto.DSIGN.Class.Signable
      (DSIGN (Crypto era))
      (Shelley.Spec.Ledger.OCert.OCertSignable era),
    Cardano.Crypto.KES.Class.Signable
      (KES (Crypto era))
      (Shelley.Spec.Ledger.BlockChain.BHBody era),
    Cardano.Crypto.VRF.Class.Signable
      (VRF (Crypto era))
      Shelley.Spec.Ledger.BaseTypes.Seed
  ) =>
  Globals ->
  LedgerView era ->
  BHeader era ->
  ChainDepState era ->
  m (ChainDepState era)
updateChainDepState
  globals
  lv
  bh
  cs@ChainDepState {csProtocol, csTickn} =
    liftEither
      . right
        ( \newPrtclState ->
            cs
              { csProtocol = newPrtclState,
                csLabNonce = prevHashToNonce (bheaderPrev . bhbody $ bh)
              }
        )
      . left (ChainTransitionError . join)
      $ res
    where
      res =
        flip runReader globals
          . applySTS @(STS.Prtcl.PRTCL era)
          $ TRC
            ( mkPrtclEnv lv epochNonce,
              csProtocol,
              bh
            )
      epochNonce = STS.Tickn.ticknStateEpochNonce csTickn

-- | Re-update the chain state based upon a new block header.
--
--   This function does no validation of whether the header is internally valid
--   or consistent with the chain it is being applied to; the caller must ensure
--   that this is valid through having previously applied it.
reupdateChainDepState ::
  forall era.
  ( Era era,
    Cardano.Crypto.DSIGN.Class.Signable
      (DSIGN (Crypto era))
      (Shelley.Spec.Ledger.OCert.OCertSignable era),
    Cardano.Crypto.KES.Class.Signable
      (KES (Crypto era))
      (Shelley.Spec.Ledger.BlockChain.BHBody era),
    Cardano.Crypto.VRF.Class.Signable
      (VRF (Crypto era))
      Shelley.Spec.Ledger.BaseTypes.Seed
  ) =>
  Globals ->
  LedgerView era ->
  BHeader era ->
  ChainDepState era ->
  ChainDepState era
reupdateChainDepState
  globals
  lv
  bh
  cs@ChainDepState {csProtocol, csTickn} =
    cs
      { csProtocol = res,
        csLabNonce = prevHashToNonce (bheaderPrev . bhbody $ bh)
      }
    where
      res =
        flip runReader globals
          . reapplySTS @(STS.Prtcl.PRTCL era)
          $ TRC
            ( mkPrtclEnv lv epochNonce,
              csProtocol,
              bh
            )
      epochNonce = STS.Tickn.ticknStateEpochNonce csTickn
