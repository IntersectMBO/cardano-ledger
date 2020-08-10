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
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen)
import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.KES.Class
import Cardano.Crypto.VRF.Class
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.Arrow (left, right)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended (PredicateFailure, TRC (..), applySTS)
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.API.Validation
import Shelley.Spec.Ledger.BaseTypes (Globals, Nonce, Seed)
import Shelley.Spec.Ledger.BlockChain (BHBody, BHeader, bhbody, bheaderPrev, prevHashToNonce)
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr)
import Shelley.Spec.Ledger.Keys (GenDelegs)
import Shelley.Spec.Ledger.LedgerState
  ( EpochState (..),
    NewEpochState (..),
    OBftSlot,
    getGKeys,
    _delegationState,
    _dstate,
    _genDelegs,
  )
import Shelley.Spec.Ledger.OCert (OCertSignable)
import Shelley.Spec.Ledger.PParams (PParams)
import qualified Shelley.Spec.Ledger.STS.Prtcl as STS.Prtcl
import Shelley.Spec.Ledger.STS.Tick (TICK, TickEnv (..))
import qualified Shelley.Spec.Ledger.STS.Tickn as STS.Tickn
import Shelley.Spec.Ledger.Serialization (decodeRecordNamed)
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.Value

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
    decodeRecordNamed
      "LedgerView"
      (const 4)
      ( LedgerView
          <$> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
      )

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
  -- | Epoch nonce
  Nonce ->
  STS.Prtcl.PrtclEnv crypto
mkPrtclEnv
  LedgerView
    { lvOverlaySched,
      lvPoolDistr,
      lvGenDelegs
    } =
    STS.Prtcl.PrtclEnv
      lvOverlaySched
      lvPoolDistr
      lvGenDelegs

view :: ShelleyState crypto v -> LedgerView crypto
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
currentLedgerView :: ShelleyState crypto v -> LedgerView crypto
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

newtype FutureLedgerViewError crypto v
  = FutureLedgerViewError [PredicateFailure (TICK crypto v)]
  deriving (Eq, Show)

-- | Anachronistic ledger view
--
--   Given a slot within the future stability window from our current slot (the
--   slot corresponding to the passed-in 'ShelleyState'), return a 'LedgerView'
--   appropriate to that slot.
futureLedgerView ::
  forall crypto m v.
  ( CV crypto v,
    MonadError (FutureLedgerViewError crypto v) m
  ) =>
  Globals ->
  ShelleyState crypto v ->
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
        . applySTS @(TICK crypto v)
        $ TRC (tickEnv, ss, slot)
    tickEnv =
      TickEnv
        (getGKeys ss)

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

instance Crypto c => NoUnexpectedThunks (ChainDepState c)

instance Crypto crypto => FromCBOR (ChainDepState crypto) where
  fromCBOR =
    decodeRecordNamed
      "ChainDepState"
      (const 3)
      ( ChainDepState
          <$> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
      )

instance Crypto crypto => ToCBOR (ChainDepState crypto) where
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

newtype ChainTransitionError crypto v
  = ChainTransitionError [PredicateFailure (STS.Prtcl.PRTCL crypto v)]
  deriving (Generic)

instance (Crypto crypto) => NoUnexpectedThunks (ChainTransitionError crypto v)

deriving instance (Crypto crypto) => Eq (ChainTransitionError crypto v)

deriving instance (Crypto crypto) => Show (ChainTransitionError crypto v)

-- | Tick the chain state to a new epoch.
tickChainDepState ::
  Globals ->
  LedgerView c ->
  -- | Are we in a new epoch?
  Bool ->
  ChainDepState c ->
  ChainDepState c
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
          . applySTS @STS.Tickn.TICKN
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
  forall crypto m v.
  ( CV crypto v,
    MonadError (ChainTransitionError crypto v) m,
    Cardano.Crypto.DSIGN.Class.Signable
      (DSIGN crypto)
      (Shelley.Spec.Ledger.OCert.OCertSignable crypto),
    Cardano.Crypto.KES.Class.Signable
      (KES crypto)
      (Shelley.Spec.Ledger.BlockChain.BHBody crypto v),
    Cardano.Crypto.VRF.Class.Signable
      (VRF crypto)
      Shelley.Spec.Ledger.BaseTypes.Seed
  ) =>
  Globals ->
  LedgerView crypto ->
  BHeader crypto v ->
  ChainDepState crypto ->
  m (ChainDepState crypto)
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
          . applySTS @(STS.Prtcl.PRTCL crypto v)
          $ TRC
            ( mkPrtclEnv lv epochNonce,
              csProtocol,
              bh
            )
      epochNonce = STS.Tickn.ticknStateEpochNonce csTickn
