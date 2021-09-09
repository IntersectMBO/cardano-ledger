{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Integration between the Shelley ledger and its corresponding (Transitional
-- Praos) protocol.
--
-- In particular, this code supports extracting the components of the ledger
-- state needed for protocol execution, both now and in a 2k-slot window.
module Shelley.Spec.Ledger.API.Protocol
  ( PraosCrypto,
    GetLedgerView (..),
    LedgerView (..),
    FutureLedgerViewError (..),
    -- $chainstate
    ChainDepState (..),
    ChainTransitionError (..),
    tickChainDepState,
    updateChainDepState,
    reupdateChainDepState,
    initialChainDepState,
    -- Re-exports
    checkLeaderValue,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen)
import Cardano.Ledger.BaseTypes
  ( Globals,
    Nonce (NeutralNonce),
    Seed,
    ShelleyBase,
    UnitInterval,
  )
import Cardano.Ledger.Core (ChainData, SerialisableData)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto, StandardCrypto)
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys
  ( DSignable,
    GenDelegPair (..),
    GenDelegs,
    Hash,
    KESignable,
    KeyHash,
    KeyRole (Genesis),
    VRFSignable,
    coerceKeyRole,
  )
import Cardano.Ledger.Serialization (decodeRecordNamed)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Slot (SlotNo)
import Cardano.Protocol.TPraos (PoolDistr)
import Cardano.Protocol.TPraos.BHeader
  ( BHBody,
    BHeader,
    bhbody,
    bheaderPrev,
    checkLeaderValue,
    prevHashToNonce,
  )
import Cardano.Protocol.TPraos.OCert (OCertSignable)
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as STS.Prtcl
import Control.Arrow (left, right)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended
  ( BaseM,
    Environment,
    PredicateFailure,
    STS,
    Signal,
    State,
    TRC (..),
    applySTS,
    reapplySTS,
  )
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.LedgerState
  ( EpochState (..),
    NewEpochState (..),
    _delegationState,
    _dstate,
    _genDelegs,
  )
import Shelley.Spec.Ledger.PParams (PParams' (..), ProtVer)
import Shelley.Spec.Ledger.STS.Chain (ChainChecksData, pparamsToChainChecksData)
import Shelley.Spec.Ledger.STS.EraMapping ()
import Shelley.Spec.Ledger.STS.Tick (TickfPredicateFailure)
import qualified Shelley.Spec.Ledger.STS.Tickn as STS.Tickn

-- =======================================================

class
  ( CC.Crypto c,
    DSignable c (OCertSignable c),
    DSignable c (Hash c EraIndependentTxBody),
    KESignable c (BHBody c),
    VRFSignable c Seed
  ) =>
  PraosCrypto c

instance PraosCrypto CC.StandardCrypto

class
  ( ChainData (ChainDepState (Crypto era)),
    SerialisableData (ChainDepState (Crypto era)),
    Eq (ChainTransitionError (Crypto era)),
    Show (ChainTransitionError (Crypto era)),
    Show (LedgerView (Crypto era)),
    Show (FutureLedgerViewError era),
    STS (Core.EraRule "TICKF" era),
    BaseM (Core.EraRule "TICKF" era) ~ ShelleyBase,
    Environment (Core.EraRule "TICKF" era) ~ (),
    State (Core.EraRule "TICKF" era) ~ NewEpochState era,
    Signal (Core.EraRule "TICKF" era) ~ SlotNo,
    PredicateFailure (Core.EraRule "TICKF" era) ~ TickfPredicateFailure era,
    HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_extraEntropy" (Core.PParams era) Nonce,
    HasField "_maxBBSize" (Core.PParams era) Natural,
    HasField "_maxBHSize" (Core.PParams era) Natural,
    HasField "_protocolVersion" (Core.PParams era) ProtVer
  ) =>
  GetLedgerView era
  where
  currentLedgerView ::
    NewEpochState era ->
    LedgerView (Crypto era)
  currentLedgerView = view

  -- $timetravel
  futureLedgerView ::
    MonadError (FutureLedgerViewError era) m =>
    Globals ->
    NewEpochState era ->
    SlotNo ->
    m (LedgerView (Crypto era))
  default futureLedgerView ::
    (MonadError (FutureLedgerViewError era) m) =>
    Globals ->
    NewEpochState era ->
    SlotNo ->
    m (LedgerView (Crypto era))
  futureLedgerView = futureView

instance PraosCrypto crypto => GetLedgerView (ShelleyEra crypto)

-- | Data required by the Transitional Praos protocol from the Shelley ledger.
data LedgerView crypto = LedgerView
  { lvD :: UnitInterval,
    lvExtraEntropy :: Nonce,
    lvPoolDistr :: PoolDistr crypto,
    lvGenDelegs :: GenDelegs crypto,
    lvChainChecks :: ChainChecksData
  }
  deriving (Eq, Show, Generic)

instance NoThunks (LedgerView crypto)

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
    { lvD,
      lvPoolDistr,
      lvGenDelegs
    } =
    STS.Prtcl.PrtclEnv
      lvD
      lvPoolDistr
      lvGenDelegs

view ::
  ( HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_extraEntropy" (Core.PParams era) Nonce,
    HasField "_maxBBSize" (Core.PParams era) Natural,
    HasField "_maxBHSize" (Core.PParams era) Natural,
    HasField "_protocolVersion" (Core.PParams era) ProtVer
  ) =>
  NewEpochState era ->
  LedgerView (Crypto era)
view
  NewEpochState
    { nesPd,
      nesEs
    } =
    LedgerView
      { lvD = getField @"_d" . esPp $ nesEs,
        lvExtraEntropy = getField @"_extraEntropy" . esPp $ nesEs,
        lvPoolDistr = nesPd,
        lvGenDelegs =
          _genDelegs . _dstate
            . _delegationState
            $ esLState nesEs,
        lvChainChecks = pparamsToChainChecksData . esPp $ nesEs
      }

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
  = FutureLedgerViewError [PredicateFailure (Core.EraRule "TICKF" era)]

deriving stock instance
  (Eq (PredicateFailure (Core.EraRule "TICKF" era))) =>
  Eq (FutureLedgerViewError era)

deriving stock instance
  (Show (PredicateFailure (Core.EraRule "TICKF" era))) =>
  Show (FutureLedgerViewError era)

-- | Anachronistic ledger view
--
--   Given a slot within the future stability window from our current slot (the
--   slot corresponding to the passed-in 'NewEpochState'), return a 'LedgerView'
--   appropriate to that slot.
futureView ::
  forall era m.
  ( MonadError (FutureLedgerViewError era) m,
    STS (Core.EraRule "TICKF" era),
    BaseM (Core.EraRule "TICKF" era) ~ ShelleyBase,
    Environment (Core.EraRule "TICKF" era) ~ (),
    State (Core.EraRule "TICKF" era) ~ NewEpochState era,
    Signal (Core.EraRule "TICKF" era) ~ SlotNo,
    PredicateFailure (Core.EraRule "TICKF" era) ~ TickfPredicateFailure era,
    HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_extraEntropy" (Core.PParams era) Nonce,
    HasField "_maxBBSize" (Core.PParams era) Natural,
    HasField "_maxBHSize" (Core.PParams era) Natural,
    HasField "_protocolVersion" (Core.PParams era) ProtVer
  ) =>
  Globals ->
  NewEpochState era ->
  SlotNo ->
  m (LedgerView (Crypto era))
futureView globals ss slot =
  liftEither
    . right view
    . left FutureLedgerViewError
    $ res
  where
    res =
      flip runReader globals
        . applySTS @(Core.EraRule "TICKF" era)
        $ TRC ((), ss, slot)

-- $chainstate
--
-- Chain state operations
--
-- The chain state is an amalgam of the protocol state and the ticked nonce.

data ChainDepState crypto = ChainDepState
  { csProtocol :: !(STS.Prtcl.PrtclState crypto),
    csTickn :: !STS.Tickn.TicknState,
    -- | Nonce constructed from the hash of the last applied block header.
    csLabNonce :: !Nonce
  }
  deriving (Eq, Show, Generic)

-- | Construct an initial chain state given an initial nonce and a set of
-- genesis delegates.
initialChainDepState ::
  Nonce ->
  Map (KeyHash 'Genesis crypto) (GenDelegPair crypto) ->
  ChainDepState crypto
initialChainDepState initNonce genDelegs =
  ChainDepState
    { csProtocol =
        STS.Prtcl.PrtclState
          ocertIssueNos
          initNonce
          initNonce,
      csTickn =
        STS.Tickn.TicknState
          initNonce
          NeutralNonce,
      csLabNonce =
        NeutralNonce
    }
  where
    ocertIssueNos =
      Map.fromList
        ( fmap
            (\(GenDelegPair hk _) -> (coerceKeyRole hk, 0))
            (Map.elems genDelegs)
        )

instance CC.Crypto crypto => NoThunks (ChainDepState crypto)

instance CC.Crypto crypto => FromCBOR (ChainDepState crypto) where
  fromCBOR =
    decodeRecordNamed
      "ChainDepState"
      (const 3)
      ( ChainDepState
          <$> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
      )

instance CC.Crypto crypto => ToCBOR (ChainDepState crypto) where
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

newtype ChainTransitionError crypto
  = ChainTransitionError [PredicateFailure (STS.Prtcl.PRTCL crypto)]
  deriving (Generic)

instance (CC.Crypto crypto) => NoThunks (ChainTransitionError crypto)

deriving instance (CC.Crypto crypto) => Eq (ChainTransitionError crypto)

deriving instance (CC.Crypto crypto) => Show (ChainTransitionError crypto)

-- | Tick the chain state to a new epoch.
tickChainDepState ::
  Globals ->
  LedgerView crypto ->
  -- | Are we in a new epoch?
  Bool ->
  ChainDepState crypto ->
  ChainDepState crypto
tickChainDepState
  globals
  LedgerView {lvExtraEntropy}
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
                lvExtraEntropy
                candidateNonce
                csLabNonce,
              csTickn,
              isNewEpoch
            )

-- | Update the chain state based upon a new block header.
--
--   This also updates the last applied block hash.
updateChainDepState ::
  forall crypto m.
  ( PraosCrypto crypto,
    MonadError (ChainTransitionError crypto) m
  ) =>
  Globals ->
  LedgerView crypto ->
  BHeader crypto ->
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
      . left ChainTransitionError
      $ res
    where
      res =
        flip runReader globals
          . applySTS @(STS.Prtcl.PRTCL crypto)
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
  forall crypto.
  PraosCrypto crypto =>
  Globals ->
  LedgerView crypto ->
  BHeader crypto ->
  ChainDepState crypto ->
  ChainDepState crypto
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
          . reapplySTS @(STS.Prtcl.PRTCL crypto)
          $ TRC
            ( mkPrtclEnv lv epochNonce,
              csProtocol,
              bh
            )
      epochNonce = STS.Tickn.ticknStateEpochNonce csTickn
