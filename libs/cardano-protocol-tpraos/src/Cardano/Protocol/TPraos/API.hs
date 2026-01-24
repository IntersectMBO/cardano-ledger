{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Integration between the Shelley ledger and its corresponding (Transitional
-- Praos) protocol.
--
-- In particular, this code supports extracting the components of the ledger
-- state needed for protocol execution, both now and in a 2k-slot window.
module Cardano.Protocol.TPraos.API (
  PraosCrypto,
  GetLedgerView (..),
  LedgerView (..),
  mkInitialShelleyLedgerView,
  FutureLedgerViewError (..),
  -- $chainstate
  ChainDepState (..),
  ChainTransitionError (..),
  tickChainDepState,
  updateChainDepState,
  reupdateChainDepState,
  initialChainDepState,
  -- Leader Schedule
  checkLeaderValue,
  getLeaderSchedule,
  -- HashHeader
  HashHeader (..),
) where

import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.BHeaderView (isOverlaySlot)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.BaseTypes (
  Globals (..),
  Nonce (NeutralNonce),
  Seed,
  ShelleyBase,
  UnitInterval,
  epochInfoPure,
 )
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Binary.Plain (FromCBOR (..), ToCBOR (..), decodeRecordNamed, encodeListLen)
import Cardano.Ledger.Chain (ChainChecksPParams, pparamsToChainChecksPParams)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Keys (GenDelegPair (..), GenDelegs (..), coerceKeyRole)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Core (EraGov)
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState (..),
  curPParamsEpochStateL,
  dsGenDelegsL,
  esLStateL,
  lsCertStateL,
 )
import Cardano.Ledger.Shelley.Translation (FromByronTranslationContext (..))
import Cardano.Ledger.Slot (SlotNo)
import Cardano.Ledger.State (EraCertState (..), PoolDistr (..), individualPoolStake)
import Cardano.Protocol.Crypto
import Cardano.Protocol.TPraos.BHeader (
  BHBody,
  BHeader,
  bhbody,
  bheaderPrev,
  checkLeaderValue,
  mkSeed,
  prevHashToNonce,
  seedL,
 )
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as STS.Prtcl
import Cardano.Protocol.TPraos.Rules.Tickn as STS.Tickn
import Cardano.Slotting.EpochInfo (epochInfoRange)
import Control.Arrow (left, right)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended (
  BaseM,
  Environment,
  STS,
  Signal,
  State,
  TRC (..),
  applySTS,
  reapplySTS,
 )
import Data.Default (def)
import Data.Either (fromRight)
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

-- =======================================================

class
  ( Crypto c
  , KES.Signable (KES c) (BHBody c)
  , VRF.Signable (VRF c) Seed
  ) =>
  PraosCrypto c

instance PraosCrypto StandardCrypto

class
  ( STS (EraRule "TICKF" era)
  , BaseM (EraRule "TICKF" era) ~ ShelleyBase
  , Environment (EraRule "TICKF" era) ~ ()
  , State (EraRule "TICKF" era) ~ NewEpochState era
  , Signal (EraRule "TICKF" era) ~ SlotNo
  , EraGov era
  , EraCertState era
  ) =>
  GetLedgerView era
  where
  currentLedgerView ::
    NewEpochState era ->
    LedgerView
  default currentLedgerView ::
    AtMostEra "Alonzo" era =>
    NewEpochState era ->
    LedgerView
  currentLedgerView = view

  -- $timetravel
  futureLedgerView ::
    MonadError (FutureLedgerViewError era) m =>
    Globals ->
    NewEpochState era ->
    SlotNo ->
    m LedgerView
  default futureLedgerView ::
    ( MonadError (FutureLedgerViewError era) m
    , AtMostEra "Alonzo" era
    ) =>
    Globals ->
    NewEpochState era ->
    SlotNo ->
    m LedgerView
  futureLedgerView = futureView

instance GetLedgerView ShelleyEra

instance GetLedgerView AllegraEra

instance GetLedgerView MaryEra

instance GetLedgerView AlonzoEra

-- Note that although we do not use TPraos in the Babbage era, we include this
-- because it makes it simpler to get the ledger view for Praos.
instance GetLedgerView BabbageEra where
  currentLedgerView
    NewEpochState {nesPd = pd, nesEs = es} =
      LedgerView
        { lvD = es ^. curPParamsEpochStateL . ppDG
        , lvExtraEntropy = error "Extra entropy is not set in the Babbage era"
        , lvPoolDistr = pd
        , lvGenDelegs = es ^. esLStateL . lsCertStateL . certDStateL . dsGenDelegsL
        , lvChainChecks = pparamsToChainChecksPParams $ es ^. curPParamsEpochStateL
        }

  futureLedgerView globals ss slot =
    liftEither
      . right (currentLedgerView @BabbageEra)
      . left FutureLedgerViewError
      $ res
    where
      res =
        flip runReader globals
          . applySTS @(EraRule "TICKF" BabbageEra)
          $ TRC ((), ss, slot)

-- Note that although we do not use TPraos in the Conway era, we include this
-- because it makes it simpler to get the ledger view for Praos.
instance GetLedgerView ConwayEra where
  currentLedgerView
    NewEpochState {nesPd = pd, nesEs = es} =
      LedgerView
        { lvD = es ^. curPParamsEpochStateL . ppDG
        , lvExtraEntropy = error "Extra entropy is not set in the Conway era"
        , lvPoolDistr = pd
        , lvGenDelegs = es ^. esLStateL . lsCertStateL . certDStateL . dsGenDelegsL
        , lvChainChecks = pparamsToChainChecksPParams $ es ^. curPParamsEpochStateL
        }

  futureLedgerView globals ss slot =
    liftEither
      . right currentLedgerView
      . left FutureLedgerViewError
      $ res
    where
      res =
        flip runReader globals
          . applySTS @(EraRule "TICKF" ConwayEra)
          $ TRC ((), ss, slot)

-- Note that although we do not use TPraos in the Dijkstra era, we include this
-- because it makes it simpler to get the ledger view for Praos.
instance GetLedgerView DijkstraEra where
  currentLedgerView
    NewEpochState {nesPd = pd, nesEs = es} =
      LedgerView
        { lvD = es ^. curPParamsEpochStateL . ppDG
        , lvExtraEntropy = error "Extra entropy is not set in the Dijkstra era"
        , lvPoolDistr = pd
        , lvGenDelegs = es ^. esLStateL . lsCertStateL . certDStateL . dsGenDelegsL
        , lvChainChecks = pparamsToChainChecksPParams $ es ^. curPParamsEpochStateL
        }

  futureLedgerView globals ss slot =
    liftEither
      . right currentLedgerView
      . left FutureLedgerViewError
      $ res
    where
      res =
        flip runReader globals
          . applySTS @(EraRule "TICKF" DijkstraEra)
          $ TRC ((), ss, slot)

-- | Data required by the Transitional Praos protocol from the Shelley ledger.
data LedgerView = LedgerView
  { lvD :: UnitInterval
  , -- Note that this field is not present in Babbage, but we require this view
    -- in order to construct the Babbage ledger view. We allow this to be lazy
    -- so that we may set it to an error. Note that `LedgerView` is never
    -- serialised, so this should not be forced except as a result of a
    -- programmer error.
    lvExtraEntropy :: ~Nonce
  , lvPoolDistr :: PoolDistr
  , lvGenDelegs :: GenDelegs
  , lvChainChecks :: ChainChecksPParams
  }
  deriving (Eq, Show, Generic)

instance NoThunks LedgerView

-- | Construct a protocol environment from the ledger view, along with the
-- current slot and a marker indicating whether this is the first block in a new
-- epoch.
mkPrtclEnv ::
  LedgerView ->
  -- | Epoch nonce
  Nonce ->
  STS.Prtcl.PrtclEnv
mkPrtclEnv
  LedgerView
    { lvD
    , lvPoolDistr
    , lvGenDelegs
    } =
    STS.Prtcl.PrtclEnv
      lvD
      lvPoolDistr
      lvGenDelegs

view ::
  (AtMostEra "Alonzo" era, EraGov era, EraCertState era) =>
  NewEpochState era ->
  LedgerView
view
  NewEpochState
    { nesPd = pd
    , nesEs = es
    } =
    let !ee = es ^. curPParamsEpochStateL . ppExtraEntropyL
     in LedgerView
          { lvD = es ^. curPParamsEpochStateL . ppDG
          , lvExtraEntropy = ee
          , lvPoolDistr = pd
          , lvGenDelegs = es ^. esLStateL . lsCertStateL . certDStateL . dsGenDelegsL
          , lvChainChecks = pparamsToChainChecksPParams $ es ^. curPParamsEpochStateL
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
  = FutureLedgerViewError (NonEmpty (PredicateFailure (EraRule "TICKF" era)))

deriving stock instance
  Eq (PredicateFailure (EraRule "TICKF" era)) =>
  Eq (FutureLedgerViewError era)

deriving stock instance
  Show (PredicateFailure (EraRule "TICKF" era)) =>
  Show (FutureLedgerViewError era)

-- | Anachronistic ledger view
--
--   Given a slot within the future stability window from our current slot (the
--   slot corresponding to the passed-in 'NewEpochState'), return a 'LedgerView'
--   appropriate to that slot.
futureView ::
  forall era m.
  ( MonadError (FutureLedgerViewError era) m
  , STS (EraRule "TICKF" era)
  , BaseM (EraRule "TICKF" era) ~ ShelleyBase
  , Environment (EraRule "TICKF" era) ~ ()
  , State (EraRule "TICKF" era) ~ NewEpochState era
  , Signal (EraRule "TICKF" era) ~ SlotNo
  , AtMostEra "Alonzo" era
  , EraGov era
  , EraCertState era
  ) =>
  Globals ->
  NewEpochState era ->
  SlotNo ->
  m LedgerView
futureView globals ss slot =
  liftEither
    . right view
    . left FutureLedgerViewError
    $ res
  where
    res =
      flip runReader globals
        . applySTS @(EraRule "TICKF" era)
        $ TRC ((), ss, slot)

-- $chainstate
--
-- Chain state operations
--
-- The chain state is an amalgam of the protocol state and the ticked nonce.

data ChainDepState = ChainDepState
  { csProtocol :: !STS.Prtcl.PrtclState
  , csTickn :: !STS.Tickn.TicknState
  , csLabNonce :: !Nonce
  -- ^ Nonce constructed from the hash of the last applied block header.
  }
  deriving (Eq, Show, Generic)

-- | Construct an initial chain state given an initial nonce and a set of
-- genesis delegates.
initialChainDepState ::
  Nonce ->
  Map (KeyHash GenesisRole) GenDelegPair ->
  ChainDepState
initialChainDepState initNonce genDelegs =
  ChainDepState
    { csProtocol =
        STS.Prtcl.PrtclState
          ocertIssueNos
          initNonce
          initNonce
    , csTickn =
        STS.Tickn.TicknState
          initNonce
          NeutralNonce
    , csLabNonce =
        NeutralNonce
    }
  where
    ocertIssueNos =
      Map.fromList
        ( fmap
            (\(GenDelegPair hk _) -> (coerceKeyRole hk, 0))
            (Map.elems genDelegs)
        )

instance NoThunks ChainDepState

instance DecCBOR ChainDepState

instance FromCBOR ChainDepState where
  fromCBOR =
    decodeRecordNamed
      "ChainDepState"
      (const 3)
      ( ChainDepState
          <$> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
      )

instance EncCBOR ChainDepState

instance ToCBOR ChainDepState where
  toCBOR
    ChainDepState
      { csProtocol
      , csTickn
      , csLabNonce
      } =
      mconcat
        [ encodeListLen 3
        , toCBOR csProtocol
        , toCBOR csTickn
        , toCBOR csLabNonce
        ]

newtype ChainTransitionError c
  = ChainTransitionError (NonEmpty (PredicateFailure (STS.Prtcl.PRTCL c)))
  deriving (Generic)

instance Crypto c => NoThunks (ChainTransitionError c)

deriving instance Crypto c => Eq (ChainTransitionError c)

deriving instance Crypto c => Show (ChainTransitionError c)

-- | Tick the chain state to a new epoch.
tickChainDepState ::
  Globals ->
  LedgerView ->
  -- | Are we in a new epoch?
  Bool ->
  ChainDepState ->
  ChainDepState
tickChainDepState
  globals
  LedgerView {lvExtraEntropy}
  isNewEpoch
  cs@ChainDepState {csProtocol, csTickn, csLabNonce} = cs {csTickn = newTickState}
    where
      STS.Prtcl.PrtclState _ _ candidateNonce = csProtocol
      err = error "Panic! tickChainDepState failed."
      newTickState =
        fromRight err
          . flip runReader globals
          . applySTS @STS.Tickn.TICKN
          $ TRC
            ( STS.Tickn.TicknEnv
                lvExtraEntropy
                candidateNonce
                csLabNonce
            , csTickn
            , isNewEpoch
            )

-- | Update the chain state based upon a new block header.
--
--   This also updates the last applied block hash.
updateChainDepState ::
  forall c m.
  ( PraosCrypto c
  , MonadError (ChainTransitionError c) m
  ) =>
  Globals ->
  LedgerView ->
  BHeader c ->
  ChainDepState ->
  m ChainDepState
updateChainDepState
  globals
  lv
  bh
  cs@ChainDepState {csProtocol, csTickn} =
    liftEither
      . right
        ( \newPrtclState ->
            cs
              { csProtocol = newPrtclState
              , csLabNonce = prevHashToNonce (bheaderPrev . bhbody $ bh)
              }
        )
      . left ChainTransitionError
      $ res
    where
      res =
        flip runReader globals
          . applySTS @(STS.Prtcl.PRTCL c)
          $ TRC
            ( mkPrtclEnv lv epochNonce
            , csProtocol
            , bh
            )
      epochNonce = STS.Tickn.ticknStateEpochNonce csTickn

-- | Re-update the chain state based upon a new block header.
--
--   This function does no validation of whether the header is internally valid
--   or consistent with the chain it is being applied to; the caller must ensure
--   that this is valid through having previously applied it.
reupdateChainDepState ::
  forall c.
  PraosCrypto c =>
  Globals ->
  LedgerView ->
  BHeader c ->
  ChainDepState ->
  ChainDepState
reupdateChainDepState
  globals
  lv
  bh
  cs@ChainDepState {csProtocol, csTickn} =
    cs
      { csProtocol = res
      , csLabNonce = prevHashToNonce (bheaderPrev . bhbody $ bh)
      }
    where
      res =
        flip runReader globals
          . reapplySTS @(STS.Prtcl.PRTCL c)
          $ TRC
            ( mkPrtclEnv lv epochNonce
            , csProtocol
            , bh
            )
      epochNonce = STS.Tickn.ticknStateEpochNonce csTickn

-- | Get the (private) leader schedule for this epoch.
--
--   Given a private VRF key, returns the set of slots in which this node is
--   eligible to lead.
getLeaderSchedule ::
  ( EraPParams era
  , VRF.VRFAlgorithm v
  , VRF.ContextVRF v ~ ()
  , VRF.Signable v Seed
  ) =>
  Globals ->
  NewEpochState era ->
  ChainDepState ->
  KeyHash StakePool ->
  VRF.SignKeyVRF v ->
  PParams era ->
  Set SlotNo
getLeaderSchedule globals ss cds poolHash key pp = Set.filter isLeader epochSlots
  where
    isLeader slotNo =
      let y = VRF.evalCertified () (mkSeed seedL slotNo epochNonce) key
       in not (isOverlaySlot a (pp ^. ppDG) slotNo)
            && checkLeaderValue (VRF.certifiedOutput y) stake f
    stake = maybe 0 individualPoolStake $ Map.lookup poolHash poolDistr
    poolDistr = unPoolDistr $ nesPd ss
    STS.Tickn.TicknState epochNonce _ = csTickn cds
    currentEpoch = nesEL ss
    ei = epochInfoPure globals
    f = activeSlotCoeff globals
    epochSlots = Set.fromList [a .. b]
    (a, b) = runIdentity $ epochInfoRange ei currentEpoch

-- | We construct a 'LedgerView' using the Shelley genesis config in the same
-- way as 'translateToShelleyLedgerState'.
mkInitialShelleyLedgerView ::
  FromByronTranslationContext ->
  LedgerView
mkInitialShelleyLedgerView transCtxt =
  let !ee = fbtcProtocolParams transCtxt ^. ppExtraEntropyL
   in LedgerView
        { lvD = fbtcProtocolParams transCtxt ^. ppDG
        , lvExtraEntropy = ee
        , lvPoolDistr = def
        , lvGenDelegs = GenDelegs $ fbtcGenDelegs transCtxt
        , lvChainChecks = pparamsToChainChecksPParams . fbtcProtocolParams $ transCtxt
        }
