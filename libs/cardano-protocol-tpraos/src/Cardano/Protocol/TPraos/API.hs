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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Integration between the Shelley ledger and its corresponding (Transitional
-- Praos) protocol.
--
-- In particular, this code supports extracting the components of the ledger
-- state needed for protocol execution, both now and in a 2k-slot window.
module Cardano.Protocol.TPraos.API
  ( PraosCrypto,
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
  )
where

import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo (AlonzoPParamsHKD (..))
import Cardano.Ledger.BHeaderView (isOverlaySlot)
import Cardano.Ledger.Babbage (BabbageEra)
import qualified Cardano.Ledger.Babbage.PParams as Babbage (BabbagePParamsHKD (..))
import Cardano.Ledger.BaseTypes
  ( Globals (..),
    Nonce (NeutralNonce),
    ProtVer,
    Seed,
    ShelleyBase,
    UnitInterval,
    epochInfoPure,
  )
import Cardano.Ledger.Binary (FromCBOR (..), ToCBOR (..), decodeRecordNamed, encodeListLen)
import Cardano.Ledger.Chain (ChainChecksPParams, pparamsToChainChecksPParams)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC (Crypto, StandardCrypto, VRF)
import Cardano.Ledger.Keys
  ( DSignable,
    GenDelegPair (..),
    GenDelegs (..),
    KESignable,
    KeyHash,
    KeyRole (..),
    SignKeyVRF,
    VRFSignable,
    coerceKeyRole,
  )
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.PoolDistr (PoolDistr (..), individualPoolStake)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis (..))
import Cardano.Ledger.Shelley.LedgerState
  ( EpochState (..),
    NewEpochState (..),
    dpsDState,
    dsGenDelegs,
    lsDPState,
  )
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.Rules (ShelleyTickfPredFailure)
import Cardano.Ledger.Slot (SlotNo)
import Cardano.Protocol.TPraos.BHeader
  ( BHBody,
    BHeader,
    bhbody,
    bheaderPrev,
    checkLeaderValue,
    mkSeed,
    prevHashToNonce,
    seedL,
  )
import Cardano.Protocol.TPraos.OCert (OCertSignable)
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as STS.Prtcl
import Cardano.Protocol.TPraos.Rules.Tickn as STS.Tickn
import Cardano.Slotting.EpochInfo (epochInfoRange)
import Control.Arrow (left, right)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended
  ( BaseM,
    Environment,
    STS,
    Signal,
    State,
    TRC (..),
    applySTS,
    reapplySTS,
  )
import Data.Either (fromRight)
import Data.Functor.Identity (runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

-- =======================================================

class
  ( CC.Crypto c,
    DSignable c (OCertSignable c),
    KESignable c (BHBody c),
    VRFSignable c Seed
  ) =>
  PraosCrypto c

instance PraosCrypto CC.StandardCrypto

class
  ( Eq (ChainTransitionError (EraCrypto era)),
    Show (ChainTransitionError (EraCrypto era)),
    Show (LedgerView (EraCrypto era)),
    Show (FutureLedgerViewError era),
    STS (EraRule "TICKF" era),
    BaseM (EraRule "TICKF" era) ~ ShelleyBase,
    Environment (EraRule "TICKF" era) ~ (),
    State (EraRule "TICKF" era) ~ NewEpochState era,
    Signal (EraRule "TICKF" era) ~ SlotNo,
    PredicateFailure (EraRule "TICKF" era) ~ ShelleyTickfPredFailure era,
    HasField "_d" (PParams era) UnitInterval,
    HasField "_maxBBSize" (PParams era) Natural,
    HasField "_maxBHSize" (PParams era) Natural,
    HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  GetLedgerView era
  where
  currentLedgerView ::
    NewEpochState era ->
    LedgerView (EraCrypto era)
  default currentLedgerView ::
    HasField "_extraEntropy" (PParams era) Nonce =>
    NewEpochState era ->
    LedgerView (EraCrypto era)
  currentLedgerView = view

  -- $timetravel
  futureLedgerView ::
    MonadError (FutureLedgerViewError era) m =>
    Globals ->
    NewEpochState era ->
    SlotNo ->
    m (LedgerView (EraCrypto era))
  default futureLedgerView ::
    ( MonadError (FutureLedgerViewError era) m,
      HasField "_extraEntropy" (PParams era) Nonce
    ) =>
    Globals ->
    NewEpochState era ->
    SlotNo ->
    m (LedgerView (EraCrypto era))
  futureLedgerView = futureView

instance CC.Crypto c => GetLedgerView (ShelleyEra c)

instance CC.Crypto c => GetLedgerView (AllegraEra c)

instance CC.Crypto c => GetLedgerView (MaryEra c)

instance CC.Crypto c => GetLedgerView (AlonzoEra c)

-- Note that although we do not use TPraos in the Babbage era, we include this
-- because it makes it simpler to get the ledger view for Praos.
instance CC.Crypto c => GetLedgerView (BabbageEra c) where
  currentLedgerView
    NewEpochState {nesPd = pd, nesEs = es} =
      LedgerView
        { lvD = getField @"_d" . esPp $ es,
          lvExtraEntropy = error "Extra entropy is not set in the Babbage era",
          lvPoolDistr = pd,
          lvGenDelegs =
            dsGenDelegs
              . dpsDState
              . lsDPState
              $ esLState es,
          lvChainChecks = pparamsToChainChecksPParams . esPp $ es
        }

  futureLedgerView globals ss slot =
    liftEither
      . right currentLedgerView
      . left FutureLedgerViewError
      $ res
    where
      res =
        flip runReader globals
          . applySTS @(EraRule "TICKF" (BabbageEra c))
          $ TRC ((), ss, slot)

-- Note that although we do not use TPraos in the Conway era, we include this
-- because it makes it simpler to get the ledger view for Praos.
instance CC.Crypto c => GetLedgerView (ConwayEra c) where
  currentLedgerView
    NewEpochState {nesPd = pd, nesEs = es} =
      LedgerView
        { lvD = getField @"_d" . esPp $ es,
          lvExtraEntropy = error "Extra entropy is not set in the Conway era",
          lvPoolDistr = pd,
          lvGenDelegs =
            dsGenDelegs
              . dpsDState
              . lsDPState
              $ esLState es,
          lvChainChecks = pparamsToChainChecksPParams . esPp $ es
        }

  futureLedgerView globals ss slot =
    liftEither
      . right currentLedgerView
      . left FutureLedgerViewError
      $ res
    where
      res =
        flip runReader globals
          . applySTS @(EraRule "TICKF" (ConwayEra c))
          $ TRC ((), ss, slot)

-- | Data required by the Transitional Praos protocol from the Shelley ledger.
data LedgerView c = LedgerView
  { lvD :: UnitInterval,
    -- Note that this field is not present in Babbage, but we require this view
    -- in order to construct the Babbage ledger view. We allow this to be lazy
    -- so that we may set it to an error. Note that `LedgerView` is never
    -- serialised, so this should not be forced except as a result of a
    -- programmer error.
    lvExtraEntropy :: ~Nonce,
    lvPoolDistr :: PoolDistr c,
    lvGenDelegs :: GenDelegs c,
    lvChainChecks :: ChainChecksPParams
  }
  deriving (Eq, Show, Generic)

instance NoThunks (LedgerView c)

-- | Construct a protocol environment from the ledger view, along with the
-- current slot and a marker indicating whether this is the first block in a new
-- epoch.
mkPrtclEnv ::
  LedgerView c ->
  -- | Epoch nonce
  Nonce ->
  STS.Prtcl.PrtclEnv c
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
  ( HasField "_d" (PParams era) UnitInterval,
    HasField "_extraEntropy" (PParams era) Nonce,
    HasField "_maxBBSize" (PParams era) Natural,
    HasField "_maxBHSize" (PParams era) Natural,
    HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  NewEpochState era ->
  LedgerView (EraCrypto era)
view
  NewEpochState
    { nesPd = pd,
      nesEs = es
    } =
    let !ee = getField @"_extraEntropy" . esPp $ es
     in LedgerView
          { lvD = getField @"_d" . esPp $ es,
            lvExtraEntropy = ee,
            lvPoolDistr = pd,
            lvGenDelegs =
              dsGenDelegs
                . dpsDState
                . lsDPState
                $ esLState es,
            lvChainChecks = pparamsToChainChecksPParams . esPp $ es
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
  = FutureLedgerViewError [PredicateFailure (EraRule "TICKF" era)]

deriving stock instance
  (Eq (PredicateFailure (EraRule "TICKF" era))) =>
  Eq (FutureLedgerViewError era)

deriving stock instance
  (Show (PredicateFailure (EraRule "TICKF" era))) =>
  Show (FutureLedgerViewError era)

-- | Anachronistic ledger view
--
--   Given a slot within the future stability window from our current slot (the
--   slot corresponding to the passed-in 'NewEpochState'), return a 'LedgerView'
--   appropriate to that slot.
futureView ::
  forall era m.
  ( MonadError (FutureLedgerViewError era) m,
    STS (EraRule "TICKF" era),
    BaseM (EraRule "TICKF" era) ~ ShelleyBase,
    Environment (EraRule "TICKF" era) ~ (),
    State (EraRule "TICKF" era) ~ NewEpochState era,
    Signal (EraRule "TICKF" era) ~ SlotNo,
    PredicateFailure (EraRule "TICKF" era) ~ ShelleyTickfPredFailure era,
    HasField "_d" (PParams era) UnitInterval,
    HasField "_extraEntropy" (PParams era) Nonce,
    HasField "_maxBBSize" (PParams era) Natural,
    HasField "_maxBHSize" (PParams era) Natural,
    HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  Globals ->
  NewEpochState era ->
  SlotNo ->
  m (LedgerView (EraCrypto era))
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

data ChainDepState c = ChainDepState
  { csProtocol :: !(STS.Prtcl.PrtclState c),
    csTickn :: !STS.Tickn.TicknState,
    -- | Nonce constructed from the hash of the last applied block header.
    csLabNonce :: !Nonce
  }
  deriving (Eq, Show, Generic)

-- | Construct an initial chain state given an initial nonce and a set of
-- genesis delegates.
initialChainDepState ::
  Nonce ->
  Map (KeyHash 'Genesis c) (GenDelegPair c) ->
  ChainDepState c
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

instance CC.Crypto c => NoThunks (ChainDepState c)

instance CC.Crypto c => FromCBOR (ChainDepState c) where
  fromCBOR =
    decodeRecordNamed
      "ChainDepState"
      (const 3)
      ( ChainDepState
          <$> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
      )

instance CC.Crypto c => ToCBOR (ChainDepState c) where
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

newtype ChainTransitionError c
  = ChainTransitionError [PredicateFailure (STS.Prtcl.PRTCL c)]
  deriving (Generic)

instance (CC.Crypto c) => NoThunks (ChainTransitionError c)

deriving instance (CC.Crypto c) => Eq (ChainTransitionError c)

deriving instance (CC.Crypto c) => Show (ChainTransitionError c)

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
                csLabNonce,
              csTickn,
              isNewEpoch
            )

-- | Update the chain state based upon a new block header.
--
--   This also updates the last applied block hash.
updateChainDepState ::
  forall c m.
  ( PraosCrypto c,
    MonadError (ChainTransitionError c) m
  ) =>
  Globals ->
  LedgerView c ->
  BHeader c ->
  ChainDepState c ->
  m (ChainDepState c)
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
          . applySTS @(STS.Prtcl.PRTCL c)
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
  forall c.
  PraosCrypto c =>
  Globals ->
  LedgerView c ->
  BHeader c ->
  ChainDepState c ->
  ChainDepState c
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
          . reapplySTS @(STS.Prtcl.PRTCL c)
          $ TRC
            ( mkPrtclEnv lv epochNonce,
              csProtocol,
              bh
            )
      epochNonce = STS.Tickn.ticknStateEpochNonce csTickn

-- | Get the (private) leader schedule for this epoch.
--
--   Given a private VRF key, returns the set of slots in which this node is
--   eligible to lead.
getLeaderSchedule ::
  ( Era era,
    VRF.Signable
      (CC.VRF (EraCrypto era))
      Seed,
    HasField "_d" (PParams era) UnitInterval
  ) =>
  Globals ->
  NewEpochState era ->
  ChainDepState (EraCrypto era) ->
  KeyHash 'StakePool (EraCrypto era) ->
  SignKeyVRF (EraCrypto era) ->
  PParams era ->
  Set SlotNo
getLeaderSchedule globals ss cds poolHash key pp = Set.filter isLeader epochSlots
  where
    isLeader slotNo =
      let y = VRF.evalCertified () (mkSeed seedL slotNo epochNonce) key
       in not (isOverlaySlot a (getField @"_d" pp) slotNo)
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
  forall c.
  ShelleyGenesis (ShelleyEra c) ->
  LedgerView c
mkInitialShelleyLedgerView genesisShelley =
  let !ee = _extraEntropy . sgProtocolParams $ genesisShelley
   in LedgerView
        { lvD = _d . sgProtocolParams $ genesisShelley,
          lvExtraEntropy = ee,
          lvPoolDistr = PoolDistr Map.empty,
          lvGenDelegs = GenDelegs $ sgGenDelegs genesisShelley,
          lvChainChecks = pparamsToChainChecksPParams . sgProtocolParams $ genesisShelley
        }
