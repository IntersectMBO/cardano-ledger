{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.MockChain where

import Cardano.Ledger.BaseTypes (BlocksMade (..), ShelleyBase)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Pretty
  ( PDoc,
    PrettyA (..),
    ppInt,
    ppKeyHash,
    ppRecord,
    ppSlotNo,
  )
import Cardano.Ledger.Shelley.LedgerState
  ( EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    StashedAVVMAddresses,
  )
import Cardano.Ledger.Shelley.RewardUpdate (PulsingRewUpdate)
import Cardano.Ledger.Shelley.Rules
  ( LedgerEnv,
    RupdEnv,
    ShelleyLEDGERS,
    ShelleyLedgersEnv (..),
    ShelleyLedgersEvent,
    ShelleyLedgersPredFailure,
    ShelleyTICK,
    ShelleyTickEvent,
    ShelleyTickPredFailure,
  )
import Cardano.Slotting.Slot (EpochNo, SlotNo)
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    trans,
    (?!),
  )
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe)
import Data.Sequence.Strict (StrictSeq (..), fromStrict)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks, ThunkInfo, noThunks)
import Test.Cardano.Ledger.Generic.Functions (TotalAda (..))
import Test.Cardano.Ledger.Generic.PrettyCore
  ( pcNewEpochState,
    ppLedgersPredicateFailure,
    ppTickPredicateFailure,
  )
import Test.Cardano.Ledger.Generic.Proof (Proof (..), Reflect (reify))

-- ================================================

data MOCKCHAIN era -- This is a Testing only STS instance

type instance Core.EraRule "MOCKCHAIN" era = MOCKCHAIN era

data MockChainFailure era
  = MockChainFromTickFailure !(ShelleyTickPredFailure era)
  | MockChainFromLedgersFailure !(ShelleyLedgersPredFailure era)
  | BlocksOutOfOrder
      !SlotNo -- The last applied block SlotNo
      !SlotNo -- The candidate block SlotNo

data MockChainEvent era
  = MockChainFromTickEvent !(ShelleyTickEvent era)
  | MockChainFromLedgersEvent !(ShelleyLedgersEvent era)

data MockBlock era = MockBlock
  { mbIssuer :: !(KeyHash 'StakePool (EraCrypto era)),
    mbSlot :: !SlotNo,
    mbTrans :: !(StrictSeq (Core.Tx era))
  }

data MockChainState era = MockChainState
  { mcsNes :: !(NewEpochState era),
    mcsLastBlock :: !SlotNo,
    mcsCount :: !Int -- Counts the blocks made
  }

deriving instance
  ( CC.Crypto (EraCrypto era),
    Eq (Core.TxOut era),
    Eq (Core.PParams era),
    Eq (State (Core.EraRule "PPUP" era)),
    Eq (StashedAVVMAddresses era)
  ) =>
  Eq (MockChainState era)

instance Show (MockChainState era) where
  show (MockChainState nes slot count) =
    show count ++ " " ++ show slot ++ "\n  " ++ show (nesBcur nes)

instance Show (MockBlock era) where
  show (MockBlock is sl _) = show is ++ " " ++ show sl

instance Reflect era => TotalAda (MockChainState era) where
  totalAda (MockChainState nes _ _) = totalAda nes

deriving instance Generic (MockChainState era)

instance (Era era, NoThunks (NewEpochState era)) => NoThunks (MockChainState era)

-- ======================================================================

instance
  ( Reflect era,
    STS (ShelleyLEDGERS era),
    STS (ShelleyTICK era),
    State (Core.EraRule "TICK" era) ~ NewEpochState era,
    Signal (Core.EraRule "TICK" era) ~ SlotNo,
    Environment (Core.EraRule "TICK" era) ~ (),
    Signal (Core.EraRule "LEDGER" era) ~ Core.Tx era,
    Environment (Core.EraRule "LEDGER" era) ~ LedgerEnv era,
    State (Core.EraRule "LEDGER" era) ~ LedgerState era,
    Embed (Core.EraRule "TICK" era) (MOCKCHAIN era)
  ) =>
  STS (MOCKCHAIN era)
  where
  type State (MOCKCHAIN era) = MockChainState era
  type Signal (MOCKCHAIN era) = (MockBlock era)
  type Environment (MOCKCHAIN era) = ()
  type BaseM (MOCKCHAIN era) = ShelleyBase
  type PredicateFailure (MOCKCHAIN era) = MockChainFailure era
  type Event (MOCKCHAIN era) = MockChainEvent era
  initialRules = [error "INITIAL RULE CALLED IN MOCKCHAIN"]
  transitionRules = [chainTransition]

chainTransition ::
  forall era.
  ( STS (ShelleyLEDGERS era),
    State (Core.EraRule "TICK" era) ~ NewEpochState era,
    Signal (Core.EraRule "TICK" era) ~ SlotNo,
    Environment (Core.EraRule "TICK" era) ~ (),
    Signal (Core.EraRule "LEDGER" era) ~ Core.Tx era,
    Environment (Core.EraRule "LEDGER" era) ~ LedgerEnv era,
    State (Core.EraRule "LEDGER" era) ~ LedgerState era,
    Embed (Core.EraRule "TICK" era) (MOCKCHAIN era)
  ) =>
  TransitionRule (MOCKCHAIN era)
chainTransition = do
  TRC (_, MockChainState nes lastSlot count, MockBlock issuer slot txs) <- judgmentContext

  lastSlot < slot ?! BlocksOutOfOrder lastSlot slot

  nes' <- trans @(Core.EraRule "TICK" era) $ TRC ((), nes, slot)

  let NewEpochState _ _ (BlocksMade current) epochState _ _ _ = nes'
      EpochState account _ ledgerState _ pparams _ = epochState

  let newblocksmade = BlocksMade (Map.unionWith (+) current (Map.singleton issuer 1))

  newledgerState <-
    trans @(ShelleyLEDGERS era) $ TRC (LedgersEnv slot pparams account, ledgerState, fromStrict txs)

  let newEpochstate = epochState {esLState = newledgerState}
      newNewEpochState = nes' {nesEs = newEpochstate, nesBcur = newblocksmade}

  pure (MockChainState newNewEpochState slot (count + 1))

-- ===========================
-- Embed instances

instance
  ( STS (ShelleyTICK era),
    Signal (Core.EraRule "RUPD" era) ~ SlotNo,
    State (Core.EraRule "RUPD" era) ~ StrictMaybe (PulsingRewUpdate (EraCrypto era)),
    Environment (Core.EraRule "RUPD" era) ~ RupdEnv era,
    State (Core.EraRule "NEWEPOCH" era) ~ NewEpochState era,
    Signal (Core.EraRule "NEWEPOCH" era) ~ EpochNo,
    State (Core.EraRule "NEWEPOCH" era) ~ NewEpochState era,
    Environment (Core.EraRule "NEWEPOCH" era) ~ ()
  ) =>
  Embed (ShelleyTICK era) (MOCKCHAIN era)
  where
  wrapFailed = MockChainFromTickFailure
  wrapEvent = MockChainFromTickEvent

instance
  ( STS (ShelleyLEDGERS era),
    State (Core.EraRule "LEDGER" era) ~ LedgerState era,
    Environment (Core.EraRule "LEDGER" era) ~ LedgerEnv era,
    Signal (Core.EraRule "LEDGER" era) ~ Core.Tx era
  ) =>
  Embed (ShelleyLEDGERS era) (MOCKCHAIN era)
  where
  wrapFailed = MockChainFromLedgersFailure
  wrapEvent = MockChainFromLedgersEvent

-- ================================================================

deriving instance
  (Show (ShelleyTickEvent era), Show (ShelleyLedgersEvent era)) => Show (MockChainEvent era)

deriving instance
  (Eq (ShelleyTickEvent era), Eq (ShelleyLedgersEvent era)) => Eq (MockChainEvent era)

deriving instance
  (Show (ShelleyTickPredFailure era), Show (ShelleyLedgersPredFailure era)) => Show (MockChainFailure era)

deriving instance
  (Eq (ShelleyTickPredFailure era), Eq (ShelleyLedgersPredFailure era)) => Eq (MockChainFailure era)

ppMockChainState ::
  Reflect era =>
  MockChainState era ->
  PDoc
ppMockChainState (MockChainState nes sl count) =
  ppRecord
    "MockChainState"
    [ ("NewEpochState", pcNewEpochState reify nes),
      ("LastBlock", ppSlotNo sl),
      ("Count", ppInt count)
    ]

instance Reflect era => PrettyA (MockChainState era) where
  prettyA = ppMockChainState

ppMockBlock :: MockBlock era -> PDoc
ppMockBlock (MockBlock iss sl txs) =
  ppRecord
    "MockBock"
    [ ("Issuer", ppKeyHash iss),
      ("Slot", ppSlotNo sl),
      ("Transactions", ppInt (length txs))
    ]

instance PrettyA (MockBlock era) where prettyA = ppMockBlock

ppMockChainFailure :: Proof era -> MockChainFailure era -> PDoc
ppMockChainFailure proof x = case proof of
  (Conway _) -> help x
  (Babbage _) -> help x
  (Alonzo _) -> help x
  (Mary _) -> help x
  (Allegra _) -> help x
  (Shelley _) -> help x
  where
    help (MockChainFromTickFailure y) = ppTickPredicateFailure y
    help (MockChainFromLedgersFailure y) = ppLedgersPredicateFailure y
    help (BlocksOutOfOrder lastslot cand) =
      ppRecord
        "BlocksOutOfOrder"
        [ ("Last applied block", ppSlotNo lastslot),
          ("Candidate block", ppSlotNo cand)
        ]

noThunksGen :: Proof era -> MockChainState era -> IO (Maybe ThunkInfo)
noThunksGen (Conway _) = noThunks []
noThunksGen (Babbage _) = noThunks []
noThunksGen (Alonzo _) = noThunks []
noThunksGen (Mary _) = noThunks []
noThunksGen (Allegra _) = noThunks []
noThunksGen (Shelley _) = noThunks []
