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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.MockChain where

import Cardano.Ledger.BaseTypes (BlocksMade (..), ShelleyBase)

-- import Cardano.Ledger.Conway.Rules (
--   ConwayLedgersEnv (..),
--  )

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Api.Era (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  AccountState,
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  StashedAVVMAddresses,
  curPParamsEpochStateL,
 )
import Cardano.Ledger.Shelley.RewardUpdate (PulsingRewUpdate)
import Cardano.Ledger.Shelley.Rules (
  LedgerEnv,
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
import Control.State.Transition (
  Embed (..),
  STS (..),
  TRC (..),
  judgmentContext,
  trans,
  (?!),
 )
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe)
import Data.Sequence.Internal (Seq)
import Data.Sequence.Strict (StrictSeq (..), fromStrict)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks, ThunkInfo, noThunks)
import Test.Cardano.Ledger.Generic.Functions (TotalAda (..))
import Test.Cardano.Ledger.Generic.PrettyCore (
  PDoc,
  PrettyA (..),
  pcKeyHash,
  pcNewEpochState,
  pcSlotNo,
  ppInt,
  ppRecord,
  ppShelleyLedgersPredFailure,
  ppTickPredicateFailure,
 )
import Test.Cardano.Ledger.Generic.Proof (Proof (..), Reflect (reify))

-- ================================================

data MOCKCHAIN era -- This is a Testing only STS instance

type instance EraRule "MOCKCHAIN" era = MOCKCHAIN era

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
  { mbIssuer :: !(KeyHash 'StakePool (EraCrypto era))
  , mbSlot :: !SlotNo
  , mbTrans :: !(StrictSeq (Tx era))
  }

data MockChainState era = MockChainState
  { mcsNes :: !(NewEpochState era)
  , mcsTickNes :: !(NewEpochState era)
  , mcsLastBlock :: !SlotNo
  , mcsCount :: !Int -- Counts the blocks made
  }

deriving instance
  ( EraTxOut era
  , Eq (StashedAVVMAddresses era)
  , Eq (GovState era)
  ) =>
  Eq (MockChainState era)

instance Show (MockChainState era) where
  show (MockChainState nes _ slot count) =
    show count ++ " " ++ show slot ++ "\n  " ++ show (nesBcur nes)

instance Show (MockBlock era) where
  show (MockBlock is sl _) = show is ++ " " ++ show sl

instance Reflect era => TotalAda (MockChainState era) where
  totalAda (MockChainState nes _ _ _) = totalAda nes

deriving instance Generic (MockChainState era)

instance (Era era, NoThunks (NewEpochState era)) => NoThunks (MockChainState era)

-- ======================================================================

-- type family LedgersEnv era where
--   LedgersEnv (ConwayEra StandardCrypto) = ConwayLedgersEnv (ConwayEra StandardCrypto)
--   LedgersEnv era = ShelleyLedgersEnv era

-- TODO WG Can you use something like this to avoid duplication of API/Validation.hs?
class LedgersEra era where
  type LedgersEnv era
  getLedgersEnv :: SlotNo -> PParams era -> AccountState -> LedgersEnv era

instance LedgersEra (ShelleyEra c) where
  type LedgersEnv (ShelleyEra c) = ShelleyLedgersEnv (ShelleyEra c)
  getLedgersEnv slot pparams account = LedgersEnv slot pparams account

instance LedgersEra (BabbageEra c) where
  type LedgersEnv (BabbageEra c) = ShelleyLedgersEnv (BabbageEra c)
  getLedgersEnv slot pparams account = LedgersEnv slot pparams account

instance LedgersEra (MaryEra c) where
  type LedgersEnv (MaryEra c) = ShelleyLedgersEnv (MaryEra c)
  getLedgersEnv slot pparams account = LedgersEnv slot pparams account

instance LedgersEra (AlonzoEra c) where
  type LedgersEnv (AlonzoEra c) = ShelleyLedgersEnv (AlonzoEra c)
  getLedgersEnv slot pparams account = LedgersEnv slot pparams account

instance LedgersEra (AllegraEra c) where
  type LedgersEnv (AllegraEra c) = ShelleyLedgersEnv (AllegraEra c)
  getLedgersEnv slot pparams account = LedgersEnv slot pparams account

instance LedgersEra (ConwayEra c) where
  type LedgersEnv (ConwayEra c) = ShelleyLedgersEnv (ConwayEra c)
  getLedgersEnv slot pparams account = LedgersEnv slot pparams account

instance
  ( LedgersEra era
  , EraGov era
  , STS (ShelleyTICK era)
  , State (EraRule "TICK" era) ~ NewEpochState era
  , Signal (EraRule "TICK" era) ~ SlotNo
  , Environment (EraRule "TICK" era) ~ ()
  , Embed (EraRule "TICK" era) (MOCKCHAIN era)
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , Environment (EraRule "LEDGERS" era) ~ LedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Embed (EraRule "LEDGERS" era) (MOCKCHAIN era)
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Eq (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "LEDGER" era))
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
    where
      chainTransition = do
        TRC (_, MockChainState nes _ lastSlot count, MockBlock issuer slot txs) <- judgmentContext
        lastSlot < slot ?! BlocksOutOfOrder lastSlot slot

        nes' <- trans @(EraRule "TICK" era) $ TRC ((), nes, slot)

        let NewEpochState _ _ (BlocksMade current) epochState _ _ _ = nes'
            EpochState account ledgerState _ _ = epochState
            pparams = epochState ^. curPParamsEpochStateL

        let newblocksmade = BlocksMade (Map.unionWith (+) current (Map.singleton issuer 1))

        newledgerState <-
          trans @(EraRule "LEDGERS" era) $
            TRC (getLedgersEnv slot pparams account, ledgerState, fromStrict txs)

        let newEpochstate = epochState {esLState = newledgerState}
            newNewEpochState = nes' {nesEs = newEpochstate, nesBcur = newblocksmade}

        pure (MockChainState newNewEpochState nes' slot (count + 1))

-- ===========================
-- Embed instances

instance
  ( STS (ShelleyTICK era)
  , Signal (EraRule "RUPD" era) ~ SlotNo
  , State (EraRule "RUPD" era) ~ StrictMaybe (PulsingRewUpdate (EraCrypto era))
  , Environment (EraRule "RUPD" era) ~ RupdEnv era
  , State (EraRule "NEWEPOCH" era) ~ NewEpochState era
  , Signal (EraRule "NEWEPOCH" era) ~ EpochNo
  , State (EraRule "NEWEPOCH" era) ~ NewEpochState era
  , Environment (EraRule "NEWEPOCH" era) ~ ()
  ) =>
  Embed (ShelleyTICK era) (MOCKCHAIN era)
  where
  wrapFailed = MockChainFromTickFailure
  wrapEvent = MockChainFromTickEvent

instance
  ( STS (ShelleyLEDGERS era)
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , Signal (EraRule "LEDGER" era) ~ Tx era
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
  (Show (ShelleyTickPredFailure era), Show (ShelleyLedgersPredFailure era)) =>
  Show (MockChainFailure era)

deriving instance
  (Eq (ShelleyTickPredFailure era), Eq (ShelleyLedgersPredFailure era)) => Eq (MockChainFailure era)

ppMockChainState ::
  Reflect era =>
  MockChainState era ->
  PDoc
ppMockChainState (MockChainState nes _ sl count) =
  ppRecord
    "MockChainState"
    [ ("NewEpochState", pcNewEpochState reify nes)
    , ("LastBlock", pcSlotNo sl)
    , ("Count", ppInt count)
    ]

instance Reflect era => PrettyA (MockChainState era) where
  prettyA = ppMockChainState

ppMockBlock :: MockBlock era -> PDoc
ppMockBlock (MockBlock iss sl txs) =
  ppRecord
    "MockBock"
    [ ("Issuer", pcKeyHash iss)
    , ("Slot", pcSlotNo sl)
    , ("Transactions", ppInt (length txs))
    ]

instance PrettyA (MockBlock era) where prettyA = ppMockBlock

ppMockChainFailure :: Reflect era => Proof era -> MockChainFailure era -> PDoc
ppMockChainFailure proof x = case proof of
  Conway -> help x
  Babbage -> help x
  Alonzo -> help x
  Mary -> help x
  Allegra -> help x
  Shelley -> help x
  where
    help (MockChainFromTickFailure y) = ppTickPredicateFailure y
    help (MockChainFromLedgersFailure y) = ppShelleyLedgersPredFailure proof y
    help (BlocksOutOfOrder lastslot cand) =
      ppRecord
        "BlocksOutOfOrder"
        [ ("Last applied block", pcSlotNo lastslot)
        , ("Candidate block", pcSlotNo cand)
        ]

noThunksGen :: Proof era -> MockChainState era -> IO (Maybe ThunkInfo)
noThunksGen Conway = noThunks []
noThunksGen Babbage = noThunks []
noThunksGen Alonzo = noThunks []
noThunksGen Mary = noThunks []
noThunksGen Allegra = noThunks []
noThunksGen Shelley = noThunks []
