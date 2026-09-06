{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Transition (
  TransitionConfig (..),
  seatInitialLeiosCommittee,
) where

import Cardano.Ledger.Alonzo.Transition (AlonzoEraTransition)
import Cardano.Ledger.BaseTypes (EpochInterval (..))
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Transition (
  ConwayEraTransition,
  conwayInjectIntoTestState,
 )
import Cardano.Ledger.Dijkstra.Era
import Cardano.Ledger.Dijkstra.Genesis
import Cardano.Ledger.Dijkstra.PParams (ppLeiosCommitteeSizeL)
import Cardano.Ledger.Dijkstra.Translation ()
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  curPParamsEpochStateL,
  esSnapshotsL,
  nesELL,
  nesEsL,
 )
import Cardano.Ledger.Shelley.Transition
import Cardano.Ledger.State (
  mkSnapShot,
  ssActiveStake,
  ssStakeMarkL,
  ssStakePoolsSnapShot,
 )
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks (..))

instance EraTransition DijkstraEra where
  data TransitionConfig DijkstraEra = DijkstraTransitionConfig
    { dtcDijkstraGenesis :: !DijkstraGenesis
    , dtcConwayTransitionConfig :: !(TransitionConfig ConwayEra)
    }
    deriving (Show, Eq, Generic)

  mkTransitionConfig = DijkstraTransitionConfig

  injectIntoTestState hasFS cfg nes =
    seatInitialLeiosCommittee <$> conwayInjectIntoTestState hasFS cfg nes

  tcPreviousEraConfigL =
    lens dtcConwayTransitionConfig (\dtc pc -> dtc {dtcConwayTransitionConfig = pc})

  tcTranslationContextL =
    lens dtcDijkstraGenesis (\dtc ag -> dtc {dtcDijkstraGenesis = ag})

instance AlonzoEraTransition DijkstraEra

-- | Seat the Leios voting committee (CIP-0164) on the initial stake snapshot.
--
-- Genesis never runs SNAP, and the snapshot it produces comes from era-generic
-- code that has no way to reach @leiosCommitteeSize@ -- so a network booting
-- straight into Dijkstra would carry an unseated committee until the snapshot
-- pipeline has turned over. Consensus fills @set@\/@go@ from @mark@ for such
-- networks, so seating @mark@ here is enough for all three.
--
-- Genesis pools are registered in the state's own epoch, so judging the seats
-- against that epoch with a one-epoch allowance honours every key genesis
-- carries; SNAP reseats them at the first epoch boundary regardless. The age
-- derived from the KES setup is not reachable here -- this runs outside
-- 'Cardano.Ledger.BaseTypes.ShelleyBase', so there are no @Globals@ to read.
seatInitialLeiosCommittee :: NewEpochState DijkstraEra -> NewEpochState DijkstraEra
seatInitialLeiosCommittee nes =
  nes & nesEsL . esSnapshotsL . ssStakeMarkL %~ reseat
  where
    reseat snap =
      mkSnapShot
        (nes ^. nesELL)
        (EpochInterval 1)
        (nes ^. nesEsL . curPParamsEpochStateL . ppLeiosCommitteeSizeL)
        (ssActiveStake snap)
        (ssStakePoolsSnapShot snap)

instance ConwayEraTransition DijkstraEra

instance NoThunks (TransitionConfig DijkstraEra)
