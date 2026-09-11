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
  MarkSnapShot (..),
  ssStakeMarkL,
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

-- | Record the Leios committee inputs (CIP-0164) on the initial mark snapshot.
--
-- Genesis never runs SNAP, and the mark it produces comes from era-generic
-- code that has no way to reach @leiosCommitteeSize@, so it carries a zero
-- size. Stamp the real epoch and committee size here; the committee itself is
-- seated when the mark rotates into the set position. Consensus fills
-- @set@\/@go@ from @mark@ for a network booting straight into Dijkstra, so
-- stamping @mark@ is enough for all three.
seatInitialLeiosCommittee :: NewEpochState DijkstraEra -> NewEpochState DijkstraEra
seatInitialLeiosCommittee nes =
  nes & nesEsL . esSnapshotsL . ssStakeMarkL %~ stampInputs
  where
    stampInputs mark =
      mark
        { msEpochNo = nes ^. nesELL
        , msLeiosCommitteeSize = nes ^. nesEsL . curPParamsEpochStateL . ppLeiosCommitteeSizeL
        }

instance ConwayEraTransition DijkstraEra

instance NoThunks (TransitionConfig DijkstraEra)
