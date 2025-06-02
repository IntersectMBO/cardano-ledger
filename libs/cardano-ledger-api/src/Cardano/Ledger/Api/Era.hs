module Cardano.Ledger.Api.Era (
  -- * Eras
  Era (
    PreviousEra,
    ProtVerLow,
    ProtVerHigh
  ),
  eraName,

  -- ** Byron
  ByronEra,

  -- ** Shelley
  ShelleyEra,

  -- ** Allegra
  AllegraEra,

  -- ** Mary
  MaryEra,

  -- ** Alonzo
  AlonzoEra,

  -- ** Babbage
  BabbageEra,

  -- ** Conway
  ConwayEra,

  -- ** Dijkstra
  DijkstraEra,

  -- ** Latest Known
  LatestKnownEra,

  -- * Protocol version

  -- ** Value level
  eraProtVerHigh,
  eraProtVerLow,

  -- ** Type level constraints
  AtLeastEra,
  AtMostEra,
  ExactEra,
  ProtVerAtLeast,
  ProtVerAtMost,
  ProtVerInBounds,
  atLeastEra,
  atMostEra,
) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (
  AtLeastEra,
  AtMostEra,
  ByronEra,
  Era (..),
  ExactEra,
  ProtVerAtLeast,
  ProtVerAtMost,
  ProtVerInBounds,
  atLeastEra,
  atMostEra,
  eraProtVerHigh,
  eraProtVerLow,
 )
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)

-- | Sometimes it is useful to specify that a type corresponds to a latest era that is
-- currently implemented
type LatestKnownEra = ConwayEra
