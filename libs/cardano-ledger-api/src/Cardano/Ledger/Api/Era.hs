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
  Shelley,
  ShelleyEra,

  -- ** Allegra
  Allegra,
  AllegraEra,

  -- ** Mary
  Mary,
  MaryEra,

  -- ** Alonzo
  Alonzo,
  AlonzoEra,

  -- ** Babbage
  Babbage,
  BabbageEra,

  -- ** Conway
  Conway,
  ConwayEra,

  -- ** Latest Known
  LatestKnownEra,

  -- * Crypto
  StandardCrypto,
  Crypto (..),

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
)
where

import Cardano.Ledger.Allegra (Allegra, AllegraEra)
import Cardano.Ledger.Alonzo (Alonzo, AlonzoEra)
import Cardano.Ledger.Babbage (Babbage, BabbageEra)
import Cardano.Ledger.Conway (Conway, ConwayEra)
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
import Cardano.Ledger.Crypto (Crypto (..), StandardCrypto)
import Cardano.Ledger.Mary (Mary, MaryEra)
import Cardano.Ledger.Shelley (Shelley, ShelleyEra)

-- | Sometimes it is useful to specify that a type corresponds to a latest era that is
-- currently implemented
type LatestKnownEra c = ConwayEra c
