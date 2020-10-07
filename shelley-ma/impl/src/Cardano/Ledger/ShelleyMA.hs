{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.ShelleyMA where

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto
import Cardano.Ledger.Era
import Cardano.Ledger.ShelleyMA.Value (Value)
import Data.Kind (Type)
import Data.Typeable (Typeable)
import Shelley.Spec.Ledger.Coin (Coin)

data MaryOrAllegra = Mary | Allegra

-- | The Shelley Mary/Allegra eras
--
-- Both eras are implemented within the same codebase, matching the formal
-- specification. They differ only in the @value@ type. Due to some annoying
-- issues with 'Coin' and 'Value' being of different kinds, we don't parametrise
-- over the value but instead over a closed kind 'MaryOrAllegra'. But this
-- should be transparent to the user.
data ShelleyMAEra (ma :: MaryOrAllegra) c

instance
  (Typeable ma, Cardano.Ledger.Crypto.Crypto c) =>
  Era (ShelleyMAEra ma c)
  where
  type Crypto (ShelleyMAEra ma c) = c

type family MAValue (x :: MaryOrAllegra) era :: Type where
  MAValue Allegra era = Coin
  MAValue Mary era = Value era

type instance Core.Value (ShelleyMAEra m c) = MAValue m (ShelleyMAEra m c)
