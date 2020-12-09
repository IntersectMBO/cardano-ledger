{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo where

import Cardano.Ledger.Alonzo.Scripts (Script)
import Cardano.Ledger.Alonzo.TxBody (TxBody)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto
import Cardano.Ledger.Era
import Cardano.Ledger.Mary.Value (Value)
import Cardano.Ledger.ShelleyMA.Metadata (Metadata)

-- | The Alonzo era
data AlonzoEra c

instance
  (Cardano.Ledger.Crypto.Crypto c) =>
  Era (AlonzoEra c)
  where
  type Crypto (AlonzoEra c) = c

type instance Core.TxBody (AlonzoEra c) = TxBody (AlonzoEra c)

type instance Core.Value (AlonzoEra c) = Value c

type instance Core.Script (AlonzoEra c) = Script (AlonzoEra c)

type instance Core.Metadata (AlonzoEra c) = Metadata (AlonzoEra c)
