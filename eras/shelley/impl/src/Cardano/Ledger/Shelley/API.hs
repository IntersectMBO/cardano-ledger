{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | API to the Shelley ledger
module Cardano.Ledger.Shelley.API
  ( module X,
    ShelleyBasedEra,
  )
where

import Cardano.Ledger.Core (ChainData, SerialisableData)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API.ByronTranslation as X
import Cardano.Ledger.Shelley.API.Genesis as X
import Cardano.Ledger.Shelley.API.Mempool as X
import Cardano.Ledger.Shelley.API.Protocol as X
import Cardano.Ledger.Shelley.API.Types as X
import Cardano.Ledger.Shelley.API.Validation as X
import Cardano.Ledger.Shelley.API.Wallet as X hiding (getLeaderSchedule)
import Cardano.Ledger.Shelley.Constraints
  ( UsesAuxiliary,
    UsesPParams,
    UsesScript,
    UsesTxBody,
    UsesTxOut,
    UsesValue,
  )
import Control.State.Transition (State)

class
  ( PraosCrypto (Crypto era),
    GetLedgerView era,
    ApplyBlock era,
    ApplyTx era,
    CanStartFromGenesis era,
    UsesValue era,
    UsesScript era,
    UsesAuxiliary era,
    UsesTxBody era,
    UsesTxOut era,
    UsesPParams era,
    ChainData (State (Core.EraRule "PPUP" era)),
    SerialisableData (State (Core.EraRule "PPUP" era))
  ) =>
  ShelleyBasedEra era

instance PraosCrypto crypto => ShelleyBasedEra (ShelleyEra crypto)
