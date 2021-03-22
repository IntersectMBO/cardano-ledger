{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | API to the Shelley ledger
module Shelley.Spec.Ledger.API
  ( module X,
    ShelleyBasedEra,
  )
where

import Cardano.Ledger.Core (ChainData, SerialisableData)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Constraints
  ( UsesAuxiliary,
    UsesPParams,
    UsesScript,
    UsesTxBody,
    UsesTxOut,
    UsesValue,
  )
import Control.State.Transition (State)
import Shelley.Spec.Ledger.API.ByronTranslation as X
import Shelley.Spec.Ledger.API.Genesis as X
import Shelley.Spec.Ledger.API.Mempool as X
import Shelley.Spec.Ledger.API.Protocol as X
import Shelley.Spec.Ledger.API.Types as X
import Shelley.Spec.Ledger.API.Validation as X
import Shelley.Spec.Ledger.API.Wallet as X

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
