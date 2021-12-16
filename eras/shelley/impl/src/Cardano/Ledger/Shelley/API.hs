{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | API to the Shelley ledger
module Cardano.Ledger.Shelley.API
  ( module X,
    ShelleyBasedEra,
  )
where

import Cardano.Ledger.Core (ChainData, SerialisableData)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys (DSignable)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API.ByronTranslation as X
import Cardano.Ledger.Shelley.API.Genesis as X
import Cardano.Ledger.Shelley.API.Mempool as X
import Cardano.Ledger.Shelley.API.Protocol as X
import Cardano.Ledger.Shelley.API.Types as X
import Cardano.Ledger.Shelley.API.Validation as X
import Cardano.Ledger.Shelley.API.Wallet as X
import Cardano.Ledger.Shelley.Constraints
  ( UsesAuxiliary,
    UsesPParams,
    UsesScript,
    UsesTxBody,
    UsesTxOut,
    UsesValue,
  )
import Control.State.Transition (State)
import Data.Sharing (FromSharedCBOR, Interns, Share)

class
  ( CC.Crypto (Crypto era),
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
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    ChainData (State (Core.EraRule "PPUP" era)),
    SerialisableData (State (Core.EraRule "PPUP" era)),
    Share (Core.TxOut era) ~ Interns (Credential 'Staking (Crypto era)),
    FromSharedCBOR (Core.TxOut era)
  ) =>
  ShelleyBasedEra era

instance
  ( CC.Crypto crypto,
    DSignable crypto (Hash crypto EraIndependentTxBody)
  ) =>
  ShelleyBasedEra (ShelleyEra crypto)
