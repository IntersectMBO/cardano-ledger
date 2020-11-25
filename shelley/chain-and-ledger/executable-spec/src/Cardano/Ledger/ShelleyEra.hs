module Cardano.Ledger.ShelleyEra where
-- TODO: export explicitly

import qualified Cardano.Ledger.Crypto as CryptoClass
import Shelley.Spec.Ledger.Coin (Coin)

-- TODO: import implicitly
import Cardano.Ledger.Era
import Shelley.Spec.Ledger.STS.Ppup
import Cardano.Ledger.Core
import Shelley.Spec.Ledger.API

data ShelleyEra c

instance CryptoClass.Crypto c => Era (ShelleyEra c) where
  type Crypto (ShelleyEra c) = c

type instance Value (ShelleyEra c) = Coin

type instance Script (ShelleyEra c) = MultiSig (ShelleyEra c)

type instance UpdateSTS (ShelleyEra c) = PPUP (ShelleyEra c)

-- | instance of MultiSignatureScript type class
instance
  (CryptoClass.Crypto c, TxBodyConstraints (ShelleyEra c)) =>
  ValidateScript (ShelleyEra c)
  where
  validateScript = validateNativeMultiSigScript
  hashScript = hashMultiSigScript


--------------------------------------------------------------------------------
-- Ledger state instances (TODO: consider putting these in a separate module)
--------------------------------------------------------------------------------

emptyUTxOState :: UTxOState (ShelleyEra c)
emptyUTxOState = UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyPPUPState
