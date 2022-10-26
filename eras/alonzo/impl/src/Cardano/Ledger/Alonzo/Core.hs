{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Ledger.Alonzo.Core
  ( AlonzoEraTxBody (..),
    ScriptIntegrityHash,

    -- * Re-exports
    module X,
  )
where

import Cardano.Ledger.Alonzo.TxOut (AlonzoEraTxOut)
import Cardano.Ledger.BaseTypes (Network)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.ShelleyMA.Core
  ( Era (..),
    EraIndependentScriptIntegrity,
    EraTxBody (..),
    ShelleyMAEraTxBody,
  )
import qualified Cardano.Ledger.ShelleyMA.Core as X
import Cardano.Ledger.TxIn (TxIn (..))
import Data.Maybe.Strict (StrictMaybe)
import Data.Set (Set)
import Lens.Micro (Lens')

type ScriptIntegrityHash c = SafeHash c EraIndependentScriptIntegrity

class (ShelleyMAEraTxBody era, AlonzoEraTxOut era) => AlonzoEraTxBody era where
  collateralInputsTxBodyL :: Lens' (TxBody era) (Set (TxIn (EraCrypto era)))

  reqSignerHashesTxBodyL :: Lens' (TxBody era) (Set (KeyHash 'Witness (EraCrypto era)))

  scriptIntegrityHashTxBodyL ::
    Lens' (TxBody era) (StrictMaybe (ScriptIntegrityHash (EraCrypto era)))

  networkIdTxBodyL :: Lens' (TxBody era) (StrictMaybe Network)
