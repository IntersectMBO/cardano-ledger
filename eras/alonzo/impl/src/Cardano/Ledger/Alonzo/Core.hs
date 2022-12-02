{-# LANGUAGE DataKinds #-}

module Cardano.Ledger.Alonzo.Core
  ( AlonzoEraTxBody (..),
    ScriptIntegrityHash,
    module Cardano.Ledger.Mary.Core,
  )
where

import Cardano.Ledger.Alonzo.TxOut (AlonzoEraTxOut)
import Cardano.Ledger.BaseTypes (Network)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.TxIn (TxIn (..))
import Data.Maybe.Strict (StrictMaybe)
import Data.Set (Set)
import Lens.Micro (Lens')

type ScriptIntegrityHash c = SafeHash c EraIndependentScriptIntegrity

class (MaryEraTxBody era, AlonzoEraTxOut era) => AlonzoEraTxBody era where
  collateralInputsTxBodyL :: Lens' (TxBody era) (Set (TxIn (EraCrypto era)))

  reqSignerHashesTxBodyL :: Lens' (TxBody era) (Set (KeyHash 'Witness (EraCrypto era)))

  scriptIntegrityHashTxBodyL ::
    Lens' (TxBody era) (StrictMaybe (ScriptIntegrityHash (EraCrypto era)))

  networkIdTxBodyL :: Lens' (TxBody era) (StrictMaybe Network)
