{-# LANGUAGE FlexibleContexts #-}

module Cardano.Ledger.Mary.Core
  ( MaryEraTxBody (..),
    module Cardano.Ledger.Allegra.Core,
  )
where

import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Mary.Value (MultiAsset (..))
import Data.Set (Set)
import Lens.Micro (Lens', SimpleGetter)

class AllegraEraTxBody era => MaryEraTxBody era where
  mintTxBodyL :: Lens' (TxBody era) (MultiAsset (EraCrypto era))

  mintValueTxBodyF :: SimpleGetter (TxBody era) (Value era)

  mintedTxBodyF :: SimpleGetter (TxBody era) (Set (ScriptHash (EraCrypto era)))
