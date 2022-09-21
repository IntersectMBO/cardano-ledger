{-# LANGUAGE FlexibleContexts #-}

module Cardano.Ledger.ShelleyMA.Core
  ( ShelleyMAEraTxBody (..),
    module Cardano.Ledger.Shelley.Core,
  )
where

import Cardano.Ledger.Mary.Value (MultiAsset (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.Val (DecodeMint, EncodeMint)
import Data.Set (Set)
import Lens.Micro (Lens', SimpleGetter)

class
  (ShelleyEraTxBody era, EncodeMint (Value era), DecodeMint (Value era)) =>
  ShelleyMAEraTxBody era
  where
  vldtTxBodyL :: Lens' (TxBody era) ValidityInterval

  mintTxBodyL :: Lens' (TxBody era) (MultiAsset (EraCrypto era))

  mintValueTxBodyF :: SimpleGetter (TxBody era) (Value era)

  mintedTxBodyF :: SimpleGetter (TxBody era) (Set (ScriptHash (EraCrypto era)))
