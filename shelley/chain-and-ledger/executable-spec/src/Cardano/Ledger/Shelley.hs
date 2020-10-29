{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Shelley where

import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.Era
import Cardano.Ledger.Val (Val)
import Shelley.Spec.Ledger.Hashing (EraIndependentTxBody, HashAnnotated (..))

--------------------------------------------------------------------------------
-- Shelley Era
--------------------------------------------------------------------------------

type TxBodyConstraints era =
  ( ChainData (TxBody era),
    AnnotatedData (TxBody era),
    HashAnnotated (TxBody era) era,
    HashIndex (TxBody era) ~ EraIndependentTxBody
  )

type ShelleyBased era =
  ( Era era,
    -- Value constraints
    Val (Value era),
    Compactible (Value era),
    ChainData (Value era),
    SerialisableData (Value era),
    SerialisableData (CompactForm (Value era)),
    -- TxBody constraints
    TxBodyConstraints era,
    -- Script constraints
    ChainData (Script era),
    AnnotatedData (Script era)
  )
