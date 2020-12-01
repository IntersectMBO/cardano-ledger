{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Shelley.Constraints where

import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core
  ( AnnotatedData,
    AuxiliaryData,
    ChainData,
    Script,
    SerialisableData,
    TxBody,
    Value,
    UpdateSTS,
    HasUpdateLogic
  )
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.Torsor (Torsor (..))
import Cardano.Ledger.Val (DecodeNonNegative, Val)
import Shelley.Spec.Ledger.Hashing
  ( EraIndependentTxBody,
    HashAnnotated (..),
  )
import Control.State.Transition (STS(State))
import Control.DeepSeq (NFData)
import NoThunks.Class (NoThunks)

--------------------------------------------------------------------------------
-- Shelley Era
--------------------------------------------------------------------------------

type TxBodyConstraints era =
  ( ChainData (TxBody era),
    AnnotatedData (TxBody era),
    HashAnnotated (TxBody era) era,
    HashIndex (TxBody era) ~ EraIndependentTxBody
  )

-- | General constraints that will hold true for ledgers which are based on
-- Shelley, and share similar serialisation formats"
type ShelleyBased era =
  ( Era era,
    -- Value constraints
    Val (Value era),
    Compactible (Value era),
    DecodeNonNegative (Value era),
    ChainData (Value era),
    Eq (CompactForm (Value era)),
    SerialisableData (Value era),
    SerialisableData (CompactForm (Value era)),
    ChainData (Delta (Value era)),
    SerialisableData (Delta (Value era)),
    Torsor (Value era),
    -- STS Constraints
    HasUpdateLogic era,
    Eq (State (UpdateSTS era)),
    Show (State (UpdateSTS era)),
    NFData (State (UpdateSTS era)),
    NoThunks (State (UpdateSTS era)),
    SerialisableData (State (UpdateSTS era)),
    -- TxBody constraints
    TxBodyConstraints era,
    -- Script constraints
    ChainData (Script era),
    AnnotatedData (Script era),
    -- AuxiliaryData constraints
    ChainData (AuxiliaryData era),
    AnnotatedData (AuxiliaryData era)
  )
