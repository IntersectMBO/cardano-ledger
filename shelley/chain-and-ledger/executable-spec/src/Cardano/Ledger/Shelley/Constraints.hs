{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Shelley.Constraints where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core
  ( AnnotatedData,
    AuxiliaryData,
    ChainData,
    Script,
    SerialisableData,
    TxBody,
    TxOut,
    Value,
  )
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Torsor (Torsor (..))
import Cardano.Ledger.Val (DecodeMint, DecodeNonNegative, EncodeMint, Val)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy)
import GHC.Records (HasField)
import Shelley.Spec.Ledger.Address (Addr)
import Shelley.Spec.Ledger.CompactAddr (CompactAddr)
import Shelley.Spec.Ledger.Hashing
  ( EraIndependentTxBody,
    HashAnnotated (..),
  )

--------------------------------------------------------------------------------
-- Shelley Era
--------------------------------------------------------------------------------

type UsesTxBody era =
  ( Era era,
    ChainData (TxBody era),
    AnnotatedData (TxBody era),
    HashAnnotated (TxBody era) era,
    HashIndex (TxBody era) ~ EraIndependentTxBody
  )

class
  ( Era era,
    Val (Value era),
    Compactible (Value era),
    ChainData (Value era),
    ChainData (Delta (Value era)),
    SerialisableData (Value era),
    SerialisableData (Delta (Value era)),
    DecodeNonNegative (Value era),
    EncodeMint (Value era),
    DecodeMint (Value era),
    Torsor (Value era)
  ) =>
  UsesValue era

class
  ( Era era,
    ChainData (TxOut era),
    ToCBOR (TxOut era),
    FromCBOR (TxOut era),
    HasField "address" (TxOut era) (Addr (Crypto era)),
    HasField "compactAddress" (TxOut era) (CompactAddr (Crypto era)),
    HasField "value" (TxOut era) (Value era)
  ) =>
  UsesTxOut era
  where
  makeTxOut :: Proxy era -> Addr (Crypto era) -> Value era -> TxOut era

type UsesScript era =
  ( Era era,
    Eq (Script era),
    Show (Script era),
    AnnotatedData (Script era)
  )

type UsesAuxiliary era =
  ( Era era,
    Eq (AuxiliaryData era),
    Show (AuxiliaryData era),
    AnnotatedData (AuxiliaryData era)
  )

-- | Apply 'c' to all the types transitively involved with Value when
-- (Core.Value era) is an instance of Compactible and Torsor
type TransValue (c :: Type -> Constraint) era =
  ( Era era,
    Compactible (Value era),
    Torsor (Value era),
    c (Value era),
    c (Delta (Value era))
  )

-- | General constraints that will hold true for ledgers which are based on
-- Shelley, and share similar serialisation formats"
type ShelleyBased era =
  ( -- Value constraints
    UsesValue era,
    -- TxBody constraints
    UsesTxBody era,
    -- Script constraints
    UsesScript era,
    -- AuxiliaryData constraints
    UsesAuxiliary era
  )

{-# LANGUAGE Deprecated ShelleyBased "Use appropriate 'Uses' constraits (e.g. `UsesValue`) instead." #-}
